; -*- lexical-binding:nil -*-

; Simple traversal of buffer-undo-list; undoing and redoing doesn't
; modify buffer-undo-list; undo in region is not supported yet.
; On simple undos:
;	if caret is not at an undo boundry, first undo moves point there;
;	text insertions are marked, if longer than undo-mark-long-insertions chars.
; Simple undo means sequence of insertions or deletions right next to each other.

; TODO Undo/redo constrained to marked region.
; Redo can stay w/o changes I think, only for undos in undo_2 skip entries with
; any changes out of range and count them.

; Some examples on how one buffer-undo-list entry looks after specific actions:
;
; 1 line move up:
; (288 . 376)
; ("
; " . -371)
; (#<marker at 459 in emacsTODO.txt> . -1)
; (#<marker at 459 in emacsTODO.txt> . -1)
; (#<marker at 459 in emacsTODO.txt> . -1)
; (#<marker at 459 in emacsTODO.txt> . -1)
; (#<marker at 350 in emacsTODO.txt> . -1)
; ("sprawdzać czy da się to połączyć (z prawej lub z lewej, bo np. C-backspace robi coś jak" . 372)
; (#<marker at 459 in emacsTODO.txt> . -10)
; (#<marker at 459 in emacsTODO.txt> . -10)
; (#<marker at 459 in emacsTODO.txt> . -10)
; (#<marker at 459 in emacsTODO.txt> . -10)
; (#<marker at 350 in emacsTODO.txt> . -62)
; 434
;
; 4 line duplicate:
; (625 . 877)
; 596
;
; Rectangle insertion:
; (915 . 916)
; (886 . 887)
; (847 . 848)
; (814 . 815)
; 912
;
; Rectangle deletion:
; ("zmieniać wszystkie" . -658)
; (#<marker at 658 in emacsTODO.txt> . -18)
; ("t znacznie lepsze" . -589)
; (" bo mi się wydaje że albo" . -478)
; 409
;
; Insertion with region deletion:
; (356 . 357)
; ("urrent-index nil)" . 356)
; (#<marker at 356 in myUndo.el> . -17)
;
; Grouped deletion of "modtime.|":
; ("modtime" . 1674)
; (#<marker at 1674 in myUndo.el> . -7)
; (#<marker at 1674 in myUndo.el> . -7)
; ("." . 1681)
; (#<marker at 1674 in myUndo.el> . -1)
; (#<marker at 1674 in myUndo.el> . -1)
; 1682

(defconst undo-mark-long-insertions 70
"Must be an integer.
On simple contiguous undos, mark inserted text if it is longer than
`undo-mark-long-insertions' chars.")

(defvar-local undo-saved-list nil)
(defvar-local undo-index nil)
(defvar-local undo-group-count nil)
(defvar-local undo-redo-list nil)

(let (
	(undo
		(lambda (n) (interactive "p")
			(cond
				((= n 0))
				((eq buffer-undo-list t)
					(message "This buffer doesn't record undo info."))
				((not buffer-undo-list)
					(message "Nothing to %sdo." (if (> n 0) "un" "re")))
				; If this is a redo command and there is nothing to redo.
				((and
						(< n 0)
						(not
							(and
								undo-redo-list
								(memq undo-saved-list (list nil buffer-undo-list)))))
					(kill-local-variable 'undo-redo-list)
					(message "Nothing to redo."))
				(t
					(let* (
						(modified (buffer-modified-p))
						; For an indirect buffer, look in the base buffer for the
						; auto-save data.
						(base-buffer (or (buffer-base-buffer) (current-buffer)))
						(recent-save (with-current-buffer base-buffer (recent-auto-save-p)))
						(count 0)
						(group_count 0)
						undo_list
						(undo_1
							(lambda ()
								(let (
									(is_simple_undo
										; Find a place where there is a jump in deleted/inserted
										; text in INSERTIONS_OR_DELETIONS and return nil, if there
										; is no such place - every modification is made next to
										; each other, return t.
										(lambda (insertions_or_deletions)
											(let* (
												(get_place
													(lambda (entry)
														(if (integerp (car entry))
															; Insertion.
															; Caret must've been at the point where text
															; was inserted, so the left border of range
															; that this entry represents.
															(car entry)
															; Deletion.
															; Deletion could happen either from the left
															; or right, so try both values.
															(let ((pos (abs (cdr entry))))
																(cons pos (+ (length (car entry)) pos))))))
												(place (funcall get_place (pop insertions_or_deletions)))
												(result t)
											)
												(while insertions_or_deletions
													(unless
														(let (
															(new_place
																(funcall get_place
																	(pop insertions_or_deletions)))
														)
															(if (integerp place)
																(if (integerp new_place)
																	(= place new_place)
																	(or
																		(= place (car new_place))
																		(= place (cdr new_place))))
																(if (integerp new_place)
																	(or
																		(= (car place) new_place)
																		(= (cdr place) new_place))
																	(or
																		(= (car place) (car new_place))
																		(= (car place) (cdr new_place))
																		(= (cdr place) (car new_place))
																		(= (cdr place) (cdr new_place))))))
														; Found non matching entry - return nil - undo is
														; not simple.
														(setq result nil)
														(setq insertions_or_deletions nil)))
												result)))
									(undo_2
										(lambda ()
											(let (
												(undo-in-progress t)
												(inhibit-read-only t)
												entry
											)
												(while (and (> n 0) undo_list)
													(-- n)
													(++ group_count)
													; Keep the same undo bounds for redo.
													(and
														; Don't add boundary when buffer-undo-list is nil.
														(consp buffer-undo-list)
														; Don't add boundary when (car buffer-undo-list) is already
														; a boundary. It's here for some weird undo entries like
														; only goto-char's or something, so no undo entry will result
														; in undoing such entry. I don't think such entries exist
														; by themselves normally, but I'm not sure it can't happen,
														; so protect against it.
														(car buffer-undo-list)
														(push nil buffer-undo-list))
													(while (progn (++ count) (setq entry (pop undo_list)))
														(cond
															; Handle an integer by setting point to that value.
															((integerp entry) (goto-char entry))
															; Element (t . TIME) records previous modtime.
															; Preserve any flag of NONEXISTENT_MODTIME_NSECS or
															; UNKNOWN_MODTIME_NSECS.
															((eq (car entry) t)
																; If this records an obsolete save
																; (not matching the actual disk file)
																; then don't mark unmodified.
																(let ((visited-file-time (visited-file-modtime)))
																	; Indirect buffers don't have a visited file, so their
																	; file-modtime can be bogus. In that case, use the
																	; modtime of the base buffer instead.
																	(and
																		(numberp visited-file-time)
																		(= visited-file-time 0)
																		(buffer-base-buffer)
																		(setq visited-file-time
																			(with-current-buffer (buffer-base-buffer)
																				(visited-file-modtime))))
																	(when (time-equal-p (cdr entry) visited-file-time)
																		(unlock-buffer)
																		(set-buffer-modified-p nil))))
															; Element (nil PROP VAL BEG . END) is a property change.
															((null (car entry))
																(let (
																	(prop (nth 1 entry))
																	(value (nth 2 entry))
																	(beg (nth 3 entry))
																	(end (nthcdr 4 entry))
																)
																	(when (or (> (point-min) beg) (< (point-max) end))
																		(error
																			"Changes to be undone are outside visible portion of buffer"))
																	(put-text-property beg end prop value)))
															; Element (BEG . END) means range was inserted.
															((integerp (car entry))
																(let ((beg (car entry)) (end (cdr entry)))
																	(when (or (> (point-min) beg) (< (point-max) end))
																		(error
																			"Changes to be undone are outside visible portion of buffer"))
																	; Jump to the place where action happens.
																	; but only if we are not at this place already -
																	; if possible, don't modify point to preserve
																	; info about it in undo list.
																	(cond
																		((> (point) end) (goto-char end))
																		((< (point) beg) (goto-char beg)))
																	(delete-region beg end)))
															; Element (apply FUN . ARGS) means call FUN to undo.
															((eq (car entry) 'apply)
																(let (
																	(currbuff (current-buffer))
																	(fun-args (cdr entry))
																)
																	(if (integerp (car fun-args))
																		; Long format: (apply DELTA START END FUN . ARGS).
																		(let* (
																			(delta (pop fun-args))
																			(start (pop fun-args))
																			(end (pop fun-args))
																			(start-mark (copy-marker start))
																			(end-mark (copy-marker end t))
																		)
																			(when (or (> (point-min) start) (< (point-max) end))
																				(error
																					"Changes to be undone are outside visible portion of buffer"))
																			(apply fun-args)
																			; Check that the function did what the entry
																			; said it would do.
																			(unless (and (= start start-mark) (= (+ delta end) end-mark))
																				(error
																					"Changes undone by function are different from the announced ones"))
																			(set-marker start-mark nil)
																			(set-marker end-mark nil))
																		(apply fun-args))
																	(unless (eq currbuff (current-buffer))
																		(error "Undo function switched buffer"))))
															; Element (STRING . POS) means STRING was deleted.
															(t
																(let* (
																	(string (car entry))
																	(pos (cdr entry))
																	(apos (abs pos))
																	valid-marker-adjustments
																)
																	(when (or (< apos (point-min)) (> apos (point-max)))
																		(error
																			"Changes to be undone are outside visible portion of buffer"))
																	; Check that marker adjustments which were recorded
																	; with the (STRING . POS) record are still valid, ie
																	; the markers haven't moved. We check their validity
																	; before reinserting the string so as we don't need to
																	; mind marker insertion-type.
																	(while
																		(and
																			(markerp (car-safe (car undo_list)))
																			(integerp (cdr (car undo_list))))
																		(++ count)
																		(let* (
																			(marker-adj (pop undo_list))
																			(m (car marker-adj))
																		)
																			(and
																				(eq (marker-buffer m) (current-buffer))
																				(= apos m)
																				(push marker-adj valid-marker-adjustments))))
																	; Insert string.
																	; Leave point on the side where it was before
																	; deleting string.
																	(goto-char apos)
																	(insert string)
																	(when (> pos 0) (goto-char apos))
																	; Adjust the valid marker adjustments.
																	(dolist (adj valid-marker-adjustments)
																		; Insert might have invalidated some of the markers
																		; via modification hooks. Update only the currently
																		; valid ones.
																		(when (marker-buffer (car adj))
																			(set-marker
																				(car adj) (- (car adj) (cdr adj)))))))))))))
								)
									; On single simple undos goto place of undo or mark
									; inserted text if any and there was no undoing of insertions.
									(when (= n 1)
										(let (insertions_or_deletions)
											; Fill insertions_or_deletions.
											(let ((undo_list_1 undo_list))
												(while (setq entry (pop undo_list_1))
													(and
														(not (integerp entry))
														(or (integerp (car entry)) (stringp (car entry)))
														(push entry insertions_or_deletions))))
											(setq insertions_or_deletions
												(nreverse insertions_or_deletions))
											(cond
												; If this isn't a normal deletion or insertion
												; containing undo.
												((not insertions_or_deletions))
												; If caret isn't in the region of insertion,
												; or isn't at the point of deletion.
												((not
														(let ((entry (car insertions_or_deletions)))
															(if (integerp (car entry))
																(in_range_inclusive
																	(point) (car entry) (cdr entry))
																(= (point) (abs (cdr entry))))))
													(when (funcall is_simple_undo insertions_or_deletions)
														; Move point to the place of undo.
														; Prefer right border on insertions,
														; for some minor reasons, like cursor normally
														; being moved to the right after insertions,
														; so it probably was there when this insertion
														; happened.
														(goto-char (abs (cdr (car insertions_or_deletions))))
														(setq n 0)))
												; If undo contains no insertions and is simple.
												((and
														(not
															(find_in_list insertions_or_deletions
																(lambda (entry) (integerp (car entry)))))
														(funcall is_simple_undo insertions_or_deletions))
													(let (
														(len
															(cl-loop
																for entry in insertions_or_deletions
																sum (length (car entry))))
													)
														; Only mark text longer than
														; undo-mark-long-insertions chars.
														(when (> len undo-mark-long-insertions)
															; Undo this entry and mark inserted text.
															(let ((caret (point)))
																(funcall undo_2)
																(if (= (point) caret)
																	(set-marker (mark-marker)
																		(+ caret len))
																	; Else move to point to the end of
																	; inserted text, even if point was
																	; moved by some integer entry, to
																	; the inside of the inserted text.
																	; This makes it different from
																	; normal undos then, not only by
																	; mark activation, but also by
																	; this caret movement.
																	(goto-char (+ caret len))
																	(set-marker (mark-marker) caret)))
															(setq deactivate-mark nil)
															(activate-mark)))))))
									(funcall undo_2))))
					)
						(if (> n 0)
							(progn
								; If this command starts a new undo chain.
								(unless (eq undo-saved-list buffer-undo-list)
									(kill-local-variable 'undo-redo-list)
									(when undo-saved-list
										(setcdr undo-saved-list
											(nthcdr undo-index undo-saved-list)))
									(setq undo-saved-list buffer-undo-list)
									(setq undo-index 1)
									(setq undo-group-count 0)
									(jumpHistory::add))
								(setq undo_list (nthcdr undo-index buffer-undo-list))
								(if (not undo_list)
									(message "Nothing to undo.")
									(setq deactivate-mark t)
									(let (buffer-undo-list)
										(funcall undo_1)
										(setq buffer-undo-list
											(cl-delete-if
												(lambda (entry)
													(or
														(integerp entry)
														(eq (car entry) t)))
												buffer-undo-list))
										(when buffer-undo-list
											(setq undo-redo-list
												(nconc
													buffer-undo-list
													(when undo-redo-list (list nil))
													undo-redo-list))))
									(+= undo-index count)
									(+= undo-group-count group_count)))
							(setq deactivate-mark t)
							(setq n (- n))
							(setq undo_list undo-redo-list)
							(let ((buffer-undo-list t)) (funcall undo_1))
							; Discard undone entries.
							(setq undo-redo-list (nthcdr count undo-redo-list))
							; Move undo-index group_count groups back.
							(when (/= group_count 0)
								(cl-loop
									with desired_group_count =
										(- undo-group-count group_count)
									with group_count_1 = 0
									for entry in (cdr buffer-undo-list)
									for i from 1
									while (< group_count_1 desired_group_count)
									unless entry do (++ group_count_1)
									finally
									(setq undo-index i)
									(setq undo-group-count group_count_1))))
						; Record what the current undo list says, so the next command
						; can tell if the buffer was modified in between.
						(and
							modified
							(not (buffer-modified-p))
							(with-current-buffer base-buffer
								(delete-auto-save-file-if-necessary recent-save))))
					(funcall after_move_hook_fn)))))
)
	(define-key global-map [?\C-z] (fn_symbol "key::undo" undo))
	(define-key global-map [?\C-\S-z] (fn_symbol "key::redo" (get_reverse_command undo))))

(provide 'myUndo)
