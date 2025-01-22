; -*- lexical-binding:nil -*-

(setq copy-region-blink-delay 0)

; Kill ring replacement.

; This still uses kill-transform-function, yank-transform-functions and
; kill-ring-max from kill-ring package.

(defvar clipboard-list nil
"Like `kill-ring', but every element should be a list:
(string_to_paste origin_string [paste_handler_fn] [paste_handler_arg]), where:
	origin_string should be a buffer name that string_to_paste comes from,
	or \"interprogram\", etc.

	paste_handler_fn is called with (or paste_handler_arg string_to_paste),
	like yank-handler. It should support current-prefix-arg and call
	after_move_hook_fn at the end. It doesn't need to worry about read-only text,
	it is handled elsewhere.

Use `clipboard-add' to add entries to this list.")

(defconst clipboard-delete-duplicates-count 20
"Number of newest elements in `clipboard-list' to check if one of them
is a duplicate of entry being added.")

(defun clipboard-maybe-add-rear-nonsticky-to-char-before ()
	(and
		(not (bobp))
		(text-properties-at (1- (point)))
		(let (deactivate-mark)
			(put-text-property (1- (point)) (point) 'rear-nonsticky t)
			t)))

(defun clipboard-add (str &optional origin_str paste_handler_fn paste_handler_arg replace)
"Add a new entry to `clipboard-list'.
ORIGIN_STR defaults to (buffer-name).
REPLACE non-nil means to replace the first element of `clipboard-list' instead
of appending a new entry."
	; Useless info: After this function, clipboard-list may be one element
	; shorter, longer, or the same, because of duplicates, replace parameter,
	; and clipboard-list being empty or full.
	(when
		(and
			; Allow the user to transform or ignore the string.
			(or
				(not kill-transform-function)
				(setq str (funcall kill-transform-function str)))
			(not (string= str "")))
		; Save to system's clipboard.
		(when interprogram-cut-function (funcall interprogram-cut-function str))
		(unless origin_str (setq origin_str (buffer-name)))
		(let (
			(get_clipboard_entry
				(lambda ()
					(let ((clipboard_entry (list str origin_str)))
						(when paste_handler_fn
							(nconc
								clipboard_entry
								(if paste_handler_arg
									(list paste_handler_fn paste_handler_arg)
									(list paste_handler_fn))))
						clipboard_entry)))
		)
			; Handle duplicates.
			(if-let (
				(duplicate_index
					(index_in_list
						(take clipboard-delete-duplicates-count clipboard-list)
						(lambda (clipboard_entry_1)
							; I used equal-including-properties, but that too often
							; leaved duplicates with almost meaningless differences
							; like 'fontified prop or something, so just use string=.
							; If kill-transform-function or filter-buffer-substring
							; functions were "better" supported by major modes, then
							; that wouldn't be necessary.
							; It's a very small problem anyway, if it even is a
							; problem.
							(string= str (car clipboard_entry_1)))))
			)
				(let ((duplicate (nth duplicate_index clipboard-list)))
					(setcar duplicate str)
					(let ((duplicate_1 (cdr duplicate)))
						; Prepend duplicate's origin_str to clipboard_entry's origin_str.
						; It is assummed that clipboard_entry's origin_str only has one
						; origin.
						; If it is already in base_origin_str, don't append it.
						(unless (string-search origin_str (car duplicate_1))
							(setcar duplicate_1
								(concat (car duplicate_1) ", " origin_str)))
						(setcdr duplicate_1
							(when paste_handler_fn
								(if paste_handler_arg
									(list paste_handler_fn paste_handler_arg)
									(list paste_handler_fn)))))
					; If duplicate is the first element and REPLACE is non-nil, don't
					; delete it and replace the second element. Idk, maybe that's wrong,
					; but if the point is to cycle first element then it would be weird
					; if this suddenly deleted the second element.
					(when (/= duplicate_index 0)
						; Delete duplicate.
						(delete_from_list_by_index clipboard-list duplicate_index)
						(if replace
							(setcar clipboard-list duplicate)
							; No point in using ntake here,
							; because number of elements stays the same.
							(push duplicate clipboard-list))))
				(if (and replace clipboard-list)
					(setcar clipboard-list (funcall get_clipboard_entry))
					(push (funcall get_clipboard_entry) clipboard-list)
					(setq clipboard-list (ntake kill-ring-max clipboard-list))))))
	nil)

(defun clipboard-default-paste-handler (str)
"Default insert function used when paste_handler_fn in `clipboard-list' is nil.
Paste and indent every line (maybe omit first one) using `indent-according-to-mode'."
	(let ((n (prefix-numeric-value current-prefix-arg)))
		(when (> n 0)
			(maybe_delete_marked_region)
			(jumpHistory::add)
			(let (
				(start (point))
				(is_first_line_blank_to_start (is_line_blank_to_start))
			)
				(while (>= (-- n) 0) (insert-and-inherit str))
				; Marker staying after inserted text.
				(let ((end_marker (copy-marker (point) t)))
					(goto-char start)
					; Indent first line only if it has only blanks before caret.
					(when is_first_line_blank_to_start (indent-according-to-mode))
					(forward-line 1)
					; Not <= because it's pretty useless to indent last empty line,
					; it's probably there by mistake anyway.
					(while (< (point) end_marker)
						(indent-according-to-mode)
						(forward-line 1))
					(goto-char end_marker)
					(set-marker end_marker nil)))
			(funcall after_move_hook_fn)))
	nil)

(defun clipboard-transform-str (str) "Pass STR through `yank-transform-functions'."
	(run-hook-wrapped
		'yank-transform-functions (lambda (fn) (setq str (funcall fn str)) nil))
	str)

(defvar clipboard-paste-fn
	(lambda (clipboard_entry)
		(funcall (or (nth 2 clipboard_entry) #'clipboard-default-paste-handler)
			(or
				(nth 3 clipboard_entry)
				(clipboard-transform-str (car clipboard_entry)))))
"Variable because rectangle::mode overrides this (not a great solution).
This function shouldn't call after_move_hook_fn.
It is called only if buffer is not read only, though text under caret can be
read only by text properties, so it is also wrapped in condition-case
buffer-read-only.")

(defun clipboard-raw-paste (str)
"Paste as plain text, without indenting or other stuff."
	(let ((n (prefix-numeric-value current-prefix-arg)))
		(when (> n 0)
			(setq str (clipboard-transform-str str))
			(maybe_delete_marked_region)
			(jumpHistory::add)
			(while (>= (-- n) 0) (insert-and-inherit str))
			(funcall after_move_hook_fn))))

(defun clipboard-get-entry-to-paste ()
"Return clipboard entry to paste - interprogram-paste, (car clipboard-list) or nil.
Also add interprogram clipboard string to `clipboard-list' if there is any."
	(when-let* (
		interprogram-paste-function
		(interprogram_paste (funcall interprogram-paste-function))
	)
		; Disable the interprogram cut function when we add the new
		; text to the kill ring, so Emacs doesn't try to own the selection,
		; with identical text.
		; Also disable the interprogram paste function,
		; so that kill-new doesn't call it repeatedly.
		(let (
			interprogram-cut-function
			interprogram-paste-function
			(add_interprogram_str (lambda (str) (clipboard-add str "Interprogram")))
		)
			(if (listp interprogram_paste)
				; Use reverse to avoid modifying external data.
				(mapc add_interprogram_str (reverse interprogram_paste))
				(funcall add_interprogram_str interprogram_paste))))
	(car clipboard-list))

; Sync interprogram-paste on emacs startup.
(clipboard-get-entry-to-paste)

(let (
	(paste
		(fn_symbol "key::paste"
			(lambda () (interactive)
				(if-let ((clipboard_entry (clipboard-get-entry-to-paste)))
					(progn
						(barf-if-buffer-read-only)
						(undo-auto-amalgamate)
						(condition-case err
							(funcall clipboard-paste-fn clipboard_entry)
							(buffer-read-only
								(message "%s." (error-message-string err))
								(funcall after_move_hook_fn))))
					(message "Clipboard is empty.")))))
)
	(define-key global-map [?\C-v] paste)
	; Paste as plain text.
	(define-key global-map [?\A-v]
		(fn_symbol "key::paste_plain"
			`(lambda () (interactive)
				(setq this-command ',paste)
				(let (
					(clipboard-paste-fn
						(lambda (clipboard_entry)
							(clipboard-raw-paste (car clipboard_entry))))
				)
					(,paste))))))

(defun clipboard-line-paste-handler (str)
"Insert STR before current line and indent every inserted line."
	(let (
		(n (prefix-numeric-value current-prefix-arg))
		(insert_str_indent_every_line_return_end_pos
			(lambda ()
				(jumpHistory::add)
				(let ((start (point)))
					(while (>= (-- n) 0) (insert-and-inherit str))
					; Marker staying after inserted text.
					(let ((end_marker (copy-marker (point) t)))
						(goto-char start)
						(while
							(progn
								(indent-according-to-mode)
								(forward-line 1)
								; Not <= because it's pretty useless to indent last
								; empty line, it's probably there by mistake anyway.
								(< (point) end_marker)))
						(prog1 (marker-position end_marker)
							(set-marker end_marker nil))))))
	)
		(when (> n 0)
			(cond
				(mark-active
					(let ((marked_line_region (region::marked_line_region)))
						(delete-region (car marked_line_region) (cdr marked_line_region)))
					(deactivate-mark)
					(goto-char (funcall insert_str_indent_every_line_return_end_pos))
					(funcall after_move_hook_fn))
				; This is completely optional, it's sometimes nicer to "ignore" blank
				; line.
				((and (is_line_blank_to_start) (is_line_blank_to_end))
					(delete-region (pos-bol) (pos-eol))
					(goto-char (funcall insert_str_indent_every_line_return_end_pos))
					(funcall after_move_hook_fn))
				(t
					(let ((caret_marker (copy-marker (point) t)))
						(forward-line 0)
						(insert ?\n)
						(forward-char -1)
						(funcall insert_str_indent_every_line_return_end_pos)
						(goto-char caret_marker)
						(set-marker caret_marker nil))
					; Position in line should be preserved - we just inserted on line
					; before current line, so current line and caret position in it
					; is unchanged.
					(let (goal_column)
						(funcall after_move_hook_fn current_column)))))))

(let (
	(copy_or_cut_then_duplicate_then_save_to_clipboard
		(lambda (start end arg delete)
			(let ((str (filter-buffer-substring start end delete)))
				(when (> (setq arg (prefix-numeric-value arg)) 1)
					(let ((str_1 str))
						(dotimes (_ (1- arg))
							(setq str (concat str str_1)))))
				(clipboard-add str))
			nil))
)

	; Copy/cut line or marked region.
	(let (
		(copy_or_cut
			(fn_symbol "key::copy"
				`(lambda (arg &optional delete) (interactive "P")
					; If mark is active, still copy/cut marked region,
					; but then concat N - 1 copies of copied/cut string if N is > 1;
					; else use prefix to copy/cut n lines forward;
					(if mark-active
						(,copy_or_cut_then_duplicate_then_save_to_clipboard
							(region-beginning) (region-end) arg delete)
						(let (start end)
							(cond
								((or
										(not arg)
										(= (setq arg (prefix-numeric-value arg)) 0))
									(setq start (pos-bol) end (pos-eol)))
								((> n 0) (setq start (pos-bol) end (pos-eol (1+ n))))
								(t (setq start (pos-bol (1+ n)) end (pos-eol))))
							(clipboard-add
								(filter-buffer-substring start end delete)
								nil
								#'clipboard-line-paste-handler))))))
	)
		(define-key global-map [?\C-c] copy_or_cut)
		(define-key global-map [?\C-x]
			(fn_symbol "key::cut"
				`(lambda (arg) (interactive "P")
					(barf-if-buffer-read-only)
					(or mark-active goal_column (set_goal_column))
					(,copy_or_cut arg t)
					(if (or mark-active (eobp))
						(funcall after_move_hook_fn)
						; Change 1 to -1 to move line backward instead of forward.
						(delete-char 1)
						; Preserve column and hscroll.
						(goto_goal_column)
						(let (goal_column) (funcall after_move_hook_fn)))))))

	; Copy/cut highlighted paren region, closest region or marked line region.
	(let (
		(shift_copy_or_cut
			(fn_symbol "key::copy_shift"
				`(lambda (arg &optional delete) (interactive "P")
					; If mark is active, use prefix to copy/cut N * marked_line_count
					; lines forward; else still copy/cut region at point, but then
					; concat N - 1 copies of copied/cut string if N is > 1.
					(if mark-active
						(let ((region (region::marked_line_region)))
							(and
								arg
								(/= (setq arg (prefix-numeric-value arg)) 0)
								(let (
									(line_count
										(1+
											(if (> (point) (mark))
												(line_count_backward (mark))
												(line_count_forward (mark)))))
								)
									(if (> arg 0)
										(save-excursion
											(goto-char (cdr region))
											(setcdr region
												(pos-eol (1+ (* line_count arg)))))
										(save-excursion
											(goto-char (car region))
											(setcar region
												(pos-bol (1+ (* line_count arg))))))))
							(clipboard-add
								(filter-buffer-substring (car region) (cdr region) delete)
								nil
								#'clipboard-line-paste-handler))
						(when-let (
							(region
								(or
									(region::paren_at_caret_region_max)
									(region::closest_region_max)))
						)
							(,copy_or_cut_then_duplicate_then_save_to_clipboard
								(car region) (cdr region) arg delete))))))
	)
		(define-key global-map [?\C-\S-c] shift_copy_or_cut)
		(define-key global-map [?\C-\S-x]
			(fn_symbol "key::cut_shift"
				`(lambda (arg) (interactive "P")
					(barf-if-buffer-read-only)
					(and mark-active (not goal_column) (set_goal_column))
					(,shift_copy_or_cut arg t)
					(if (or (not mark-active) (eobp))
						(funcall after_move_hook_fn)
						; Change 1 to -1 to move line backward instead of forward.
						(delete-char 1)
						; Preserve column and hscroll.
						(goto_goal_column)
						(let (goal_column) (funcall after_move_hook_fn))))))))

; ==================================== Helm interface ====================================

(define_face 'clipboard-helm-short-representation-face '((t :foreground "#08A0F7"))
"Face used by strings representing newlines, blanks and too long string -
<n>, <s> and ....")
(defconst clipboard-helm-preview-buffer-name "*helm-copy-paste-preview*")
(defconst clipboard-helm-short-string-max-length 110
"Max length of short representation strings.
It was previously set to (window-width (helm-window)) on helm startup,
but it's generally not ideal as helm window can be resized, so it's
probably better to render a little more than half of frame's width.")

; In some places it's necessary to use window-live-p instead of just non-nil test,
; because user can delete this window at any point during helm session.
(defvar clipboard-helm-preview-window nil)

(defun clipboard-helm-preview-window-selection-change-local (window)
"WINDOW is always `clipboard-helm-preview-window'.
If preview window has just been selected,
activate highlightLine::mode and highlightParens::auto::mode,
if it has been deselected, deactivate them."
	; Selected window is the one selected just now.
	(if (not (eq window (selected-window)))
		(with-selected-window window
			(when highlightLine::mode (highlightLine::mode -1))
			(when highlightParens::auto::mode (highlightParens::auto::mode -1)))
		(unless highlightLine::mode
			(highlightLine::mode)
			; See highlightLine::mode doc on why this call is necessary.
			(highlightLine::highlight window))
		(unless highlightParens::auto::mode (highlightParens::auto::mode))))

(defconst clipboard-helm-preview-buffer-local-keymap
	(let ((keymap (make-sparse-keymap)))
		; Confirm changes done in preview buffer and save this modified
		; candidate in clipboard-list.
		; This is very rarely useful, and also this doesn't modify paste_handler_arg,
		; so it can be pasted incorrectly if that's the case.
		(define-key keymap [?\C-g]
			(lambda () (interactive)
				; We may be in an empty buffer and selection may not exist.
				(when-let ((cand (helm-get-selection)))
					(let (
						(cand_index (get-text-property 0 'clipboard-helm-index cand))
						(candidates
							(gethash clipboard-helm-source helm-candidate-cache))
						(str (filter-buffer-substring (point-min) (point-max)))
					)
						(remove-text-properties 0 1 '(clipboard-helm-index) str)
						(setcar (nth cand_index clipboard-list) str)
						; Save to system's clipboard.
						(and
							(= cand_index 0)
							interprogram-cut-function
							(funcall interprogram-cut-function str))
						(let ((cand_in_helm (nth cand_index candidates)))
							(setcar cand_in_helm (clipboard-helm-string-to-display str))
							(setcdr cand_in_helm
								(let ((str_copy (copy-sequence str)))
									(put-text-property
										0 1 'clipboard-helm-index cand_index str_copy)
									str_copy)))
						; Don't run persistent-action, that would erase buffer and
						; insert string, moving cursor in the process.
						(let ((follow (helm-source-follow clipboard-helm-source)))
							(setf (helm-source-follow clipboard-helm-source) nil)
							(helm-force-update cand_index t)
							(setf (helm-source-follow clipboard-helm-source) follow))
						(message "Clipboard history updated.")))))
		keymap))

(defun clipboard-helm-string-to-display (str)
"Return string with newlines as \"<n>\" and blanks other than one space as \"<s>\".
str's max width and length is clipboard-helm-short-string-max-length.
If it would be longer than that, append \"...\" to it
(now useless, but why not, see clipboard-helm-short-string-max-length docs why).
Don't modify str."
	(let* (
		(str_length (length str))
		(newline_count 0)
		; Holds pairs of indexes indicating start and end of blank.
		; Start is index of first blank char and end is index of first
		; non-blank char after start (or str_length).
		(blank_index_vector (make-vector (* 2 str_length) 0))
		(blank_index_vector_length 0)
		(blank_length 0) ; Char count that summed blanks occupy.
		(i 0)
		(set_blank_end
			(lambda ()
				(++ blank_index_vector_length)
				(while (and (< (++ i) str_length) (find_in_vector_= " \t" (aref str i))))
				(+= blank_length
					(- i (aref blank_index_vector (1- blank_index_vector_length))))
				(aset blank_index_vector blank_index_vector_length i)
				(++ blank_index_vector_length)))
	)
		(while (< i str_length)
			(cond
				((= ?\n (aref str i))
					(++ newline_count)
					(++ i))
				((= ?\t (aref str i))
					(aset blank_index_vector blank_index_vector_length i)
					(funcall set_blank_end))
				((and
						(= ?\s (aref str i))
						(/= (1+ i) str_length)
						(find_in_vector_= " \t" (aref str (1+ i))))
					(aset blank_index_vector blank_index_vector_length i)
					(++ i)
					(funcall set_blank_end))
				(t (++ i))))
		(setq i 0)
		(let (string_to_display string_to_display_length)
			(let (
				(string_to_display_full_length
					(+
						str_length
						(* 2 newline_count)
						(- blank_length)
						(* 3 (/ blank_index_vector_length 2))))
			)
				(if
					(<=
						string_to_display_full_length
						clipboard-helm-short-string-max-length)
					(setq
						string_to_display_length string_to_display_full_length
						string_to_display (make-string string_to_display_length ?\s t))
					; Cut string to fit in helm-window and set it's last 3 chars
					; to "..." with special face.
					(setq string_to_display_length
						(- clipboard-helm-short-string-max-length 3))
					(setq string_to_display
						(make-string clipboard-helm-short-string-max-length ?\s t))
					(set_string_part string_to_display "..." string_to_display_length)
					(set-text-properties
						string_to_display_length
						clipboard-helm-short-string-max-length
						'(face clipboard-helm-short-representation-face)
						string_to_display)))
			(let (
				(string_to_display_i 0)
				(blank_i 0)
				(set_string_part_or_exit_loop
					(lambda (str)
						(if (>= (+ 2 string_to_display_i) string_to_display_length)
							(setq string_to_display_i string_to_display_length)
							(set_string_part string_to_display str string_to_display_i)
							(set-text-properties
								string_to_display_i
								(+ 3 string_to_display_i)
								'(face clipboard-helm-short-representation-face)
								string_to_display)
							(+= string_to_display_i 2))))
			)
				(while (< string_to_display_i string_to_display_length)
					(cond
						((= (aref str i) ?\n)
							(funcall set_string_part_or_exit_loop "<n>")
							(++ i))
						; If i = blank start, skip to blank end.
						((and
								(< blank_i blank_index_vector_length)
								(= (aref blank_index_vector blank_i) i))
							(funcall set_string_part_or_exit_loop "<s>")
							(setq i (aref blank_index_vector (++ blank_i)))
							(++ blank_i))
						(t
							(aset string_to_display string_to_display_i (aref str i))
							(set-text-properties
								string_to_display_i
								(1+ string_to_display_i)
								(text-properties-at i str)
								string_to_display)
							(++ i)))
					(++ string_to_display_i)))
			string_to_display)))

(defun clipboard-helm-after-window-setup ()
	; This window is never marked 'no-other-window t,
	; so keybind in global-map for other-window is not changed
	; and should be used to switch to this window.

	; Let-bind window-min-width and height to a larger value,
	; so this window won't be shown if there is too little space.
	; These values are customizable.
	(if
		(setq clipboard-helm-preview-window
			(let (
				(window-min-width 80)
				(window-min-height 8)
			)
				(window::split_window_sensibly)))
		(let ((preview_buffer (get-buffer-create clipboard-helm-preview-buffer-name)))
			(with-current-buffer preview_buffer
				; When window isn't selected, don't display cursor there.
				(setq cursor-in-non-selected-windows nil)

				(setq-local window-selection-change-functions
					(cons
						#'clipboard-helm-preview-window-selection-change-local
						window-selection-change-functions))
				(use-local-map clipboard-helm-preview-buffer-local-keymap))
			(set-window-buffer clipboard-helm-preview-window preview_buffer)
			(set-window-dedicated-p clipboard-helm-preview-window t)
			(setq helm-onewindow-p nil)
			(setf (helm-source-follow clipboard-helm-source) t))
		(setf (helm-source-follow clipboard-helm-source) nil))
	; Important to not leave this on for nested helm calls,
	; for example helm-minibuffer-history.
	(remove-hook 'helm-after-window-setup-hook #'clipboard-helm-after-window-setup)
	nil)

(defun clipboard-helm-after-update ()
"Clean `clipboard-helm-preview-window's buffer if follow mode is on and
there is no candidate at point.
It's just to keep preview window visually in sync with what is in helm-buffer."
	(and
		(helm-empty-buffer-p)
		(eq helm-default-source clipboard-helm-source)
		(helm-source-follow clipboard-helm-source)
		(window-live-p clipboard-helm-preview-window)
		(not (is_buffer_empty (window-buffer clipboard-helm-preview-window)))
		(with-current-buffer (window-buffer clipboard-helm-preview-window)
			(erase-buffer))))

(defun clipboard-helm-paste-action (str &optional raw)
	(if (and buffer-read-only (not inhibit-read-only))
		(message "Buffer is read-only.")
		(let* (
			(index (get-text-property 0 'clipboard-helm-index str))
			(clipboard_entry (nth index clipboard-list))
		)
			(condition-case err
				(if raw
					(clipboard-raw-paste (car clipboard_entry))
					(funcall clipboard-paste-fn clipboard_entry))
				(buffer-read-only
					(message "%s." (error-message-string err))
					(funcall after_move_hook_fn)))
			; Move clipboard_entry to the first place, if it isn't on it already.
			(when (/= index 0)
				; Save to system's clipboard.
				(when interprogram-cut-function
					(funcall interprogram-cut-function (car clipboard_entry)))
				(delete_from_list_by_index clipboard-list index)
				(push clipboard_entry clipboard-list))))
	nil)

(defun clipboard-helm-raw-paste-action (str) (clipboard-helm-paste-action str t))

(defun clipboard-helm-paste-without-move-action (str &optional raw)
	(if (and buffer-read-only (not inhibit-read-only))
		(message "Buffer is read-only.")
		(let (
			(clipboard_entry
				(nth (get-text-property 0 'clipboard-helm-index str) clipboard-list))
		)
			(condition-case err
				(if raw
					(clipboard-raw-paste (car clipboard_entry))
					(funcall clipboard-paste-fn clipboard_entry))
				(buffer-read-only
					(message "%s." (error-message-string err))
					(funcall after_move_hook_fn)))))
	nil)

(defun clipboard-helm-raw-paste-without-move-action (str)
	(clipboard-helm-paste-without-move-action str t))

(defun clipboard-helm-sort-by-highest-index (candidates)
	(sort candidates
		(lambda (c_1 c_2)
			(>
				(get-text-property 0 'clipboard-helm-index c_1)
				(get-text-property 0 'clipboard-helm-index c_2)))))

(defconst clipboard-helm-source
	(helm-source-sync-make nil
		:name "Clipboard"
		:match-on-real t
		:follow t
		:sort nil ; Keep in order of recency.
		:mode-line
			(lambda ()
				(when-let ((real (get-text-property (point) 'helm-real)))
					(concat
						"  From: "
						(nth
							1
							(nth
								(get-text-property 0 'clipboard-helm-index real)
								clipboard-list))
						"  Chars: " (number-to-string (length real))
						"  Lines: " (number-to-string (1+ (cl-count ?\n real))))))
		:persistent-action
			(lambda (str)
				(when (window-live-p clipboard-helm-preview-window)
					(with-current-buffer (window-buffer clipboard-helm-preview-window)
						(erase-buffer)
						(insert str))))
		:candidates
			(lambda ()
				(let ((string_index 0))
					(mapcar
						(lambda (clipboard_entry)
							(cons
								(clipboard-helm-string-to-display (car clipboard_entry))
								(let ((str_copy (copy-sequence (car clipboard_entry))))
									(put-text-property
										0 1 'clipboard-helm-index string_index str_copy)
									(++ string_index)
									str_copy)))
						clipboard-list)))
		:keymap
			(let ((keymap (make-sparse-keymap)))
				(set-keymap-parent keymap helm-map)
				(define-key keymap [A-return]
					(helm-make-action-command #'clipboard-helm-raw-paste-action))
				(define-key keymap [S-return]
					(helm-make-action-command #'clipboard-helm-paste-without-move-action))
				(define-key keymap [A-S-return]
					(helm-make-action-command #'clipboard-helm-raw-paste-without-move-action))
				; Reorder marked.
				(define-key keymap [?\C-r]
					(lambda () (interactive)
						(let (
							(candidates (gethash clipboard-helm-source helm-candidate-cache))
						)
							(let ((i 0))
								(dolist (
									cand
									(clipboard-helm-sort-by-highest-index
										(helm-marked-candidates))
								)
									(let (
										(index
											(+
												(get-text-property
													0 'clipboard-helm-index cand)
												i))
									)
										(push (nth index clipboard-list) clipboard-list)
										(push (nth index candidates) candidates)
										(++ index)
										(delete_from_list_by_index clipboard-list index)
										(delete_from_list_by_index candidates index))
									(++ i)))
							(let ((i 0))
								(dolist (cand candidates)
									(put-text-property
										0 1 'clipboard-helm-index i (cdr cand))
									(++ i)))
							(puthash clipboard-helm-source candidates helm-candidate-cache))
						; Save to system's clipboard.
						(and
							clipboard-list
							interprogram-cut-function
							(funcall interprogram-cut-function
								(car (car clipboard-list))))
						(helm-force-update)))
				(helm-add-delete-binds
					keymap
					(lambda (cand cache)
						; Candidates are sorted from the one with highest value of
						; clipboard-helm-index, so deleting them without adding any
						; offset is fine.
						(let ((index (get-text-property 0 'clipboard-helm-index cand)))
							(if (= index 0)
								(setq
									clipboard-list (cdr clipboard-list)
									cache (cdr cache))
								(delete_from_list_by_index clipboard-list index)
								(delete_from_list_by_index cache index)))
						cache)
					t
					(lambda (cache)
						(let ((i 0))
							(dolist (cand cache)
								(put-text-property 0 1 'clipboard-helm-index i (cdr cand))
								(++ i)))
						cache)
					#'clipboard-helm-sort-by-highest-index)
				keymap)
		:action
			(list
				(cons "Paste" #'clipboard-helm-paste-action)
				(cons "Paste as plain text" #'clipboard-helm-raw-paste-action)
				(cons "Paste without moving" #'clipboard-helm-paste-without-move-action)
				(cons
					"Paste without moving as plain text"
					#'clipboard-helm-raw-paste-without-move-action)
				(cons
					"Delete marked"
					(lambda (_)
						(dolist (
							cand
							(clipboard-helm-sort-by-highest-index
								(helm-marked-candidates))
						)
							(let (
								(index (get-text-property 0 'clipboard-helm-index cand))
							)
								(if (= index 0)
									(setq clipboard-list (cdr clipboard-list))
									(delete_from_list_by_index clipboard-list index))))))
				(cons
					"Reorder marked"
					; Move marked candidates to the top of clipboard-list.
					(lambda (_)
						(let ((i 0))
							(dolist (
								cand
								(clipboard-helm-sort-by-highest-index
									(helm-marked-candidates))
							)
								(let (
									(index
										(+
											(get-text-property
												0 'clipboard-helm-index cand)
											i))
								)
									(push (nth index clipboard-list) clipboard-list)
									(delete_from_list_by_index clipboard-list (1+ index)))
								(++ i)))
						; Save to system's clipboard.
						(and
							clipboard-list
							interprogram-cut-function
							(funcall interprogram-cut-function
								(car (car clipboard-list)))))))))

(defvar clipboard-helm-running nil
"Let-bounded around helm call to tell if source is running.")

(define-key global-map [?\C-\S-v]
	(lambda () (interactive)
		; Don't nest in itself, it won't work.
		(unless clipboard-helm-running
			; Sync clipboard-list with interprogram-paste.
			(clipboard-get-entry-to-paste)
			(add-hook 'helm-after-window-setup-hook #'clipboard-helm-after-window-setup)
			(add-hook 'helm-after-update-hook #'clipboard-helm-after-update)

			(let ((clipboard-helm-running t))
				(helm
					:sources (list clipboard-helm-source)
					; noresume because many (if not all) actions are saving results in
					; clipboard-list, not in cached candidates, so after executing
					; action candidates will be obsolete, and also it's not really
					; useful to resume this helm call.
					:resume 'noresume
					:allow-nest t
					:helm-truncate-lines t
					:show-trailing-whitespace show-trailing-whitespace))

			; Helm restores window configuration, so this window was already deleted.
			(when clipboard-helm-preview-window
				(setq clipboard-helm-preview-window nil)
				(when-let (
					; That's why access this buffer not by window-buffer but
					; by buffer's name.
					(preview_buffer (get-buffer clipboard-helm-preview-buffer-name))
				)
					(kill-buffer preview_buffer)))
			(remove-hook 'helm-after-window-setup-hook
				#'clipboard-helm-after-window-setup)
			(remove-hook 'helm-after-update-hook #'clipboard-helm-after-update))))

(provide 'myClipboard)
