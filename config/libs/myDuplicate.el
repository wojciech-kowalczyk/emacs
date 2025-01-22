; -*- lexical-binding:nil -*-

(let (
	(duplicate_region
		; Return moved distance to easily set mark if necessary.
		; If insert space is nil, insert newline.
		(lambda (region insert_space n)
			(setq n (abs n))
			(undo-auto-amalgamate)
			(let (
				(caret (point))
				(str
					(concat
						(if insert_space " " "\n")
						(filter-buffer-substring (car region) (cdr region))))
			)
				(jumpHistory::add)
				(goto-char (cdr region))
				(dotimes (_ n) (insert str))
				(let ((offset (* (length str) n)))
					(goto-char (+ caret offset))
					offset))))
)

	; Duplicate line(s).
	(define-key global-map [?\C-d]
		(fn_symbol "key::duplicate_line"
			`(lambda (n) (interactive "p")
				(when (/= n 0)
					(if (not mark-active)
						(,duplicate_region (cons (pos-bol) (pos-eol)) nil n)
						(set-marker
							(mark-marker)
							(+
								(mark)
								(,duplicate_region (region::marked_line_region) nil n)))
						(setq deactivate-mark nil))
					; Don't reset goal_column, our position in line hasn't changed.
					(let (goal_column) (funcall after_move_hook_fn current_column))))))

	; Duplicate marked region, highlighted paren region or surrounding region.
	(define-key global-map [?\C-\S-d]
		(fn_symbol "key::duplicate_region"
			`(lambda (n) (interactive "p")
				(when (/= n 0)
					(let (
						(is_single_line_region
							(lambda (region)
								(save-excursion
									(goto-char (car region))
									(is_caret_on_the_same_line (cdr region)))))
					)
						(if mark-active
							(let ((marked_region (region::marked_region)))
								(set-marker
									(mark-marker)
									(+
										(mark)
										(,duplicate_region
											marked_region
											(funcall is_single_line_region marked_region)
											n)))
								(setq deactivate-mark nil)
								(funcall after_move_hook_fn))
							(when-let (
								(region
									(or
										(region::paren_at_caret_region_max)
										(region::closest_region_max)))
							)
								(,duplicate_region
									region
									(or
										(funcall is_single_line_region region)
										; Save first line's indentation.
										(progn
											(setcar region
												(save-excursion
													(goto-char (car region))
													(pos-bol)))
											nil))
									n)
								(funcall after_move_hook_fn)))))))))
(provide 'myDuplicate)
