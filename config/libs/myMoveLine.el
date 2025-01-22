; -*- lexical-binding:nil -*-

; Move line or marked lines prefix lines down.
(let (
	(move_line_down
		(lambda (n) (interactive "p")
			(when (/= n 0)
				(undo-auto-amalgamate)
				(cond
					(mark-active
						(let ((marked_line_region (region::marked_line_region)))
							(unless
								(if (> n 0)
									(= (cdr marked_line_region) (point-max))
									(= (car marked_line_region) (point-min)))
								(let (
									(mark_to_caret_distance (- (mark) (point)))
									(caret_marker (copy-marker (point)))
								)
									(if (> n 0)
										(progn
											(goto-char (cdr marked_line_region))
											(delete-char 1)
											(let (
												(line_content
													(filter-buffer-substring
														(point) (pos-eol n) t))
											)
												; Insert before marked region.
												(goto-char (car marked_line_region))
												(set-marker-insertion-type caret_marker t)
												(insert line_content ?\n)))
										(let (
											(end_marker (copy-marker (cdr marked_line_region)))
										)
											(goto-char (car marked_line_region))
											(delete-char -1)
											(let (
												(line_content
													(filter-buffer-substring
														(pos-bol (+ n 2)) (point) t))
											)
												; Insert after marked region.
												(goto-char end_marker)
												(insert ?\n line_content))
											(set-marker end_marker nil)))
									(goto-char caret_marker)
									(set-marker (mark-marker)
										(+ caret_marker mark_to_caret_distance))
									(set-marker caret_marker nil))
								(setq deactivate-mark nil))))
					((not (if (> n 0) (at_last_buffer_line) (at_first_buffer_line)))
						(let* (
							(line_end (pos-eol))
							(char_count_to_line_end (- line_end (point)))
							(line_content (filter-buffer-substring (pos-bol) line_end t))
						)
							(if (> n 0)
								(progn (delete-char 1) (forward-line n))
								(delete-char -1) (forward-line (1+ n)))
							(insert line_content ?\n)
							(forward-char (- (1+ char_count_to_line_end))))))
				(let (goal_column) (funcall after_move_hook_fn current_column)))))
)
	(define-key global-map [C-S-down] (fn_symbol "key::move_line_down" move_line_down))
	(define-key global-map [C-S-up]
		(fn_symbol "key::move_line_up"
			`(lambda (n) (interactive "p")
				(setq this-command ',move_line_down)
				(,move_line_down (- n))))))

(provide 'myMoveLine)
