; -*- lexical-binding:nil -*-

; Paren deletion.

(defun delete_highlighted_parens ()
	(when-let ((paren_at_caret_region_max (region::paren_at_caret_region_max)))
		(let ((pos (point)))
			(goto-char (car paren_at_caret_region_max))
			(withDemotedError buffer-read-only
				(delete-char 1)
				; If caret was before opening paren.
				(when (/= (point) pos) (-- pos))
				(goto-char (1- (cdr paren_at_caret_region_max)))
				(delete-char -1)
				; If caret was after closing paren.
				(when (= (1+ (point)) pos) (-- pos)))
			(goto-char pos)
			t)))

(defunWithBase
	(lambda (region_min)
		(when region_min
			(let ((pos (point)))
				(goto-char (car region_min))
				(withDemotedError buffer-read-only
					(delete-char -1)
					(-- pos)
					(goto-char (cdr region_min))
					(delete-char -1))
				(goto-char pos)
				t)))
	(delete_quotes (&optional search_limit)
		(,base (region::string_region_min search_limit)))
	(delete_closest_parens (&optional search_limit)
	"Delete closest parens (including quotes)."
		(or (,base (region::closest_paren_region_min)) (delete_quotes search_limit))))

; Region deletion.

(defunWithBase
	(lambda (region_max)
		(when region_max
			(withDemotedError buffer-read-only
				(delete-region (car region_max) (cdr region_max)))
			t))
	(delete_highlighted_paren_region () "Deletion include parens."
		(,base (region::paren_at_caret_region_max)))
	(delete_quote_region (&optional search_limit) "Deletion include parens."
		(,base (region::string_region_max search_limit)))
	(delete_closest_paren_region (&optional search_limit)
	"Delete closest surrounding paren expression (including quotes)."
		(or
			(,base (region::closest_paren_region_max))
			(delete_quote_region search_limit))))

(let (
	(make_command
		(lambda (group_this_command positive_fn negative_fn)
			`(lambda (n) (interactive "p")
				(when (/= n 0)
					(withDemotedError buffer-read-only
						(barf-if-buffer-read-only)
						,(when group_this_command
							; Group all this simple delete commands in one undo.
							'(setq this-command 'key::backspace))
						(undo-auto-amalgamate)
						(when (maybe_delete_marked_region)
							(if (> n 0) (-- n) (++ n)))
						(when (/= n 0)
							(funcall (if (> n 0) #',positive_fn #',negative_fn) n))
						(frame-make-pointer-invisible)
						(funcall after_move_hook_fn))))))
)

	; Backspace with auto indenting where seems useful.
	; Also deleting things like (|).
	; If prefix arg is negative, just delete forward.
	(define-key global-map [backspace]
		(progn
			(fset 'key::backspace
				(funcall make_command
					nil
					(lambda (n)
						(let (char_before)
							(while (and (> n 0) (setq char_before (char-before)))
								(-- n)
								(let (
									(caret (point))
									(maybe_indent_line_or_delete_surrounding_parens
										(lambda ()
											(if (is_line_blank_to_start)
												(indent-according-to-mode)
												; If caret is surrounded by matching parens, but closing
												; paren is on the next line, maybe after some indent,
												; delete this indent and newline, so caret will be closely
												; surrounded. This is here to be able to insert parens,
												; then insert newline between them, then trigger this
												; special behaviour of adding extra newline between them,
												; and then just by pressing backspace, revert this special
												; behaviour. E.g. (keybindings are in square brackets):
												; = |
												; [?\{] = {|}
												; [return] = {
												;     |
												; }
												; [backspace] = {|}
												; [backspace] = |
												; This is only an approximate check if backspace
												; reverts this situation, but I think it works well.
												; It will trigger after pressing backspace in
												; following situations:
												; [whatever]{
												; [blanks]|[blanks]
												; [blanks]}[whatever]

												; Rules here should approximately match rules in
												; insert::should_insert_double_char.
												(unless (inside_string)
													(let ((position_after_initial_delete (point)))
														(skip-chars-forward " \t")
														; If caret wasn't on a blank line.
														(if (/= (following-char) ?\n)
															(goto-char position_after_initial_delete)
															(forward-char 1)
															(skip-chars-forward " \t")
															(if
																(syntax::is_surrounded_by_matching_parens
																	position_after_initial_delete (point) t)
																(delete-region
																	position_after_initial_delete (point))
																(goto-char
																	position_after_initial_delete))))))))
								)
									(cond
										((= char_before ?\n)
											(delete-char -1)
											(funcall maybe_indent_line_or_delete_surrounding_parens))
										((is_line_blank_to_start)
											(delete-char -1)
											(let ((line_start_position (pos-bol)))
												(when (/= line_start_position (point-min))
													; Skip forward to later compare position of first
													; non blank char before and after indenting.
													(skip-chars-forward " \t")
													(let ((first_non_blank_char_column (current-column)))
														(indent-according-to-mode)
														; If after delete-char -1 line's indent
														; is too small.
														(when (< first_non_blank_char_column (current-column))
															; Delete blanks and newline.
															(delete-region (1- line_start_position) (point))
															(funcall
																maybe_indent_line_or_delete_surrounding_parens))))))
										; Like in maybe_indent_line_or_delete_surrounding_parens,
										; but for double space.
										; E.g.
										; = |
										; [?\C-?] = /*|*/
										; [?\s] = /* | */
										; [backspace] = /*|*/
										; [backspace] = |
										((and
												(= char_before (following-char) ?\s)
												(syntax::is_surrounded_by_matching_parens
													(1- caret) (1+ caret)))
											(delete-region (1- caret) (1+ caret)))
										(t
											; If we are surrounded by matching parens, delete them.
											(if-let (
												(parens_lengths
													; proper_same_delimiters_check nil to delete stuff
													; like "string"|"string" -> "string|string".
													(syntax::is_surrounded_by_matching_parens caret caret))
											)
												(delete-region
													(- caret (car parens_lengths))
													(+ caret (cdr parens_lengths)))
												(let (
													(is_after_line_comment_starter
														(lambda ()
															(and
																(is_after_string comment::line_string)
																; If this comment ends right before
																; line comment starter.
																(eq
																	(nth 8 (syntax-ppss))
																	(- (point) (length comment::line_string))))))
												)
													; If caret is one space or right after line comment starter,
													; delete this comment starter, and maybe delete line.
													(if
														(not
															(and
																comment::line_string
																(save-excursion
																	(when (= char_before ?\s)
																		(forward-char -1))
																	(funcall is_after_line_comment_starter))))
														(delete-char -1)
														; Go before line comment starter.
														(goto-char (nth 8 (syntax-ppss)))
														; Skip blanks.
														(skip-chars-backward " \t")
														; If line is empty, delete it.
														(if (/= (preceding-char) ?\n)
															(delete-region (point) caret)
															(delete-region (1- (point)) caret)
															(cond
																; If line is empty, indent it.
																((is_line_blank_to_start)
																	(indent-according-to-mode)))))))))))))
																; If we are right after comment starter,
																; insert one space.
																; This should be customizable, not always one
																; space.
																; Well, actually it's not very useful and is
																; sometimes annoying.
		;														((funcall is_after_line_comment_starter)
		;															(insert ?\s)))))))))))))))
					(lambda (n) (ignore-error end-of-buffer (delete-char (- n))))))
			#'key::backspace))

	; Simple delete-char.
	(let (
		(delete_char_forward
			(funcall make_command
				t
				(lambda (n) (ignore-error end-of-buffer (delete-char n)))
				(lambda (n) (ignore-error beginning-of-buffer (delete-char n)))))
	)
		(define-key global-map [S-backspace]
			(fn_symbol "key::delete_char_backward"
				(get_reverse_command delete_char_forward)))
		(bind_many_keys global-map [[delete] [S-delete]]
			(fn_symbol "key::delete_char_forward" delete_char_forward)))

	; Delete region from caret to control_move_left/right.
	(let (
		(delete_control_move_left
			(funcall make_command
				t
				(lambda (n)
					(let ((end (point)))
						(dotimes (_ n) (control_move_left))
						; [1] Preserve position in case it throws read-only error.
						(delete-region (point) (goto-char end))))
				(lambda (n)
					(let ((start (point)))
						(dotimes (_ (- n)) (control_move_right))
						(let ((end (point)))
							(goto-char start)
							(delete-region start end))))))
	)
		(define-key global-map [C-backspace]
			(fn_symbol "key::delete_control_left" delete_control_move_left))
		(define-key global-map [C-delete]
			(fn_symbol "key::delete_control_right"
				(get_reverse_command delete_control_move_left))))

	; Delete line or marked lines.
	(let (
		(delete_line_backward
			(lambda (n) (interactive "p")
				(when (/= n 0)
					(withDemotedError buffer-read-only
						(barf-if-buffer-read-only)
						(unless goal_column (set_goal_column))
						(setq this-command 'key::delete_line_backward)
						(undo-auto-amalgamate)
						(if mark-active
							(let (
								(region (region::marked_line_region))
								(line_count
									(when (/= (abs n) 1)
										(1+
											(if (> (point) (mark))
												(line_count_backward (mark))
												(line_count_forward (mark))))))
							)
								; Wrap this in let because deactivate-mark will exit
								; rectangle::mode if it is on, which will call
								; after_move_hook, which will set goal_column to nil.
								(let (goal_column) (deactivate-mark))
								(if line_count
									(if (> n 0)
										(progn
											(goto-char (car region))
											(forward-line (- (* (1- n) line_count)))
											; Try to delete one more newline, because
											; (cdr region) is an eol.
											(cond
												((not (bobp)) (forward-char -1))
												((/= (cdr region) (point-max))
													(setcdr region (1+ (cdr region)))))
											(delete-region (point) (cdr region)))
										(goto-char (cdr region))
										(and
											(/= (forward-line (* (- n) line_count)) 0)
											(/= (car region) (point-min))
											(setcar region (1- (car region))))
										(delete-region (car region) (point)))
									(if (> n 0)
										(cond
											((/= (car region) (point-min))
												(setcar region (1- (car region))))
											((/= (cdr region) (point-max))
												(setcdr region (1+ (cdr region)))))
										(cond
											((/= (cdr region) (point-max))
												(setcdr region (1+ (cdr region))))
											((/= (car region) (point-min))
												(setcar region (1- (car region))))))
									(delete-region (car region) (cdr region))))
							(if (> n 0)
								(let ((end (pos-eol)))
									(forward-line (- (1- n)))
									(cond
										((not (bobp)) (forward-char -1))
										((/= end (point-max)) (++ end)))
									(delete-region (point) end))
								(let ((start (pos-bol)))
									(and
										(/= (forward-line (- n)) 0)
										(/= start (point-min))
										(-- start))
									(delete-region start (point)))))
						(frame-make-pointer-invisible)
						; Preserve column and hscroll.
						(goto_goal_column)
						(let (goal_column) (funcall after_move_hook_fn))))))
	)
		(define-key global-map [C-S-backspace]
			(fn_symbol "key::delete_line_backward" delete_line_backward))
		(define-key global-map [C-S-delete]
			(fn_symbol "key::delete_line_forward"
				(get_reverse_command delete_line_backward))))

	; Delete surrounding (or left if between regions) progressive-control-move region.
	(let (
		(delete_progressive_control_move_left
			(funcall make_command
				t
				(lambda (n)
					(let ((caret (point)) start end)
						(progressive_control_move_left)
						(let ((pos (point)))
							(progressive_control_move_right)
							(setq end (point))
							(goto-char pos))
						(dotimes (_ (1- n)) (progressive_control_move_left))
						(setq start (point))
						(goto-char caret)
						(delete-region start end)))
				(lambda (n)
					(let ((caret (point)) start end)
						(progressive_control_move_right)
						(let ((pos (point)))
							(progressive_control_move_left)
							(setq start (point))
							(goto-char pos))
						(dotimes (_ (- (1+ n))) (progressive_control_move_right))
						(setq end (point))
						(goto-char caret)
						(delete-region start end)))))
	)
		(bind_many_keys global-map [[A-backspace] [A-S-backspace]]
			(fn_symbol "key::delete_progressive_control_left"
				delete_progressive_control_move_left))
		(bind_many_keys global-map [[A-delete] [A-S-delete]]
			(fn_symbol "key::delete_progressive_control_right"
				(get_reverse_command delete_progressive_control_move_left))))

	; Delete highlighted paren expression or surrounding region.
	(define-key global-map [A-C-backspace]
		(fn_symbol "key::delete_region"
			(funcall make_command
				nil
				(lambda (n)
					(while
						(and
							(> n 0)
							(or
								(delete_highlighted_paren_region)
								(delete_closest_paren_region)))
						(-- n)))
				(lambda (n)
					(while
						(and
							(< n 0)
							(or
								(delete_highlighted_paren_region)
								(delete_closest_paren_region)))
						(++ n))))))

	; Delete highlighted or surrounding parens.
	(define-key global-map [A-C-S-backspace]
		(fn_symbol "key::delete_parens"
			(funcall make_command
				nil
				(lambda (n)
					(while
						(and
							(> n 0)
							(or (delete_highlighted_parens) (delete_closest_parens)))
						(-- n)))
				(lambda (n)
					(while
						(and
							(< n 0)
							(or (delete_highlighted_parens) (delete_closest_parens)))
						(++ n)))))))

(provide 'myDelete)
