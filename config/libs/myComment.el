; -*- lexical-binding:nil -*-

(defvar-local comment::line_string nil
"Line comment string, e.g. \"//\" in C, \";\" in elisp.")
(defvar-local comment::region_start_string nil
"Region comment start string, e.g. \"/*\" in C.")
(defvar-local comment::region_end_string nil
"Region comment end string, e.g. \"*/\" in C.")

; Mode hooks

(let (
	(make_hook
		(lambda (line &optional region_start region_end)
			(fn_symbol "comment"
				`(lambda ()
					,(if line
						`(setq comment::line_string ,line)
						'(kill-local-variable 'comment::line_string))
					,(if region_start
						`(setq comment::region_start_string ,region_start)
						'(kill-local-variable 'comment::region_start_string))
					,(if region_end
						`(setq comment::region_end_string ,region_end)
						'(kill-local-variable 'comment::region_end_string))))))
)
	(let ((lisp_hook (funcall make_hook ";")))
		(add-hook 'lisp-data-mode-hook lisp_hook)
		(add-hook 'minibuffer-mode-hook lisp_hook))
	(add-hook 'sh-mode-hook (funcall make_hook "#"))
	(let ((c_hook (funcall make_hook "//" "/*" "*/")))
		(add-hook 'c-mode-hook c_hook)
		(add-hook 'c-ts-mode-hook c_hook)
		(add-hook 'c++-mode-hook c_hook)
		(add-hook 'c++-ts-mode-hook c_hook))
	(add-hook 'cmake-mode-hook (funcall make_hook "#" "#[[" "]]"))
	(add-hook 'nxml-mode-hook (funcall make_hook nil "<!--" "-->")))

; Region

(defunWithBase
	(lambda (line_comment_fn region_comment_fn)
		(let ((syntax_ppss (nthcdr 4 (syntax-ppss))))
			(when (car syntax_ppss)
				(let ((maybe_comment_start (nth 4 syntax_ppss)))
					(cond
						((and
								comment::line_string
								(save-excursion
									(goto-char maybe_comment_start)
									(is_before_string comment::line_string)))
							; line comment
							(cons
								(save-excursion
									(goto-char maybe_comment_start)
									(funcall line_comment_fn))
								'lineComment))
						(comment::region_start_string
							; region comment
							(funcall region_comment_fn)))))))

	(region::comment_region_min_start_and_type ()
		(,base
			(lambda ()
				(cl-loop
					with column_indent = (current-column)
					with if_current_line_is_not_line_comment_region_part_then_go_back_to_line_comment_region_min_start =
						(lambda ()
							(move-to-column column_indent)
							(unless (is_before_string comment::line_string)
								(forward-line 1)
								(move-to-column column_indent)
								t))
					when (= (forward-line -1) -1) ; When at start of buffer.
						return
							(funcall
								if_current_line_is_not_line_comment_region_part_then_go_back_to_line_comment_region_min_start)
					until
						(funcall
							if_current_line_is_not_line_comment_region_part_then_go_back_to_line_comment_region_min_start))
				(+ (point) (length comment::line_string)))
			(lambda ()
				(cons
					(+ maybe_comment_start (length comment::region_start_string))
					'regionComment))))

	(region::comment_region_min_end_and_type (&optional search_limit)
		(,base
			(lambda ()
				(pos-eol
					(cl-loop
						with column_indent = (current-column)
						do
						(forward-line 1)
						(move-to-column column_indent)
						when (at_last_buffer_line)
							; Return line end position of this line
							; if it's commented, else of previous line.
							return (if (is_before_string comment::line_string) 1 0)
						; When this line isn't commented,
						; return end of previous line as comment end.
						unless (is_before_string comment::line_string)
							return 0)))
			(lambda ()
				(when-let (
					(maybe_comment_end
						(save-excursion
							(search-forward
								comment::region_end_string
								(region::search_limit_end search_limit)
								t)))
				)
					(cons
						(- maybe_comment_end (length comment::region_start_string))
						'regionComment))))))

(defsubst region::comment_region_min_start ()
	(car (region::comment_region_min_start_and_type)))
(defsubst region::comment_region_min_end (&optional search_limit)
	(car (region::comment_region_min_end_and_type search_limit)))

(defun region::comment_region_min_and_type (&optional search_limit)
"Return ((start . end) . type) or nil."
	(when-let (
		(comment_region_min_start_and_type (region::comment_region_min_start_and_type))
		(comment_region_min_end_and_type
			(region::comment_region_min_end_and_type search_limit))
	)
		(setcar comment_region_min_start_and_type
			(cons
				(car comment_region_min_start_and_type)
				(car comment_region_min_end_and_type)))
		comment_region_min_start_and_type))

(defun region::comment_region_min (&optional search_limit)
	(when-let (
		(comment_region_min_start_and_type (region::comment_region_min_start_and_type))
		(comment_region_min_end_and_type
			(region::comment_region_min_end_and_type search_limit))
	)
		(setcdr comment_region_min_start_and_type (car comment_region_min_end_and_type))
		comment_region_min_start_and_type))

(defun region::comment_region_max
	(&optional search_limit include_final_newline_in_line_comment)
	(when-let (
		(comment_region_min_and_type (region::comment_region_min_and_type search_limit))
	)
		(let ((comment_region_min (car comment_region_min_and_type)))
			(if (eq (cdr comment_region_min_and_type) 'lineComment)
				(progn
					(setcar comment_region_min
						(-
							(car comment_region_min)
							(length comment::line_string)))
					(and
						include_final_newline_in_line_comment
						(/= (cdr comment_region_min) (point-max))
						(setcdr comment_region_min (1+ (cdr comment_region_min)))))
				(setcar comment_region_min
					(-
						(car comment_region_min)
						(length comment::region_start_string)))
				(setcdr comment_region_min
					(+
						(cdr comment_region_min)
						(length comment::region_end_string))))
			comment_region_min)))

(defun region::comment_region_min_to_max
	(region_min &optional include_final_newline_in_line_comment)
"If region_min is a comment region, then extend it (by modifying region_min cell)
to contain it's delimiters and return it; else return nil.
region_min must really be region min for this function to return accurate data."
	(cond
		((and
				comment::line_string
				(>= (- (car region_min) (length comment::line_string)) (point-min))
				(save-excursion
					(goto-char (- (car region_min) (length comment::line_string)))
					(is_before_string comment::line_string)))
			(setcar region_min (- (car region_min) (length comment::line_string)))
			(and
				include_final_newline_in_line_comment
				(/= (cdr region_min) (point-max))
				(setcdr region_min (1+ (cdr region_min))))
			region_min)
		((and
				comment::region_start_string
				(save-excursion
					(goto-char (cdr region_min))
					(is_before_string comment::region_end_string)))
			(setcar region_min (- (car region_min) (length comment::region_start_string)))
			(setcdr region_min (+ (cdr region_min) (length comment::region_end_string)))
			region_min)))

(defun region::comment_region_min_start_to_max (region_min_start)
	(let (region_max_start)
		(when
			(or
				(and
					comment::line_string
					(>=
						(setq region_max_start
							(- region_min_start (length comment::line_string)))
						(point-min))
					(save-excursion
						(goto-char region_max_start)
						(is_before_string comment::line_string)))
				(and
					comment::region_start_string
					(>=
						(setq region_max_start
							(- region_min_start (length comment::region_start_string)))
						(point-min))
					(save-excursion
						(goto-char region_max_start)
						(is_before_string comment::region_start_string))))
			region_max_start)))

(defun region::comment_region_min_end_to_max
	(region_min_end &optional include_final_newline_in_line_comment)
	(cond
		((and comment::line_string (eq (char-after region_min_end) ?\n))
			(when include_final_newline_in_line_comment (++ region_min_end))
			region_min_end)
		((and
				comment::region_start_string
				(save-excursion
					(goto-char region_min_end)
					(is_before_string comment::region_end_string)))
			(+ region_min_end (length comment::region_end_string)))))


(defun comment::skip_blanks_backward ()
"Like (skip-chars-backward \"\\s\\t\\n\"), but also skipping
line comment starters if they actually start a comment.
Leave caret before skipped blanks or at bobp.
Only to use with multi line comments."
	(let ((comment_starter_string (char-to-string (aref comment::line_string 0))))
		(while
			(progn
				(skip-chars-backward " \t")
				(let ((position (point)))
					(skip-chars-backward comment_starter_string)
					(skip-chars-backward " \t")
					(unless (bobp)
						; If after skipping blanks and comment starter,
						; we are at the start of line, continue skipping
						; blank commented lines, otherwise go back
						; to the last confirmed blank position and exit loop.
						(if (= (preceding-char) ?\n)
							(progn (forward-char -1) t)
							(goto-char position) nil)))))))

; Commenting selected lines or region.

; Comment or uncomment line(s).
; Prefix can matter.
; If mark is active, comment or uncomment marked lines.
; If there is no prefix or prefix = 0, comment or uncomment current line.
; Else comment current line and prefix lines forward; e.g. with prefix = 1,
; comment current and the next line, with prefix = -1, comment current and previous line.
(define-key global-map [?\C-/]
	(fn_symbol "key::comment_line"
		(lambda (arg) (interactive "P")
			(barf-if-buffer-read-only)
			(cond
				(mark-active
					(let ((marked_line_region (region::marked_line_region)))
						(saveCaretAndMark
							(goto-char (car marked_line_region))
							(skip-chars-forward " \t")
							(cond
								((and comment::line_string (is_before_string comment::line_string))
									(region::apply_on_marked_line_region
										(lambda ()
											(skip-chars-forward " \t")
											(when (is_before_string comment::line_string)
												(delete-char (length comment::line_string))))
										marked_line_region))
								((and
										(not comment::line_string)
										comment::region_start_string
										(is_before_string comment::region_start_string)
										(save-excursion
											(goto-char (cdr marked_line_region))
											(skip-chars-backward " \t")
											(is_after_string comment::region_end_string)))
									(delete-char (length comment::region_start_string))
									(goto-char
										(-
											(cdr marked_line_region)
											(length comment::region_start_string)))
									(skip-chars-backward " \t")
									(delete-char (- (length comment::region_end_string))))
								(comment::line_string
									(region::apply_on_marked_line_region
										(lambda () (insert comment::line_string))
										marked_line_region))
								(comment::region_start_string
									(insert comment::region_start_string)
									(goto-char
										(-
											(cdr marked_line_region)
											(length comment::region_start_string)))
									(insert comment::region_end_string))))))
				((or (not arg) (= (setq arg (prefix-numeric-value arg)) 0))
					(save-excursion
						(forward-line 0)
						(let ((line_start (point)))
							(skip-chars-forward " \t")
							(cond
								((and
										comment::line_string
										(is_before_string comment::line_string))
									(delete-char (length comment::line_string)))
								((and
										comment::region_start_string
										(is_before_string comment::region_start_string))
									(let ((first_non_blank_char_position (point)))
										(goto-char (pos-eol))
										(skip-chars-backward " \t")
										(when (is_after_string comment::region_end_string)
											(delete-char (- (length comment::region_end_string)))
											(goto-char first_non_blank_char_position)
											(delete-char (length comment::region_start_string)))))
								(comment::line_string
									(goto-char line_start)
									(insert comment::line_string))
								(comment::region_start_string
									(goto-char line_start)
									(insert comment::region_start_string)
									(goto-char (pos-eol))
									(insert comment::region_end_string))))))
				(t
					(jumpHistory::add)
					(forward-line 0)
					(let ((line_start (point)))
						(skip-chars-forward " \t")
						(let (
							(maybe_move_to_next_line
								(if (> arg 0)
									(lambda ()
										(and
											(>= (-- arg) 0)
											(= (forward-line 1) 0)
											(= (preceding-char) ?\n)))
									(lambda ()
										(and (<= (++ arg) 0) (= (forward-line -1) 0)))))
						)
							(cond
								((and
										comment::line_string
										(is_before_string comment::line_string))
									(delete-char (length comment::line_string))
									(while (funcall maybe_move_to_next_line)
										(skip-chars-forward " \t")
										(when (is_before_string comment::line_string)
											(delete-char (length comment::line_string)))))
								; Still provide some weird support for region comments -
								; uncomment only lines like /* line */.
								((and
										comment::region_start_string
										(is_before_string comment::region_start_string)
										(progn
											(goto-char (pos-eol))
											(skip-chars-backward " \t")
											(is_after_string comment::region_end_string)))
									(delete-char (- (length comment::region_end_string)))
									(goto-char line_start)
									(skip-chars-forward " \t")
									(delete-char (length comment::region_start_string))
									(while (funcall maybe_move_to_next_line)
										(skip-chars-forward " \t")
										(when (is_before_string comment::region_start_string)
											(let ((first_non_blank_char_position (point)))
												(goto-char (pos-eol))
												(skip-chars-backward " \t")
												(when (is_after_string comment::region_end_string)
													(delete-char
														(- (length comment::region_end_string)))
													(goto-char first_non_blank_char_position)
													(delete-char
														(length comment::region_start_string)))))))
								(comment::line_string
									(goto-char line_start)
									(insert comment::line_string)
									(while (funcall maybe_move_to_next_line)
										(insert comment::line_string)))
								((not comment::region_start_string))
								((> arg 0)
									(goto-char line_start)
									(insert comment::region_start_string)
									(forward-line arg)
									(goto-char (pos-eol))
									(insert comment::region_end_string))
								(t
									(goto-char (pos-eol))
									(insert comment::region_end_string)
									(goto-char line_start)
									(forward-line arg)
									(insert comment::region_start_string)))))))
			(funcall after_move_hook_fn))))

; Comment or uncomment region.
; (key-parse "C-?") instead of [?\C-?], because ?\C-? = (key-parse "DEL") = 127,
; so hitting ctrl+? on keyboard results in command event = (key-parse "C-?"), not 127.
; Idk, seems like this is the only key that needs this.
(define-key global-map (key-parse "C-?")
	(fn_symbol "key::comment_region"
		(lambda () (interactive)
			(barf-if-buffer-read-only)
			(if mark-active
				(when comment::region_start_string
					(let* (
						(region (region::marked_region))
						(is_caret_start (= (point) (car region)))
						(start_marker (copy-marker (car region)))
						(end_marker (copy-marker (cdr region) t))
						before_comment_start_position
						(is_after_str
							(lambda (pos str)
								(goto-char pos)
								(skip-chars-backward " \t")
								(is_after_string str)))
						(is_before_str
							(lambda (pos str)
								(goto-char pos)
								(skip-chars-forward " \t")
								(is_before_string str)))
					)
						; If region encloses or is enclosed by regional comment.
						(if
							(cond
								; If region has comment start inside.
								((setq before_comment_start_position
										(funcall is_before_str
											(car region) comment::region_start_string))
									; If region has comment end inside.
									(when
										(funcall is_after_str
											(cdr region) comment::region_end_string)
										(forward-char (- (length comment::region_end_string)))
										t))
								; If region has comment start outside.
								((setq before_comment_start_position
										(funcall is_after_str
											(car region) comment::region_start_string))
									; If region has comment end outside.
									(funcall is_before_str
										(cdr region) comment::region_end_string)))
							(progn
								(delete-char (length comment::region_end_string))
								(goto-char before_comment_start_position)
								(delete-char (length comment::region_start_string)))
							(goto-char (car region))
							(insert comment::region_start_string)
							(goto-char (- (cdr region) (length comment::region_start_string)))
							(insert comment::region_end_string))
						(if is_caret_start
							(progn
								(goto-char start_marker)
								(set-marker (mark-marker) end_marker))
							(goto-char end_marker)
							(set-marker (mark-marker) start_marker))
						(set-marker start_marker nil)
						(set-marker end_marker nil))
					(setq deactivate-mark nil))
				(if-let ((comment_region_min_and_type (region::comment_region_min_and_type)))
					(let (
						(comment_region_end_marker
							(copy-marker (cdr (car comment_region_min_and_type))))
					)
						(save-excursion
							(if (eq (cdr comment_region_min_and_type) 'lineComment)
								(progn
									(goto-char (car (car comment_region_min_and_type)))
									(delete-char (- (length comment::line_string)))
									(while
										(progn
											(forward-line 1)
											(< (point) comment_region_end_marker))
										(skip-chars-forward " \t")
										(delete-char (length comment::line_string))))
								(goto-char (car (car comment_region_min_and_type)))
								(delete-char (- (length comment::region_start_string)))
								(goto-char comment_region_end_marker)
								(delete-char (length comment::region_end_string))))
						(set-marker comment_region_end_marker nil))
					(when comment::region_start_string
						(insert comment::region_start_string)
						(insert comment::region_end_string)
						(forward-char (- (length comment::region_end_string))))))
			(funcall after_move_hook_fn))))

(provide 'myComment)
