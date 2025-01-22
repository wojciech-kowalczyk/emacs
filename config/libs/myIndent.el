; -*- lexical-binding:nil -*-

; Auto indent.
(define-key global-map [?\C-i]
	(lambda (arg) (interactive "P")
		(if mark-active
			(progn
				(saveCaretAndMark
					(region::apply_on_marked_line_region #'indent-according-to-mode))
				(funcall after_move_hook_fn))
			(unless goal_column (set_goal_column))
			(undo-auto-amalgamate)
			(indent-according-to-mode)
			(cond
				((not arg) (forward-line 1))
				((> (setq arg (prefix-numeric-value arg)) 0)
					(while
						(and
							(= (forward-line 1) 0)
							(= (preceding-char) ?\n)
							(>= (-- arg) 0))
						(indent-according-to-mode)))
				((< arg 0)
					(while (and (= (forward-line -1) 0) (<= (++ arg) 0))
						(indent-according-to-mode))))
			; Preserve column and hscroll.
			(goto_goal_column)
			(let (goal_column) (funcall after_move_hook_fn)))))

; Changed to always convert tabs to spaces.
(fset 'indent-line-to
	(lambda (column)
		(forward-line 0)
		(delete-region (point) (progn (skip-chars-forward " \t") (point)))
		(indent-to column)))

(fset 'lisp-indent-line
	(lambda (&optional indent) (interactive)
		(let ((caret (point)))
			(forward-line 0)
			(let ((pos (- (point-max) caret)))
				(unless indent (setq indent (calculate-lisp-indent (lisp-ppss))))
				(if (not indent)
					(goto-char caret)
					(indent-line-to (if (listp indent) (car indent) indent))
					; If initial point was within line's indentation,
					; position after the indentation;
					; else stay at the same point in text.
					(when (> (- (point-max) pos) (point))
						(goto-char (- (point-max) pos))))))))

(fset 'calculate-lisp-indent
	(lambda (&optional parse-start)
"Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
		(save-excursion
			(forward-line 0)
			(let (
				(indent-point (point))
				(retry t)
				state
				desired-indent
				whitespace-after-open-paren
				calculate-lisp-indent-last-sexp
				containing-sexp
			)
				(cond
					((or (markerp parse-start) (integerp parse-start))
						(goto-char parse-start))
					(parse-start (setq state parse-start))
					(t (beginning-of-defun)))
				(unless state
					; Find outermost containing sexp.
					(while (< (point) indent-point)
						(setq state (parse-partial-sexp (point) indent-point 0))))
				; Find innermost containing sexp.
				(while (and retry state (> (car state) 0))
					(setq retry nil)
					(setq calculate-lisp-indent-last-sexp (nth 2 state))
					(setq containing-sexp (nth 1 state))
					; Position following last unclosed open.
					(goto-char (1+ containing-sexp))
					; Is there a complete sexp since then?
					(when
						(and
							calculate-lisp-indent-last-sexp
							(> calculate-lisp-indent-last-sexp (point)))
						; Yes, but is there a containing sexp after that?
						(let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp indent-point 0)))
							(when (setq retry (nth 1 peek)) (setq state peek)))))
				(unless retry
					; Innermost containing sexp found
					(goto-char (1+ containing-sexp))
					(setq desired-indent
						(cond
							; If sexp is surrounded by newlines, e.g.
							; (when-let (
							;     (foo (bar))
							; |)
							((and
									(is_line_blank_to_end)
									(save-excursion
										(goto-char indent-point)
										(skip-chars-forward "\s\t")
										(and
											(not (eobp))
											(= ?\) (syntax::raw_to_class (syntax-after (point)))))))
								(current-indentation))
							((or
									; There is no expression between opening paren and indent-point.
									(not calculate-lisp-indent-last-sexp)
									(and
										; We know that sexp has at least one element before
										; indent-point, so only add indent if sexp has only
										; one line before indent-point, e.g.
										; (if (bobp)
										; |
										; ->
										; (if (bobp)
										;     |
										; or
										; (and
										; |    (bobp)
										; ->
										; (and
										;     |(bobp)
										(not (is_line_blank_to_end))
										(<= calculate-lisp-indent-last-sexp (pos-eol))))
								(+ standard-indent (current-indentation)))
							(t
								; Indent beneath first sexp on the same line as
								; 'calculate-lisp-indent-last-sexp'.
								(goto-char calculate-lisp-indent-last-sexp)
								(current-indentation)))))
				; Point is at the point to indent under unless we are inside a string.
				; Call indentation hook except when overridden by lisp-indent-offset
				; or if the desired indentation has already been computed.
				(cond
					; Inside a string, don't change indentation.
					((nth 3 state) nil)
					; 'lisp-indent-offset' override.
					((and lisp-indent-offset containing-sexp)
						; Indent by constant offset
						(goto-char containing-sexp)
						(+ (current-column) lisp-indent-offset))
					(desired-indent)
					; When not in a sexp, return column of (pos-bol) (so probably 0).
					(t (current-column)))))))

(provide 'myIndent)
