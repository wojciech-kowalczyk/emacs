; -*- lexical-binding:nil -*-

(electric-indent-mode -1)

; This won't be really used, because I prefer undoing all text inserted in the
; line, which will almost always be have less than 70 chars (and newline
; insertion separates undo groups).
(setq amalgamating-undo-limit 70)

(defun insert::post_self_insert_hook () (funcall after_move_hook_fn))

(add-hook 'post-self-insert-hook #'insert::post_self_insert_hook)

(defvar syntax::text_syntax_table prog-mode-syntax-table
"Syntax table used when pairing inside comments and strings.
Original: `electric-pair-text-syntax-table'.")

; Parens to use in strings and comments.
; This should be in sync with syntax::text_syntax_table.
(defconst syntax::opening_paren_string "([{")
(defconst syntax::closing_paren_string ")]}")

(defun syntax::get_syntax (position)
"Return (syntax_char . pair) for char after POSITION or nil if char is out of bounds."
	(when-let ((syntax (syntax-after position)))
		(let ((syntax_class (syntax::raw_to_class syntax)))
			(cons
				syntax_class
				(if (find_in_vector_= "\"$" syntax_class)
					(char-after position)
					(cdr syntax))))))

(defun syntax::should_use_text_syntax (position)
"Non-nil if should use `syntax::text_syntax_table'
for determining syntax of char after POSITION.
POSITION must be < (point-max)."
	(save-excursion
		(goto-char position)
		(not (or (inside_code) (progn (forward-char 1) (inside_code))))))

(defmacro syntax::withTextSyntax (&rest body)
	`(let ((syntax_table (syntax-table)) parse-sexp-lookup-properties)
		(set-syntax-table syntax::text_syntax_table)
		(prog1 (progn ,@body) (set-syntax-table syntax_table))))

(defun syntax::is_surrounded_by_matching_parens
	(left right &optional proper_same_delimiters_check)
"LEFT should be position after possibly opening char, possibly out of bounds,
RIGHT position before possibly closing char, possibly out of bounds.

Return nil or cons (opening_paren_length . closing_paren_length), e.g.
- in C /* and */ (2 . 2)
- in xml <!-- and --> (4 . 3)
- in C ( and ) (1 . 1).

If PROPER_SAME_DELIMITERS_CHECK is nil, don't check if left is inside string
or same paired delimiters.
Different results for these situations (< and > are left and right):
- \"string\"<>\"string\" if PROPER_SAME_DELIMITERS_CHECK is nil, return non-nil;
- $inside$<>$inside$ if PROPER_SAME_DELIMITERS_CHECK is nil, return non-nil."
	(when-let (
		(char_before (char-before left))
		(char_after (char-after right))
		((not (is_backslashed (1- left))))
		((not (is_backslashed right)))
	)
		(let (
			(should_use_text_syntax_before (syntax::should_use_text_syntax (1- left)))
			(should_use_text_syntax_after (syntax::should_use_text_syntax right))
		)
			; They must be equal to be matching.
			(when (eq should_use_text_syntax_before should_use_text_syntax_after)
				(cond
					(should_use_text_syntax_before
						; Could be done with syntax::text_syntax_table, but I guess this
						; is simpler.
						(when-let (
							(opening_paren_index
								(index_in_vector_=
									syntax::opening_paren_string char_before))
							(closing_paren_index
								(index_in_vector_=
									syntax::closing_paren_string char_after))
							((= opening_paren_index closing_paren_index))
						)
							(cons 1 1)))
					((let* (
						(syntax (syntax-after (1- left)))
						(syntax_class (syntax::raw_to_class syntax))
					)
							(cond
								((= syntax_class ?\() (eq char_after (cdr syntax)))
								((= syntax_class ?\")
									(and
										(= char_after char_before)
										(or
											(not proper_same_delimiters_check)
											(save-excursion
												(goto-char left)
												(inside_string)))))
								((= syntax_class ?$)
									(and
										(= char_after char_before)
										(or
											(not proper_same_delimiters_check)
											(eq
												(1- left)
												(nth
													1
													(save-excursion
														(syntax-ppss left)))))))
								((= syntax_class ?|)
									(=
										(syntax::raw_to_class
											(save-excursion (syntax-after right)))
										?|))))
						(cons 1 1))
					((and
							comment::region_start_string
							(save-excursion
								(goto-char left)
								(is_after_string comment::region_start_string))
							(save-excursion
								(goto-char right)
								(is_before_string comment::region_end_string)))
						(cons
							(length comment::region_start_string)
							(length comment::region_end_string))))))))


(defun insert::with_text_syntax (string_or_comment_start fn)
"Call FN with `syntax::text_syntax_table' active.
STRING_OR_COMMENT_START must be non-nil.
Based on `electric-pair--with-syntax'."
	; Here we assume that the `syntax-ppss' cache has already been filled
	; past `string-or-comment' with data corresponding to the "normal" syntax
	; (this should be the case because string_or_comment_start was returned
	; in the `nth 8' of `syntax-ppss').
	; Maybe we should narrow-to-region so that `syntax-ppss' uses the narrow cache?
	(syntax-ppss-flush-cache string_or_comment_start)
	(let (syntax-propertize-function)
		(unwind-protect
			(with-syntax-table syntax::text_syntax_table (funcall fn))
			(syntax-ppss-flush-cache string_or_comment_start))))


(defun insert::should_insert_double_char ()
"If pressing space or enter should insert double space/newline.
E.g. from while (true) {|} to while (true) { | },
from while (true) {|} to
while (true) {
	|
}
or from /*|*/ to /* | */, etc."
	(and
		; Very often strings will start/end with a single space,
		; or have other unusual endings, so don't interfere with that.
		(not (inside_string))
		; proper_same_delimiters_check non-nil to avoid triggering in
		; situations like "string"|"string".
		(syntax::is_surrounded_by_matching_parens (point) (point) t)))

; Smart insert space.
(define-key global-map " "
	(fn_symbol "key::space"
		(lambda () (interactive)
			(barf-if-buffer-read-only)
			(setq this-command 'self-insert-command)
			(undo-auto-amalgamate)
			(maybe_delete_marked_region)
			(if current-prefix-arg
				(let ((n (prefix-numeric-value current-prefix-arg)))
					(when (> n 0) (insert-char ?\s n t)))
				(if (not (insert::should_insert_double_char))
					(insert-and-inherit ?\s)
					(insert-and-inherit "\s\s")
					(forward-char -1)))
			(frame-make-pointer-invisible)
			(funcall after_move_hook_fn))))

; Basic insert space.
(define-key global-map [?\S-\s]
	(fn_symbol "key::space_shift"
		(lambda (n) (interactive "p")
			(when (> n 0)
				(barf-if-buffer-read-only)
				(setq this-command 'self-insert-command)
				(undo-auto-amalgamate)
				(maybe_delete_marked_region)
				(insert-char ?\s n t)
				(frame-make-pointer-invisible)
				(funcall after_move_hook_fn)))))

(defun insert::delete_surrounding_blanks ()
	(let ((caret (point)))
		(skip-chars-backward " \t")
		(let ((start (point)))
			(goto-char caret)
			(skip-chars-forward " \t")
			(delete-region start (point))))
	nil)

; Smart insert newline.
(defun key::enter (n) (interactive "p")
	(when (> n 0)
		(barf-if-buffer-read-only)
		; Here don't set this-command self-insert-command, because this
		; command is supposed to have different auto undo boundries, e.g.
		; multiple [return]s should be grouped together, but
		; many self-insert-commands, [return], many self-insert-commands,
		; should separate them. Hmm, idk, maybe that's actually worse than
		; just treating [return] as self-insert-command too and setting
		; amalgamating-undo-limit to like 30...
		(undo-auto-amalgamate)
		(maybe_delete_marked_region)
		(dotimes (_ n)
			; Delete trailing whitespace from original line.
			(insert::delete_surrounding_blanks)
			(let (comment_start)
				(cond
					((insert::should_insert_double_char)
						(insert-and-inherit "\n\n")
						(forward-char -1)
						(indent-according-to-mode)
						(let ((caret (point)))
							(forward-line 1)
							(indent-according-to-mode)
							(goto-char caret)))
					; If we are inside line comment and it starts at a line indent
					; column, continue that comment on new line.
					((and
							comment::line_string
							(setq comment_start (nth 8 (syntax-ppss)))
							(save-excursion
								(goto-char comment_start)
								(is_before_string comment::line_string)))
						(let ((line_end (point)))
							(goto-char comment_start)
							(let ((comment_column (current-column)))
								(goto-char line_end)
								(insert-and-inherit ?\n)
								(indent-according-to-mode)
								(and
									; Only continue comment if it is at proper
									; indentation, e.g. not after some code, like here:
									; (defun the_wrath_of_man () ; comment
									;     |
									; here comment won't be continued.
									(= comment_column (current-column))
									; Also don't continue if there is comment starter
									; after caret already, like here:
									; Nice |; comment.
									(not (is_before_string comment::line_string))
									(let (
										(new_line_comment_start (point))
										(comment_start_after_starter
											(+ comment_start (length comment::line_string)))
									)
										; Delete following blanks.
										(skip-chars-forward " \t")
										(delete-region new_line_comment_start (point))
										; Copy indent string from previous line, as it isn't
										; known what format should indent have.
										(goto-char comment_start_after_starter)
										(skip-chars-forward " \t")
										(let (
											(in_comment_indent_string
												(buffer-substring-no-properties
													comment_start_after_starter (point)))
										)
											(goto-char new_line_comment_start)
											(insert-and-inherit
												comment::line_string
												in_comment_indent_string)))))))
					(t
						(insert-and-inherit ?\n)
						(indent-according-to-mode)))))
		(frame-make-pointer-invisible)
		(funcall after_move_hook_fn)))

(define-key global-map [return] #'key::enter)

; Insert newline with indent and trailing space deletion.
(define-key global-map [S-return]
	(fn_symbol "key::enter_shift"
		(lambda (n) (interactive "p")
			(when (> n 0)
				(barf-if-buffer-read-only)
				(setq this-command 'key::enter)
				(undo-auto-amalgamate)
				(maybe_delete_marked_region)
				(dotimes (_ n)
					(insert::delete_surrounding_blanks)
					(insert-and-inherit ?\n)
					(indent-according-to-mode))
				(frame-make-pointer-invisible)
				(funcall after_move_hook_fn)))))

; Basic insert newline.
(define-key global-map [C-return]
	(fn_symbol "key::enter_control"
		(lambda (n) (interactive "p")
			(when (> n 0)
				(barf-if-buffer-read-only)
				(setq this-command 'key::enter)
				(undo-auto-amalgamate)
				(maybe_delete_marked_region)
				(insert-char ?\n n t)
				(frame-make-pointer-invisible)
				(funcall after_move_hook_fn)))))

; If mark is active, add cursor on every marked line, otherwise insert newline,
; indent new line, go back to starting position.
(define-key global-map [A-return]
	(fn_symbol "key::enter_alt"
		(lambda (n) (interactive "p")
			(when (> n 0)
				(if (not mark-active)
					(let ((line_new_end (point)))
						(barf-if-buffer-read-only)
						(setq this-command 'key::enter)
						(undo-auto-amalgamate)
						(dotimes (_ n)
							(insert-and-inherit ?\n)
							(indent-according-to-mode))
						(goto-char line_new_end)
						(frame-make-pointer-invisible))
					(save-excursion
						(goto-char (mark))
						(move-to-column current_column)
						(set-marker (mark-marker) (point)))
					(call-interactively #'mc/edit-lines))
				(funcall after_move_hook_fn)))))


(defun insert::should_tab_skip_char ()
"Non-nil if pressing tab should skip char after caret, instead of inserting a tab."
	(let ((char_before (char-before)) (char_after (char-after)))
		(and
			char_before
			; If char before is ?\n or ?\t, then probably indenting happens,
			; so insert tab, not skip paren.
			; This isn't ideal, could use something better.
			char_after
			(not (find_in_vector_= "\n\t" char_before))
			; If char after has a matching closing paren.
			(let* (
				(syntax
					(if (syntax::should_use_text_syntax (point))
						(aref syntax::text_syntax_table char_after)
						(syntax-after (point))))
				(syntax_class (syntax::raw_to_class syntax))
			)
				(or
					(find_in_vector_= "\"|$" syntax_class)
					(and (cdr syntax) (/= syntax_class ?\())
					; One random special case. If there will be any other, this
					; should be expanded into its own hook/function/etc.
					(and
						(derived-mode-p 'c-mode 'c++-mode)
						(= syntax_class ?\))
						(= char_after ?\;)))))))

(defun key::tab (n) (interactive "p")
	(when (> n 0)
		(barf-if-buffer-read-only)
		; This too is it's own thing, not self-insert-command.
		(undo-auto-amalgamate)
		(if mark-active
			; Indent marked line region.
			(saveCaretAndMark
				(region::apply_on_marked_line_region
					(lambda () (insert-char ?\t n t))))
			(dotimes (_ n)
				(if (insert::should_tab_skip_char)
					; Skip paren after.
					(forward-char 1)
					(insert-and-inherit ?\t)))
			(frame-make-pointer-invisible))
		(funcall after_move_hook_fn)))

(define-key global-map [tab] #'key::tab)

(define-key global-map [S-tab]
	(fn_symbol "key::tab_shift"
		(lambda (n) (interactive "p")
			(when (> n 0)
				(barf-if-buffer-read-only)
				; Group this together with [tab].
				(setq this-command 'key::tab)
				(undo-auto-amalgamate)
				(let (
					; Delete one tab or max of tab-width spaces after caret.
					(delete_indent
						(lambda ()
							(dotimes (_ n)
								(delete-char
									(if (= (following-char) ?\t)
										1
										(-
											(skip-chars-forward
												" " (+ (point) tab-width))))))))
				)
					(if mark-active
						(saveCaretAndMark
							(region::apply_on_marked_line_region delete_indent))
						; This is a special behaviour for inserting tab when
						; [tab] would skip char after.
						(if (insert::should_tab_skip_char)
							(insert-char ?\t n t)
							(save-excursion
								(forward-line 0)
								(funcall delete_indent)))
						(frame-make-pointer-invisible)))
				(funcall after_move_hook_fn)))))

(defun insert::surround_region (opening_paren_char region n)
"Surround REGION with matching parens N times, if OPENING_PAREN_CHAR has a
matching paren in current scope.
Return non-nil on success.
Leave caret after last inserted closing paren."
	(let (syntax)
		(when
			(find_in_vector_=
				"(\"$"
				(if (inside_code)
					; I'm afraid this won't work around something
					; like this in c++: template|typename|
					; because basic char-syntax is probably ?., not ?\(.
					(car
						(setq syntax
							(cons
								(char-syntax opening_paren_char)
								(matching-paren opening_paren_char))))
					(syntax::raw_to_class
						(setq syntax
							(aref syntax::text_syntax_table opening_paren_char)))))
			(goto-char (car region))
			(insert-char opening_paren_char n t)
			(goto-char (+ (cdr region) n))
			(insert-char (or (cdr syntax) opening_paren_char) n t)
			t)))

(defun insert::generic_paren_command (n)
"Generic command for inserting parens.

Every normal one-char paren (opening and closing) can be bound to this,
though probably most major modes should have their own commands handling
specific parens better than this general function.

If mark is active, surround marked region with parens.
Otherwise, insert opening and/or closing paren, or if char is a closing paren
and this exact paren is right after caret and balance around caret is preserved,
then just move past this paren instead of inserting it, to preserve balance.

`last-command-event' must be a char.

Based on `electric-pair-post-self-insert-function'."
	(interactive "p")
	(when (> n 0)
		(barf-if-buffer-read-only)
		(setq this-command 'self-insert-command)
		(undo-auto-amalgamate)
		(if mark-active
			(let (
				(marked_region (region::marked_region))
				(caret (point))
				(mark (mark))
			)
				(if (insert::surround_region last-command-event marked_region n)
					(progn
						(goto-char (+ caret n))
						(set-marker (mark-marker) (+ mark n))
						(setq deactivate-mark nil))
					(deactivate-mark)
					(delete-region (car marked_region) (cdr marked_region))
					(insert-char last-command-event n t)))
			(dotimes (_ n)
				(let ((string_or_comment_start (nth 8 (syntax-ppss))))
					(insert-and-inherit last-command-event)
					(unless (is_backslashed (1- (point)))
						(let* (
							(syntax_info_before
								; If caret is just after string or comment start
								; or before string or comment end,
								; then use last-command-event's normal syntactic meaning -
								; string or comment delimiter.
								(if (and string_or_comment_start (nth 8 (syntax-ppss)))
									(aref syntax::text_syntax_table last-command-event)
									(syntax-after (1- (point)))))
							(syntax_class_before
								(syntax::raw_to_class syntax_info_before))
							(pair_char
								(if (find_in_vector_= "\"$" syntax_class_before)
									last-command-event
									(cdr syntax_info_before)))
						)
							(when pair_char
								(cond
									; Insert matching pair if inserted char wasn't a closing
									; paren and it was inserted outside of comments and
									; strings. Only do it outside comments and strings,
									; because otherwise it would often be annoying - e.g.
									; :(, std::cout << "(" << foo << ")", etc.
									; Maybe these situations could be accounted for in some
									; ways, but it would require complex logic and many
									; special handlings, so I don't think it's worth it.
									((and
											(not string_or_comment_start)
											(/= syntax_class_before ?\)))
										; Insert closing matching paren.
										(insert-and-inherit pair_char)
										(forward-char -1))
									; Call indent-according-to-mode if inserted closing
									; paren at the start of line.
									; This could even be indenting every line in the just
									; closed scope...
									((and
											(cond
												((= syntax_class_before ?\)) t)
												((find_in_vector_= "\"|" syntax_class_before)
													string_or_comment_start)
												((= syntax_class_before ?$)
													(not (eq (point) (nth 1 (syntax-ppss))))))
											(save-excursion
												(forward-char -1)
												(is_line_blank_to_start)))
										(indent-according-to-mode)))))))))
		(frame-make-pointer-invisible)
		(funcall after_move_hook_fn))
	nil)

(defun insert::surround_symbol_command (n) (interactive "p")
	(when-let (
		((> n 0))
		((not mark-active))
		(symbol_region (region::symbol_region (inside_code)))
	)
		(barf-if-buffer-read-only)
		(undo-auto-amalgamate)
		(let ((caret (point)))
			(when
				(insert::surround_region
					(event-basic-type last-command-event)
					symbol_region
					n)
				(goto-char (+ caret n))
				(frame-make-pointer-invisible)
				(funcall after_move_hook_fn))))
	nil)

; All usual one-char opening parens.
; This should be in sync with syntax::text_syntax_table.
(bind_many_keys global-map [[?\C-\(] [?\C-\[] [?\C-\{] [?\C-\"] [?\C-']]
	#'insert::surround_symbol_command)

(bind_many_keys global-map ["(" ")" "[" "]" "{" "}" "\"" "'"]
	#'insert::generic_paren_command)

(provide 'myInsert)
