; -*- lexical-binding:nil -*-

; Keeping the same column for vertical movement.
; This should be window-local.

(defvar-local goal_column nil
"Similar to `temporary-goal-column'.
Used to stay in the same column and preserve hscroll during vertical movement.
Set by `set_goal_column' and used by `goto_goal_column'.
Values:
	nil -
		Set by `after_move_hook_fn', will be set by some vertical movement
		commands, like moving by lines, deleting lines, pasting lines, etc.
	(column . hscroll) -
		Move to column and set hscroll.
		Valid value for `goto_goal_column'.
	(column) -
		Move to column.
		Valid value for `goto_goal_column'.")
(defvar-local current_column 0
"Column number to show in mode-line after every command.")
(defvar-local now_scrolling nil
"Another mechanism trying to mitigate undesired behaviour of emacs moving caret
when scrolling.
If non-nil, then setting region with shift modifier will use position stored in
now_scrolling.
Commands moving caret reset this to nil (outside of scrolling commands).
Without this, shift-click with caret and mark further between than one window
height would be impossible.")

(defvar after_move_hook_fn
	(lambda (&optional column _weak)
		(setq now_scrolling nil)
		(setq goal_column nil)
		(setq current_column (or column (current-column)))
		nil)
"Function that should be called after every command that moves caret
(except scrolling commands, they call scroll::after_move_hook_fn).

After some commands that usually don't move caret, but they sometimes might,
WEAK should be non-nil, to not change current_column in some buffers that
only want \"actual\" caret movement commands to update current_column.
Right now this argument only matters for rectangle::mode, because
there current_column, unlike everywhere else, can be /= (current-column).")

; Syntax and movement units for modes.

; My movement system doesn't use emacs' syntax system, because it is very
; limiting and hard to work with.
; For example, there is no easy way of modifying buffer and then reverting changes,
; without messing with undo and at the same time letting run functions propertizing
; buffers content, to set syntactic properties.
; Supporting these functions would be necessary, because for example emacs' syntax system
; only allows for 2-char comment starters/enders, while xml has 4-char comment starter <!--.
; So instead of emacs' syntax system, I use something like this for movement:
; 2 main categories of movement unit:
; subsymbol, consisting always only of letters or digits,
;	used to separate different styles of symbol writing, like camelCase
;	(subsymbols: camel, Case), S_S_CASE (subsymbols: S, S, CASE), ...
; symbol, consisting of subsymbols and symbol extensions,
;	like camelCase, S_S_CASE, but not foo->bar or std::crap.
; I thought of adding the third one, for foo->bar.baz like stuff,
; but this is not easy. Examples why:
; I guess it should contain things like foo[i]->bar, foo()->bar,
; which definitely would need regex, and that's slow;
; It should distinguish between things like {std::crap} and {for (int i: array)},
; where in this second one expression should probably only contain {i},
; but in first one it should contain whole {std::crap}.
; So for now that's it.

(defvar-local syntax::unit_string_vector nil
"Vector containing strings defining single syntactic units, that are hard
to recognize otherwise, like ->, <=, /*, ... in C.
Elements are used from first to last, so they should be in precedence - more specific
(longer) ones should come first.

Just straight strings, not regexes, because finding wheter caret is inside a (possibly)
long regex match can be very slow, also search limits would need to be specified to work
in large buffers, so it's just not worth for now I think.")

(defvar-local movement_unit_string_vector nil
"like 'syntax::unit_string_vector' but without:
sequences of same chars, like // in C, as they are skipped by default,
one-char-length strings, as control-move always moves by at least one char.")


(let (
	(fn
		(fn_symbol "syntax::unit_string_vector"
			(lambda ()
				(setq syntax::unit_string_vector
					[
						"//" "/*" "*/" ; Could add /**, **/ and ///, it's safe now.
						"->" "."
						"<<=" ">>=" "&=" "|=" "^=" "+=" "-=" "*=" "/=" "%="
						"--" "++"
						"==" "!=" ">=" "<=" "<" ">" "&&" "||"
						"&" "|" "^" "!" "~" "+" "-" "*" "/" "%"
						":"
					]))))
)
	(add-hook 'c-mode-hook fn)
	(add-hook 'c-ts-mode-hook fn))

(let (
	(fn
		(fn_symbol "syntax::unit_string_vector"
			(lambda ()
				(setq syntax::unit_string_vector
					[
						"//" "/*" "*/"
						"->" "." "::"
						"<<=" ">>=" "&=" "|=" "^=" "+=" "-=" "*=" "/=" "%="
						"--" "++"
						"<=>" "==" "!=" ">=" "<=" "<" ">" "&&" "||"
						"&" "|" "^" "!" "~" "+" "-" "*" "/" "%"
						":"
					]))))
)
	(add-hook 'c++-mode-hook fn)
	(add-hook 'c++-ts-mode-hook fn))

(add-hook 'lisp-data-mode-hook
	(fn_symbol "syntax::unit_string_vector"
		(lambda () (setq syntax::unit_string_vector [";"]))))


(let (
	(fn
		(fn_symbol "movement_unit_string_vector"
			(lambda ()
				(setq movement_unit_string_vector
					[
						"/*" "*/"
						"->"
						"<<=" ">>=" "&=" "|=" "^=" "+=" "-=" "*=" "/=" "%="
						"!=" ">=" "<="
					]))))
)
	(add-hook 'c-mode-hook fn)
	(add-hook 'c-ts-mode-hook fn))

(let (
	(fn
		(fn_symbol "movement_unit_string_vector"
			(lambda ()
				(setq movement_unit_string_vector
					[
						"/*" "*/"
						"->"
						"<=>" "<<=" ">>=" "&=" "|=" "^=" "+=" "-=" "*=" "/=" "%="
						"!=" ">=" "<="
					]))))
)
	(add-hook 'c++-mode-hook fn)
	(add-hook 'c++-ts-mode-hook fn))

(add-hook 'lisp-data-mode-hook
	(fn_symbol "movement_unit_string_vector"
		(lambda () (setq movement_unit_string_vector nil))))

; Control-move - literally movement commands with control modifier -
; moving by words or symbols.

(defunWithBase
	(lambda (fn)
		(unless (bobp)
			(forward-char -1)
			(let ((char_after (following-char)))
				(cond
					((funcall fn char_after))
					((find_in_vector_= "\s\t\n" char_after)
						(skip-chars-backward " \t")
						(skip-chars-backward "\n"))
					; I guess there could be prefixes, char quotes and backslashes,
					; but they are generally always single chars, so I guess it's fine.
					((cl-loop
							for movement_unit_string across movement_unit_string_vector
							for position = (is_inside_string movement_unit_string)
							when position
								return
									(goto-char
										(- position (length movement_unit_string)))))
					((if (syntax::should_use_text_syntax (point))
							(let (
								(syntax_class
									(syntax::raw_to_class
										(aref syntax::text_syntax_table char_after)))
							)
								(when (memq syntax_class '(?\( ?\)))
									(syntax::withTextSyntax
										(skip-syntax-backward
											(char-to-string syntax_class)))
									t))
							(let (
								(syntax_class
									(syntax::raw_to_class (syntax-after (point))))
							)
								(when (memq syntax_class '(?\( ?\)))
									(skip-syntax-backward (char-to-string syntax_class))
									t))))
					(t
						(skip-chars-backward
							(if (= ?^ char_after)
								"\\^"
								(char-to-string char_after))))))))

	(control_move_left ()
		(,base
			(lambda (char_after)
				(cond
					((is_lower_letter char_after)
						(skip-chars-backward "[:lower:]")
						(when (is_upper_letter (preceding-char)) (forward-char -1))
						t)
					((is_upper_letter char_after)
						(unless (is_camel_humps) (skip-chars-backward "[:upper:]"))
						t)
					((is_digit char_after) (skip-chars-backward "[:digit:]") t)))))

	(progressive_control_move_left ()
		(,base
			(lambda (char_after)
				(if (syntax::should_use_text_syntax (point))
					(when
						(memq
							(syntax::raw_to_class
								(aref syntax::text_syntax_table char_after))
							'(?w ?_))
						(syntax::withTextSyntax (skip-syntax-backward "w_"))
						t)
					(when (memq (syntax::raw_to_class (syntax-after (point))) '(?w ?_))
						(skip-syntax-backward "w_")
						t))))))

(defunWithBase
	(lambda (fn)
		(unless (eobp)
			(forward-char 1)
			(let ((char_before (preceding-char)))
				(cond
					((funcall fn char_before))
					((find_in_vector_= " \t" char_before) (skip-chars-forward " \t"))
					; Only use this in char_before is newline, not also maybe blank,
					; to jump to the end of trailing spaces if they exist, not skip them.
					((= ?\n char_before)
						(skip-chars-forward "\n")
						(skip-chars-forward " \t"))
					((cl-loop
							for movement_unit_string across movement_unit_string_vector
							for position = (is_inside_string movement_unit_string)
							when position
								return (goto-char position)))
					((if (syntax::should_use_text_syntax (1- (point)))
							(let (
								(syntax_class
									(syntax::raw_to_class
										(aref syntax::text_syntax_table char_before)))
							)
								(when (memq syntax_class '(?\( ?\)))
									(syntax::withTextSyntax
										(skip-syntax-forward
											(char-to-string syntax_class)))
									t))
							(let (
								(syntax_class
									(syntax::raw_to_class (syntax-after (1- (point)))))
							)
								(when (memq syntax_class '(?\( ?\)))
									(skip-syntax-forward (char-to-string syntax_class))
									t))))
					(t
						(skip-chars-forward
							(if (= ?^ char_before)
								"\\^"
								(char-to-string char_before))))))))

	(control_move_right ()
		(,base
			(lambda (char_before)
				(cond
					((is_lower_letter char_before) (skip-chars-forward "[:lower:]") t)
					((is_upper_letter char_before)
						(skip-chars-forward (if (is_camel_humps) "[:lower:]" "[:upper:]"))
						t)
					((is_digit char_before) (skip-chars-forward "[:digit:]") t)))))

	(progressive_control_move_right ()
		(,base
			(lambda (char_before)
				(if (syntax::should_use_text_syntax (1- (point)))
					(when
						(memq
							(syntax::raw_to_class
								(aref syntax::text_syntax_table char_before))
							'(?w ?_))
						(syntax::withTextSyntax (skip-syntax-forward "w_"))
						t)
					(when
						(memq (syntax::raw_to_class (syntax-after (1- (point)))) '(?w ?_))
						(skip-syntax-forward "w_")
						t))))))

; Movement commands.

; Generally they act on visual lines, constrain movement to fields and skip invisible text.
; Intangible, point-entered and point-left properties are completely ignored -
; I don't know of anything that needs them, display property in large part
; makes intangible property obsolete and it doesn't mess with buffer positions.
; If I'll find something that needs point-entered and point-left functions,
; maybe cursor-sensor-mode should be used instead. Maybe if that won't work well
; enough, adjusted implementations of movement commands should be used for that
; mode, which shouldn't be hard to do.
;
; Also skipping invisible text isn't easy at all, every searching/scanning function like
; scan-list, search-forward, etc. would need to ignore invisible text, which isn't really
; possible right now, so not all commands provide proper ignoring of invisible text.
; None of them leave caret inside invisible text though (unless buffer narrowing matters
; in cases like "...<narrowed_pa[rt|>...]", where < is point-min, [ is start of
; invisible text and | is caret).

; Left and right

(defun key::right (n) (interactive "p")
	(when (/= n 0)
		(if (> n 0)
			(while (and (not (eobp)) (> n 0))
				(-- n)
				(forward-char 1)
				(skip_invisible_forward))
			(while (and (not (bobp)) (< n 0))
				(++ n)
				(forward-char -1)
				(skip_invisible_backward))))
	nil)

(fset 'key::left (get_reverse_command #'key::right))

(defun key::left_base (n left_fn right_fn)
	(when (/= n 0)
		(let ((caret (point)))
			(if (> n 0)
				(progn
					(dotimes (_ n)
						(funcall left_fn)
						(skip_invisible_backward))
					(constrain-to-field nil caret t)
					(skip_invisible_backward))
				(dotimes (_ (- n))
					(funcall right_fn)
					(skip_invisible_forward))
				(constrain-to-field nil caret t)
				(skip_invisible_forward))))
	nil)

(defun key::left_control (n) (interactive "p")
	(key::left_base n #'control_move_left #'control_move_right))

(fset 'key::right_control (get_reverse_command #'key::left_control))

(defun key::left_alt (n) (interactive "p")
	(key::left_base n #'progressive_control_move_left #'progressive_control_move_right))

(fset 'key::right_alt (get_reverse_command #'key::left_alt))

(dolist (
	key_>_<_move_fn
	(list
		(vector [left] #'> #'key::left)
		(vector [C-left] #'> #'key::left_control)
		(vector [right] #'< #'key::right)
		(vector [C-right] #'< #'key::right_control))
)
	(define-key global-map (aref key_>_<_move_fn 0)
		`(lambda () (interactive)
			(if (not mark-active)
				(call-interactively #',(aref key_>_<_move_fn 2))
				(when (,(aref key_>_<_move_fn 1) (point) (mark)) (goto-char (mark)))
				(deactivate-mark))
			(funcall after_move_hook_fn))))

; Functions like expand region, but just move caret.

(defvar region::get_intermediate_unit_start_fn nil
"Movement unit, bigger than symbol and smaller than paragraph.
Used in interactive jumping to scope start/end and in expanding region.
For example, text-mode sets it to sentence, c-mode to statement.
Called with limit start and end, result must be in this range (inclusive).
If caret is at start of unit, this function should return caret, unless
limit_end < caret, then it should return nil or start of unit
that caret is in but isn't on the start of.
For example, c-mode normally returns statement delimited by comma,
but if limit_end < caret and caret is already at start of statement
delimited by comma, it tries to extend this unit to one delimited by
semicolon.")

(defvar region::get_intermediate_unit_end_fn nil
"Like `region::get_intermediate_unit_start_fn'.")

(add-hook 'text-mode-hook
	(fn_symbol "region::intermediate_unit"
		(lambda ()
			(setq-local
				region::get_intermediate_unit_start_fn
					(lambda (limit_start limit_end)
						(when-let (
							(sentence_start (region::sentence_start limit_start))
							((<= sentence_start limit_end))
						)
							sentence_start))
				region::get_intermediate_unit_end_fn
					(lambda (limit_start limit_end)
						(when-let (
							(sentence_end (region::sentence_end limit_end))
							((>= sentence_end limit_start))
						)
							sentence_end))))))

(add-hook 'c-mode-hook
	(fn_symbol "region::intermediate_unit"
		(lambda ()
			(setq-local
				region::get_intermediate_unit_start_fn
					(lambda (limit_start limit_end)
						(let (
							(pos
								(save-excursion
									(unless (eobp) (forward-char 1))
									(c-beginning-of-statement 1)
									(point)))
						)
							(when (in_range_inclusive pos limit_start limit_end)
								pos)))
				region::get_intermediate_unit_end_fn
					(lambda (limit_start limit_end)
						(let (
							(pos
								(save-excursion
									(unless (bobp) (forward-char -1))
									(c-end-of-statement 1)
									(point)))
						)
							(when (in_range_inclusive pos limit_start limit_end)
								pos)))))))

; Old with attempted comma support, but it turns out only beginning-of-statement-1 supports it,
; and there is no easy way to get end of statement delimited by comma,
; so for now I just use beginning-of-statement and end-of-statement that don't use commas
; as delimiters.
;(add-hook 'c-mode-hook
;	(fn_symbol "region::intermediate_unit"
;		(lambda ()
;			(setq-local
;				region::get_intermediate_unit_start_fn
;					(lambda (limit_start limit_end)
;						(let* ((caret (point))
;							   (get_statement_start_if_just_before_or_inside_statement
;								(lambda (commas_as_delimiters)
;									(save-excursion
;										; If not moved to previous statement.
;										(if (not
;												(eq
;													'previous
;													(c-beginning-of-statement-1
;														limit_start nil nil commas_as_delimiters)))
;											(point)
;											(let ((maybe_start (point)))
;												(goto-char (1+ caret))
;												; If caret was on statement start.
;												(if (c-beginning-of-statement-1
;														caret nil nil commas_as_delimiters t)
;													caret
;													maybe_start))))))
;							   (start ; With commas as delimiters.
;								(funcall get_statement_start_if_just_before_or_inside_statement t)))
;							(cond
;								((<= start limit_end) start)
;								((<=
;										(setq start
;											(funcall
;												get_statement_start_if_just_before_or_inside_statement
;												nil))
;										limit_end)
;									start))))
;				region::get_intermediate_unit_end_fn
;					(lambda (limit_start limit_end)
;						(let* ((caret (point))
;							   (get_statement_end_if_just_after_or_inside_statement
;								(lambda (commas_as_delimiters)
;									(save-excursion
;										; If not moved to next statement.
;										(if (not
;												(eq
;													'previous
;													(c-beginning-of-statement-1
;														limit_start nil nil commas_as_delimiters)))
;											(point)
;											(let ((maybe_start (point)))
;												(goto-char (1+ caret))
;												; If caret was on statement start.
;												(if (c-beginning-of-statement-1
;														caret nil nil commas_as_delimiters t)
;													caret
;													maybe_start))))))
;							   (end ; With commas as delimiters.
;								(funcall get_statement_end_if_just_after_or_inside_statement t)))
;							(cond
;								((>= end limit_start) end)
;								((>=
;										(setq end
;											(funcall
;												get_statement_end_if_just_after_or_inside_statement
;												nil))
;										limit_start)
;									end))))))))

(defun key::left_alt_control (n)
"Jump to current scope start, or if already at it, jump to outer scope start.
Can be repeated until at point-min.
One exception: if caret is exactly after closing paren,
then jump to it's matching opening paren.
So this can be used to jump to highlighted opening paren or to surrounding scope start.
Also saves positions in jumpHistory if jumping at least two chars."
	(interactive "p")
	(when (/= n 0)
		(jumpHistory::add)
		(let ((caret_1 (point)) (n_1 n) (pos t))
			(if (> n 0)
				(progn
					(while (and (not (bobp)) pos (> n 0))
						(-- n)
						(let (
							(caret (point))
							(get_intermediate_unit_start_or_limit_fn
								(lambda (limit_start limit_end)
									(or
										(and
											region::get_intermediate_unit_start_fn
											(funcall region::get_intermediate_unit_start_fn
												limit_start limit_end))
										limit_start)))
							(at_scope_start_fn ; Should be called with caret at region max start.
								(lambda ()
									(let (
										(limit_region
											(if-let (
												(opening_paren (nth 1 (syntax-ppss)))
												(closing_paren (ignore-errors (scan-lists (point) 1 1)))
											)
												(cons (1+ opening_paren) (1- closing_paren))
												(cons (point-min) (point-max))))
									)
										(if-let (
											((region::in_paragraph limit_region))
											(paragraph_start (region::paragraph_start limit_region t))
										)
											(funcall get_intermediate_unit_start_or_limit_fn
												paragraph_start (point))
											(car limit_region)))))
							(inside_string_or_comment_fn
								(lambda (
									string_or_comment_region_min
									forward_char_arg_if_at_string_or_comment_start
									get_sentence_start_fn
									get_paragraph_start_fn
								)
									(if (= caret (car string_or_comment_region_min))
										(save-excursion
											(forward-char forward_char_arg_if_at_string_or_comment_start)
											(funcall at_scope_start_fn))
										(if-let (
											(paren_index
												(index_in_vector_= syntax::closing_paren_string (preceding-char)))
											((not (is_backslashed (1- caret))))
											(opening_paren_pos
												(save-excursion
													(forward-char -1)
													(region::find_opening_paren
														paren_index (car string_or_comment_region_min))))
										)
											opening_paren_pos
											(let (
												(paragraph_or_sentence_or_limit
													(lambda (limit_region)
														(if-let (
															(paragraph_start
																; Reminder that paragraph_start may be > caret,
																; because of added skipping blanks at the end of
																; region::paragraph_start.
																; I'm not sure how to handle situation when it's > caret -
																; there are 2 options: find previous paragraph's start,
																; or treat this like there is no paragraph before caret,
																; so skip to jumping to closest opening paren
																; or string or comment start.
																; Now I choose the second option just because it's easier,
																; that is a rare situation anyway
																; and there is no intuitive behaviour for this situation.
																(funcall get_paragraph_start_fn limit_region))
															((< paragraph_start caret))
														)
															(let (
																(sentence_start
																	(funcall get_sentence_start_fn paragraph_start))
															)
																(if (= caret sentence_start)
																	paragraph_start
																	sentence_start))
															(car limit_region))))
											)
												(if-let (
													(closest_opening_paren
														(region::find_closest_opening_paren
															(car string_or_comment_region_min)))
												)
													(if
														(=
															caret
															; Get actual position, because now it's
															; (position_before . index).
															(setq closest_opening_paren (1+ (car closest_opening_paren))))
														(save-excursion
															(forward-char -1)
															; We moved out of scope, so use the smallest unit
															; in outer scope - sentence.
															; Here it will always be non-nil and it can be = (point),
															; but that's ok, as we moved already.
															(funcall get_sentence_start_fn
																(car string_or_comment_region_min)))
														(funcall paragraph_or_sentence_or_limit
															(cons closest_opening_paren (pos-eol))))
													(funcall paragraph_or_sentence_or_limit
														string_or_comment_region_min)))))))
						)
							(setq pos
								(cond
									((inside_string)
										; Sanity check.
										(when (setq pos (region::string_region_min))
											(funcall inside_string_or_comment_fn
												pos
												-1
												#'region::sentence_start
												(lambda (limit_region)
													(when (region::in_paragraph limit_region)
														(region::paragraph_start limit_region t))))))
									((inside_comment)
										; Sanity check.
										(when (setq pos (region::comment_region_min_and_type))
											(funcall inside_string_or_comment_fn
												(car pos)
												(-
													(length
														(if (eq (cdr pos) 'lineComment)
															comment::line_string
															comment::region_start_string)))
												(lambda (limit)
													(region::sentence_start_in_comment limit (cdr pos)))
												(lambda (limit_region)
													(when
														(if (eq (cdr pos) 'lineComment)
															(region::in_paragraph_in_line_comment)
															(region::in_paragraph limit_region))
														(region::paragraph_start_in_comment
															limit_region (cdr pos) t))))))
									(t ; Outside of comments and strings.
										(let ((syntax_class (syntax::raw_to_class (syntax-after (1- caret)))))
											(cond
												; After closing quote (because after opening quote case
												; is handled by string branch).
												((memq syntax_class '(?\" ?|))
													(save-excursion
														(forward-char -1)
														(region::string_region_max_start)))
												; After closing paren.
												((or
														(= syntax_class ?\))
														(and
															(= syntax_class ?$)
															(not (eq (nth 1 (syntax-ppss)) (1- caret)))))
													(ignore-errors (scan-lists caret -1 0)))
												; After opening paren.
												((memq syntax_class '(?\( ?$))
													(save-excursion
														(forward-char -1)
														(funcall at_scope_start_fn)))
												(t ; Przypomnienie o todosie w myRegion - to poniżej jest złe.
													; Like at_scope_start_fn but can't stay in place.
													; ((1- caret), because we know that char before is not a paren)
													(let (
														(limit_region
															(if-let (
																(opening_paren (nth 1 (syntax-ppss (1- caret))))
																(closing_paren (ignore-errors (scan-lists (1- caret) 1 1)))
															)
																(cons (1+ opening_paren) (1- closing_paren))
																(cons (point-min) (point-max))))
													)
														(goto-char caret)
														(if-let (
															((region::in_paragraph limit_region))
															(paragraph_start (region::paragraph_start limit_region t))
															((< paragraph_start caret))
														)
															(funcall get_intermediate_unit_start_or_limit_fn
																paragraph_start (1- caret))
															(car limit_region)))))))))
							(when pos (goto-char pos))))
					(skip_invisible_backward))
				(while (and (not (eobp)) pos (< n 0))
					(++ n)
					(let (
						(caret (point))
						(get_intermediate_unit_end_or_limit_fn
							(lambda (limit_start limit_end)
								(or
									(and
										region::get_intermediate_unit_end_fn
										(funcall region::get_intermediate_unit_end_fn
											limit_start limit_end))
									limit_end)))
						(at_scope_end_fn ; Should be called with caret at region max end.
							(lambda ()
								(let (
									(limit_region
										(if-let (
											(opening_paren (nth 1 (syntax-ppss)))
											(closing_paren (ignore-errors (scan-lists (point) 1 1)))
										)
											(cons (1+ opening_paren) (1- closing_paren))
											(cons (point-min) (point-max))))
								)
									(if-let (
										((region::in_paragraph limit_region))
										(paragraph_end (region::paragraph_end limit_region t))
									)
										(funcall get_intermediate_unit_end_or_limit_fn
											(point) paragraph_end)
										(cdr limit_region)))))
						(inside_string_or_comment_fn
							(lambda
								(
									string_or_comment_region_min
									forward_char_arg_if_at_string_or_comment_end
									get_sentence_end_fn
									get_paragraph_end_fn
								)
								(if (= caret (cdr string_or_comment_region_min))
									(save-excursion
										(forward-char forward_char_arg_if_at_string_or_comment_end)
										(funcall at_scope_end_fn))
									(if-let (
										(paren_index
											(index_in_vector_= syntax::opening_paren_string (following-char)))
										((not (is_backslashed caret)))
										(closing_paren_pos
											(save-excursion
												(forward-char 1)
												(region::find_closing_paren
													paren_index (cdr string_or_comment_region_min))))
									)
										closing_paren_pos
										(let (
											(paragraph_or_sentence_or_limit
												(lambda (limit_region)
													(if-let (
														(paragraph_end
															; Reminder that paragraph_end may be < caret,
															; because of added skipping blanks at the end of
															; region::paragraph_end.
															; I'm not sure how to handle situation when it's < caret -
															; there are 2 options: find next paragraph's end,
															; or treat this like there is no paragraph after caret,
															; so skip to jumping to closest closing paren
															; or string or comment end.
															; Now I choose the second option just because it's easier,
															; that is a rare situation anyway
															; and there is no intuitive behaviour for this situation.
															(funcall get_paragraph_end_fn limit_region))
														((> paragraph_end caret))
													)
														(let (
															(sentence_end
																(funcall get_sentence_end_fn paragraph_end))
														)
															(if (= caret sentence_end)
																paragraph_end
																sentence_end))
														(cdr limit_region))))
										)
											(if-let (
												(closest_closing_paren
													(region::find_closest_closing_paren
														(cdr string_or_comment_region_min)))
											)
												(if
													(=
														caret
														; Get actual position, because now it's
														; (position_after . index).
														(setq closest_closing_paren
															(1- (car closest_closing_paren))))
													(save-excursion
														(forward-char 1)
														; We moved out of scope, so use the smallest unit
														; in outer scope - sentence.
														; Here it will always be non-nil and it can be = (point),
														; but that's ok, as we moved already.
														(funcall get_sentence_end_fn
															(cdr string_or_comment_region_min)))
													(funcall paragraph_or_sentence_or_limit
														(cons (pos-bol) closest_closing_paren)))
												(funcall paragraph_or_sentence_or_limit
													string_or_comment_region_min)))))))
					)
						(setq pos
							(cond
								((inside_string)
									; Sanity check.
									(when (setq pos (region::string_region_min))
										(funcall inside_string_or_comment_fn
											pos
											1
											#'region::sentence_end
											(lambda (limit_region)
												(when (region::in_paragraph limit_region)
													(region::paragraph_end limit_region t))))))
								((inside_comment)
									; Sanity check.
									(when (setq pos (region::comment_region_min_and_type))
										(funcall inside_string_or_comment_fn
											(car pos)
											(if (eq (cdr pos) 'lineComment)
												1
												(length comment::region_end_string))
											(lambda (limit)
												(region::sentence_end_in_comment limit (cdr pos)))
											(lambda (limit_region)
												(when
													(if (eq (cdr pos) 'lineComment)
														(region::in_paragraph_in_line_comment)
														(region::in_paragraph limit_region))
													(region::paragraph_end_in_comment
														limit_region (cdr pos) t))))))
								(t ; Outside of comments and strings.
									(let ((syntax_class (syntax::raw_to_class (syntax-after caret))))
										(cond
											; Before opening quote (because before closing quote case
											; is handled by string branch).
											((memq syntax_class '(?\" ?|))
												(save-excursion
													(forward-char 1)
													(region::string_region_max_end)))
											; Before opening paren.
											((or
												(= syntax_class ?\()
												(and
													(= syntax_class ?$)
													(eq
														(nth 1 (save-excursion (syntax-ppss (1+ caret))))
														caret)))
												(ignore-errors (scan-lists caret 1 0)))
											; Before closing paren.
											((memq syntax_class '(?\) ?$))
												(save-excursion
													(forward-char 1)
													(funcall at_scope_end_fn)))
											(t
												; Like at_scope_end_fn but can't stay in place.
												; ((1+ caret), because we know that char after is not a paren)
												(let (
													(limit_region
														(if-let (
															(opening_paren (nth 1 (syntax-ppss (1+ caret))))
															(closing_paren (ignore-errors (scan-lists (1+ caret) 1 1)))
														)
															(cons (1+ opening_paren) (1- closing_paren))
															(cons (point-min) (point-max))))
												)
													(goto-char caret)
													(if-let (
														((region::in_paragraph limit_region))
														(paragraph_end (region::paragraph_end limit_region t))
														((> paragraph_end caret))
													)
														(funcall get_intermediate_unit_end_or_limit_fn
															(1+ caret) paragraph_end)
														(cdr limit_region)))))))))
						(when pos (goto-char pos))))
				(skip_invisible_forward))
			(constrain-to-field nil caret_1 t)
			(if (> n_1 0) (skip_invisible_backward) (skip_invisible_forward))))
	nil)

(fset 'key::right_alt_control (get_reverse_command #'key::left_alt_control))

; Up and down

(defun set_goal_column ()
"Set `goal_column'. `current_column' must be set."
	(setq goal_column
		(let ((hscroll (window-hscroll)))
			(cons
				(let ((event (posn-at-point)) x)
					(cond
						; Handle the `overflow-newline-into-fringe' case.
						((eq (nth 1 event) 'right-fringe) (window-body-width))
						((setq x (car (posn-x-y event)))
							(/
								(float (- x (line-number-display-width t)))
								(frame-char-width)))
						; When we move beyond the first/last character visible in
						; the window, posn-at-point will return nil.

						; If lines can be wrapped.
						((and
								(= hscroll 0)
								(not
									(or
										truncate-lines
										(truncated-partial-width-window-p))))
							(mod current_column (window-body-width)))
						(t (- current_column hscroll))))
				hscroll))))

(defun goto_goal_column (&optional n)
"Move to column, maybe set hscroll, move N or 0 lines forward."
	(unless n (setq n 0))
	(and
		(cdr goal_column)
		(/= (cdr goal_column) (window-hscroll))
		(set-window-hscroll nil (cdr goal_column)))
	(vertical-motion (cons (car goal_column) n) nil nil t))

(defun maybe_adjust_vscroll (moved_backward)
"Adjust vscroll to only show bottom default-line-height pixels of current line,
if that line is taller than window, MOVED_BACKWARD is non-nil and point is
before window-start."
	(and
		moved_backward
		(< (point) (window-start))
		(let (
			(line_height (line-pixel-height))
			(edges (window-inside-pixel-edges))
		)
			; > line_height window_height
			(when (> line_height (- (nth 3 edges) (nth 1 edges) 1))
				(set-window-vscroll nil (- line_height (default-line-height)) t)))))

; line-move-1
(defun logical_move_up (n)
	(let ((caret (point)) (n_1 n))
		(if (> n 0)
			(progn
				(forward-line 0)
				(while (and (> n 0) (not (bobp)))
					(unless (invisible-p (1- (point))) (-- n))
					(forward-line -1))
				(constrain-to-field nil caret t t)
				; Idk, maybe wrong. At least won't move > caret
				; (assuming that caret was not in invisible text).
				(skip_invisible_forward))
			(while (and (< n 0) (not (eobp)))
				(forward-line 1)
				(unless (invisible-p (1- (point))) (++ n)))
			(constrain-to-field nil caret t t)
			(skip_invisible_backward))
		; This wasn't here for some reason, idk why this would differ from
		; visual movement.
		(goto_goal_column)
		(maybe_adjust_vscroll (> n_1 0))))

; Core part of key::up, n must be != 0.
; Modified version of line-move.
(defun line_move (n)
	(setq now_scrolling nil)
	(unless goal_column (set_goal_column))
	; For now probably always unnecessary, but it probably just returns a cached
	; value if this is really unnecessary, so it's fine.
	(setq current_column (current-column))
	(let (
		(caret (point))
		(truncated (or truncate-lines (truncated-partial-width-window-p)))
	)
		(unless
			(and
				auto-window-vscroll
				; Only vscroll for single line moves.
				(= (abs n) 1)
				; Under scroll-conservatively, the display engine does this
				; better.
				(= scroll-conservatively 0)
				; But don't vscroll in a keyboard macro.
				(not defining-kbd-macro)
				(not executing-kbd-macro)
				(not (and truncated (long-line-optimizations-p)))
				(if (> n 0)
					; Move up.
					; If already vscrolled, reduce vscroll.
					(let (
						(vscroll (window-vscroll nil t))
						(default_line_height (default-line-height))
					)
						(when (> vscroll default_line_height)
							(set-window-vscroll nil (- vscroll default_line_height) t)
							t))
					; Move down.
					(let* (
						(line_height (window-line-height -1))
						(rowh (car line_height))
						(vpos (nth 1 line_height))
						(ypos (nth 2 line_height))
						(rbot (nth 3 line_height))
						(this_line_height (window-line-height))
						(this_height (car this_line_height))
						(this_ypos (nth 2 this_line_height))
						(default_line_height (default-line-height))
						(screen_lines (window-screen-lines))
						(edges (window-inside-pixel-edges))
						(window_height (- (nth 3 edges) (nth 1 edges) 1))
						py vscroll last-line
					)
						(when (> (mod screen_lines 1.0) 0.0)
							(setq screen_lines (round (+ screen_lines 0.5))))
						(when
							(or
								(null line_height)
								(>= rbot default_line_height)
								(<= ypos (- default_line_height))
								(null this_line_height)
								(<= this_ypos (- default_line_height)))
							(unless line_height
								(let ((wend (pos-visible-in-window-p t nil t)))
									(setq rbot (nth 3 wend))
									(setq rowh  (nth 4 wend))
									(setq vpos (nth 5 wend))))
							(unless this_line_height
								(let ((wstart (pos-visible-in-window-p nil nil t)))
									(setq this_ypos (nth 2 wstart))
									(setq this_height (nth 4 wstart))))
							(setq py
								(or
									(nth 1 this_line_height)
									(let ((event (posn-at-point)))
										(if-let ((col-row (posn-actual-col-row event)))
											(- (cdr col-row) (window-vscroll))
											(cdr (posn-col-row event))))))
							; VPOS > 0 means the last line is only partially visible.
							; But if the part that is visible is at least as tall
							; as the default font, that means the line is actually
							; fully readable, and something like line-spacing is
							; hidden. So in that case we accept the last line in
							; the window as still visible, and consider the margin
							; as starting one line later.
							(when (and vpos (> vpos 0))
								(setq last-line (min (- screen_lines scroll-margin) vpos))
								(unless
									(and
										rowh
										(>= rowh (default-font-height))
										(< rowh default_line_height))
									(-- last-line)))
							(cond
								; If last line of window is fully visible, and vscrolling
								; more would make this line invisible, move forward.
								((and
										(or
											(<
												(setq vscroll (window-vscroll nil t))
												default_line_height)
											(null this_height)
											(<= this_height default_line_height))
										(or (null rbot) (= rbot 0)))
									nil)
								; If cursor is not in the bottom scroll margin, and the
								; current line is not too tall, or if there's a
								; continuation line below this one, move forward.
								((and
										(or (null this_height) (<= this_height window_height))
										vpos
										(> vpos 0)
										(or (< py last-line) (display--line-is-continued-p)))
									nil)
								; When already vscrolled, we vscroll some more if we can,
								; or clear vscroll and move forward at end of tall image.
								((> vscroll 0)
									(when
										(or
											(and rbot (> rbot 0))
											(and this_height (> this_height default_line_height)))
										(set-window-vscroll nil (+ vscroll default_line_height) t)))
								; If cursor just entered the bottom scroll margin, move forward,
								; but also optionally vscroll one line so redisplay won't recenter.
								((and vpos (> vpos 0) (= py last-line))
									; Don't vscroll if the partially-visible line at window
									; bottom is not too tall (a.k.a. "just one more text
									; line"): in that case, we do want redisplay to behave
									; normally, i.e. recenter or whatever.
									;
									; Note: ROWH + RBOT from the value returned by
									; pos-visible-in-window-p give the total height of the
									; partially-visible glyph row at the end of the window.
									; As we are dealing with floats, we disregard sub-pixel
									; discrepancies between that and DEFAULT_LINE_HEIGHT.
									(and
										rowh
										rbot
										(>= (- (+ rowh rbot) window_height) 1)
										(set-window-vscroll nil default_line_height t))
									(logical_move_up n)
									t)
								; If there are lines above the last line, scroll-up one line.
								((and vpos (> vpos 0)) (scroll-up 1) t)
								; Finally, start vscroll.
								(t (set-window-vscroll nil default_line_height t) t))))))
			(set-window-vscroll nil 0 t)
			; When the text in the window is scrolled to the left,
			; display-based motion doesn't make sense (because each
			; logical line occupies exactly one screen line).
			(if (or truncated (/= (window-hscroll) 0))
				(logical_move_up n)
				; vertical-motion can move more than it was asked to if it
				; moves across display strings with newlines.
				(and
					(if (> n 0)
						(<= (goto_goal_column (- n)) (- n))
						(>= (goto_goal_column (- n)) (- n)))
					(or
						(> n 0)
						(/= (point) caret)
						; If the goal column lies on a display string,
						; `vertical-motion' advances the cursor to the end
						; of the string. For n > 0, this can cause the
						; cursor to get stuck.
						(vertical-motion (- n))))
				; If we moved into a tall line, set vscroll to make
				; scrolling through tall images more smooth.
				(maybe_adjust_vscroll (> n 0)))))
	(setq current_column (current-column)))

(defun key::up (n) (interactive "p")
	(when (/= n 0)
		(let (mark)
			(if
				(and
					mark-active
					(progn
						(setq mark (mark))
						(deactivate-mark)
						; If caret and mark are on the same line,
						; deactivate mark and do a normal line move.
						(not (is_caret_on_the_same_line mark))))
				(when
					(if (> n 0)
						(> (point) mark)
						(< (point) mark))
					(goto-char mark)
					(funcall after_move_hook_fn))
				(line_move n)))))

(fset 'key::down (get_reverse_command #'key::up))

(define-key global-map [up] #'key::up)
(define-key global-map [down] #'key::down)

(defun key::up_shift (n) (interactive "p")
	(when (/= n 0)
		(let ((caret (point)))
			(line_move n)
			(cond
				(mark-active (when (= (point) (mark)) (deactivate-mark)))
				((/= (point) caret) (set-mark caret))))))

(fset 'key::down_shift (get_reverse_command #'key::up_shift))

(define-key global-map [S-up] #'key::up_shift)
(define-key global-map [S-down] #'key::down_shift)

; Home and end

; With prefix use logical movement.

(defun key::home (arg) (interactive "P")
	(jumpHistory::add)
	(let ((caret (point)) line_start)
		(if arg
			(progn
				(forward-line 0)
				(while (and (not (bobp)) (invisible-p (1- (point))))
					(forward-line -1))
				(setq line_start (point))
				(skip-chars-forward " \t")
				(if (= (point) caret)
					(goto-char line_start)
					(skip_invisible_backward)))
			(vertical-motion 0)
			(setq line_start (point))
			(goto-char caret)
			(goto_visual_line_end)
			(let ((line_end (point)))
				(goto-char line_start)
				(skip-chars-forward " \t" line_end)
				(if (or (= (point) line_end) (= (point) caret))
					(goto-char line_start)
					(skip_invisible_backward))))
		(constrain-to-field nil caret t))
	(skip_invisible_forward))

(defun key::end (arg) "Return non-nil to track eol." (interactive "P")
	(jumpHistory::add)
	(let ((caret (point)))
		(if arg
			(while (and (not (eobp)) (invisible-p (goto-char (pos-eol))))
				(forward-char 1))
			(goto_visual_line_end))
		(let ((new_caret (point)))
			(and
				(not buffer-read-only)
				(is_line_blank_to_start)
				(condition-case nil (indent-according-to-mode)
					(buffer-read-only
						; I'm not sure how indent-according-to-mode handles
						; read-only errors, but in case it leaves point at some
						; random spot, return to some reasonable position.
						(goto-char new_caret))))
			; If after moving, point is the same as the one after maybe
			; indenting and constraining to field, track end of line.
			(let (
				(maybe_track_eol
					(=
						(point)
						new_caret
						; This min is because it turns out constrain-to-field
						; needs this arg to be in range.
						(constrain-to-field nil (min caret (point-max)) t)))
			)
				(skip_invisible_backward)
				(and
					; I added this now, better to split it into more and less
					; "smart" version w/o and with prefix.
					(not arg)
					maybe_track_eol
					; But don't track if skipping invisible text moved us from eol.
					(eolp)
					(setq goal_column
						(list
							; If lines are truncated.
							(if
								(and
									(= (window-hscroll) 0)
									(not
										(or
											truncate-lines
											(truncated-partial-width-window-p))))
								; Keep mostright column, aka visual eol.
								(window-body-width)
								; most-positive-fixnum is too much because
								; vertical-motion has a random limit in it to MAX_INT,
								; so 2147483648.
								long-line-threshold))))))))

(let (
	(end
		(lambda (arg)
			(if (key::end arg)
				; If tracking end of lines, don't reset goal_column.
				(let (goal_column) (funcall after_move_hook_fn))
				(funcall after_move_hook_fn))))
)

	(define-key global-map [end]
		`(lambda (arg) (interactive "P")
			(deactivate-mark)
			(,end arg)))

	(define-key global-map [S-end]
		`(lambda (arg) (interactive "P")
			(let ((caret (point)))
				(,end arg)
				(cond
					(mark-active (when (= (point) (mark)) (deactivate-mark)))
					((/= (point) caret) (set-mark caret)))))))

; Goto field bounds, or with prefix, accessible portion of buffer.
(defun key::home_control (arg) (interactive "P")
	(jumpHistory::add)
	(goto-char (if arg (point-min) (field-beginning))))
(defun key::end_control (arg) (interactive "P")
	(jumpHistory::add)
	(goto-char (if arg (point-max) (field-end))))

; Wrappers for common move actions

(defun bind_non_shift (key fn)
	(define-key global-map key
		`(lambda () (interactive)
			(deactivate-mark)
			(call-interactively #',fn)
			(funcall after_move_hook_fn))))

(defun bind_shift (key fn)
	(define-key global-map key
		`(lambda () (interactive)
			(let ((caret (point)))
				(call-interactively #',fn)
				(cond
					(mark-active (when (= (point) (mark)) (deactivate-mark)))
					((/= (point) caret) (set-mark caret)))
				(funcall after_move_hook_fn)))))

(bind_shift [S-left] #'key::left)
(bind_shift [C-S-left] #'key::left_control)
(bind_non_shift [A-left] #'key::left_alt)
(bind_shift [A-S-left] #'key::left_alt)
(bind_non_shift [A-C-left] #'key::left_alt_control)
(bind_shift [A-C-S-left] #'key::left_alt_control)

(bind_shift [S-right] #'key::right)
(bind_shift [C-S-right] #'key::right_control)
(bind_non_shift [A-right] #'key::right_alt)
(bind_shift [A-S-right] #'key::right_alt)
(bind_non_shift [A-C-right] #'key::right_alt_control)
(bind_shift [A-C-S-right] #'key::right_alt_control)

(bind_non_shift [home] #'key::home)
(bind_shift [S-home] #'key::home)
(bind_non_shift [C-home] #'key::home_control)
(bind_shift [C-S-home] #'key::home_control)

(bind_non_shift [C-end] #'key::end_control)
(bind_shift [C-S-end] #'key::end_control)

(unintern 'bind_non_shift nil)
(unintern 'bind_shift nil)

;(defun is_inside_regex (regex &optional distance)
;"Return t if caret is inside, before or after match for regex.
;Set the match data from the earliest such match ending at or after caret.
;Optional argument distance limits search for regex forward and backward.
;So when caret is between two matches, left one is in match data.
;Original: thing-at-point-looking-at."
;	(let* ((caret_position (point))
;		   (match_start_position (when (looking-at regex) caret_position))
;		   forward_bound
;		   backward_bound)
;		(when distance
;			(setq forward_bound (+ caret_position distance)
;				  backward_bound (- caret_position distance)))
;		(save-excursion
;			; Search back repeatedly from end of next match.
;			; This may fail if next match ends before this match does.
;			(re-search-forward regex forward_bound 'limit)
;			(let ((right_match_end_or_limit_position (point)))
;				(when
;					(and
;						(re-search-backward regex backward_bound t) ; If this is nil, then we reached backward_bound.
;						; Avoid infinite loops with some regexes, such as "^" or "", which never move caret.
;						(< (point) right_match_end_or_limit_position)
;						(or
;							(> (point) caret_position) ; If caret_position < (match-beginning 0).
;							(progn ; Extend match-end past search start.
;								; This is obviously always true here.
;								; It's called to get accurate, properly maximally extended match for regex, because match-end
;								; provided by re-search-backward is always <= position from which re-search-backward was called.
;								(looking-at regex)
;								; When (match-beginning 0) <= caret_position <= (match-end 0).
;								(when (>= (match-end 0) caret_position)
;									(setq match_start_position (point))))))
;					(while
;						(and
;							(re-search-backward regex backward_bound t) ; If this is nil, then we reached backward_bound.
;							(or
;								(> (point) caret_position) ; If caret_position < (match-beginning 0).
;								(progn ; Extend match-end past search start.
;									; This is obviously always true here.
;									; It's called to get accurate, properly maximally extended match for regex, because match-end
;									; provided by re-search-backward is always <= position from which re-search-backward was called.
;									(looking-at regex)
;									; When (match-beginning 0) <= caret_position <= (match-end 0).
;									(when (>= (match-end 0) caret_position)
;										(setq match_start_position (point)))))))))
;			(when match_start_position
;				(goto-char match_start_position)
;				; Back up a char at a time in case search skipped intermediate match straddling caret_position.
;				; This is only to set accurate match-beginning data, because the one that is now set is >= backward_bound.
;				; I guess, maybe not.
;				(while
;					(and
;						(not (bobp))
;						(progn (forward-char -1) (looking-at regex))
;						(>= (match-end 0) caret_position)
;						(setq match_start_position (point))))
;				(goto-char match_start_position)
;				(looking-at regex))))) ; This is always true here, it's called only to restore match data.


; Pozostałości ze skakania po regionach, kiepskie bo nie kierunkowe, więc nieprzewidywalne.
;(defun jump_to_further_region_end (get_region_function)
;	(when-let ((region (funcall get_region_function)))
;		(goto-char ; If closer to region start, then go to region end.
;			(if (< (abs (- (point) (car region))) (abs (- (point) (cdr region))))
;				(cdr region)
;				(car region)))))
;
;(defun jumpToCloserRegionEnd (getRegionFunction) "Jump to closer region end or if at region end, then jump to region start."
;	(when-let ((region (funcall getRegionFunction)))
;		(goto-char
;			(cond
;				((= (point) (car region)) (cdr region))
;				((= (point) (cdr region)) (car region))
;				((< (abs (- (point) (car region))) (abs (- (point) (cdr region)))) (car region))
;				(t (cdr region))))))

(provide 'myMovement)
