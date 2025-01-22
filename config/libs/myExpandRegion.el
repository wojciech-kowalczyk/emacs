; -*- lexical-binding:nil -*-

(defvar-local expandRegion::history_list nil
"Holds temporary history of consecutive steps of expanding region in form
(caret_position . mark_position) to use in contracting region.")

(defun expandRegion::escape ()
"To be used in `escape-key-hook' to go back to starting position
instead of leaving caret in place like usually."
	(when
		(and
			(cdr expandRegion::history_list)
			; If caret and mark equal last saved region state.
			(equal
				(car expandRegion::history_list)
				(region::marked_region)))
		; Move caret to starting position.
		(goto-char (car (car (last expandRegion::history_list))))
		(funcall after_move_hook_fn))
	(deactivate-mark)
	t)

(defun expandRegion::deactivate_mark ()
	(kill-local-variable 'expandRegion::history_list)
	(remove_hook 'escape-key-hook #'expandRegion::escape t)
	(remove-hook 'deactivate-mark-hook #'expandRegion::deactivate_mark t)
	nil)

(defun key::w_control (n) "Expand or contract region." (interactive "p")
	(cond
		((< n 0)
			(if (and mark-active (cdr expandRegion::history_list))
				(let (
					(caret (point))
					(marked_region (region::marked_region))
					(last_expand_region (car expandRegion::history_list))
				)
					; If caret and mark equal last saved region state.
					(when (equal marked_region last_expand_region)
						(while (and (< n 0) (cdr expandRegion::history_list))
							(setq expandRegion::history_list (cdr expandRegion::history_list))
							(setq last_expand_region (car expandRegion::history_list))
							(goto-char (car last_expand_region))
							(if (eq (cdr last_expand_region) 'firstEntry)
								(deactivate-mark)
								(set-marker (mark-marker) (cdr last_expand_region))
								; Keep original order of caret and mark.
								(when (= caret (cdr marked_region))
									(swap_caret_and_mark)))
							(++ n))))
				(kill-local-variable 'expandRegion::history_list)))
		((and
				(> n 0)
				(not
					; If marked region covers entire buffer or buffer is empty.
					(if mark-active
						(or
							(and (= (point-min) (mark)) (= (point-max) (point)))
							(and (= (point-max) (mark)) (= (point-min) (point))))
						(is_buffer_empty))))
			(let ((region t))
				(while (and (> n 0) region)
					(let (
						(caret (point)) ; Const.
						(simple_region_min_to_max
							(lambda ()
								(setcar region (1- (car region)))
								(setcdr region (1+ (cdr region)))))
						(comment_region_min_to_max
							(lambda ()
								(if (eq comment_type 'lineComment)
									(progn
										(setcar region (- (car region) (length comment::line_string)))
										(when (/= (cdr region) (point-max))
											(setcdr region (1+ (cdr region)))))
									(setcar region (- (car region) (length comment::region_start_string)))
									(setcdr region (+ (cdr region) (length comment::region_end_string))))))
						(maybe_contract_paragraph_region
							; Modify cell. Move caret. Only for non-line-comment regions.
							(lambda (paragraph_region min_position max_position)
								(goto-char (car paragraph_region))
								(skip-chars-forward "\s\t")
								(let ((start (point)))
									(when (>= min_position start)
										(goto-char (cdr paragraph_region))
										(skip-chars-backward "\s\t")
										(when (<= max_position (point))
											(setcar paragraph_region start)
											(setcdr paragraph_region (point))
											t)))))
						(call_inside_string_or_comment_in_string
							(lambda ()
								; Just a sanity check, because I guess it's possible that some
								; old syntax might mess something up.
								; Or maybe string is opened, but there is end of buffer before
								; string terminator, or something like this.
								; Very rare, so just exit here and do nothing.
								(when (setq region (region::string_region_min))
									(funcall inside_string_or_comment
										simple_region_min_to_max
										(lambda () (region::sentence_region region))
										(lambda () (region::paragraph_region region))))))
						(call_inside_string_or_comment_in_comment
							(lambda ()
								(funcall inside_string_or_comment
									comment_region_min_to_max
									(lambda ()
										(region::sentence_region_in_comment region comment_type))
									(lambda ()
										(region::paragraph_region_in_comment region comment_type)))))
						(get_intermediate_unit
							(lambda (left right) ; 'region' should be a limiting region, probably paragraph.
								(goto-char right)
								(when-let* (
									region::get_intermediate_unit_start_fn
									(intermediate_unit_start
										(funcall region::get_intermediate_unit_start_fn
											(car region) (cdr region)))
									((>= left intermediate_unit_start))
									(intermediate_unit_end
										(progn
											(goto-char intermediate_unit_start)
											(funcall region::get_intermediate_unit_end_fn
												(car region) (cdr region))))
									((<= right intermediate_unit_end))
									((not
										(and
											(= intermediate_unit_start left)
											(= intermediate_unit_end right))))
								)
									(setcar region intermediate_unit_start)
									(setcdr region intermediate_unit_end))
								nil))
					)
						(if mark-active
							(let (
								(marked_region (region::marked_region)) ; Const.
								(inside_string_or_comment
									(lambda (
										region_min_to_max_fn get_sentence_region_fn get_paragraph_region_fn
									)
										(let (sentence_region paragraph_region)
											(cond
												((or
														; If marked_region already covers entire string
														; or comment.
														(equal region marked_region)
														; If left marked_region end is outside of string
														; or comment.
														(not
															(in_range_inclusive
																(car marked_region) (car region) (cdr region))))
													(funcall region_min_to_max_fn))
												; Mark the entire symbol, if we are in one.
												((let (
													(is_right_in_symbol
														(find_in_vector_=
															"w_"
															(syntax::raw_to_class
																(aref syntax::text_syntax_table
																	(char-before (cdr marked_region))))))
													(is_left_in_symbol
														(find_in_vector_=
															"w_"
															(syntax::raw_to_class
																(aref syntax::text_syntax_table
																	(char-after (car marked_region))))))
												)
														(when (or is_right_in_symbol is_left_in_symbol)
															(let (actually_moved)
																(goto-char (cdr marked_region))
																(when is_right_in_symbol
																	(setq actually_moved
																		(/=
																			(syntax::withTextSyntax
																				; Limit search so we don't
																				; accidentally move out of
																				; comment, if comment ender has
																				; symbol syntax in text.
																				(skip-syntax-forward
																					"w_" (cdr region)))
																			0)))
																(let ((right (point)))
																	(goto-char (car marked_region))
																	(when is_left_in_symbol
																		(if actually_moved
																			(syntax::withTextSyntax ; Same here.
																				(skip-syntax-backward
																					"w_" (car region)))
																			(setq actually_moved
																				(/=
																					; Same here.
																					(syntax::withTextSyntax
																						(skip-syntax-backward
																							"w_" (car region)))
																					0))))
																	(if actually_moved
																		(setq region (cons (point) right))
																		(goto-char (cdr marked_region))
																		nil))))))
												((when-let (
													(closest_opening_paren
														(region::find_closest_opening_paren (car region)))
													(closest_closing_paren
														(region::find_closest_closing_paren (cdr region)))
													; If parens match.
													((= (cdr closest_opening_paren) (cdr closest_closing_paren)))
													; If closest paren region contains marked region.
													; (> because it's max region)
													((> (car marked_region) (car closest_opening_paren)))
												)
														(setcar region (1+ (car closest_opening_paren)))
														(setcdr region (1- (car closest_closing_paren)))
														; If closest paren region is equal to marked region,
														; expand it to contain delimiters and return.
														(when (equal region marked_region)
															(funcall simple_region_min_to_max)
															t)))
												((and
														(setq sentence_region (funcall get_sentence_region_fn))
														(>= (car marked_region) (car sentence_region))
														(not (equal sentence_region marked_region)))
													(setq region sentence_region))
												((and
														(setq paragraph_region (funcall get_paragraph_region_fn))
														(>= (car marked_region) (car paragraph_region))
														(not (equal paragraph_region marked_region)))
													(setcar region (car paragraph_region))
													(setcdr region (cdr paragraph_region))
													; If contracted region is exactly the same as current
													; marked region, revert to expanded paragraph region.
													(and
														(funcall maybe_contract_paragraph_region
															region (car marked_region) (cdr marked_region))
														(equal region marked_region)
														(setq region paragraph_region)))))
										nil))
							)
								(goto-char (cdr marked_region))
								(cond
									((inside_string) (funcall call_inside_string_or_comment_in_string))

									((inside_comment)
										; Sanity check.
										(when (setq region (region::comment_region_min_and_type))
											(let ((comment_type (cdr region)))
												(setq region (car region))
												(goto-char (car marked_region))
												; Check later that checks if left marked_region end is in
												; comment region can succeed, while left end is actually outside
												; of comment. This can happen after stepping into comment
												; crossing line comment starter, like this:
												; // nice
												; <//> comment
												; So to prevent this, I added this check.
												(if (not (inside_comment))
													(funcall comment_region_min_to_max)
													(goto-char (cdr marked_region))
													(funcall call_inside_string_or_comment_in_comment)))))

									; These two branches below are here, because by marking some syntactic unit
									; from syntax::unit_string_vector, we may have stepped into string or
									; comment, so we now want to mark it in its entirety.
									; Obviously caret or mark may be manually moved in string or comment, but
									; that's just completely wrong, so don't do any more checks for that.

									; If only left marked_region end is in string.
									((progn (goto-char (car marked_region)) (inside_string))
										(setq region (region::string_region_max)))
									; If only left marked_region end is in comment.
									((inside_comment) (setq region (region::comment_region_max)))

									; Mark the entire symbol, if we are in one.
									((let (
										(is_right_in_symbol
											(find_in_vector_=
												"w_"
												(syntax::raw_to_class (syntax-after (1- (cdr marked_region))))))
										(is_left_in_symbol
											(find_in_vector_=
												"w_"
												(syntax::raw_to_class (syntax-after (car marked_region)))))
									)
											(when (or is_right_in_symbol is_left_in_symbol)
												(let (actually_moved)
													(goto-char (cdr marked_region))
													(when is_right_in_symbol
														(setq actually_moved (/= (skip-syntax-forward "w_") 0)))
													(let ((right (point)))
														(goto-char (car marked_region))
														(when is_left_in_symbol
															(if actually_moved
																(skip-syntax-backward "w_")
																(setq actually_moved
																	(/= (skip-syntax-backward "w_") 0))))
														(when actually_moved
															(setq region (cons (point) right))
															t))))))

									; Extend backwards on prefixes.
									((progn
											(goto-char (car marked_region))
											(/= (skip-syntax-backward "'\\/") 0))
										(setq region (cons (point) (cdr marked_region))))

									; Set region to innermost paren region if we are in one,
									; otherwise to entire buffer region.
									((if-let (
										(opening_paren (nth 1 (syntax-ppss (cdr marked_region))))
										(closing_paren (ignore-errors (scan-lists (cdr marked_region) 1 1)))
										((> (car marked_region) opening_paren))
									)
											(progn
												(setq region (cons (1+ opening_paren) (1- closing_paren)))
												; If we cover this paren region, extend it to max paren region
												; and return it.
												(when (equal region marked_region)
													(funcall simple_region_min_to_max)
													t))
											; This will never be empty, because there is a check
											; for empty buffer earlier.
											(setq region (cons (point-min) (point-max)))
											nil))

									; Get paragraph region.
									; If it doesn't exist, or is equal to currently marked region,
									; exit (so mark paren region or entire buffer).
									; Try to contract it.
									; If it contracted is equal to currently marked region,
									; return expanded paragraph region.
									; Otherwise save (maybe contracted) paragraph region in 'region'
									; and proceed.
									((if-let (
										(paragraph_region (region::paragraph_region region))
										((>= (car marked_region) (car paragraph_region)))
										((not (equal paragraph_region marked_region)))
									)
											(progn
												(setcar region (car paragraph_region))
												(setcdr region (cdr paragraph_region))
												; If contracted region is exactly the same as current
												; marked region, return expanded paragraph region.
												(and
													(funcall maybe_contract_paragraph_region
														region (car marked_region) (cdr marked_region))
													(equal region marked_region)
													(setq region paragraph_region)))
											t))

									; Set 'region' to intermediate unit region limited by paragraph region,
									; if this unit exists and is larger that currently marked region.
									(t (funcall get_intermediate_unit (car marked_region) (cdr marked_region))))

								(if (not region)
									(goto-char caret)
									(push region expandRegion::history_list)
									(set-marker (mark-marker) (car region))
									(goto-char (cdr region))
									; If caret was originally on the left, keep it there.
									(when (= caret (car marked_region))
										(swap_caret_and_mark))))

							(setq region
								(let* (
									(char_after (char-after))
									(char_before (char-before))
									(syntax_class_after
										(when char_after
											(syntax::raw_to_class
												(if (syntax::should_use_text_syntax caret)
													(aref syntax::text_syntax_table char_after)
													(syntax-after caret)))))
									(syntax_class_before
										(when char_before
											(syntax::raw_to_class
												(if (syntax::should_use_text_syntax (1- caret))
													(aref syntax::text_syntax_table char_before)
													(syntax-after (1- caret))))))
									(inside_string_or_comment
										(lambda (
											region_min_to_max_fn get_sentence_region_fn get_paragraph_region_fn
										)
											(or
												; When string/comment is empty, e.g. "", /**/ or //\n.
												(when (= (car region) (cdr region))
													(funcall region_min_to_max_fn)
													region)
												; Highlighted parens.
												(let (paren_index paren_position)
													(cond
														((and
																(setq paren_index
																	(index_in_vector_=
																		syntax::opening_paren_string char_after))
																(not (is_backslashed caret))
																(setq paren_position
																	(save-excursion
																		(forward-char 1)
																		(region::find_closing_paren
																			paren_index (cdr region)))))
															(setq region (cons caret paren_position))
															(when (= (car region) (cdr region))
																(funcall simple_region_min_to_max))
															region)
														((and
																(setq paren_index
																	(index_in_vector_=
																		syntax::closing_paren_string
																		char_before))
																(not (is_backslashed (1- caret)))
																(setq paren_position
																	(save-excursion
																		(forward-char -1)
																		(region::find_opening_paren
																			paren_index (car region)))))
															(setq region (cons paren_position caret))
															(when (= (car region) (cdr region))
																(funcall simple_region_min_to_max))
															region)))
												; Sentence/paragraph/closest_paren region.
												(when-let (
													(closest_opening_paren
														(region::find_closest_opening_paren (car region)))
													(closest_closing_paren
														(region::find_closest_closing_paren (cdr region)))
													; If parens match.
													((= (cdr closest_opening_paren) (cdr closest_closing_paren)))
												)
													(setcar region (1+ (car closest_opening_paren)))
													(setcdr region (1- (car closest_closing_paren)))
													; If this is an empty region, e.g. (|).
													(when (= (car region) (cdr region))
														(funcall simple_region_min_to_max)
														region))
												; Even if closest_paren_region_min exists, there is a possibility
												; that sentence_region will be nil while paragraph_region won't,
												; like here: Sentence 1. | Sentence 2.
												(funcall get_sentence_region_fn)
												(when-let ((paragraph_region (funcall get_paragraph_region_fn)))
													(funcall maybe_contract_paragraph_region
														paragraph_region caret caret)
													paragraph_region)
												region)))
								)
									(cond
										; Symbol.
										((memq syntax_class_after '(?w ?_))
											(forward-char 1)
											; Control move doesn't use syntax.
											(control_move_left)
											(let ((mark (point)))
												(goto-char caret)
												(control_move_right)
												(cons mark (point))))
										((memq syntax_class_before '(?w ?_))
											(control_move_left)
											(cons (point) caret))

										((inside_string) (funcall call_inside_string_or_comment_in_string))

										((inside_comment)
											; Sanity check.
											(when (setq region (region::comment_region_min_and_type))
												(let ((comment_type (cdr region)))
													(setq region (car region))
													(funcall call_inside_string_or_comment_in_comment))))

										; Prefixes (they don't exist in text, only in code).
										((/= (skip-syntax-backward "\\/'") 0) (cons (point) caret))

										; After closing parens (this first because it has higher highlight
										; precedence).
										((memq syntax_class_before '(?\" ?|))
											(forward-char -1)
											(cons (region::string_region_max_start) caret))
										((and
												(or
													(eq syntax_class_before ?\))
													(and
														(eq syntax_class_before ?$)
														(not (eq (nth 1 (syntax-ppss)) (1- caret)))))
												; This isn't region, it's opening paren position.
												(setq region (ignore-errors (scan-lists caret -1 0))))
											(cons region caret))

										; Before opening parens.
										((memq syntax_class_after '(?\" ?|))
											(forward-char 1)
											(cons caret (region::string_region_max_end)))
										((and
												(or
													(eq syntax_class_after ?\()
													(and
														(eq syntax_class_after ?$)
														(eq
															(nth 1 (save-excursion (syntax-ppss (1+ caret))))
															caret)))
												; This isn't region, it's closing paren position.
												(setq region (ignore-errors (scan-lists caret 1 0))))
											(cons caret region))

										; Check if we are in some syntactic unit, like those in c++:
										;   ->, ::, +=, ...
										; But also comment starter/enders; they will require special handling
										; in following expand_region calls, because this is the only way of
										; moving caret inside/outside comment while mark is outside/inside
										; comment.
										; For example (| - caret, <> - region bounds):
										; |/* -> </*>, *|/ -> <*/>, etc.
										; This is not ideal, as for example dot operator in C will be impossible
										; to mark with this function, because symbol around dot will always
										; be preferred, e.g. symbol1.symbol2.
										; Also when they are two units back to back, then this with higher
										; priority will be chosen, even if it is before caret, which again
										; isn't ideal.
										((cl-loop
												with position = nil
												for unit_string across syntax::unit_string_vector
												thereis
													(cond
														((setq position (is_before_string unit_string))
															(cons caret position))
														((setq position (is_after_string unit_string))
															(cons position caret))
														((and
																(> (length unit_string) 1)
																(setq position (is_inside_string unit_string)))
															(cons (- position (length unit_string)) position)))))

										; There could be expanding on blanks around, but that's mostly useless
										; and there would need to be protection from stepping into line comments
										; over line breaks.

										; TODO Przypomnienie o todosie w myRegion opisującym jak to jest błędne
										; (tylko w lispie pareny są tak ważne, a tu powinien być syntactic scope).
										; Get innermost paren region or buffer region if we are not in a paren
										; region.
										((if-let (
											(opening_paren (nth 1 (syntax-ppss)))
											(closing_paren (ignore-errors (scan-lists caret 1 1)))
										)
												; If paren region is empty, return it expanded.
												(if (= opening_paren (- closing_paren 2))
													(cons opening_paren closing_paren)
													(setq region (cons (1+ opening_paren) (1- closing_paren)))
													nil)
												; This will never be empty, because there is a check for empty
												; buffer earlier.
												(setq region (cons (point-min) (point-max)))
												nil))

										((if-let ((paragraph_region (region::paragraph_region region)))
												(progn
													(setq region paragraph_region)
													(funcall maybe_contract_paragraph_region region caret caret)
													nil)
												region))

										(t (funcall get_intermediate_unit caret caret) region))))

							(goto-char caret)
							(when region
								; On "manual" mark deactivation, go to starting position -
								; car of first entry in expandRegion::history_list.
								(add_hook 'escape-key-hook #'expandRegion::escape 15 t)
								(add-hook 'deactivate-mark-hook #'expandRegion::deactivate_mark nil t)
								; Save caret original position as first entry in expandRegion::history_list.
								(setq expandRegion::history_list (list (cons caret 'firstEntry)))
								; After all, also save caret in jumpHistory, because sometimes I accidentally
								; click something else than escape, and this function can leave caret basically
								; anywhere (from point-min to point-max in giant buffers), so later it's hard
								; to go back, so save it to be safe.
								(jumpHistory::add)
								(push region expandRegion::history_list)
								(goto-char (cdr region))
								(set-mark (car region)))))
					(-- n)))))
	nil)

(fset 'key::w_control_shift (get_reverse_command #'key::w_control))

(define-key global-map [?\C-w]
	(lambda () (interactive)
		(call-interactively #'key::w_control)
		(funcall after_move_hook_fn)))
(define-key global-map [?\C-\S-w]
	(lambda () (interactive)
		(call-interactively #'key::w_control_shift)
		(funcall after_move_hook_fn)))

; Mark field, or with prefix arg, accessible portion of buffer.
; Press escape right away to return to previous position (mainly for missclicks).

(defvar key::a::data nil "(original_position . marked_region).")

(defun key::a::escape ()
"To be used in `escape-key-hook' to go back to starting position
instead of leaving caret in place like usually."
	(when (equal (cdr key::a::data) (region::marked_region))
		(goto-char (car key::a::data))
		(funcall after_move_hook_fn))
	(deactivate-mark)
	t)

(defun key::a::deactivate_mark ()
	(kill-local-variable 'key::a::data)
	(remove_hook 'escape-key-hook #'key::a::escape t)
	(remove-hook 'deactivate-mark-hook #'key::a::deactivate_mark t)
	nil)

(defun key::a (arg) (interactive "P")
	(let (start end)
		(if arg
			(setq start (point-min) end (point-max))
			(setq start (field-beginning) end (field-end)))
		(when (and (/= start end) (not (and (= (point) start) (eq (mark) end))))
			(setq-local key::a::data (cons (point) (cons start end)))
			(add_hook 'escape-key-hook #'key::a::escape 15 t)
			(add-hook 'deactivate-mark-hook #'key::a::deactivate_mark nil t)
			(jumpHistory::add)
			(goto-char start)
			(set-mark end)
			(funcall after_move_hook_fn))))

(define-key global-map [?\C-a] #'key::a)

(provide 'myExpandRegion)
