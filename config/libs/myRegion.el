; -*- lexical-binding:nil -*-

; Definitions:
;	region -
;		cons cell (start_position . end_position),
;		where start_position <= end_position.
;	region min - region not containing delimiters; e.g. "<string>".
;	region max - region containing delimiters; e.g. <"string">.
;
; In some functions returning different versions of regions,
; just "region" depends on caret position - if caret is inside region min,
; then region min is returned, else region max, e.g.
; "st|ring" -> "<string>"; "string|" -> "<string>"; |"string" -> <"string">.

; Search limit

(defun region::search_limit_start (&optional search_limit)
"SEARCH_LIMIT is nil meaning whole buffer or int meaning max number of chars to
search."
	(if (and search_limit (> (- (point) search_limit) (point-min)))
		(- (point) search_limit)
		(point-min)))
(defun region::search_limit_end (&optional search_limit)
"SEARCH_LIMIT is nil meaning whole buffer or int meaning max number of chars to
search."
	(if (and search_limit (< (+ (point) search_limit) (point-max)))
		(+ (point) search_limit)
		(point-max)))

; String region

(defun region::string_region_max_start ()
	(let ((syntax_ppss (nthcdr 3 (syntax-ppss))))
		(when (car syntax_ppss) (nth 5 syntax_ppss))))

(defun region::string_region_max_end (&optional search_limit)
	(when-let ((string_delimiter (nth 3 (syntax-ppss))))
		(save-excursion
			(if (eq t string_delimiter) ; Generic string.
				(let ((parse-sexp-lookup-properties t))
					(cl-loop
						do (skip-syntax-forward "^|")
						until (eobp)
						unless (is_backslashed (point)) return (1+ (point))
						do (forward-char 1)))
				(setq search_limit (region::search_limit_end search_limit))
				(let ((string_delimiter_string (char-to-string string_delimiter)))
					(cl-loop
						while (search-forward string_delimiter_string search_limit t)
						unless (is_backslashed (1- (point))) return (point)))))))

(defun region::string_region_max (&optional search_limit)
	(when-let (
		(string_region_max_start (region::string_region_max_start))
		(string_region_max_end (region::string_region_max_end search_limit))
	)
		(cons string_region_max_start string_region_max_end)))

(defun region::string_region_min (&optional search_limit)
	(when-let ((string_region_max (region::string_region_max search_limit)))
		(cons (1+ (car string_region_max)) (1- (cdr string_region_max)))))

(defun region::string_region_min_start ()
	(when-let ((string_region_max_start (region::string_region_max_start)))
		(1+ string_region_max_start)))

(defun region::string_region_min_end (&optional search_limit)
	(when-let ((string_region_max_end (region::string_region_max_end search_limit)))
		(1- string_region_max_end)))

(defun region::string_region_min_to_max (region_min)
"If region_min is a string region, then extend it (by modifying region_min cell)
to contain it's delimiters and return it; else return nil.
region_min must really be region min for this function to return accurate data."
	(let (
		(region_max_start (1- (car region_min)))
		(region_min_end (cdr region_min))
	)
		(when
			(and
				(let (
					(start_syntax_class_char (car (syntax::get_syntax region_max_start)))
				)
					(cond
						((eq ?\" start_syntax_class_char)
							(and
								(eq
									(char-after region_max_start)
									(char-after region_min_end))
								(eq ?\" (car (syntax::get_syntax region_min_end)))))
						((eq ?| start_syntax_class_char)
							(eq ?| (car (syntax::get_syntax region_min_end))))))
				(not
					(or
						(is_backslashed region_max_start)
						(is_backslashed region_min_end))))
			(setcar region_min region_max_start)
			(setcdr region_min (1+ region_min_end))
			region_min)))

(defun region::string_region_min_start_to_max (region_min_start)
	(and
		(memq (car (syntax::get_syntax (1- region_min_start))) '(?\" ?|))
		(not (is_backslashed (1- region_min_start)))
		(1- region_min_start)))

(defun region::string_region_min_end_to_max (region_min_end)
	(and
		(memq (car (syntax::get_syntax region_min_end)) '(?\" ?|))
		(not (is_backslashed region_min_end))
		(1+ region_min_end)))

; Finding specified parens

(defun region::find_opening_paren (paren_index limit)
"Returns position before opening paren or nil.
Closing paren should be after caret (or further to the right).
It's just simple search for opening and closing paren, checking if
they aren't backslashed (which is here to work more similar to uncommented
code parts), increasing or decreasing depth and failing after reaching limit.
Because of that, it will match things like ([)].
Don't use syntax tables and properties."
	(let* (
		(opening_paren (aref syntax::opening_paren_string paren_index))
		(closing_paren (aref syntax::closing_paren_string paren_index))
		; Not quite regex, but arg for skip-chars-backward.
		(opening_or_closing_paren_regex (string ?^ opening_paren closing_paren))
		(depth 0)
	)
		(save-excursion
			(catch 'position
				(while
					(progn
						(skip-chars-backward opening_or_closing_paren_regex limit)
						(/= (point) limit))
					(forward-char -1)
					(cond
						((is_backslashed (point)))
						((= (char-after) closing_paren) (++ depth))
						((= depth 0) (throw 'position (point)))
						(t (-- depth))))))))

(defun region::find_closing_paren (paren_index limit)
"Returns position after closing paren or nil.
Opening paren should be before caret (or further to the left).
It's just simple search for opening and closing paren, checking if
they aren't backslashed (which is here to work more similar to uncommented
code parts), increasing or decreasing depth and failing on reaching limit.
Don't use syntax tables and properties."
	(let* (
		(opening_paren (aref syntax::opening_paren_string paren_index))
		(closing_paren (aref syntax::closing_paren_string paren_index))
		; Not quite regex, but arg for skip-chars-forward.
		(opening_or_closing_paren_regex (string ?^ opening_paren closing_paren))
		(depth 0)
	)
		(save-excursion
			(catch 'position
				(while
					(progn
						(skip-chars-forward opening_or_closing_paren_regex limit)
						(/= (point) limit))
					(forward-char 1)
					(cond
						((is_backslashed (1- (point))))
						((= (char-before) opening_paren) (++ depth))
						((= depth 0) (throw 'position (point)))
						(t (-- depth))))))))

; Finding unspecified "closest" parens

(defun region::find_closest_opening_paren (limit)
"Returns (position_before_paren . paren_index) or nil.
Works for things like ([)].
Don't use syntax tables and properties."
	(let* (
		; Not quite regex, but arg for skip-chars-backward.
		(every_opening_or_closing_paren_regex
			(concat "^" syntax::opening_paren_string syntax::closing_paren_string))
		(depth_vector (make-vector (length syntax::opening_paren_string) 0))
	)
		(save-excursion
			(catch 'position_and_paren_index
				(while
					(progn
						(skip-chars-backward every_opening_or_closing_paren_regex limit)
						(/= (point) limit))
					(forward-char -1)
					(unless (is_backslashed (point))
						(let ((char (following-char)) paren_index)
							(cond
								((setq paren_index
										(index_in_vector_=
											syntax::opening_paren_string char))
									(when (= 0 (aref depth_vector paren_index))
										(throw 'position_and_paren_index
											(cons (point) paren_index)))
									(aset depth_vector paren_index
										(1+ (aref depth_vector paren_index))))
								((setq paren_index
										(index_in_vector_=
											syntax::closing_paren_string char))
									(aset depth_vector paren_index
										(1- (aref depth_vector paren_index))))))))))))

(defun region::find_closest_closing_paren (limit)
"Returns (position_after_paren . paren_index) or nil.
Works for things like ([)].
Don't use syntax tables and properties."
	(let* (
		; Not quite regex, but arg for skip-chars-forward.
		(every_opening_or_closing_paren_regex
			(concat "^" syntax::opening_paren_string syntax::closing_paren_string))
		(depth_vector (make-vector (length syntax::opening_paren_string) 0))
	)
		(save-excursion
			(catch 'position_and_paren_index
				(while
					(progn
						(skip-chars-forward every_opening_or_closing_paren_regex limit)
						(/= (point) limit))
					(forward-char 1)
					(unless (is_backslashed (1- (point)))
						(let ((char (char-before)) paren_index)
							(if
								(setq paren_index
									(index_in_vector_= syntax::closing_paren_string char))
								(if (= 0 (aref depth_vector paren_index))
									(throw 'position_and_paren_index
										(cons (point) paren_index))
									(aset depth_vector paren_index
										(1- (aref depth_vector paren_index))))
								(setq paren_index
									(index_in_vector_= syntax::opening_paren_string char))
								(aset depth_vector paren_index
									(1+ (aref depth_vector paren_index)))))))))))

; Paren at caret and it's match.

; Some rules:
; - everything is checked for being backslashed,
; - in comments and strings only chars from syntax::opening_paren_string
; are allowed to be opening parens.
; - syntax::get_syntax is used for getting syntax, so syntax::text_syntax_table
; must be in sync with syntax::opening_paren_string.
(defun region::paren_at_caret_region_info ()
"Return (region_max . region) or nil if there is no paren before or after point
or no matching paren was found.
This includes quotes."
	; before_... describe syntax_class_before, so for example before_closing_paren
	; doesn't mean that caret is before closing paren, but that char before
	; is a closing paren.
	(let (
		(before_closing_paren ; Final.
			(lambda ()
				(when
					(setq position
						(save-excursion
							(forward-char -1)
							(if
								(setq position
									(or
										(region::string_region_min_start)
										(region::comment_region_min_start)))
								(region::find_opening_paren
									(index_in_vector_=
										syntax::closing_paren_string char_before)
									position)
								(nth 1 (syntax-ppss)))))
					(cons (cons position caret) (cons position caret)))))
		(before_opening_paren ; Final.
			(lambda ()
				(when
					(setq position
						(if
							(setq position
								(or
									(region::string_region_min_end)
									(region::comment_region_min_end)))
							(region::find_closing_paren
								(index_in_vector_=
									syntax::opening_paren_string char_before)
								position)
							(ignore-errors (scan-lists (1- caret) 1 0))))
					(cons (cons (1- caret) position) (cons caret (1- position))))))
		; Not final - used to check if we are after closing quote.
		(before_closing_quote
			(lambda ()
				(when
					(setq position
						(save-excursion
							(forward-char -1)
							(region::string_region_max_start)))
					(cons (cons position caret) (cons position caret)))))
		(before_opening_quote ; Final.
			(lambda ()
				(when (setq position (region::string_region_max_end))
					(cons (cons (1- caret) position) (cons caret (1- position))))))
		(before_opening_same_paren ; Final.
			(lambda ()
				(when (setq position (ignore-errors (scan-lists (1- caret) 1 0)))
					(cons (cons (1- caret) position) (cons caret (1- position))))))
		(before_closing_same_paren ; Final.
			(lambda ()
				(when (setq position (save-excursion (nth 1 (syntax-ppss (1- caret)))))
					(cons (cons position caret) (cons position caret)))))
		(after_opening_paren ; Final.
			(lambda ()
				(when
					(setq position
						(save-excursion
							(forward-char 1)
							(if
								(setq position
									(or
										(region::string_region_min_end)
										(region::comment_region_min_end)))
								(region::find_closing_paren
									(index_in_vector_=
										syntax::opening_paren_string char_after)
									position)
								(ignore-errors (scan-lists caret 1 0)))))
					(cons (cons caret position) (cons caret position)))))
		(after_quote ; Final.
			(lambda ()
				(cond
					; If we are just before closing quote.
					((setq position (region::string_region_max_start))
						(cons (cons position (1+ caret)) (cons (1+ position) caret)))
					; If we are just before opening quote.
					((setq position
							(save-excursion
								(forward-char 1)
								(region::string_region_max_end)))
						(cons (cons caret position) (cons caret position))))))
		(after_opening_same_paren ; Final.
			(lambda ()
				(when (setq position (ignore-errors (scan-lists caret 1 0)))
					(cons (cons caret position) (cons caret position)))))
		(is_after_opening_same_paren
			; If innermost enclosing list is started by char after,
			; meaning that char after is an opening paren.
			(lambda () (eq (nth 1 (save-excursion (syntax-ppss (1+ caret)))) caret)))
		(is_before_opening_same_paren
			; If innermost enclosing list is started by char before,
			; meaning that char before is an opening paren.
			(lambda () (eq (nth 1 (syntax-ppss)) (1- caret))))
		(caret (point))
		(char_after (char-after))
		(char_before (char-before))
		position
		syntax_class_after
		syntax_class_before
	)
		(when (and char_after (not (is_backslashed caret)))
			(setq syntax_class_after
				(syntax::raw_to_class
					(if (syntax::should_use_text_syntax caret)
						(aref syntax::text_syntax_table char_after)
						(syntax-after caret))))
			(unless (memq syntax_class_after '(?\( ?\) ?\" ?$ ?|))
				(setq syntax_class_after nil)))
		(when (and char_before (not (is_backslashed (1- caret))))
			(setq syntax_class_before
				(syntax::raw_to_class
					(if (syntax::should_use_text_syntax (1- caret))
						(aref syntax::text_syntax_table char_before)
						(syntax-after (1- caret)))))
			(unless (memq syntax_class_before '(?\( ?\) ?\" ?$ ?|))
				(setq syntax_class_before nil)))
		(cond
			((and syntax_class_after syntax_class_before)
				(cond
					((memq syntax_class_before '(?\" ?|))
						(cond
							; If we are just after closing quote.
							((funcall before_closing_quote))
							((and ; Opening paren is the only meaningful one here.
									(= syntax_class_after ?\()
									(setq position (region::string_region_min_end))
									(setq position
										(save-excursion
											(forward-char 1)
											(region::find_closing_paren
												(index_in_vector_=
													syntax::opening_paren_string
													char_after)
												position))))
								(cons (cons caret position) (cons caret position)))
							; If we are just after opening quote
							; (we definitely should be, but maybe there will be
							; end of buffer or something, so check anyway).
							(t (funcall before_opening_quote))))
					((= syntax_class_before ?\)) (funcall before_closing_paren))
					((and ; This must be outside of comment or string.
							(= syntax_class_before ?$)
							(not (funcall is_before_opening_same_paren)))
						(funcall before_closing_same_paren))
					; Now we now that char before is an opening paren
					; (either ?$ and outside code, or ?\().
					((memq syntax_class_after '(?\" ?|)) (funcall after_quote))
					((= syntax_class_after ?\() (funcall after_opening_paren))
					((and ; This must be outside of comment or string.
							(= syntax_class_after ?$)
							(funcall is_after_opening_same_paren))
						(funcall after_opening_same_paren))
					; If char before is an opening paren.
					((= syntax_class_before ?$) (funcall before_opening_same_paren))
					; (= syntax_class_before ?\()
					(t (funcall before_opening_paren))))
			(syntax_class_before
				(cond
					((memq syntax_class_before '(?\" ?|))
						(or
							; If we are just after closing quote.
							(funcall before_closing_quote)
							; If we are just after opening quote.
							(funcall before_opening_quote)))
					((= syntax_class_before ?\)) (funcall before_closing_paren))
					((= syntax_class_before ?\() (funcall before_opening_paren))
					; (= syntax_class_before ?$)
					((funcall is_before_opening_same_paren)
						(funcall before_opening_same_paren))
					; Char before is a closing paren.
					(t (funcall before_closing_same_paren))))
			(syntax_class_after
				(cond
					((memq syntax_class_after '(?\" ?|)) (funcall after_quote))
					((= syntax_class_after ?\))
						(when
							(setq position
								(if
									(setq position
										(or
											(region::string_region_min_start)
											(region::comment_region_min_start)))
									(region::find_opening_paren
										(index_in_vector_=
											syntax::closing_paren_string char_after)
										position)
									(nth 1 (syntax-ppss))))
							(cons (cons position (1+ caret)) (cons (1+ position) caret))))
					((= syntax_class_after ?\() (funcall after_opening_paren))
					; (= syntax_class_after ?$)
					((funcall is_after_opening_same_paren)
						(funcall after_opening_same_paren))
					; Char after is a closing paren.
					((setq position (nth 1 (syntax-ppss)))
						(cons (cons position (1+ caret)) (cons (1+ position) caret))))))))

(defsubst region::paren_at_caret_region () (cdr (region::paren_at_caret_region_info)))
(defsubst region::paren_at_caret_region_max () (car (region::paren_at_caret_region_info)))

(defun region::paren_at_caret_region_min ()
	(when-let ((paren_at_caret_region_max (region::paren_at_caret_region_max)))
		(setcar paren_at_caret_region_max (1+ (car paren_at_caret_region_max)))
		(setcdr paren_at_caret_region_max (1- (cdr paren_at_caret_region_max)))
		paren_at_caret_region_max))

; Closest paren region.

(defun region::closest_paren_region_max ()
	(let (opening_paren closing_paren)
		(if-let (
			(limit_region_min
				(or (region::string_region_min) (region::comment_region_min)))
		)
			(and
				(setq opening_paren
					(region::find_closest_opening_paren (car limit_region_min)))
				; Not sure what's faster here, closest from caret or specified
				; from opening_paren_position.
				(setq closing_paren
					(region::find_closest_closing_paren (cdr limit_region_min)))
				(= (cdr opening_paren) (cdr closing_paren))
				(cons (car opening_paren) (car closing_paren)))
			(and
				(setq opening_paren (nth 1 (syntax-ppss)))
				(setq closing_paren (ignore-errors (scan-lists (point) 1 1)))
				(cons opening_paren closing_paren)))))

(defun region::closest_paren_region_min ()
	(when-let ((closest_paren_region_max (region::closest_paren_region_max)))
		(setcar closest_paren_region_max (1+ (car closest_paren_region_max)))
		(setcdr closest_paren_region_max (1- (cdr closest_paren_region_max)))
		closest_paren_region_max))

; Closest region -
; 1) unspecified closest paren region, 2) string region, 3) comment region.

(defun region::closest_region_max (&optional search_limit include_newline_in_line_comment)
"Search for closest_paren_region, then string_region, then comment_region."
	(or
		(region::closest_paren_region_max)
		(region::string_region_max search_limit)
		(region::comment_region_max search_limit include_newline_in_line_comment)))

(defun region::closest_region_min (&optional search_limit)
"Search for closest_paren_region, then string_region, then comment_region."
	(or
		(region::closest_paren_region_min)
		(region::string_region_min search_limit)
		(region::comment_region_min search_limit)))

(defun region::closest_region_min_start ()
	(if-let (
		(limit (or (region::string_region_min_start) (region::comment_region_min_start)))
	)
		(if-let ((closest_opening_paren (region::find_closest_opening_paren limit)))
			(1+ (car closest_opening_paren))
			limit)
		(when-let ((closest_opening_paren (nth 1 (syntax-ppss))))
			(1+ closest_opening_paren))))

(defun region::closest_region_min_end ()
	(if-let (
		(limit (or (region::string_region_min_end) (region::comment_region_min_end)))
	)
		(if-let ((closest_closing_paren (region::find_closest_closing_paren limit)))
			(1- (car closest_closing_paren))
			limit)
		(when-let ((closest_closing_paren (ignore-errors (scan-lists (point) 1 1))))
			(1- closest_closing_paren))))


(defun region::closest_region_min_to_max
	(region_min &optional include_newline_in_line_comment)
"Extend region_min (by modifying region_min cell) to contain it's delimiters and
return it.
region_min must really be region min for this function to return accurate result."
	(or
		(region::string_region_min_to_max region_min)
		(region::comment_region_min_to_max region_min include_newline_in_line_comment)
		(progn
			(setcar region_min (1- (car region_min)))
			(setcdr region_min (1+ (cdr region_min)))
			region_min)))

(defun region::closest_region_min_start_to_max (region_min_start)
	(or
		(region::string_region_min_start_to_max region_min_start)
		(region::comment_region_min_start_to_max region_min_start)
		(1- region_min_start)))

(defun region::closest_region_min_end_to_max
	(region_min_end &optional include_newline_in_line_comment)
"Like region::closest_region_min_to_max."
	(or
		(region::string_region_min_end_to_max region_min_end)
		(region::comment_region_min_end_to_max
			region_min_end include_newline_in_line_comment)
		(1+ region_min_end)))

; Symbol region.

(defun region::symbol_region (use_code_syntax)
	(let (
		(symbol_region
			(let (
				(get_symbol_region
					(lambda ()
						(cons
							(save-excursion (skip-syntax-backward "w_") (point))
							(save-excursion (skip-syntax-forward "w_") (point)))))
			)
				(if use_code_syntax
					(funcall get_symbol_region)
					(syntax::withTextSyntax (funcall get_symbol_region)))))
	)
		(when (/= (car symbol_region) (cdr symbol_region)) symbol_region)))

; Sentence region

(defun region::sentence_start (limit &optional is_at_sentence_start_fn)
"Return beginning of sentence that caret is in, or beginning of previous one.

limit can be something like region::string_region_min_start.
It must be >= (point-min).

is_at_sentence_start_fn is called on every start of sexp, with no args.
It should return non-nil if we are at sentence start.
It defaults to matching either:
	at least one full blank line before sentence start,
	. or ? or ! followed by either:
		one space or newline followed by not lower letter,
		more than one space or newline, or at least one tab.

If already at beginning of a sentence, return caret position.
If not in a sentence, search for beginning of previous sentence,
if none found, return nil.
If scan-sexps crash on opening paren or something, treat this as reaching limit -
skip blanks and return beginning of sentence if caret was in one.
E.g. (Nice| sentence.) -> (|Nice sentence.) -> crash -> return (|Nice sentence.)

It used to use regex, but function (is_at_sentence_start_fn) is more flexible."
	(unless is_at_sentence_start_fn
		(setq is_at_sentence_start_fn
			(lambda ()
				(and
					(not (eobp))
					(or
						(save-excursion
							(skip-chars-backward " \t")
							(or
								(bobp)
								(and
									; If current line is blank to start.
									(= (preceding-char) ?\n)
									(progn
										(forward-char -1)
										(is_line_blank_to_start)))))
						(save-excursion
							(let ((position (point)))
								(and
									(< (skip-chars-backward "\s\t\n") 0)
									(find_in_vector_= ".?!" (preceding-char))
									(let ((char_after (following-char)))
										(cond
											((= char_after ?\t) t)
											((= char_after ?\s)
												(not
													(is_lower_letter
														; This char-after will never be nil
														; because of previous (not (eobp)) and
														; (< (skip-chars-backward "\s\t\n") 0).
														(char-after (1+ (point))))))
											; (= char_after ?\n)
											((not (is_lower_letter (char-after position)))
												t)))))))))))
	(let (
		(caret (point))
		(general_regex
			(concat "^\s\t\n" syntax::opening_paren_string syntax::closing_paren_string))
		(exit_if_reached_limit
			(lambda ()
				(when (<= (point) limit)
					(goto-char limit)
					(skip-chars-forward "\s\t\n")
					(throw 'result
						(when (<= (point) caret)
							(point))))))
	)
		(when (>= caret limit)
			; If we already are at sentence start.
			(if
				(and
					(not (eobp))
					(let ((char_after (following-char)))
						(and
							(not (find_in_vector_= "\s\t\n" char_after))
							(or
								(not
									(find_in_vector_=
										syntax::closing_paren_string char_after))
								(is_backslashed (point)))))
					(funcall is_at_sentence_start_fn))
				caret
				(save-excursion
					(skip-chars-backward "\s\t\n")
					(catch 'result
						(while t
							; Skip to start of previous sexp.
							(skip-chars-backward general_regex)
							(funcall exit_if_reached_limit)
							(let ((char_before (preceding-char)))
								(if
									(find_in_vector_=
										syntax::opening_paren_string char_before)
									(if (is_backslashed (1- (point)))
										(forward-char -1)
										; Always exit here.
										(setq limit (point))
										(funcall exit_if_reached_limit))
									(if-let (
										(paren_index
											(index_in_vector_=
												syntax::closing_paren_string char_before))
									)
										(progn
											(forward-char -1)
											(when-let (
												((not (is_backslashed (point))))
												(opening_paren_position
													(region::find_opening_paren
														paren_index limit))
											)
												(goto-char opening_paren_position)))
										; char_before = space, tab or newline.
										(if (funcall is_at_sentence_start_fn)
											(throw 'result (point))
											(skip-chars-backward "\s\t\n"))))))))))))

(defun region::sentence_end (limit &optional is_at_sentence_end_fn)
"Return end of sentence that caret is in, or end of next one.
This position is after terminators, like .?!.

limit can be something like region::string_region_min_end.
It must be <= (point-max).

is_at_sentence_end_fn is called on every end of sexp, with no args.
It should return non-nil if we are at sentence end.
It defaults to matching something like this:
either:
	at least one full blank line after sentence end,
	. or ? or ! followed by either:
		one space or newline followed by not lower letter,
		more than one space or newline, or at least one tab.

If already at end of a sentence, return caret position.
If not in a sentence, search for end of next sentence, if none found, return nil.
If scan-sexps crash on closing paren or something, treat this as reaching limit -
return end of sentence if caret was in one.
E.g. (Nice| sentence.) -> (Nice sentence|.) -> crash -> return (Nice sentence.|)

It used to use regex, but function (is_at_sentence_end_fn) is more flexible."
	(unless is_at_sentence_end_fn
		(setq is_at_sentence_end_fn
			(lambda ()
				(or
					(save-excursion
						(skip-chars-forward " \t")
						(or
							(eobp)
							(and
								; If current line is blank to end.
								(= (following-char) ?\n)
								(progn
									(forward-char 1)
									(is_line_blank_to_end)))))
					(and
						(find_in_vector_= ".?!" (preceding-char))
						(let ((char_after (following-char)))
							; This returns nil if char after is ?\), ?\], etc.,
							; but it will be handled by branch reaching closing paren,
							; so it's fine.
							(cond
								((= char_after ?\t) t)
								((= char_after ?\s)
									(not
										(and
											(setq char_after (char-after (1+ (point))))
											(is_lower_letter char_after))))
								((= char_after ?\n)
									(save-excursion
										(skip-chars-forward "\s\t\n")
										(not (is_lower_letter (following-char))))))))))))
	(let (
		(caret (point))
		(general_regex
			(concat "^\s\t\n" syntax::opening_paren_string syntax::closing_paren_string))
		(exit_if_reached_limit
			(lambda ()
				(when (>= (point) limit)
					(goto-char limit)
					(skip-chars-backward "\s\t\n")
					(throw 'result (when (>= (point) caret) (point))))))
	)
		(when (<= caret limit)
			; If we already are at sentence end.
			(if
				(and
					(not (bobp))
					(let ((char_before (preceding-char)))
						(and
							(not (find_in_vector_= "\s\t\n" char_before))
							(or
								(not
									(find_in_vector_=
										syntax::opening_paren_string char_before))
								(is_backslashed (1- (point))))))
					(funcall is_at_sentence_end_fn))
				caret
				(save-excursion
					(skip-chars-forward "\s\t\n")
					(catch 'result
						(while t
							; Skip to start of previous sexp.
							(skip-chars-forward general_regex)
							(funcall exit_if_reached_limit)
							(let ((char_after (following-char)))
								(if
									(find_in_vector_=
										syntax::closing_paren_string char_after)
									(if (is_backslashed (point))
										(forward-char 1)
										; Always exit here.
										(setq limit (point))
										(funcall exit_if_reached_limit))
									(if-let (
										(paren_index
											(index_in_vector_=
												syntax::opening_paren_string char_after))
									)
										(progn
											(forward-char 1)
											(when-let (
												((not (is_backslashed (1- (point)))))
												(closing_paren_position
													(region::find_closing_paren
														paren_index limit))
											)
												(goto-char closing_paren_position)))
										; char_before = space, tab or newline.
										(if (funcall is_at_sentence_end_fn)
											(throw 'result (point))
											(skip-chars-forward "\s\t\n"))))))))))))

; Now this doesn't support line comments that have starters that use
; different chars. I don't even know of any language that would meet this criteria,
; e.g. C - "//", Python and Cmake - "#", Elisp - ";", Sql - "--", ...
; (xml don't have line comments, only region ones)
(defun region::sentence_start_in_comment (limit comment_type)
"limit shouldn't be less than comment_region_min_start.
comment_type should be either 'lineComment or 'regionComment."
	(if (eq comment_type 'lineComment)
		(let* (
			(caret (point))
			(comment_starter_char (aref comment::line_string 0))
			(comment_starter_string (char-to-string comment_starter_char))
			(general_regex
				(concat
					"^\s\t\n"
					syntax::opening_paren_string
					syntax::closing_paren_string
					comment_starter_string))
			(is_at_sentence_start_fn
				(lambda ()
					(and
						(not (eobp))
						(or
							(save-excursion
								(skip-chars-backward " \t")
								(skip-chars-backward comment_starter_string)
								(skip-chars-backward " \t")
								(or
									(bobp)
									(and
										; If current line is blank to start.
										(= (preceding-char) ?\n)
										(progn
											(forward-char -1)
											(skip-chars-backward " \t")
											(skip-chars-backward comment_starter_string)
											(skip-chars-backward " \t")
											(bolp)))))
							(save-excursion
								(let ((position (point)))
									(comment::skip_blanks_backward)
									(and
										(/= (point) position)
										(find_in_vector_= ".?!" (preceding-char))
										(let ((char_after (following-char)))
											(cond
												((= char_after ?\t) t)
												((= char_after ?\s)
													; This char-after will never be nil
													; because of previous (not (eobp))
													; and (/= (point) position).
													(not
														(is_lower_letter
															(char-after (1+ (point))))))
												((= char_after ?\n)
													; Same here.
													(not
														(is_lower_letter
															(char-after position)))))))))))))
			(exit_if_reached_limit
				(lambda ()
					(when (<= (point) limit)
						(goto-char limit)
						(skip-chars-forward "\s\t\n")
						(while
							(and
								(is_line_blank_to_start)
								(> (skip-chars-forward comment_starter_string) 0)
								(> (skip-chars-forward "\s\t\n") 0)))
						(throw 'result (when (<= (point) caret) (point))))))
		)
			(when (>= caret limit)
				; If we already are at sentence start.
				(if
					(and
						(not (eobp))
						(let ((char_after (following-char)))
							(and
								(not (find_in_vector_= "\s\t\n" char_after))
								; Comment starter char may be a beginning of a sentence,
								; if it doesn't actually start a comment.
								(or
									(/= char_after comment_starter_char)
									(save-excursion
										(skip-chars-backward comment_starter_string)
										(not (is_line_blank_to_start))))
								(or
									(not
										(find_in_vector_=
											syntax::closing_paren_string char_after))
									(is_backslashed (point)))))
						(funcall is_at_sentence_start_fn))
					caret
					(save-excursion
						(comment::skip_blanks_backward)
						(skip-chars-backward comment_starter_string)
						(catch 'result
							(while t
								; Skip to start of previous sexp.
								(skip-chars-backward general_regex)
								(funcall exit_if_reached_limit)
								(let ((char_before (preceding-char)))
									(if (find_in_vector_= syntax::opening_paren_string char_before)
										(if (is_backslashed (1- (point)))
											(forward-char -1)
											; Always exit here.
											(setq limit (point))
											(funcall exit_if_reached_limit))
										(if-let (
											(paren_index
												(index_in_vector_=
													syntax::closing_paren_string char_before))
										)
											(progn
												(forward-char -1)
												(when-let (
													((not (is_backslashed (point))))
													(opening_paren_position
														(region::find_opening_paren paren_index limit))
												)
													(goto-char opening_paren_position)))
											(if (= char_before comment_starter_char)
												; If char before serves as a comment starter.
												(if
													(save-excursion
														(skip-chars-backward comment_starter_string)
														(is_line_blank_to_start))
													; Allow sentences like this:
													;Sentence.
													; If we are at sentence start.
													(if
														(and
															(not (eobp))
															(let ((char_after (following-char)))
																(and
																	(not (find_in_vector_= "\s\t\n" char_after))
																	; No need to check if char after is
																	; a comment starter, like in first if,
																	; because we know it's not, because
																	; of (skip-chars-backward general_regex).
																	(or
																		(not
																			(find_in_vector_=
																				syntax::closing_paren_string
																				char_after))
																		(is_backslashed (point)))))
															(funcall is_at_sentence_start_fn))
														(throw 'result (point))
														(comment::skip_blanks_backward)
														(skip-chars-backward comment_starter_string))
													; Char before is not a comment starter.
													(skip-chars-backward comment_starter_string))
												; char_before = space, tab or newline.
												(if (funcall is_at_sentence_start_fn)
													(throw 'result (point))
													(comment::skip_blanks_backward)
													(skip-chars-backward comment_starter_string))))))))))))
		(region::sentence_start limit)))

(defun region::sentence_end_in_comment (limit comment_type)
"limit shouldn't be less than comment_region_min_end.
comment_type should be either 'lineComment or 'regionComment."
	(if (eq comment_type 'lineComment)
		(let* (
			(caret (point))
			(comment_starter_char (aref comment::line_string 0))
			(comment_starter_string (char-to-string comment_starter_char))
			(general_regex
				(concat
					"^\s\t\n"
					syntax::opening_paren_string
					syntax::closing_paren_string))
			(is_at_sentence_end_fn
				(lambda ()
					(or
						(save-excursion
							(skip-chars-forward " \t")
							(or
								(eobp)
								(and
									; If current line is blank to end.
									(= (following-char) ?\n)
									(progn
										(forward-char 1)
										(skip-chars-forward " \t")
										(skip-chars-forward comment_starter_string)
										(skip-chars-forward " \t")
										(eolp)))))
						(and
							(find_in_vector_= ".?!" (preceding-char))
							(let ((char_after (following-char)))
								(cond
									((= char_after ?\t) t)
									((= char_after ?\s)
										(not
											(and
												(setq char_after (char-after (1+ (point))))
												(is_lower_letter char_after))))
									((= char_after ?\n)
										(save-excursion
											; Skip blanks and comment starters.
											; Like comment::skip_blanks_backward but forward.
											(while
												(progn
													(forward-char 1)
													(skip-chars-forward " \t")
													(let ((position (point)))
														(skip-chars-forward comment_starter_string)
														(skip-chars-forward " \t")
														(and
															(not (eobp))
															; If after skipping blanks and comment starter,
															; we are at the start of line, continue skipping
															; blank commented lines, otherwise go to the end
															; of sexp and exit inner loop.
															(or
																(= (following-char) ?\n)
																(progn (goto-char position) nil))))))
											(not (is_lower_letter (following-char)))))))))))
			(exit_if_reached_limit
				(lambda ()
					(when (>= (point) limit)
						(goto-char limit)
						(comment::skip_blanks_backward)
						(throw 'result (when (>= (point) caret) (point))))))
		)
			(when (<= caret limit)
				; If we already are at sentence end.
				(if
					(and
						(not (bobp))
						(let ((char_before (preceding-char)))
							(and
								(not (find_in_vector_= "\s\t\n" char_before))
								; Comment starter char may be an end of a sentence,
								; if it doesn't actually start a comment.
								(or
									(/= char_before comment_starter_char)
									(save-excursion
										(skip-chars-backward comment_starter_string)
										(not (is_line_blank_to_start))))
								(or
									(not
										(find_in_vector_=
											syntax::opening_paren_string char_before))
									(is_backslashed (1- (point))))))
						(funcall is_at_sentence_end_fn))
					caret
					(save-excursion
						(skip-chars-forward "\s\t\n")
						(catch 'result
							(while t
								; Skip to end of next sexp.
								(skip-chars-forward general_regex)
								(funcall exit_if_reached_limit)
								(let ((char_after (following-char)))
									(if (find_in_vector_= syntax::closing_paren_string char_after)
										(if (is_backslashed (point))
											(forward-char 1)
											; Always exit here.
											(setq limit (point))
											(funcall exit_if_reached_limit))
										(if-let (
											(paren_index
												(index_in_vector_=
													syntax::opening_paren_string char_after))
										)
											(progn
												(forward-char 1)
												(when-let (
													((not (is_backslashed (1- (point)))))
													(closing_paren_position
														(region::find_closing_paren
															paren_index limit))
												)
													(goto-char closing_paren_position)))
											; char_after = space, tab or newline.
											(if (funcall is_at_sentence_end_fn)
												(throw 'result (point))
												(skip-chars-forward "\s\t\n")))))))))))
		(region::sentence_end limit)))

; Paragraph region

(defun region::in_paragraph (limit_region_min)
	(save-excursion
		(goto-char (min (pos-eol) (cdr limit_region_min)))
		(skip-chars-backward " \t")
		; If after skipping blanks we are at start of line
		; or at or before limit, then line can be considered
		; blank, so a paragraph separator.
		(not (or (bolp) (<= (point) (car limit_region_min))))))

(defun region::in_paragraph_in_line_comment ()
	(save-excursion
		(goto-char (pos-eol))
		(skip-chars-backward " \t")
		(skip-chars-backward (char-to-string (aref comment::line_string 0)))
		(skip-chars-backward " \t")
		(not (bolp))))

;(defun region::default_is_at_paragraph_line (_is_first)
;"Default arg for region::paragraph_start/end."
;	(move-to-left-margin)
;	(not
;		(looking-at-p
;			(if (and fill-prefix (not (string= fill-prefix "")))
;				(concat (regexp-quote fill-prefix) "?[\s\t]*$")
;				"[\s\t]*$"))))

; TODO it would be better if this worked similar to sentence region -
; skipping sexps, not moving line by line.
; E.g.
; int foo() {
;     int a;
;     int b;
;
;     int c;
;     int d;
; }
; paragraph region will be
; <int foo() {
;     int a;
;     int b;>
; It would be better if it would skip to end of function foo
; and continue search for paragraph separator there.
; E.g.
; <int foo() {
;     int a;
;     int b;
;
;     int c;
;     int d;
; }>
; Well, actually not really, because the point is to stay in the same scope,
; and it is not always based on parens. In lisp I think it is always parens,
; but in C and python, not always.
; Ok so the solution is to use forward-sexp and alike in code,
; and manual searching for parens in strings and comments.
; Also forward-sexp can step in comments, which absolutely shouldn't happen,
; and it makes expand-region work incorrectly.
; But it's going to be pretty complicated, so lets do this some other day.
; Also this absolutely shouldn't require entire limit_region_min to search only
; for beginning of paragraph. Now it's completely undocumented, but (cdr limit_region_min)
; is only used once on one line, and if it's pos-eol on this line then it is useless,
; and 90% of the time it is just that, so it's much better to require caret to be
; on the start/end of line when calling region::paragraph_end/start instead of requiring
; limit_region_min like now.
; Also now functions like expand-region take either closest paren region or
; (cons (point-min) (point-max)), when they actually should take current scope bounds,
; which are always parens in lisp, but again not in C or python, so that's just wrong.
(defun region::paragraph_start
	(limit_region_min &optional skip_blanks is_at_paragraph_line_fn)
"If not in a paragraph, move backward to previous paragraph.
Return nil if caret is before limit or is not in
a paragraph and there is no paragraph before caret after limit.

skip_blanks non-nil means to not return start of line, but skip blanks forward
and return that position instead.

If skip_blanks is nil and returned position = limit, then it can be not start of line,
like here: /* Multi
line
comment */
returned position will be: /*| Multi..."
	(unless is_at_paragraph_line_fn
		(setq is_at_paragraph_line_fn
			(lambda (is_first)
				(when is_first
					; Move to end of line or before limit.
					(goto-char (min (pos-eol) (cdr limit_region_min))))
				(skip-chars-backward " \t")
				; If after skipping blanks we are at start of line
				; or at or before limit, then line can be considered
				; blank, so a paragraph separator.
				(not (or (bolp) (<= (point) (car limit_region_min)))))))
	(let (found_paragraph_line)
		(save-excursion
			(let (
				(first_loop_fn
					(lambda (is_first)
						(let ((position (point)))
							(and
								(>= position (car limit_region_min))
								(not
									(setq found_paragraph_line
										(funcall is_at_paragraph_line_fn is_first)))
								(/= position (goto-char (pos-eol 0)))))))
			)
				; Move backward over paragraph-separating lines.
				(when (funcall first_loop_fn t)
					(while (funcall first_loop_fn nil))))
			(when found_paragraph_line
				; Now we know that there is at least one paragraph line after limit.
				; Search backward for paragraph separator.
				(let ((position (point)))
					(while
						(and
							(/= position (setq position (goto-char (pos-eol 0))))
							; I added this (not (bobp)) because this (pos-eol 0) above,
							; when called from first buffer line, returns (point-min),
							; and if (car limit_region_min) == (point-min), then
							; found_paragraph_line is set to nil, so after this loop
							; point is set to (pos-bol 2), which is wrong.
							; I'm really not sure if this doesn't create some
							; other bug though...
							(not (bobp))
							(>= position (car limit_region_min))
							(setq found_paragraph_line
								(funcall is_at_paragraph_line_fn nil)))))
				; If found actual separator line, go to next line start.
				(goto-char (if found_paragraph_line (car limit_region_min) (pos-bol 2)))
				(when skip_blanks (skip-chars-forward " \t"))
				(point)))))

(defun region::paragraph_end
	(limit_region_min &optional skip_blanks is_at_paragraph_line_fn)
"If not in a paragraph, move forward to next paragraph.
Return nil if caret is after limit or is not in
a paragraph and there is no paragraph after caret before limit.

skip_blanks non-nil means to not return end of line, but skip blanks backward
and return that position instead.

If skip_blanks is nil and returned position = limit, then it can be not end of line,
like here: /* Multi
line
comment */
returned position will be: comment |*/"
	(unless is_at_paragraph_line_fn
		(setq is_at_paragraph_line_fn
			(lambda (is_first)
				(when is_first
					; Move to start of line or after limit.
					(goto-char (max (pos-bol) (car limit_region_min))))
				(skip-chars-forward " \t")
				; If after skipping blanks we are at end of line
				; or at or after limit, then line can be considered
				; blank, so a paragraph separator.
				(not (or (eolp) (>= (point) (cdr limit_region_min)))))))
	(let (found_paragraph_line)
		(save-excursion
			; Move forward over paragraph-separating lines.
			(let (
				(first_loop_fn
					(lambda (is_first)
						(let ((position (point)))
							(and
								(<= position (cdr limit_region_min))
								(not
									(setq found_paragraph_line
										(funcall is_at_paragraph_line_fn is_first)))
								(/= position (goto-char (pos-bol 2)))))))
			)
				(when (funcall first_loop_fn t)
					(while (funcall first_loop_fn nil))))
			(when found_paragraph_line
				; Now we know that there is at least one paragraph line before limit.
				; Search forward for paragraph separator.
				(let ((position (point)))
					(while
						(and
							(/= position (setq position (goto-char (pos-bol 2))))
							(<= position (cdr limit_region_min))
							(setq found_paragraph_line
								(funcall is_at_paragraph_line_fn nil)))))
				; If found actual separator line, go to previous line end.
				(goto-char (if found_paragraph_line (cdr limit_region_min) (pos-eol 0)))
				(when skip_blanks (skip-chars-backward " \t"))
				(point)))))

; This doesn't support line comments that have starters that use different chars.
(defun region::paragraph_start_in_comment
	(limit_region_min comment_type &optional skip_blanks)
"limit may be comment_region_min_start.
If limit_region_min is inside region::comment_region_min, then returned
paragraph start is always inside this comment (so never at start of line)."
	(if (eq comment_type 'lineComment)
		(let* (
			(comment_starter_string (char-to-string (aref comment::line_string 0)))
			(paragraph_start
				(region::paragraph_start
					limit_region_min
					nil
					(lambda (is_first)
						(when is_first (goto-char (pos-eol)))
						(skip-chars-backward " \t")
						(skip-chars-backward comment_starter_string)
						(skip-chars-backward " \t")
						(not (bolp)))))
		)
			(when paragraph_start
				(save-excursion
					(goto-char paragraph_start)
					(if (= paragraph_start (car limit_region_min))
						(when skip_blanks
							(skip-chars-forward " \t")
							(setq paragraph_start (point)))
						(skip-chars-forward " \t")
						(skip-chars-forward comment_starter_string)
						(when skip_blanks (skip-chars-forward " \t"))
						(setq paragraph_start (point))))
				paragraph_start))
		(region::paragraph_start
			limit_region_min
			skip_blanks
			(lambda (is_first)
				(when is_first
					; Move to end of line or before comment ender.
					(search-forward comment::region_end_string (pos-eol) 0))
				(skip-chars-backward " \t")
				; If after skipping blanks we are at start of line
				; or at or before limit, then line can be considered
				; blank, so a paragraph separator.
				(not (or (bolp) (<= (point) (car limit_region_min))))))))

(defun region::paragraph_end_in_comment
	(limit_region_min comment_type &optional skip_blanks)
"limit may be comment_region_min_end."
	(region::paragraph_end
		limit_region_min
		skip_blanks
		(if (eq comment_type 'lineComment)
			(lambda (is_first)
				(when is_first (goto-char (pos-bol)))
				(skip-chars-forward " \t")
				(skip-chars-forward (char-to-string (aref comment::line_string 0)))
				(skip-chars-forward " \t")
				(not (eolp)))
			(lambda (is_first)
				(when is_first
					; Move to start of line or after comment starter.
					(search-backward comment::region_start_string (pos-bol) 0))
				(skip-chars-forward " \t")
				; If after skipping blanks we are at end of line
				; or at or after limit, then line can be considered
				; blank, so a paragraph separator.
				(not (or (eolp) (>= (point) (cdr limit_region_min))))))))

(defunWithBase
	(lambda (start_fn end_fn)
		(when-let ((sentence_start (funcall start_fn (car limit_region_min))))
			(let (
				(sentence_end
					(save-excursion
						(goto-char sentence_start)
						(funcall end_fn (cdr limit_region_min))))
			)
				(when (<= (point) sentence_end)
					(cons sentence_start sentence_end)))))

	(region::sentence_region (limit_region_min)
"limit_region_min can be (cons (point-min) (point-max)), region::string_region_min, etc."
		(,base #'region::sentence_start #'region::sentence_end))

	(region::paragraph_region (limit_region_min)
"limit_region_min can be (cons (point-min) (point-max)), region::string_region_min, etc."
		(,base
			(lambda (_) (region::paragraph_start limit_region_min))
			(lambda (_) (region::paragraph_end limit_region_min))))

	(region::sentence_region_in_comment (limit_region_min comment_type)
"limit_region_min should be a (sub)region of comment_region_min.
comment_type should be either 'lineComment or 'regionComment."
		(,base
			(lambda (limit) (region::sentence_start_in_comment limit comment_type))
			(lambda (limit) (region::sentence_end_in_comment limit comment_type))))

	(region::paragraph_region_in_comment (limit_region_min comment_type)
		(,base
			(lambda (limit)
				(region::paragraph_start_in_comment limit_region_min comment_type))
			(lambda (limit)
				(region::paragraph_end_in_comment limit_region_min comment_type)))))

; Marked region

(defun region::marked_region ()
	(let ((caret (point)) (mark (mark)))
		(if (> caret mark)
			(cons mark caret)
			(cons caret mark))))

(defun region::marked_line_region ()
"Return beginning of the first line and end of the last line of marked region."
	(let ((caret (point)) (mark (mark)) start)
		(save-excursion
			(if (> caret mark)
				(progn
					(goto-char mark)
					(setq start (pos-bol))
					(goto-char caret))
				(setq start (pos-bol))
				(goto-char mark))
			(cons start (pos-eol)))))

(defun region::apply_on_marked_line_region (fn &optional region marker_type)
"Call FN with no args on beginning of every line of marked line region,
starting from the top line.
Return marker tracking region end. Leave caret after the last forward-line call,
so at bol after the last line of REGION or eob. Because of this, this function
should often be wrapped in `saveCaretAndMark'.
Don't check if mark is active.
FN should't change caret's line (it can insert newlines though).
If REGION is nil, use marked region.
REGION can be unsorted.
REGION is not modified.

Position of elements of REGION inside a line USUALLY don't matter, it only
matters on what line they are.
UPDATE: The position closing REGION can matter, if FN will insert newline
at the last region line. If for example this position is at the start of the
line and FN will insert newline at the end of it, then FN won't be called
on this new line.
So I also added MARKER_TYPE arg to allow better control of it."
	(unless region (setq region (cons (point) (mark))))
	(let (start end_marker)
		(if (> (car region) (cdr region))
			(setq start (cdr region) end_marker (car region))
			(setq start (car region) end_marker (cdr region)))
		(setq end_marker (copy-marker end_marker marker_type))
		(goto-char start)
		(forward-line 0)
		(while
			(progn
				(funcall fn)
				(and
					(= (forward-line 1) 0)
					(= (preceding-char) ?\n)
					(<= (point) end_marker))))
		(prog1 (marker-position end_marker) (set-marker end_marker nil))))

(provide 'myRegion)
