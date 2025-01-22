; -*- lexical-binding:nil -*-

(defmacro += (var &rest numbers_or_markers) `(setq ,var (+ ,var ,@numbers_or_markers)))
(defmacro -= (var &rest numbers_or_markers) `(setq ,var (- ,var ,@numbers_or_markers)))
(defmacro ++ (var) "prefix increment" `(setq ,var (1+ ,var)))
(defmacro -- (var) "prefix decrement" `(setq ,var (1- ,var)))

(defsubst return_first_arg (&rest args) (car args))
(defsubst return_second_arg (&rest args) (nth 1 args))

(defun fn_symbol (symbol_name fn_definition)
"Make symbol, fset it to fn_definition, return that symbol."
	(let ((symbol (make-symbol symbol_name)))
		(fset symbol fn_definition)
		symbol))

(defmacro lambdaSymbol (&rest lambda_content) (declare (doc-string 2))
	`(fn_symbol "" (lambda ,@lambda_content)))

(defmacro timeBody (&rest body)
"Execute body and return cons (time_taken . body_return_value)."
	`(let ((time (current-time)) (body_return_value (progn ,@body)))
		(cons (float-time (time-subtract nil time)) body_return_value)))

(defmacro messageTimeBody (message_format_string &rest body)
	`(let ((time_and_return_value (timeBody ,@body)))
		 (message ,message_format_string (car time_and_return_value))
		 (cdr time_and_return_value)))

(defconst SPACE_STRING_VECTOR
	(cl-loop
		for i to 200
		vconcat (vector (make-string i ?\s))))

(defun get_space_string (n) "If n < 200, don't modify returned string."
	(if (> n 200)
		(make-string n ?\s)
		(aref SPACE_STRING_VECTOR n)))

(defmacro defvar-window-local (symbol &optional doc_string default_value_form)
"Define getter SYMBOL and gv-setter for SYMBOL, getting or setting window-parameter.

DEFAULT_VALUE_FORM if non-nil, is evaluated every time when getter is called
and window parameter is nil."
	(declare (doc-string 2))
	`(progn
		(gv-define-setter ,symbol (value &optional window)
			(list #'set-window-parameter window '',symbol value))
		(fset ',symbol
			(lambda (&optional window)
				,(concat
					"Auto-generated getter of a window-local variable `"
					(symbol-name symbol)
					"'."
					(when doc_string (concat "\n\n" doc_string))
					(when default_value_form
						(concat
							"\n\nThe form to obtain the default value:\n"
							(format "%S" default_value_form))))
				,(if default_value_form
					`(or
						(window-parameter window ',symbol)
						(setf (,symbol window) ,default_value_form))
					`(window-parameter window ',symbol))))
		nil))

(defmacro defunWithBase (base_fn &rest name_args_body_lists)
"name_args_body_lists should be lists defining functions,
e.g. (name_like_in_defun (args_like_in_defun) body_like_in_defun).
These functions can call (,base args).

This macro is a more consize version of something like:
(let ((base (lambda (args) body)))
	(fset (intern \"function_name_1\")
		`(lambda (args) [...] (,base [...])))
	(fset ...))..."
	`(let ((base (fn_symbol "base" ,base_fn)))
		,@(mapcar
			(lambda (name_args_body_list)
				`(fset (intern ,(symbol-name (car name_args_body_list)))
					,(cons #'backquote (list (cons #'lambda (cdr name_args_body_list))))))
			name_args_body_lists)))

(defmacro withTempBuffer (&rest body)
	`(let ((temp_buffer (generate-new-buffer " *temp*" t)))
		(prog1
			(with-current-buffer temp_buffer
				(let ((inhibit-modification-hooks t) (inhibit-read-only t))
					,@body))
			(kill-buffer temp_buffer))))

(defmacro saveCaretAndMark (&rest body)
"Save caret and mark using advancing markers and set deactivate-mark to nil."
	(let (
		(caret_marker (make-symbol "caret_marker"))
		(mark_marker (make-symbol "mark_marker"))
	)
		`(let (
			(,caret_marker (copy-marker (point) t))
			(,mark_marker (copy-marker (mark) t))
		)
			(prog1 (progn ,@body)
				(setq deactivate-mark nil)
				(goto-char ,caret_marker)
				(set-marker (mark-marker) ,mark_marker)
				(set-marker ,caret_marker nil)
				(set-marker ,mark_marker nil)))))

(defmacro withDemotedErrors (&rest body)
"Convert errors to messages.
Return nil if error was catched (or body just returned nil)."
	`(condition-case err
		(progn ,@body)
		(error (message "%s." (error-message-string err)) nil)))

(defmacro withDemotedError (condition &rest body)
"Convert error to a message.
Return nil if error was catched (or body just returned nil)."
	`(condition-case err
		(progn ,@body)
		(,condition (message "%s." (error-message-string err)) nil)))

(defun delete_from_list_by_index (list index)
"Delete one element from list. Index must be > 0. Return nil."
	(let ((cell_before (nthcdr (1- index) list)))
		(setcdr cell_before (nthcdr 2 cell_before)))
	nil)

(defun insert_in_list_by_index (list index object)
"Insert OBJECT into LIST. INDEX must be in [1, (length list)] range. Return nil."
	(let ((cell_before (nthcdr (1- index) list)))
		(setcdr cell_before (cons object (cdr cell_before))))
	nil)

(defun cons_swap (cons_cell)
	(let ((swap (car cons_cell)))
		(setcar cons_cell (cdr cons_cell))
		(setcdr cons_cell swap))
	cons_cell)

(defun propertize_no_copy (str &rest property_value_pairs)
	(let ((str_length (length str)))
		(while property_value_pairs
			(put-text-property
				0 str_length (pop property_value_pairs) (pop property_value_pairs) str)))
	str)

(defun is_buffer_empty (&optional buffer) (= (buffer-size buffer) 0))

(defun swap_caret_and_mark ()
	(let ((caret (point))) (goto-char (mark)) (set-marker (mark-marker) caret)))

(defun get_mark_column () (save-excursion (goto-char (mark)) (current-column)))

(defun in_range_inclusive (number_or_marker min_ max_)
"Return t if number_or_marker is in range [min, max]."
	(and (>= number_or_marker min_) (<= number_or_marker max_)))
(defun in_range_exclusive (number_or_marker min_ max_)
"Return t if number_or_marker is in range (min, max)."
	(and (> number_or_marker min_) (< number_or_marker max_)))

(defconst LOWER_CASE_POLISH_LETTER_STRING "ęóąśłżźćń")
(defconst UPPER_CASE_POLISH_LETTER_STRING "ĘÓĄŚŁŻŹĆŃ")
(defconst POLISH_LETTER_STRING
	(concat LOWER_CASE_POLISH_LETTER_STRING UPPER_CASE_POLISH_LETTER_STRING))

(defun is_lower_polish_letter (char) "char may be nil."
	(find_in_vector_eq LOWER_CASE_POLISH_LETTER_STRING char))
(defun is_upper_polish_letter (char) "char may be nil."
	(find_in_vector_eq UPPER_CASE_POLISH_LETTER_STRING char))
; Ó 211 ; ó 243 ; Ą 260 ą 261 Ć 262 ć 263 ; Ę 280 ę 281 ; Ł 321 ł 322 Ń 323 ń 324 ; Ś 346 ś 347 ; Ź 377 ź 378 Ż 379 ż 380
(defun is_polish_letter (char)
	(or
		(in_range_inclusive char 260 263) ; Ą ą Ć ć
		(in_range_inclusive char 321 324) ; Ł ł Ń ń
		(in_range_inclusive char 377 380) ; Ź ź Ż ż
		(find_in_vector_= "ęĘóÓśŚ" char)))

; Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pc Pd Ps Pe Pi Pf Po Sm Sc Sk So Zs Zl Zp Cc Cf Cs Co Cn
; 1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
(defun get_unicode_category (char) (aref unicode-category-table char))

; I saw this somewhere: (characterp (get-char-code-property char 'lowercase)).

; These are around 9 times faster than (string-match "[[:lower:]]" (char-to-string char)),
; but actually produce different results:
;(let (a b)
;	(let (case-fold-search)
;		(dotimes (i 10000)
;			(unless
;				(eq
;					(is_lower_letter i)
;					(not (not (string-match "[[:lower:]]" (char-to-string i)))))
;				(if (is_lower_letter i) (push i a) (push i b)))))
;	(cons (mapconcat #'char-to-string a) (mapconcat #'char-to-string b)))
; returns
; ("ℴℯℓℏℎℊᴀֈևՠϼΰΐʯʮʭʬʫʪʩʨʧʦʥʤʣʢʡʠʟʜʛʚʙʘʗʖʕʓʑʐʏʎʍʆʅʄʁɿɾɼɻɺɹɸɷɶɴɳɰɮɭɧɤɢɟɞɝɚɘɕȹȸȷȶȵȴȡǰƾƺƫƪƛƍſŉĸı"
; . "ⓩⓨⓧⓦⓥⓤⓣⓢⓡⓠⓟⓞⓝⓜⓛⓚⓙⓘⓗⓖⓕⓔⓓⓒⓑⓐⅿⅾⅽⅼⅻⅺⅹⅸⅷⅶⅵⅴⅳⅲⅱⅰͅ").
; So elisp regex uses a different method; I'm not sure which is better.
(defun is_upper_letter (char) (= 1 (get_unicode_category char)))
(defun is_lower_letter (char) (= 2 (get_unicode_category char)))
(defun is_letter (char) (in_range_inclusive (get_unicode_category char) 1 5))
(defun is_digit (char) (in_range_inclusive (get_unicode_category char) 9 10))

; Binding keys

(defconst KEY_MODIFIER_VECTOR ["" "S-" "C-" "A-" "C-S-" "A-S-" "A-C-" "A-C-S-"]
"Doesn't include M.")

(defun bind_many_keys (keymap key_vector_sequence fn)
	(mapc (lambda (key_vector) (define-key keymap key_vector fn)) key_vector_sequence))


; Unused
(defun before_or_inside_regex (regex distance)
"Return match end position if caret is before or inside regex, else nil.
match-end from this is accurate, unlike match-beginning."
	(if (looking-at regex)
		(match-end 0)
		(let ((limit (- (point) distance)) (caret_position (point)))
			(save-excursion
				(cl-loop
					while (and (not (bobp)) (> (point) limit))
					do (forward-char -1)
					when (looking-at regex)
						return
							(if (< caret_position (match-end 0))
								(match-end 0)))))))

(defun move_to_column_chop (column)
"If column is in the middle of a tab, then change that tab to spaces.
If column was after the end of line, return column moved to, nil otherwise."
	(let ((column_after_move (move-to-column column)))
		(cond
			((> column_after_move column) ; When column is in the middle of tab.
				(delete-char -1)
				(insert (get_space_string (- column_after_move (current-column))))
				(forward-char (- column column_after_move))
				nil)
			((< column_after_move column) ; When column is after the end of line.
				column_after_move))))

(defun move_to_column_chop_and_extend (column)
"If column is in the middle of a tab, then change that tab to spaces.
Add tabs and/or spaces if column is after end of line.
Return:
- 'chopped if column was in the middle of the tab
- t if column was after the end of line
- nil otherwise."
	(let ((column_after_move (move-to-column column)))
		(cond
			((> column_after_move column) ; When column is in the middle of tab.
				(delete-char -1)
				; Because of tab stops, tab can have width [1, tab-width].
				(insert (get_space_string (- column_after_move (current-column))))
				(forward-char (- column column_after_move))
				'chopped)
			((< column_after_move column) ; When column is after the end of line.
				(insert ?\t)
				(while (< (current-column) column)
					(insert ?\t))
				(when (> (current-column) column)
					(delete-char -1)
					(insert (get_space_string (- column (current-column)))))
				t))))

(defun move_to_column_recognize_tabs (column)
"Like move-to-column, but if column is inside a tab, don't just move past it,
but check which end of this tab is closer to column, and move there.
Return column moved to."
	(let ((column_after_move (move-to-column column)))
		(if (<= column_after_move column)
			column_after_move
			(forward-char -1)
			(let ((column_before_tab (current-column)))
				; Count left end as closer if column is <= 0.5 width of a tab,
				; so the same as posn-point now counts every char.
				; (if (< (+ column_after_move column_before_tab) (* 2 column))
				(if (< (- column_after_move column) (- column column_before_tab))
					(progn (forward-char 1) column_after_move)
					column_before_tab)))))

(defun buffer_substring_tabs_to_spaces (start_position end_position)
"Return string with all properties copied from current buffer with
tabs changed to spaces based on actual tab-stops, not on tab-width.
Leave caret at end_position."
	(goto-char start_position)
	; tab_group_info = (start_index_in_str end_index_in_str position_after_last_tab)
	(let ((str_length 0) tab_group_info_list)
		(while (< (point) end_position)
			(if (= ?\t (char-after))
				(let ((column_before_first_tab (current-column)))
					(while
						(progn
							(forward-char 1)
							(and (< (point) end_position) (= ?\t (char-after)))))
					(push
						(list str_length (+= str_length (- (current-column) column_before_first_tab)) (point))
						tab_group_info_list))
				(++ str_length)
				(forward-char 1)))
		(setq tab_group_info_list (nreverse tab_group_info_list))
		; This multibyte is actually necessary - it turns out string won't turn into a multibyte one
		; after asetting one of it's chars to a non-ascii value.
		(let ((str (make-string str_length ?\s t))
			  (i 0)
			  (copy_chars
				(lambda (end) ; Dynamic binding: str, i.
					(while (< i end)
						(aset str i (char-after))
						(forward-char 1)
						(++ i)))))
			(goto-char start_position)
			(while (< i str_length)
				(if-let ((tab_group_info (pop tab_group_info_list)))
					(let ((str_index_before_copy i) (position_before_copy (point)))
						(funcall copy_chars (pop tab_group_info))
						; Copy text properties.
						; Could be done by just copying them char by char, but then string that would normally be #("1234" 0 4 (face default)),
						; would instead look like this: #("1234" 0 1 (face default) 1 2 (face default) 2 3 (face default) 3 4 (face default)).
						; The compromise way I do this now is to group tabs, so ends of that groups are the only points
						; of unnecessary splits of text props, as that's pretty easy and efficient to do.
						; Spaces added in place of tabs have properties copied from first tab in a group.
						(let ((position_after_first_tab (1+ (point))) next_property_change)
							(while
								(/=
									(setq next_property_change (next-property-change position_before_copy nil position_after_first_tab))
									position_after_first_tab)
								(set-text-properties
									str_index_before_copy
									(+= str_index_before_copy (- next_property_change position_before_copy))
									(text-properties-at position_before_copy)
									str)
								(setq position_before_copy next_property_change)))
						; Copy properties from first tab to spaces.
						(set-text-properties
							str_index_before_copy
							(setq i (pop tab_group_info))
							(text-properties-at position_before_copy)
							str)
						(goto-char (car tab_group_info)))
					(let ((str_index_before_copy i) (position_before_copy (point)))
						(funcall copy_chars str_length)
						(let ((position_after_copy (point)) next_property_change)
							(while
								(progn
									(setq next_property_change (next-property-change position_before_copy nil position_after_copy))
									(set-text-properties
										str_index_before_copy
										(+= str_index_before_copy (- next_property_change position_before_copy))
										(text-properties-at position_before_copy)
										str)
									(/= next_property_change position_after_copy))
								(setq position_before_copy next_property_change))))))
			str)))

(defun line_tabs_to_spaces ()
"Change tabs to spaces in current line.
Text props on spaces are not preserved.
Leave caret at eol.
Return nil."
	(forward-line 0)
	(let ((bol (point)))
		(while
			(progn
				(skip-chars-forward "^\t\n")
				(= (following-char) ?\t))
			(let ((column_before (current-column)) (pos (point)))
				(skip-chars-forward "\t")
				(let ((column_after (current-column)))
					(delete-region pos (point))
					(insert (get_space_string (- column_after column_before))))))))

(defun line_count_backward (position)
"Return number of newlines between caret and position.
If position is on the same line as caret or on line after caret, return 0."
	(let ((i 0))
		(save-excursion
			(while (and (<= position (point)) (search-backward "\n" position t))
				(++ i)))
		i))
(defun line_count_forward (position)
"Return number of newlines between caret and position.
If position is on the same line as caret or on line before caret, return 0."
	(let ((i 0))
		(save-excursion
			(while (and (> position (point)) (search-forward "\n" position t))
				(++ i)))
		i))

(defun is_caret_on_the_same_line (position)
	(= (pos-bol) (save-excursion (goto-char position) (pos-bol))))

(defun goto_line (line_number) "Probably slow (doc of goto-line says that)."
	(goto-char (point-min)) (forward-line (1- line_number)))

(defun at_first_buffer_line () (not (save-excursion (search-backward "\n" nil t))))
(defun at_last_buffer_line () (not (save-excursion (search-forward "\n" nil t))))

(defun is_line_blank_to_start ()
"Return t if line has only blanks from caret to line start."
	(save-excursion (skip-chars-backward "\s\t") (memq (char-before) '(?\n nil))))

(defun is_line_blank_to_end ()
"Return t if line has only blanks from caret to line end."
	(save-excursion (skip-chars-forward "\s\t") (memq (char-after) '(?\n nil))))

(defun is_before_string (str)
"Return match-end position if caret is just before string."
	(save-excursion (search-forward str (+ (point) (length str)) t)))
(defun is_after_string (str)
"Return match-start position if caret is just after string."
	(save-excursion (search-backward str (- (point) (length str)) t)))

(defun is_inside_string (str)
"Return match-end position if caret is inside string.
Work even at start and end of buffer.
Don't work correctly with strings whose length is 1."
	(let ((caret (point)) (str_length (length str)))
		(let ((distance_to_move (1- str_length)))
			(save-excursion
				(goto-char (- caret distance_to_move))
				(search-forward str (+ caret distance_to_move) t)))))

(defun is_backslashed_str (index str)
	(let ((backslash_count 0))
		(while (and (>= (-- index) 0) (= (aref str index) ?\\)) (++ backslash_count))
		(= 1 (% backslash_count 2))))

(defun string_search_not_backslashed (needle haystack &optional start_index)
"Return index of start of the first occurrence of not backslashed NEEDLE in HAYSTACK.
Start searching at START_INDEX or 0."
	(let ((i (or start_index 0)))
		(while
			(and
				(setq i (string-search needle haystack i))
				(is_backslashed_str i haystack))
			(++ i))
		i))

; I moved this from myInsert for now.
(defun syntax::raw_to_class (raw_syntax_descriptor)
"Convert syntax class char, like ?\\( ?\\) ?\\\" etc.,
from raw syntax descriptor (cons) like that returned by `syntax-after'."
	(syntax-class-to-char (logand (car raw_syntax_descriptor) 65535)))

(defun is_backslashed (position)
"Returns t if char after position is \"cancelled\" by backslash or char quote."
	(or
		; Backslash.
		(= (% (save-excursion (goto-char position) (skip-syntax-backward "\\")) 2) -1)
		; Character quote.
		(when-let ((syntax_before (syntax-after (1- position))))
			(= (syntax::raw_to_class syntax_before) ?/))))

(defun inside_string () (nth 3 (syntax-ppss)))
(defun inside_comment ()
	(and
		(nth 4 (syntax-ppss))
		; Unfortunately for me, I assumed that if we are inside comment ender,
		; then (nth 4 (syntax-ppss)) will be nil, but it isn't.

		; WOW another exciting discovery! It turns out, in nxml mode
		; when you are inside comment starter, (nth 4 (syntax-ppss))
		; says you are in a comment, where at least in c-mode it works
		; exactly the other way around.
		; So here is a temporary "fix".
		(not
			(and
				comment::region_start_string
				(or
					(and
						(> (length comment::region_end_string) 1)
						(is_inside_string comment::region_end_string))
					(and
						(> (length comment::region_start_string) 1)
						(is_inside_string comment::region_start_string)))))))
(defun inside_code ()
"Return non-nil if caret isn't inside string or comment."
	(not (or (inside_string) (inside_comment))))


(defun is_camel_humps ()
"Non-nil if caret is in a camelHumps symbol.
If right after skipping upper letters and digits there is a lower letter, return t."
	(or
		(save-excursion
			; Continuing through symbol extensions was more annoying than helpful -
			; in something like foo_BAR, it would all be non camelHumps, so in BAR
			; jumping would be happening char-by-char.
			; That really only annoyed, never added anything, so I removed it.
			; So now symbol extensions are borders that bound this search for
			; lower case letter.
			(skip-chars-forward "[:upper:][:digit:]")
			(is_lower_letter (following-char)))
		(save-excursion
			(skip-chars-backward "[:upper:][:digit:]")
			(is_lower_letter (preceding-char)))))

(defun maybe_delete_marked_region ()
	(when mark-active
		(delete-region (region-beginning) (region-end))
		(deactivate-mark)
		t))

(defun skip_invisible_forward ()
	(while (and (not (eobp)) (invisible-p (point))) (forward-char 1)))

(defun skip_invisible_backward ()
	(while (and (not (bobp)) (invisible-p (1- (point)))) (forward-char -1)))

(defun goto_visual_line_end ()
	(when (= (vertical-motion 1) 1)
		(forward-char -1)
		(skip_invisible_backward))
	nil)

(defun goto_window_last_visible_line_start () (move-to-window-line -1) nil)

(defun get_window_last_column ()
	(- (window-body-width) (round (line-number-display-width 'columns))))
(defun get_window_last_visible_column ()
	(+ (window-hscroll) (get_window_last_column)))

(defun is_caret_below_window ()
"Return t if caret is not (or partially) visible
and will cause window scroll on next redisplay."
	(>
		(point)
		(save-excursion
			(goto_window_last_visible_line_start)
			(goto_visual_line_end)
			(point))))

(defun get_window_body_frame_pixel_top ()
"Return position of the topmost pixel of body of selected window relative
to left-top corner of it's frame (just like (mouse-pixel-position))."
	(+ (window-pixel-top) (nth 1 (pos-visible-in-window-p (window-start) nil t))))

; Unsure if that's all, or if there are some other factors that should be included.
; Works well for my current config.
(defun get_window_body_frame_pixel_left ()
"Return position of the leftmost pixel of body of selected window relative
to left-top corner of it's frame (just like (mouse-pixel-position))."
	(+
		(frame-internal-border-width)
		(window-pixel-left)
		(line-number-display-width t)
		(car (window-fringes))
		(if (eq 'left (car (window-current-scroll-bars)))
			(window-scroll-bar-width)
			0)
		(if-let ((window_left_margin (car (window-margins))))
			(* window_left_margin (frame-char-width))
			0)))


(defun color_name_to_24 (color_name_string)
"Convert color name string like \"red\" to 24-bit color string like \"#ff0000\".
Letters in returned string are lowercase.
Incidentally, if color_name_string is actually a 24-bit color string,
return it (also lowercased)."
	(let ((rgb_color_list (color-name-to-rgb color_name_string)))
		(color-rgb-to-hex
			(pop rgb_color_list)
			(pop rgb_color_list)
			(car rgb_color_list)
			2)))

(defun set_string_part (str substr start_index)
"Set part of str from start_index to (+ start_index (length substr)) be substr.
Don't copy substr's text properties.
Don't check str's bounds."
	(dotimes (i (length substr))
		(aset str start_index (aref substr i))
		(++ start_index)))

(defun string-ignore-case= (str_1 str_2) (string= (downcase str_1) (downcase str_2)))

(defmacro tempFindInSequence (cl_loop_keyword sequence pred)
	`(cl-loop
		for element ,cl_loop_keyword ,sequence
		when (funcall ,pred element) return element))
(defmacro tempIndexInSequence (cl_loop_keyword sequence pred)
	`(cl-loop
		for i from 0
		for element ,cl_loop_keyword ,sequence
		when (funcall ,pred element) return i))

(defun find_in_list (list pred)
"Return element the first time pred returns non-nil.
pred is called with this loop's element."
	(tempFindInSequence in list pred))
(defun find_in_vector (vector pred)
"Return element the first time pred returns non-nil.
pred is called with this loop's element."
	(tempFindInSequence across vector pred))
(defun index_in_list (list pred)
"Return index the first time pred returns non-nil.
pred is called with this loop's element."
	(tempIndexInSequence in list pred))
(defun index_in_vector (vector pred)
"Return index the first time pred returns non-nil.
pred is called with this loop's element."
	(tempIndexInSequence across vector pred))

(unintern 'tempFindInSequence)
(unintern 'tempIndexInSequence)

(defun find_in_list_= (list number_or_marker) "Use = to compare."
	(find_in_list list (lambda (element) (= element number_or_marker))))
(defun find_in_list_eq (list object) "Use eq to compare."
	(find_in_list list (lambda (element) (eq element object))))
(defun find_in_list_equal (list object) "Use equal to compare."
	(find_in_list list (lambda (element) (equal element object))))
(defun find_in_list_string= (list string) "Use string= to compare."
	(find_in_list list (lambda (element) (string= element string))))
(defun find_in_vector_= (vector number_or_marker) "Use = to compare."
	(find_in_vector vector (lambda (element) (= element number_or_marker))))
(defun find_in_vector_eq (vector object) "Use eq to compare."
	(find_in_vector vector (lambda (element) (eq element object))))
(defun find_in_vector_equal (vector object) "Use equal to compare."
	(find_in_vector vector (lambda (element) (equal element object))))
(defun find_in_vector_string= (vector string) "Use string= to compare."
	(find_in_vector vector (lambda (element) (string= element string))))

(defun index_in_list_= (list number_or_marker) "Use = to compare."
	(index_in_list list (lambda (element) (= element number_or_marker))))
(defun index_in_list_eq (list object) "Use eq to compare."
	(index_in_list list (lambda (element) (eq element object))))
(defun index_in_list_equal (list object) "Use equal to compare."
	(index_in_list list (lambda (element) (equal element object))))
(defun index_in_list_string= (list string) "Use string= to compare."
	(index_in_list list (lambda (element) (string= element string))))
(defun index_in_vector_= (vector number_or_marker) "Use = to compare."
	(index_in_vector vector (lambda (element) (= element number_or_marker))))
(defun index_in_vector_eq (vector object) "Use eq to compare."
	(index_in_vector vector (lambda (element) (eq element object))))
(defun index_in_vector_equal (vector object) "Use equal to compare."
	(index_in_vector vector (lambda (element) (equal element object))))
(defun index_in_vector_string= (vector string) "Use string= to compare."
	(index_in_vector vector (lambda (element) (string= element string))))

; Unused.
(defun merge_strings (string_seq &optional separator remove_empty_strings)
	(mapconcat
		#'identity
		(delete-dups
			(mapcan
				(lambda (str)
					(split-string str separator remove_empty_strings))
				string_seq))
		separator))

; Unused.
(defun create_xpm (width height color)
"Create an XPM bitmap using width, height and color."
	(propertize " "
		'display
			(create-image
				(let ((quote_and_dots_string (concat "\"" (make-string width ?.))))
					(concat
						(format
							"/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
							width height color color)
						(cl-loop
							repeat (1- height)
							with str = (concat quote_and_dots_string "\",\n")
							concat str)
						quote_and_dots_string
						"\"};"))
				'xpm t
				:ascent 'center)))

(defun define_face (face spec &optional doc_string)
"Face should be a symbol, spec a list like '((t . face_attribute_plist)).
Redefine face if it already exists.
Set both default and frame specific values.
Don't support face aliasing, 'face-override-spec attribute
and old '((t face_attribute_plist)) like spec format.
Don't use this with 'default face, it seems like it needs different handling;
like (face-spec-set 'default '((t . face_attribute_plist)))."
	(declare (doc-string 3))
	(when doc_string (put face 'face-documentation (purecopy doc_string)))
	(put face 'face-defface-spec (purecopy spec))
	; Initialize the face if it doesn't exist.
	(dolist (frame (frame-list)) (internal-make-lisp-face face frame))
	; Define frame-local faces for all frames from X resources.
	(make-face-x-resource-internal face)
	; Set/update frames' faces.
	; If face is customized or themed, set the custom spec from theme-face records.
	(let ((theme_faces (get face 'theme-face)))
		(dolist (frame (frame-list))
			; Reset frame's face.
			(apply #'set-face-attribute face frame face--attributes-unspecified)
			(let (
				(is_attribute_acceptable_by_frame
					(lambda (attribute_key)
						(assq attribute_key face-x-resources)))
				(set_frame_face_attributes
					; Dynamic binding: face, frame, is_attribute_acceptable_by_frame.
					(lambda (attribute_plist)
						(let (face_attribute_plist_for_frames)
							(while attribute_plist
								(when
									(funcall is_attribute_acceptable_by_frame
										(car attribute_plist))
									(push (car attribute_plist)
										face_attribute_plist_for_frames)
									(push (nth 1 attribute_plist)
										face_attribute_plist_for_frames))
								(setq attribute_plist (nthcdr 2 attribute_plist)))
							(apply #'set-face-attribute
								face frame (nreverse face_attribute_plist_for_frames)))))
				(choose_face_spec
					; Return the proper attributes for FRAME, out of SPEC.
					; Value is a plist of face attributes in the form of attribute-value pairs.
					; If no match is found, return 'noMatch (no nil because spec might be nil).
					; Dynamic binding: frame.
					(lambda (spec_)
						(let (result default_list match_found)
							(while spec_
								(let* (
									(spec_entry (pop spec_))
									(display_requirement (car spec_entry))
									(attribute_plist (cdr spec_entry))
								)
									; If the condition is `default', that sets the default
									; for following conditions.
									(cond
										((eq display_requirement 'default)
											(setq default_list attribute_plist))
										; Otherwise, if it matches, use it.
										((face-spec-set-match-display display_requirement frame)
											(setq result attribute_plist
												spec_ nil
												match_found t)))))
							(cond
								(default_list (append default_list result))
								(match_found result)
								(t 'noMatch)))))
				theme_face_applied
			)
				(when theme_faces
					(dolist (elt (reverse theme_faces))
						(let ((attribute_plist (funcall choose_face_spec (nth 1 elt))))
							(unless (eq attribute_plist 'noMatch)
								(funcall set_frame_face_attributes attribute_plist)
								(setq theme_face_applied t)))))
				; If there was a spec applicable to FRAME, that overrides the
				; defface spec entirely rather than inheriting from it, with the
				; exception of the :extend attribute (which is inherited).

				; If there was no spec applicable to FRAME, apply the defface
				; spec as well as any applicable X resources.
				(if theme_face_applied
					(when-let (
						((eq 'unspecified (face-attribute face :extend frame t)))
						(extend_sub_plist
							(plist-member (funcall choose_face_spec spec) :extend))
						((funcall is_attribute_acceptable_by_frame :extend))
					)
						(set-face-attribute face frame :extend (nth 1 extend_sub_plist)))
					(funcall set_frame_face_attributes (funcall choose_face_spec spec)))
				; Set the `scroll-bar-foreground' and `scroll-bar-background'
				; frame parameters, because the face is handled
				; by setting those two parameters.
				(when (eq face 'scroll-bar)
					(set-frame-parameter
						frame 'scroll-bar-foreground (face-foreground face))
					(set-frame-parameter
						frame 'scroll-bar-background (face-background face))))))
	nil)

; Old crap:

;change-major-mode-hook
;highlightLine::global_mode-cmhh
;(lambda ()
;	(add-to-list 'highlightLine::global_mode-buffers (current-buffer))
;	(add-hook 'post-command-hook #'highlightLine::global_mode-check-buffers))
;highlightLine::global_mode-check-buffers
;(lambda ()
;	(highlightLine::global_mode-enable-in-buffers)
;	(remove-hook 'post-command-hook #'highlightLine::global_mode-check-buffers))
;
;after-change-major-mode-hook
;highlightLine::global_mode-enable-in-buffers
;(lambda ()
;	(let ((buffers highlightLine::global_mode-buffers))
;		(setq highlightLine::global_mode-buffers nil)
;		(dolist (buf buffers)
;			(when (buffer-live-p buf)
;				(with-current-buffer buf
;					(or
;						highlightLine::mode-set-explicitly
;						(eq highlightLine::mode-major-mode major-mode)
;						(if highlightLine::mode
;							(progn
;								(highlightLine::mode -1)
;								(unless (string-prefix-p " *" (buffer-name))
;									(highlightLine::mode)))
;							(unless (string-prefix-p " *" (buffer-name))
;								(highlightLine::mode))))
;					(setq highlightLine::mode-major-mode major-mode))))))
;
;highlightLine::global_mode
;(lambda (&optional arg)
;	(setq highlightLine::global_mode
;		(cond
;			((eq arg 'toggle) (not highlightLine::global_mode))
;			((and (numberp arg) (< arg 1)) nil)
;			(t t)))
;	(setq global-minor-modes (delq 'highlightLine::global_mode global-minor-modes))
;	(when highlightLine::global_mode
;		(push 'highlightLine::global_mode global-minor-modes))
;	(if highlightLine::global_mode
;		(progn
;			(add-hook 'after-change-major-mode-hook #'highlightLine::global_mode-enable-in-buffers)
;			(add-hook 'find-file-hook #'highlightLine::global_mode-check-buffers)
;			(add-hook 'change-major-mode-hook #'highlightLine::global_mode-cmhh))
;		(remove-hook 'after-change-major-mode-hook #'highlightLine::global_mode-enable-in-buffers)
;		(remove-hook 'find-file-hook #'highlightLine::global_mode-check-buffers)
;		(remove-hook 'change-major-mode-hook #'highlightLine::global_mode-cmhh))
;	(dolist (buf (buffer-list))
;		(with-current-buffer buf
;			(cond
;				(highlightLine::global_mode
;					(unless (string-prefix-p " *" (buffer-name))
;						(highlightLine::mode)))
;				(highlightLine::mode
;					(highlightLine::mode -1)))))
;	(run-hooks
;		'highlightLine::global_mode-hook
;		(if highlightLine::global_mode
;			'highlightLine::global_mode-on-hook
;			'highlightLine::global_mode-off-hook))
;	; interactive message
;	(force-mode-line-update)
;	highlightLine::global_mode)

(defvar buffer_with_major_mode_list
	; Approximate first value (probably only *scratch* and *Messages*).
	(cl-loop
		for buffer in (buffer-list)
		unless (eq (buffer-local-value 'major-mode buffer) 'fundamental-mode)
			collect buffer)
"List with buffers that have run `after-change-major-mode-hook' at least once,
e.g. buffers that have set their major mode at least once.

This is used by `define_globalized_minor_mode' to recognize if buffer
wants to give a chance to turn on globalized minor modes in it.

Many temporary buffers, that are shown to users, but are not for normal
editing purposes, don't want to have, for example, hl-line-mode,
display-line-numbers-mode and others turn on in them, and this is inefficient
to have these modes turned on in them forcefully, and then having to turn them off.
Better to have some other system like this to distinguish these buffers.

Technically, all buffers have a major mode (fundamental-mode), but that's
because they are created with major-mode set to fundamental-mode,
not because fundamental-mode function was actually called in them.")

(defun buffer_with_major_mode_list_clear ()
	(setq buffer_with_major_mode_list
		(cl-delete-if-not #'buffer-live-p buffer_with_major_mode_list)))

(add-hook
	'after-change-major-mode-hook
	(fn_symbol "buffer_with_major_mode_list-after-change-major-mode-hook"
		(lambda ()
			(buffer_with_major_mode_list_clear)
			(cl-pushnew
				(current-buffer) buffer_with_major_mode_list :test #'eq))))

(defun define_globalized_minor_mode
	(global_mode mode turn_on_fn &optional body_fn clear_old_mode)
"Much simpler globalized major mode, only hooks to `after-change-major-mode-hook'.
`clear_old_mode' means to clear variables, functions and hooks created by
`define-globalized-minor-mode'."
	(when clear_old_mode
		(when (symbol-value global_mode) (funcall global_mode -1))
		(let (
			(clear_symbol
				(lambda (symbol)
					(kill-local-variable symbol)
					(makunbound symbol)
					(fmakunbound symbol)
					(unintern symbol nil)
					nil))
			(global_mode_name (symbol-name global_mode))
		)
			; MODE-buffers
			(funcall clear_symbol (intern (concat global_mode_name "-buffers")))

			; MODE-major-mode
			(funcall clear_symbol (intern (concat (symbol-name mode) "-major-mode")))

			; MODE-predicate
			(funcall clear_symbol
				(intern
					(concat
						(replace-regexp-in-string "-mode\\'" "" global_mode_name)
						"-modes")))

			; MODE-set-explicitly
			(let ((set_explicitly (intern (concat global_mode_name "-set-explicitly"))))
				; Remove from local minor mode hook.
				(remove-hook (intern (concat (symbol-name mode) "-hook")) set_explicitly)
				(funcall clear_symbol set_explicitly))

			; MODE-cmhh
			(let ((cmhh (intern (concat global_mode_name "-cmhh"))))
				(remove-hook 'change-major-mode-hook cmhh)
				(funcall clear_symbol cmhh))

			; MODE-enable-in-buffers
			(let (
				(enable_in_buffers
					(intern (concat global_mode_name "-enable-in-buffers")))
			)
				(remove-hook 'after-change-major-mode-hook enable_in_buffers)
				(funcall clear_symbol enable_in_buffers))

			; MODE-check-buffers
			(let ((check_buffers (intern (concat global_mode_name "-check-buffers"))))
				(remove-hook 'find-file-hook check_buffers)
				; Remove hook that cmhh adds.
				(remove-hook 'post-command-hook check_buffers)
				(funcall clear_symbol check_buffers))))

	(let (
		(hook
			(fn_symbol
				(concat (symbol-name global_mode) "-after-change-major-mode-hook")
				`(lambda () (unless ,mode (,turn_on_fn)))))
	)
		(eval
			`(define-minor-mode ,global_mode "Nó" :global t
				(if ,global_mode
					(progn
						(add-hook 'after-change-major-mode-hook #',hook)
						(buffer_with_major_mode_list_clear)
						(dolist (buffer buffer_with_major_mode_list)
							(with-current-buffer buffer (unless ,mode (,turn_on_fn)))))
					(remove-hook 'after-change-major-mode-hook #',hook)
					(buffer_with_major_mode_list_clear)
					(dolist (buffer buffer_with_major_mode_list)
						(with-current-buffer buffer (when ,mode (,mode -1)))))
				,(when body_fn (list body_fn)))))
	nil)

; I already once forgot about shadowing of LIST, so use these ___ names.
(defun map_modify_list (fn___ list___)
"Like mapcar, but only for lists and modify original list instead of
creating a new one."
	(let ((list___1 list___))
		(while list___1
			(setcar list___1 (funcall fn___ (car list___1)))
			(setq list___1 (cdr list___1))))
	list___)

(defun map_modify_vector (fn___ vector___ &optional pass_index)
"Like mapcar, but only for vectors and modify original vector instead of
creating a list.
If PASS_INDEX is non-nil, call FN with a second arg - index."
	(if pass_index
		(dotimes (i___ (length vector___))
			(aset vector___ i___ (funcall fn (aref vector___ i___) i___)))
		(dotimes (i___ (length vector___))
			(aset vector___ i___ (funcall fn (aref vector___ i___)))))
	vector___)

(defun get_permutations (list)
"Return list of all permutations of elements of LIST.
Every element of LIST shouldn't be `eq' to any other."
	(if list
		(mapcan
			(lambda (element)
				(mapcar
					(lambda (permutation) (cons element permutation))
					(get_permutations (remq element list))))
			list)
		'(nil)))

; Rewritten from https://www.quickperm.org/01example.php.
; This version is twice as fast as this above.
(defun get_permutations_quick (seq)
"Return list of lists, where every list is a permutation of elements of SEQ.
SEQ is not modified, nor are its elements.
If SEQ is empty, return nil.
Elements of SEQ don't matter at all
(only index comparing is used, no comparing of elements).

8 element sequence takes around 0.038s to calculate,
9 element - around 0.5s,
10 element - around 7.5s."
	(unless (length= seq 0)
		(let* (
			(a (vconcat seq))
			(len (length a))
			(p (make-vector (1+ len) nil))
			(result_list (list (mapcar #'identity a)))
		)
			; Init p.
			(dotimes (i (1+ len)) (aset p i i))
			(let ((i 1)) ; Upper index.
				(while (< i len)
					(aset p i (1- (aref p i))) ; Decrease index "weight" for i by one.
					(let* (
						; Lower index.
						; If i is odd then j = p[i], else j = 0.
						; (% 1 0) throws arith-error.
						; Original: (* (% i (aref p i)) 2).
						(j (if (cl-oddp i) (aref p i) 0))
						(swap (aref a j))
					)
						(aset a j (aref a i))
						(aset a i swap))
					(push (mapcar #'identity a) result_list)
					(setq i 1)
					(while (= (aref p i) 0)
						(aset p i i) ; Reset p[i] zero value.
						(++ i))))
			result_list)))

(defun remove_face_text_property (str face)
"Remove `face' from 'face text properties of `str'. Return `str'."
	(let (
		(maybe_remove_face
			(lambda (start end)
				(when-let ((prop (get-text-property start 'face str)))
					(cond
						((listp prop)
							(and
								; Unless this is a single face spec.
								(not (keywordp (car prop)))
								(memq face prop)
								(if (cdr prop)
									(put-text-property start end 'face (remq face prop) str)
									(remove-text-properties start end '(face) str))))
						((eq face prop)
							(remove-text-properties start end '(face) str))))))
		(start 0)
		end
	)
		(while (setq end (next-single-property-change start 'face str))
			(funcall maybe_remove_face start end)
			(setq start end))
		(funcall maybe_remove_face start (length str)))
	str)

(defun face_range_list_from_str (str face)
"Return list of ranges that has FACE in STR.

FACE can be a plist or a symbol.

All face text properties are supported, e.g.
list of faces (plists or symbols, can be mixed) or just single faces."
	(let (
		(index 0)
		range_list
		range
		(has_face
			(lambda (index)
				(let ((face_list (get-text-property index 'face str)))
					(unless (and (listp face_list) (not (keywordp (car face_list))))
						(setq face_list (list face_list)))
					(memq face face_list))))
	)
		(when (funcall has_face 0) (setq range (list 0)))
		(while (setq index (next-single-property-change index 'face str))
			(if (funcall has_face index)
				; If range has started and we still are in FACE zone,
				; continue searching for its end.
				(unless range
					; We just entered new FACE zone - open new range.
					(setq range (list index)))
				(when range
					; We just reached end of FACE zone.
					(setcdr range index)
					(push range range_list)
					(setq range nil))))
		(when range
			; Close last range.
			(setcdr range (length str))
			(push range range_list))
		(nreverse range_list)))

(defun merge_sort_predicates (&rest predicates)
"Merge sort predicates into one function, so that calling `sort' with
this function will return results equal to calling `sort' many times like this:
(sort (sort (sort list pred3) pred2) pred1).
So PREDICATES should be in order from the most important to the least one.

For example, when you want a list of strings sorted from shortest to longest,
but also, as a less important sorting, when strings have equal length,
sort them alphabetically, you can use
(sort list (merge_sort_predicates #'length<-sort #'string<)),
instead of (sort (sort list #'string<) #'length<-sort).
Option with merged function is around 3-4 times faster.
In \"best\" cases (e.g. sort predicate function seemingly random, like here:
(let (asd) (sort some_list (lambda (_ _) (setq asd (not asd))))))
can be up to 10 times faster.
In wort cases (e.g. (sort some_list (merge_sort_predicates #'string< #'string>)))
can be up to 3 times slower."
	(let* (
		(fn '(lambda (object_1 object_2)))
		(cell (cdr fn))
	)
		(while (cdr predicates)
			(setcdr cell
				(list
					(let ((pred (car predicates)))
						`(or
							(,pred object_1 object_2)
							,(setq cell `(and (not (,pred object_2 object_1))))))))
			(setq cell (cdr cell))
			(setq predicates (cdr predicates)))
		(setcdr cell (list (list (car predicates) 'object_1 'object_2)))
		fn))

(defun add_fn_doc (fn doc) "Add DOC to a lambda FN." (declare (doc-string 2))
	(setcdr (cdr fn) (cons doc (nthcdr 2 fn))))

; Add handling of a 'history-delete-duplicates symbol property to allow overriding
; deleting duplicates, similar to 'history-length, but because there is no way
; to know if symbol has a property with value nil, and history-delete-duplicates
; is boolean, then use 'on and 'off values.
(advice-add 'add-to-history :around
	(lambda (fn history-var newelt &optional maxelt keep-all)
		(let (
			(history-delete-duplicates
				(if-let ((overriding_value (get history-var 'history-delete-duplicates)))
					(or (eq overriding_value 'on) (not (eq overriding_value 'off)))
					history-delete-duplicates))
		)
			(funcall fn history-var newelt maxelt keep-all))))

(defun get_reverse_command (command) `(lambda (n) (interactive "p") (,command (- n))))


(defun get_string_pixel_width (str &optional window)
	(with-work-buffer (insert str) (car (buffer-text-pixel-size nil window t))))

; Important note about this function:
; (get_string_pixel_height (propertize " " 'face ...)) returns the height of char ?\s,
; (get_string_pixel_height (propertize " \n" 'face ...)) returns the height of the entire
; first line.
; The difference is that the default font dictates the minimun height of line,
; so the second example will never be less than that, while the first is only
; concerned about this single char's height, no matter the default face.
; That differs from the normal window-text-pixel-size, where the default
; face always matters, even with no newline present.
(defun get_string_pixel_height (str &optional window)
	(with-work-buffer (insert str) (cdr (buffer-text-pixel-size nil window t))))

(defun pixel_to_canonical_char_width (pixel_width)
"Given PIXEL_WIDTH of some string, return this string's width measured in units
of selected frame's canonical character width."
	(/ (float pixel_width) (default-font-width)))

(defun define_face_with_default_height (new_face_symbol face_plist)
"Define face with height equal to the default font height of current frame.

This makes it easy to define faces using other than default fonts
which have a different height, so they can be seamlessly used without
changing line height.

FACE_PLIST must not include :height, because that will be added here."
	(define_face new_face_symbol
		`((t
			,@face_plist
			:height
			,(/
				(float (default-font-height))
				(get_string_pixel_height (propertize " " 'face face_plist)))))))

; ======================================== HOOKS ========================================

; Implement hooks with better support for mixed local-global depths,
; e.g. ([fn1 global depth = 10] [fn2 local depth = 20] [fn3 global depth = 30])
; will work as expected.
; Don't restrict to -100..100, any integer or float is fine.
; Don't use symbols, lambdas are fine.
; Don't support legacy stuff and remove some checks like boundp.
; Hook's value will be an alist (depth . fn_list).
; When calling hook (through run_hook, run_hook_until_success, etc.),
; global and local values are merged, which causes a problem -
; global functions with the same depth as local ones are called after local ones
; (my arbitrary decision, could be other way around).

(defun add_hook (hook fn &optional depth local)
"Return new value of hook."
	(unless depth (setq depth 0))
	(let (
		(delete_duplicate
			(lambda (previous_alist_i)
				(let ((alist_i (cdr previous_alist_i)))
					(while alist_i
						(let ((cell (car alist_i)))
							(when (memq fn (cdr cell))
								(unless (setcdr cell (delq fn (cdr cell)))
									(setcdr previous_alist_i (cdr alist_i)))
								(setq alist_i nil)))
						(setq previous_alist_i alist_i)
						(setq alist_i (cdr alist_i))))))
		alist
		set_fn
	)
		(if (not local)
			(setq
				alist (default-value hook)
				set_fn #'set-default)
			(setq set_fn #'set)
			; If already has a local value.
			(if (local-variable-p hook)
				(setq alist (symbol-value hook))
				(set (make-local-variable hook) nil)))
		(cond
			((not alist) (setq alist (list (list depth fn))))
			; If depth is the lowest.
			((< depth (car (car alist)))
				; Prepend (list depth fn).
				(setq alist (cons (list depth fn) alist))
				; Delete duplicate.
				(let ((alist_i (cdr alist)) (previous_alist_i alist))
					(while alist_i
						(let ((cell (car alist_i)))
							(when (memq fn (cdr cell))
								(unless (setcdr cell (delq fn (cdr cell)))
									; Delete cell without any function in it.
									(setcdr previous_alist_i (cdr alist_i)))
								(setq alist_i nil)))
						(setq previous_alist_i alist_i)
						(setq alist_i (cdr alist_i)))))
			(t
				(let (
					(add_fn
						; Append or prepend fn (according to add-hook doc).
						(lambda (cell)
							(if (> depth 0)
								(nconc cell (list fn))
								(setcdr cell (cons fn (cdr cell))))))
					(alist_i alist)
					previous_alist_i
					duplicate_handled
					done
				)
					; Handle problematic first cell.
					(let ((cell (car alist)))
						; If fn is in the first cell.
						(when (memq fn (cdr cell))
							(setq duplicate_handled t)
							; Delete fn from cell.
							(setcdr cell (delq fn (cdr cell)))
							(cond
								; If depth is the same, just add fn to it.
								((= depth (car cell))
									(funcall add_fn cell)
									(setq done t))
								; If cell still have at least one function in it.
								((cdr cell)
									; Enter loop below, but first skip its first
									; iteration - executing it would be useless now.
									(setq alist_i (cdr alist))
									(setq previous_alist_i alist))
								; If after cell removal, alist will not be empty.
								((cdr alist)
									; Remove cell.
									(setq alist (cdr alist))
									; Handle problematic new first cell.
									; If depth is the lowest.
									(if (< depth (car (car alist)))
										; Prepend (list depth fn).
										(setq
											alist (cons (list depth fn) alist)
											done t)
										; Update alist_i to new value of alist.
										; Now we can safely enter loop below,
										; as depth won't be < new first cell's depth.
										(setq alist_i alist)))
								(t
									; Alist would be empty, just reuse the old cell.
									(setcar cell depth)
									(setcdr cell (list fn))
									(setq done t)))))
					; If we are not done (fn was not in the first cell or it was,
					; but fn wasn't added to any cell yet).
					(unless done
						; Find cell to insert fn to
						; or insert new cell into alist
						; or append new cell.
						(while alist_i
							(let ((cell (car alist_i)))
								(cond
									((= depth (car cell))
										; Delete duplicate if it hasn't been already.
										(unless duplicate_handled
											(if (memq fn (cdr cell))
												(setcdr cell (delq fn (cdr cell)))
												(funcall delete_duplicate alist_i)))
										(funcall add_fn cell)
										(setq alist_i nil)
										(setq done t))
									((< depth (car cell))
										; Insert new cell before this one.
										(setcdr previous_alist_i (cons (list depth fn) alist_i))
										; Delete duplicate if it hasn't been already.
										(unless duplicate_handled
											(funcall delete_duplicate (cdr previous_alist_i)))
										(setq alist_i nil)
										(setq done t))
									(t
										(when (and (not duplicate_handled) (memq fn (cdr cell)))
											(setq duplicate_handled t)
											(unless (setcdr cell (delq fn (cdr cell)))
												; Delete cell without any function in it.
												(setcdr previous_alist_i (cdr alist_i))
												(setq alist_i previous_alist_i)))
										(setq previous_alist_i alist_i)
										(setq alist_i (cdr alist_i))))))
						; If depth is the highest, append new cell.
						(unless done (setcdr previous_alist_i (list (list depth fn))))))))
		(funcall set_fn hook alist)))

(defun remove_hook (hook fn &optional local)
"Modify global or local value. Return nil.
You can use (kill-local-variable hook) to kill hook's local value."
	; Hooks are set to nil when they first become local, so if caller wants to
	; modify local value but hook doesn't have a local value, do nothing.
	(unless (and local (not (local-variable-p hook)))
		(let (alist set_fn)
			(if local
				(setq alist (symbol-value hook) set_fn #'set)
				(setq alist (default-value hook) set_fn #'set-default))
			; Handle first cell.
			(let ((cell (car alist)))
				(if (memq fn (cdr cell))
					(unless (setcdr cell (delq fn (cdr cell)))
						(funcall set_fn hook (cdr alist)))
					(let ((previous_alist alist))
						(while alist
							(setq cell (car alist))
							(when (memq fn (cdr cell))
								(unless (setcdr cell (delq fn (cdr cell)))
									(setcdr previous_alist (cdr alist)))
								(setq alist nil))
							(setq previous_alist alist)
							(setq alist (cdr alist))))))))
	nil)

(defun merge_hook (hook)
"Return list of functions - result of combination of hook's global and local values."
	(let (
		(global (default-value hook))
		(local (when (local-variable-p hook) (symbol-value hook)))
		merged_value
	)
		(while (or global local)
			(setq merged_value
				(append
					merged_value
					(cond
						((and global local)
							(let ((global_cell (car global)) (local_cell (car local)))
								(cond
									((< (car global_cell) (car local_cell))
										(setq global (cdr global))
										(cdr global_cell))
									((> (car global_cell) (car local_cell))
										(setq local (cdr local))
										(cdr local_cell))
									(t
										(setq global (cdr global))
										(setq local (cdr local))
										; Local functions come first.
										(append (cdr local_cell) (cdr global_cell))))))
						(global
							(prog1
								(cdr (car global))
								(setq global (cdr global))))
						(t
							(prog1
								(cdr (car local))
								(setq local (cdr local))))))))
		merged_value))

(defun run_hook (hook &rest args)
	(dolist (fn (merge_hook hook))
		(apply fn args))
	nil)

(defun run_hook_until_success (hook &rest args)
	(cl-loop
		for fn in (merge_hook hook)
		thereis (apply fn args)))

(defun run_hook_until_failure (hook &rest args)
	(cl-loop
		for fn in (merge_hook hook)
		always (apply fn args)))

(defun run_hook_wrapped (hook wrap_fn &rest args)
	(cl-loop
		for fn in (merge_hook hook)
		thereis (apply wrap_fn fn args)))

(provide 'myUtility)
