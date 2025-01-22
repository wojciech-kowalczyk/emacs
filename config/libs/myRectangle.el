; -*- lexical-binding:nil -*-

; Rectangle mode doesn't work well with non-mono-fonts and with bi-directional text.
; It doesn't work at all with wrapped lines, so it sets truncate-lines in
; current-buffer to t.

; TODO
;
; rectangle::mode should be window-local.
;
; There should be a binding for undo, to not deactivate mark and correctly
; move rectangle. I'm not sure if that's possible without saving information
; in undo entries about the state of rectangle. If that will turn out to be a problem,
; then a at least move rectangle correctly after simple undos deleting or inserting
; text on the same line.
;
; Major performance increase with rectangles larger than window height is possible
; by simply limiting overlay processing in rectangle::highlight_region to
; [window-start..window-end]. It's called as pre-redisplay-function anyway,
; so that's completely safe - these overlays will never be out of date.

(defvar-local rectangle::caret_column nil
"Nil if caret isn't inside a tab or after end of line.
Otherwise rectangle (displayed) caret column.")

(defvar-local rectangle::mark_column nil
"Nil if mark isn't inside a tab or after end of line.
Otherwise rectangle (displayed) mark column.")

(defun rectangle::reset_columns ()
	(kill-local-variable 'rectangle::caret_column)
	(kill-local-variable 'rectangle::mark_column)
	nil)

(defun rectangle::columns ()
"Return (left_column . right_column) of rectangle.
Uses `current_column'."
	(let ((mark_column (or rectangle::mark_column (get_mark_column))))
		(if (> current_column mark_column)
			(cons mark_column current_column)
			(cons current_column mark_column))))

(defun rectangle::move_to_and_set_caret_column (column)
"Move to COLUMN, update and return `rectangle::caret_column'."
	(setq rectangle::caret_column
		(let ((column_after_move (move-to-column column)))
			(when (/= column_after_move column)
				; If move-to-column overshot, move back one char.
				(when (> column_after_move column) (forward-char -1))
				column))))

(defun rectangle::move_to_and_set_mark_column (column)
"Move to column and update `rectangle::mark_column'.
Also set mark to position after move."
	(let ((caret_column rectangle::caret_column))
		(setq rectangle::mark_column (rectangle::move_to_and_set_caret_column column))
		(setq rectangle::caret_column caret_column))
	(set-marker (mark-marker) (point))
	nil)

(defun rectangle::apply (fn)
"Call FN with 2 args: left_column, right_column; on every rectangle line.
FN shouldn't leave caret on a different line."
	(save-excursion
		(let (
			(marked_region (region::marked_region))
			(rectangle_columns (rectangle::columns))
		)
			(goto-char (cdr marked_region))
			(let ((last_line_end_marker (copy-marker (pos-eol))))
				(goto-char (car marked_region))
				(forward-line 0)
				(while
					(progn
						(funcall fn (car rectangle_columns) (cdr rectangle_columns))
						; Only continue if we actually moved to next line, to prevent
						; infinite loops when calling fn will move cursor, so
						; (forward-line 1) would never return 1.
						; So if 'last_line_end_marker' = end-of-buffer, this loop will
						; still call 'fn' on last buffer line just one time,
						; no matter if last buffer line is empty or not.
						(and
							(= (forward-line 1) 0)
							(= (preceding-char) ?\n)
							(<= (point) last_line_end_marker))))
				(set-marker last_line_end_marker nil))))
	nil)

(defun rectangle::apply_command (fn get_new_mark_column_fn get_new_caret_column_fn)
"Like `rectangle::apply' but for commands.
Set mark on it's original line and column that get_new_mark_column_fn return,
then do accordingly with caret.
Call `barf-if-buffer-read-only', set `deactivate-mark' to nil,
set `current_column' to column returned by get_new_caret_column_fn."
	(barf-if-buffer-read-only)
	; Based on rectangle::apply.
	(let (
		(marked_region (cons (point) (mark)))
		(rectangle_columns (rectangle::columns))
		(caret_on_first_line t)
	)
		(when (> (point) (mark))
			(cons_swap marked_region)
			(setq caret_on_first_line nil))
		(goto-char (cdr marked_region))
		; Save end of the last line of the rectangle.
		(let ((last_line_end_marker (copy-marker (pos-eol))))
			(goto-char (car marked_region))
			(forward-line 0)
			; Save beginning of the first line of the rectangle.
			(let ((first_line_start_position (point)))
				(while
					(progn
						(funcall fn (car rectangle_columns) (cdr rectangle_columns))
						(and
							(= (forward-line 1) 0)
							(= (preceding-char) ?\n)
							(<= (point) last_line_end_marker))))
				(goto-char
					(if caret_on_first_line
						last_line_end_marker first_line_start_position))
				(rectangle::move_to_and_set_mark_column
					(funcall get_new_mark_column_fn
						(car rectangle_columns) (cdr rectangle_columns)))
				(goto-char
					(if caret_on_first_line
						first_line_start_position last_line_end_marker)))
			(set-marker last_line_end_marker nil))
		(setq current_column
			(funcall get_new_caret_column_fn
				(car rectangle_columns) (cdr rectangle_columns))))
	(rectangle::move_to_and_set_caret_column current_column)
	(setq deactivate-mark nil)
	(setq now_scrolling nil))


(defun rectangle::paste_handler (line_list &optional n)
"Delete marked region and insert lines in LINE_LIST.
After this function, if `rectangle::mode' is on, the mark is at the upper right
corner and caret at the lower right corner of the new rectangle,
else caret is just left after last inserted line.
If N is nil, `current-prefix-arg' is used."
	(unless n (setq n (abs (prefix-numeric-value current-prefix-arg))))
	(when (/= n 0)
		(let (
			(insert_fn ; First line must already be inserted.
				(lambda (column)
					(let (
						(fn
							(lambda (line_list_1)
								(dolist (line line_list_1)
									(when
										(or
											(/= (forward-line 1) 0)
											(/= (preceding-char) ?\n))
										(insert ?\n))
									(move_to_column_chop_and_extend column)
									(insert line))))
					)
						(funcall fn (cdr line_list))
						(dotimes (_ (1- n)) (funcall fn line_list)))))
		)
			(if rectangle::mode
				(let (left_column)
					(let (
						(marked_region (region::marked_region))
						right_column
						first_line_start
					)
						(let ((rectangle_columns (rectangle::columns)))
							(setq left_column (car rectangle_columns))
							(setq right_column (cdr rectangle_columns)))
						; Reset columns because we know that caret and mark won't be
						; inside tab or after eol after this function.
						(rectangle::reset_columns)
						(goto-char (cdr marked_region))
						(let ((end_marker (copy-marker (pos-eol))))
							(goto-char (car marked_region))
							(forward-line 0)
							; Save this because rectangle::delete_line might delete chars
							; before (car marked_region), so it may not point at the same
							; line anymore.
							(setq first_line_start (point))
							(while
								(progn
									(rectangle::delete_line left_column right_column)
									(and
										(= (forward-line 1) 0)
										(= (preceding-char) ?\n)
										(<= (point) end_marker))))
							(set-marker end_marker nil))
						(goto-char first_line_start))
					(move_to_column_chop_and_extend left_column)
					(jumpHistory::add)
					(insert (car line_list))
					; Move this line one line up if you want to make mark end at the
					; upper left corner of the new rectangle.
					(set-marker (mark-marker) (point))
					(funcall insert_fn left_column)
					(setq deactivate-mark nil))
				(maybe_delete_marked_region)
				(jumpHistory::add)
				(let ((column (current-column)))
					(insert (car line_list))
					(funcall insert_fn column))))
		(funcall after_move_hook_fn)))

(defun rectangle::paste (clipboard_entry) "`current-prefix-arg' can be used."
	; If this clipboard_entry is from rectangle::mode.
	(if (eq (nth 2 clipboard_entry) 'rectangle::paste_handler)
		(rectangle::paste_handler (nth 3 clipboard_entry))
		; Otherwise, if string has just one line, insert it on every rectangle line.
		; Maybe in the future there could be something like this:
		; break string into list of lines and cycle through them inserting
		; them one on each rectangle line.
		; There are some problems though: first line would
		; probably have different indent, and every other line has
		; its unchangeable indent, so this idea would need some changes.
		; For now just message that pasting multi-line strings is not supported.
		(let ((str (clipboard-transform-str (car clipboard_entry))))
			(if (string-search "\n" str)
				(message "Rectangle mode can't paste multi-line strings.")
				; Reuse rectangle::paste_handler.
				(rectangle::paste_handler
					(list str)
					(1+
						(if (> (point) (mark))
							(line_count_backward (mark))
							(line_count_forward (mark)))))))))

(defun rectangle::delete_line (left_column right_column)
	; Unless left column is after the end of line.
	(unless (move_to_column_chop left_column)
		(delete-region (point) (progn (move_to_column_chop right_column) (point)))))

; Display

(defun rectangle::highlight_region (window region_overlay)
	(with-selected-window window
		(let ((caret (point)) (mark (mark)))
			; If region hasn't changed, return cached region.
			; As shown below, first 5 elements of region_overlay list are used to
			; identify if it matches current state.
			(if
				(and
					; region_overlay can be an overlay here.
					(eq 'rectangle (car-safe region_overlay))
					(eq (buffer-chars-modified-tick) (nth 1 region_overlay))
					(eq caret (nth 2 region_overlay))
					(eq mark (nth 3 region_overlay))
					(eq rectangle::caret_column (nth 4 region_overlay))
					(eq rectangle::mark_column (nth 5 region_overlay)))
				region_overlay
				(let (
					(aligned_string
						(lambda (column)
							(propertize " " 'display (list 'space :align-to column))))
					(old_region_overlay
						(if (eq 'rectangle (car-safe region_overlay))
							(nthcdr 6 region_overlay)
							(funcall redisplay-unhighlight-region-function region_overlay)
							nil))
					new_region_overlay
				)
					; Disable highlightParens::auto::mode when caret is inside tab or
					; after end of line.
					(cond
						(rectangle::caret_column
							(when highlightParens::auto::mode
								(highlightParens::auto::mode -1)))
						; Else enable it again, if it was on when rectangle::mode was
						; activated.
						((and
								rectangle::saved_highlight_parens_auto_mode
								(not highlightParens::auto::mode))
							(highlightParens::auto::mode)))
					(rectangle::apply
						(lambda (left_column right_column)
							(let (line_overlay)
								; old_region_overlay list can be nil if rectangle spans
								; more lines than before, or it just didn't exist before
								; this redisplay.
								(if old_region_overlay
									(setq line_overlay (pop old_region_overlay))
									(setq line_overlay (make-overlay 1 1))
									(overlay-put line_overlay 'window window)
									(overlay-put line_overlay 'face 'region))
								(let (
									(column_after_move_to_left_column (move-to-column left_column))
									(position_after_move_to_left_column (point))
									(column_after_move_to_right_column (move-to-column right_column))
									(position_after_move_to_right_column (point))
								)
									(overlay-put line_overlay 'before-string
										; Unless left_column is "normal".
										(when (/= column_after_move_to_left_column left_column)
											; When left_column is in the middle of a tab.
											(when (> column_after_move_to_left_column left_column)
												(-- position_after_move_to_left_column))
											(let ((str (funcall aligned_string left_column)))
												; Add highlightLine::face if highlightLine::mode
												; is on and caret is on this line.
												; If we don't do that here, this spacing
												; appears in default face.
												(and
													highlightLine::mode
													(in_range_inclusive caret (pos-bol) (pos-eol))
													(propertize_no_copy str
														'face 'highlightLine::face))
												str)))
									(overlay-put line_overlay 'after-string
										(cond
											; right_column is "normal".
											((= column_after_move_to_right_column right_column) nil)
											; right_column is past the end of line.
											((< column_after_move_to_right_column right_column)
												(when (/= left_column right_column)
													(let (
														(space_to_right_column_string
															(funcall aligned_string right_column))
													)
														(put-text-property
															0 1 'face 'region space_to_right_column_string)
														; If cursor happens to be here, draw it at the right
														; place.
														(when
															(or
																(and
																	(not rectangle::caret_column)
																	(= caret position_after_move_to_right_column))
																; If caret is after the end of current line and is
																; at left column (or caret is in the middle of the
																; tab, but it doesn't matter here).
																;
																; (= caret position_after_move_to_left_column)
																; is for ensuring that caret is on currently
																; processed line.
																; (eq left_column rectangle::caret_column) is for
																; ensuring that caret is on the left of
																; (or exactly at, but it doesn't matter here)
																; right_column, because if caret is at
																; right_column (and left_column != right_column),
																; then 'cursor property shouldn't be set.
																(and
																	(= caret position_after_move_to_left_column)
																	(eq left_column rectangle::caret_column)))
															(put-text-property
																0 1 'cursor 1 space_to_right_column_string))
														space_to_right_column_string)))
											(t ; right_column is in the middle of a tab.
												(-- position_after_move_to_right_column)
												(when (/= left_column right_column)
													(let (
														(space_to_right_column_string
															(funcall aligned_string right_column))
													)
														(put-text-property
															0 1 'face 'region space_to_right_column_string)
														(when
															(and
																; If columns are inside the same tab or left
																; column is before this tab.
																(=
																	caret
																	position_after_move_to_left_column
																	position_after_move_to_right_column)
																; If caret is before tab that right column is
																; in, or caret is in the middle of a tab in
																; the current line and caret is at left_column.
																(or
																	(not rectangle::caret_column)
																	(= left_column rectangle::caret_column)))
															; For some reason this spacing is the only one that
															; really needs 1 instead of t as 'cursor value...
															(put-text-property
																0 1 'cursor 1 space_to_right_column_string))
														space_to_right_column_string)))))
									(move-overlay
										line_overlay
										position_after_move_to_left_column
										position_after_move_to_right_column
										(current-buffer))
									; Make zero-width rectangles visible by displaying them
									; as a vertical line of virtual cursors.
									; Every cursor has a height of its row, 3 pixel width,
									; 1 pixel before left_column, 2 after.
									; That can be changed of course.
									(when (= left_column right_column)
										(overlay-put line_overlay 'virtual-cursor
											(cons
												'bar
												(face-attribute 'region :background nil t)))))
								(push line_overlay new_region_overlay))))
					; Delete old overlays, if any.
					(mapc #'delete-overlay old_region_overlay)
					(nconc
						(list
							'rectangle
							(buffer-chars-modified-tick)
							caret
							mark
							rectangle::caret_column
							rectangle::mark_column)
						new_region_overlay))))))

; Clean old rectangle-mode hooks. Don't forget to clear this after changing rectangle.el.
(remove-function redisplay-unhighlight-region-function #'rectangle--unhighlight-for-redisplay)
(remove-function redisplay-highlight-region-function #'rectangle--highlight-for-redisplay)
(remove-function region-extract-function #'rectangle--extract-region)
(remove-function region-insert-function #'rectangle--insert-region)

; It's a mess - caret is window-local, mark is buffer-local, region overlay is
; window-local, modes and redisplay-(un)highlight-functions with rectangle::mode on are
; buffer-local.
; So function below is a global override, because (I guess) after switching to a different
; buffer with rectangle::mode off from one with it on in the same window, will call
; default redisplay-unhighlight-region-function to delete overlay that is in rectangle
; format, so it won't know how to delete it, so it will be forever lost undeleted.
;
; Update: I think now that just calling setq-local in rectangle::mode body would work,
; but it would require modifying internal-region-overlay in all windows displaying buffer.
; Ehh, it doesn't matter, when mark and mark overlay will be window local, then it will
; be fixed.
(setq redisplay-unhighlight-region-function
	(lambda (region_overlay)
		(when region_overlay
			(if (overlayp region_overlay)
				(delete-overlay region_overlay)
				(mapc #'delete-overlay (nthcdr 6 region_overlay))
				; I guess it's a weird way of ensuring that next
				; redisplay-highlight-region-function call will redo the overlay.
				(setcar (cdr region_overlay) nil)))
		nil))

; Mode

(defun rectangle::marked_region ()
	(concat
		"Columns: "
		(number-to-string
			(let ((rectangle_columns (rectangle::columns)))
				(- (cdr rectangle_columns) (car rectangle_columns))))
		" Lines: "
		(number-to-string
			(1+
				(if (> (point) (mark))
					(line_count_backward (mark))
					(line_count_forward (mark)))))))

(defun rectangle::post_command () (when deactivate-mark (rectangle::deactivate_mark)))
(defun rectangle::deactivate_mark () (rectangle::mode -1))

(defconst rectangle::keymap (make-keymap))

(defvar rectangle::saved_highlight_parens_auto_mode nil)
(defvar rectangle::saved_truncate_lines nil)
(defvar rectangle::saved_show_trailing_whitespace nil)

(define-minor-mode rectangle::mode
"Toggle the region as rectangular. Sets and activates mark at caret if it's inactive."
	:keymap rectangle::keymap

	(rectangle::reset_columns)
	(if rectangle::mode
		(progn
			(setq-local
				rectangle::saved_highlight_parens_auto_mode highlightParens::auto::mode
				rectangle::saved_truncate_lines truncate-lines
				rectangle::saved_show_trailing_whitespace show-trailing-whitespace)

			(add-hook 'post-command-hook #'rectangle::post_command -99 t)
			(add-hook 'deactivate-mark-hook #'rectangle::deactivate_mark nil t)
			(setq-local redisplay-highlight-region-function #'rectangle::highlight_region)
			(setq truncate-lines t)
			(setq show-trailing-whitespace nil)
			(setq-local clipboard-paste-fn #'rectangle::paste)
			(setq-local mode-line-marked-region-fn #'rectangle::marked_region)
			(setq-local scroll::after_move_hook_fn
				(lambda ()
					; When caret moved, move it back to it's proper column.
					; This sometimes will trigger even if caret hasn't moved,
					; but that's not a problem at all.
					(when now_scrolling
						(rectangle::move_to_and_set_caret_column current_column))
					nil))
			(setq-local after_move_hook_fn
				; See doc of after_move_hook_fn for WEAK arg.
				(lambda (&optional column weak)
					; No need to set goal_column here, it will be set by call to
					; after_move_hook_fn when exiting rectangle mode.
					(setq now_scrolling nil)
					(unless weak
						(setq rectangle::caret_column nil)
						(setq current_column (or column (current-column))))))
			(unless mark-active (set-mark (point))))
		(remove-hook 'post-command-hook #'rectangle::post_command t)
		(remove-hook 'deactivate-mark-hook #'rectangle::deactivate_mark t)
		(kill-local-variable 'redisplay-highlight-region-function)
		; Prevent leaving active mark right at caret.
		(when (= (point) (mark)) (deactivate-mark))
		(and
			rectangle::saved_highlight_parens_auto_mode
			(not highlightParens::auto::mode)
			(highlightParens::auto::mode))
		(setq truncate-lines rectangle::saved_truncate_lines)
		(setq show-trailing-whitespace rectangle::saved_show_trailing_whitespace)
		(kill-local-variable 'rectangle::saved_highlight_parens_auto_mode)
		(kill-local-variable 'rectangle::saved_truncate_lines)
		(kill-local-variable 'rectangle::saved_show_trailing_whitespace)
		(kill-local-variable 'clipboard-paste-fn)
		(kill-local-variable 'mode-line-marked-region-fn)
		(kill-local-variable 'scroll::after_move_hook_fn)
		(kill-local-variable 'after_move_hook_fn)
		(funcall after_move_hook_fn)))

; ======================================== MOUSE ========================================

(defun rectangle::set_mouse_position (position_info) "Return column moved to."
	(mouse::set_line_precise position_info)
	(let ((mouse_column (mouse::column)))
		(rectangle::move_to_and_set_caret_column mouse_column)
		mouse_column))

(defun rectangle::mouse_drag_region ()
"This doesn't set mouse position initially,
because some functions want to set this together with mark."
	(set-transient-map
		(mouse::get_transient_map
			(lambda (position_info)
				; Set this (only needed once) because rectangle
				; seemingly inserts padding up to the mouse position,
				; so 50% times let's say pointer will be 'text, 50% "normal",
				; so make it 100% 'text to not flicker like that.
				(setq void-text-area-pointer 'text)
				(rectangle::set_mouse_position position_info))
			(lambda (scroll_fn _)
				; scroll_fn will set current_column to (current-column).
				(let ((column current_column))
					(funcall scroll_fn)
					(rectangle::move_to_and_set_caret_column column)
					column))
			(lambda (scroll_fn _)
				(funcall scroll_fn)
				(let ((mouse_column (mouse::column)))
					(rectangle::move_to_and_set_caret_column mouse_column)
					mouse_column)))
		t
		`(lambda ()
			(setq
				scroll-margin ',scroll-margin
				auto-hscroll-mode ',auto-hscroll-mode
				track-mouse ',track-mouse
				mouse-fine-grained-tracking ',mouse-fine-grained-tracking
				void-text-area-pointer ',void-text-area-pointer
				now_scrolling nil)))
	; Suppress automatic scrolling near the edges while tracking
	; movement, as it interferes with the natural dragging behavior
	; (point will unexpectedly be moved beneath the pointer, making
	; selections in auto-scrolling margins impossible).
	(setq
		scroll-margin 0
		auto-hscroll-mode nil
		track-mouse 0 ; Make events relate to start frame.
		mouse-fine-grained-tracking t)
	nil)

; Bind repeated events too, to shadow bindings, for example in global-map.
(let (
	(bind
		(lambda (key_modifier symbol_name binding)
			(setq binding (fn_symbol symbol_name binding))
			(dolist (multi_modifier '("" "double-" "triple-"))
				(mouse::bind_in_fringes
					rectangle::keymap
					(vector
						(intern
							(concat
								key_modifier
								multi_modifier
								"down-mouse-1")))
					binding))))
)
	; For these 2 functions below: mark is always active here, so no need to
	; call activate-mark.

	(funcall bind
		""
		"key::rectangle::down_mouse_1"
		(lambda (event) (interactive "e")
			(setq event (nth 1 event))
			(mouse::set_window (posn-window event))
			(jumpHistory::add)
			(mouse::cache_drag)
			(setq current_column (rectangle::set_mouse_position event))
			(setq rectangle::mark_column rectangle::caret_column)
			(set-marker (mark-marker) (point))
			(setq now_scrolling nil)
			(rectangle::mouse_drag_region)))

	(funcall bind
		"S-"
		"key::rectangle::down_mouse_1_shift"
		(lambda (event) (interactive "e")
			(setq event (nth 1 event))
			(mouse::set_window (posn-window event))
			(if (not now_scrolling)
				(jumpHistory::add)
				(set-marker (mark-marker) now_scrolling)
				(setq rectangle::mark_column nil))
			(mouse::cache_drag)
			(setq current_column (rectangle::set_mouse_position event))
			(setq now_scrolling nil)
			(rectangle::mouse_drag_region))))

; Bind to ignore other mouse actions that don't have any appropriate behaviour
; in rectangle mode.
(mapc
	(lambda (key_modifier)
		(or
			(string= key_modifier "")
			(string= key_modifier "S-")
			(dolist (multi_modifier '("" "double-" "triple-"))
				(mouse::bind_in_fringes
					rectangle::keymap
					(vector (intern (concat key_modifier multi_modifier "down-mouse-1")))
					#'ignore))))
	KEY_MODIFIER_VECTOR)

; ====================================== Other keys ======================================

(bind_many_keys global-map [[down-mouse-2] [insert]] #'rectangle::mode)

; Movement

; Special versions of left and right, that can step into tabs and beyond end of lines,
; but cannot move between lines. They just move one column at a time.
(let (
	(left
		(lambda (n) (interactive "p")
			(let ((desired_column (- current_column n)))
				(when (>= desired_column 0)
					(rectangle::move_to_and_set_caret_column desired_column)
					(setq current_column desired_column)))
			(setq now_scrolling nil)))
)
	(bind_many_keys rectangle::keymap [[left] [S-left]]
		(fn_symbol "key::rectangle::left" left))
	(bind_many_keys rectangle::keymap [[right] [S-right]]
		(fn_symbol "key::rectangle::right" (get_reverse_command left))))

(let (
	; Move right rectangle column to the left one, or if they are already the same,
	; then move them n column left.
	(control_shift_left
		(lambda (n) (interactive "p")
			(when (/= n 0)
				(let (
					(mark_column (or rectangle::mark_column (get_mark_column)))
					(move_mark
						(lambda ()
							(save-excursion
								(goto-char (mark))
								(rectangle::move_to_and_set_mark_column current_column))))
					(move_caret
						(lambda ()
							(rectangle::move_to_and_set_caret_column mark_column)
							(setq current_column mark_column)))
				)
					(if (> n 0)
						(progn
							(cond
								; If caret column is the left one.
								((< current_column mark_column)
									(-- n) (funcall move_mark))
								; If caret column is the right one.
								((> current_column mark_column)
									(-- n) (funcall move_caret)))
							; Now caret and mark column are the same.
							(while (and (> n 0) (> current_column 0))
								(-- n)
								(-- current_column)
								(funcall move_mark)
								(rectangle::move_to_and_set_caret_column current_column)))
						(cond
							; If caret column is the left one.
							((< current_column mark_column) (++ n) (funcall move_caret))
							; If caret column is the right one.
							((> current_column mark_column) (++ n) (funcall move_mark)))
						; Now caret and mark column are the same.
						(while (< n 0)
							(++ n)
							(++ current_column)
							(funcall move_mark)
							(rectangle::move_to_and_set_caret_column current_column)))))
			(setq now_scrolling nil)))
)
	(define-key rectangle::keymap [C-S-left]
		(fn_symbol "key::rectangle::left_control_shift" control_shift_left))
	(define-key rectangle::keymap [C-S-right]
		(fn_symbol "key::rectangle::right_control_shift"
			(get_reverse_command control_shift_left))))

; Special versions of up and down, allowing for stepping into tabs and beyond
; ends of lines.
(let (
	(down
		(lambda (n) (interactive "p")
			(forward-line n)
			(rectangle::move_to_and_set_caret_column current_column)
			(setq now_scrolling nil)))
)
	(bind_many_keys rectangle::keymap [[down] [S-down]]
		(fn_symbol "key::rectangle::down" down))
	(bind_many_keys rectangle::keymap [[up] [S-up]]
		(fn_symbol "key::rectangle::up" (get_reverse_command down))))

(let (
	; Move ceiling of the rectangle to it's floor, or if they are already the same,
	; then move them both one line down.
	(control_shift_down
		(lambda (n) (interactive "p")
			(when (/= n 0)
				(let (
					(mark_column (or rectangle::mark_column (get_mark_column)))
					(move_mark
						(lambda ()
							(save-excursion
								(rectangle::move_to_and_set_mark_column mark_column))))
					(move_caret
						(lambda ()
							(goto-char (mark))
							(rectangle::move_to_and_set_caret_column current_column)))
				)
					(if (> n 0)
						(if (> (point) (mark))
							(when (/= (line_count_backward (mark)) 0)
								(-- n)
								(funcall move_mark))
							(when (/= (line_count_forward (mark)) 0)
								(-- n)
								(funcall move_caret)))
						(if (> (point) (mark))
							(when (/= (line_count_backward (mark)) 0)
								(++ n)
								(funcall move_caret))
							(when (/= (line_count_forward (mark)) 0)
								(++ n)
								(funcall move_mark))))
					(when (/= n 0)
						(forward-line n)
						(rectangle::move_to_and_set_mark_column mark_column)
						(rectangle::move_to_and_set_caret_column current_column))))
			(setq now_scrolling nil)))
)
	(define-key rectangle::keymap [C-S-down]
		(fn_symbol "key::rectangle::down_control_shift" control_shift_down))
	(define-key rectangle::keymap [C-S-up]
		(fn_symbol "key::rectangle::up_control_shift"
			(get_reverse_command control_shift_down))))

; Deletion.
(let (
	(backspace
		(lambda (n) (interactive "p")
			(when (/= n 0)
				(barf-if-buffer-read-only)
				(setq this-command 'key::rectangle::backspace)
				(undo-auto-amalgamate)
				(let (
					(delete_rectangle
						(lambda ()
							(rectangle::apply_command
								#'rectangle::delete_line
								#'return_first_arg
								#'return_first_arg)))
				)
					(let ((rectangle_columns (rectangle::columns)))
						(when (/= (car rectangle_columns) (cdr rectangle_columns))
							(if (> n 0) (-- n) (++ n))
							(funcall delete_rectangle)
							(setq current_column (car rectangle_columns))))
					(let ((column (- current_column n)))
						(when (and (/= n 0) (>= column 0))
							(setq current_column column)
							(rectangle::move_to_and_set_caret_column column)
							(funcall delete_rectangle))))
				(setq now_scrolling nil))))
)
	(bind_many_keys rectangle::keymap
		[
			[backspace] [S-backspace]
			[C-backspace] ; C-S-backspace is for line deletion so leave it.
			[A-backspace] [A-S-backspace] [A-C-S-backspace]
		]
		(fn_symbol "key::rectangle::backspace" backspace))
	(bind_many_keys rectangle::keymap
		[
			[delete] [S-delete]
			[C-delete] ; C-S-delete is for line deletion so leave it.
			[A-delete] [A-S-delete] [A-C-S-delete]
		]
		(fn_symbol "key::rectangle::delete" (get_reverse_command backspace))))

; Rectangle version of exchange-point-and-mark.
; If rectangle is in one line or in one column - swap caret and mark,
; else move caret and mark to their corresponding next rectangle vertex (clock-wise).
(define-key rectangle::keymap [?\C-s]
	(fn_symbol "key::rectangle::swap_caret_and_mark"
		(lambda () (interactive)
			(let (
				(mark_column (or rectangle::mark_column (get_mark_column)))
				(rectangle_swap_caret_and_mark
					(lambda ()
						(swap_caret_and_mark)
						(let ((swap rectangle::mark_column))
							(setq rectangle::mark_column rectangle::caret_column)
							(setq rectangle::caret_column swap))
						(setq current_column mark_column)))
				(horizontal_swap
					(lambda ()
						(save-excursion
							(goto-char (mark))
							(rectangle::move_to_and_set_mark_column current_column))
						(rectangle::move_to_and_set_caret_column mark_column)
						(setq current_column mark_column)))
				(vertical_swap
					(lambda ()
						(let ((mark (mark)))
							(rectangle::move_to_and_set_mark_column mark_column)
							(goto-char mark))
						(rectangle::move_to_and_set_caret_column current_column)))
			)
				(funcall
					(cond
						((= current_column mark_column) rectangle_swap_caret_and_mark)
						((> (point) (mark))
							(cond
								; If caret and mark are on the same line.
								((= (line_count_backward (mark)) 0)
									rectangle_swap_caret_and_mark)
								; If caret is the down right corner.
								((> current_column mark_column) horizontal_swap)
								; If caret is the down left corner.
								(t vertical_swap)))
						; If caret and mark are on the same line.
						((= (line_count_forward (mark)) 0) rectangle_swap_caret_and_mark)
						; If caret is the up right corner.
						((> current_column mark_column) vertical_swap)
						; If caret is the up left corner.
						(t horizontal_swap))))
			(setq now_scrolling nil))))

; Move mark to caret, to make zero width and zero height rectangle.
(define-key rectangle::keymap [?\C-\S-s]
	(fn_symbol "key::rectangle::contract"
		(lambda () (interactive)
			(setq rectangle::mark_column rectangle::caret_column)
			(set-marker (mark-marker) (point))
			(setq now_scrolling nil))))

; Inserting chars

; Every char except tab and newline.
; Delete selection and insert char on every line reaching rectangle's left column.
; Reminder: use rectangle after indent function (for now it's rectangle::tab_shift)
; first, to make sure that every line reaches rectangle's left column.
(set-char-table-range (nth 1 rectangle::keymap) (cons ?\s (max-char))
	(fn_symbol "key::rectangle::self_insert_command"
		(lambda (n) (interactive "p")
			(when (> n 0)
				(undo-auto-amalgamate)
				(let ((fully_on_the_right t))
					(rectangle::apply_command
						(lambda (left_column right_column)
							(unless (move_to_column_chop left_column)
								(setq fully_on_the_right nil)
								(delete-region
									(point)
									(progn (move_to_column_chop right_column) (point)))
								; last-command-event is a character that invoked this command.
								(insert-and-inherit last-command-event)))
						(lambda (left_column _)
							(if fully_on_the_right left_column (1+ left_column)))
						(lambda (left_column _)
							(if fully_on_the_right left_column (1+ left_column))))
					(unless fully_on_the_right
						(dotimes (_ (1- n))
							(rectangle::apply_command
								(lambda (left_column _)
									; Use simple move-to-column, because move_to_column_chop
									; only makes a difference when stepping into tab,
									; which won't happen because call to
									; rectangle::apply_command above dealt with any existing
									; tabs and we won't insert new ones because now [tab]
									; has a different binding below.
									(when (= (move-to-column left_column) left_column)
										(insert-and-inherit last-command-event)))
								(lambda (left_column _) (1+ left_column))
								(lambda (left_column _) (1+ left_column))))))))))

; Like above, but column that rectangle will be left on is checked to
; match appropriate tab-stop.
(define-key rectangle::keymap [tab]
	(fn_symbol "key::rectangle::tab"
		(lambda (n) (interactive "p")
			(when (> n 0)
				(setq this-command 'key::rectangle::self_insert_command)
				(undo-auto-amalgamate)
				(let (new_left_column)
					(rectangle::apply_command
						(lambda (left_column right_column)
							(unless (move_to_column_chop left_column)
								(delete-region
									(point)
									(progn (move_to_column_chop right_column) (point)))
								(insert-and-inherit ?\t)
								(unless new_left_column
									(setq new_left_column (current-column)))))
						(lambda (left_column _) (or new_left_column left_column))
						(lambda (left_column _) (or new_left_column left_column)))
					(when new_left_column
						(dotimes (_ (1- n))
							(rectangle::apply_command
								(lambda (left_column _)
									; Here it is also safe because we are aligned with
									; tab-stops.
									(when (= (move-to-column left_column) left_column)
										(insert-and-inherit ?\t)))
								(lambda (left_column _) (+ left_column tab-width))
								(lambda (left_column _) (+ left_column tab-width))))))))))

; Insert tabs and/or spaces on lines ending earlier than left rectangle column,
; to make them reach that column, then move right column to left column.
; It's like indent but after line contents.
; For example, can be used in C for macro spanning muliple lines,
; to indent to specified column and later insert backslash there.
(define-key rectangle::keymap [S-tab]
	(fn_symbol "key::rectangle::tab_shift"
		(lambda () (interactive)
			(rectangle::apply_command
				(lambda (left_column _) (move_to_column_chop_and_extend left_column))
				#'return_first_arg
				#'return_first_arg))))

; Insert newline on all lines, without adding any indentation anywhere.
; End rectangle mode, as it's unclear how rectangle after this action should look.
(bind_many_keys rectangle::keymap
	[[return] [S-return] [C-return] [C-S-return] [A-C-S-return] [A-return] [A-S-return]]
	(fn_symbol "key::rectangle::enter"
		(lambda (n) (interactive "p")
			(when (> n 0)
				(barf-if-buffer-read-only)
				; Based on rectangle::apply.
				(let (
					(marked_region (region::marked_region))
					(rectangle_columns (rectangle::columns))
					crashed_into_eob
				)
					(goto-char (cdr marked_region))
					; Save end of the last line of the rectangle.
					(let ((last_line_end_marker (copy-marker (pos-eol))))
						(goto-char (car marked_region))
						; Save top of the rectangle, because caret will be left at the bottom.
						(jumpHistory::add)
						(while
							(progn
								(rectangle::delete_line
									(car rectangle_columns) (cdr rectangle_columns))
								(insert-char ?\n n t)
								(and
									(setq crashed_into_eob (= (forward-line 1) 0))
									(= (preceding-char) ?\n)
									(<= (point) last_line_end_marker))))
						(set-marker last_line_end_marker nil))
					; Leave caret after the last inserted newline.
					(if crashed_into_eob
						(forward-line 0)
						(forward-char -1)))
				(deactivate-mark))))) ; Deactivate mark and end rectangle mode.

; Duplicate.
(bind_many_keys rectangle::keymap [[?\C-d] [?\C-\S-d]]
	(fn_symbol "key::rectangle::duplicate"
		(lambda (n) (interactive "p")
			(when (/= n 0)
				(barf-if-buffer-read-only)
				(setq n (abs n))
				(undo-auto-amalgamate)
				(let ((max_column_after_insert -1))
					(rectangle::apply_command
						(lambda (left_column right_column)
							(unless (move_to_column_chop left_column)
								(let (
									(str
										(filter-buffer-substring
											(point)
											(progn
												(move_to_column_chop right_column)
												(point))))
								)
									(when (> n 1)
										(let (
											(padding
												(get_space_string
													; Max 0 for safety (for chars
													; with width > 1, like ?\e).
													(max
														0
														(-
															right_column
															left_column
															(string-width str)))))
											(str_1 str)
										)
											(dotimes (_ (1- n))
												(setq str (concat str padding str_1)))))
									(move_to_column_chop_and_extend right_column)
									(insert str)
									(setq max_column_after_insert
										(max max_column_after_insert (current-column))))))
						; Mark the duplicated part.
						#'return_second_arg
						(lambda (left_column right_column)
							(if (= max_column_after_insert -1)
								right_column max_column_after_insert))))))))

; Copy and cut.
(let (
	(make_command
		(lambda (symbol_name get_rectangle_line_list_fn)
			(fn_symbol symbol_name
				`(lambda () (interactive)
					(when-let (
						; Omit 0-width rectangles.
						((not
							(and
								(= (point) (mark))
								(eq rectangle::mark_column rectangle::caret_column))))
						(rectangle_line_list (nreverse (,get_rectangle_line_list_fn)))
					)
						(clipboard-add
							(mapconcat #'identity rectangle_line_list "\n")
							nil
							#'rectangle::paste_handler
							rectangle_line_list))))))
	(get_apply_fn
		(lambda (delete)
			`(lambda (left_column right_column)
				(push
					(let ((buffer (current-buffer)))
						(withTempBuffer
							(insert
								(with-current-buffer buffer
									(filter-buffer-substring
										(point) (goto-char (pos-eol)))))
							,(when delete
								'(with-current-buffer buffer
									(move_to_column_chop left_column)
									(delete-region
										(point)
										(progn
											(move_to_column_chop right_column)
											(point)))))
							(line_tabs_to_spaces)
							; If left column is after the end of line.
							(if (< (move-to-column left_column) left_column)
								(get_space_string (- right_column left_column))
								(let* (
									last_column
									(str
										(buffer-substring
											(point)
											(progn
												(setq last_column
													(move-to-column right_column))
												(point))))
								)
									; If right_column was after the end
									; of line, add spaces.
									(if (< last_column right_column)
										(concat
											str
											(get_space_string
												(- right_column last_column)))
										str)))))
					line_list))))
)
	(bind_many_keys rectangle::keymap [[?\C-c] [?\C-\S-c]]
		(funcall make_command
			"key::rectangle::copy"
			; Return rectangle as a list of strings, one for each line of the rectangle.
			`(lambda ()
				(let (line_list)
					(rectangle::apply #',(funcall get_apply_fn nil))
					line_list))))
	(bind_many_keys rectangle::keymap [[?\C-x] [?\C-\S-x]]
		(funcall make_command
			"key::rectangle::cut"
			`(lambda ()
				(let (line_list)
					(rectangle::apply_command
						#',(funcall get_apply_fn t)
						#'return_first_arg
						#'return_first_arg)
					line_list)))))

; Number lines.
(defvar rectangle::number_lines_format_string_history nil)
(define-key rectangle::keymap [?\C-n]
	(fn_symbol "key::rectangle::number_lines"
		(lambda (n) (interactive "p")
			(let (
				(format_string
					(read-string
						"Format string (default %d): "
						nil
						'rectangle::number_lines_format_string_history
						"%d"))
				(fully_on_the_right t)
			)
				(rectangle::apply_command
					(lambda (left_column right_column)
						(unless (move_to_column_chop left_column)
							(setq fully_on_the_right nil)
							(let ((position_after_move_to_left_column (point)))
								(move_to_column_chop right_column)
								(delete-region position_after_move_to_left_column (point)))
							(insert (format format_string n))
							(++ n)))
					#'return_first_arg
					(lambda (left_column right_column)
						(if fully_on_the_right
							left_column
							(+ left_column (length (number-to-string (1- n)))))))))))

; Make these commands not deactivate mark.
(dolist (
	keys_and_fn
	(list
		(cons [[C-left]] #'key::left_control)
		(cons [[C-right]] #'key::right_control)
		(cons [[A-left] [A-S-left]] #'key::left_alt)
		(cons [[A-right] [A-S-right]] #'key::right_alt)
		(cons [[A-C-left] [A-C-S-left]] #'key::left_alt_control)
		(cons [[A-C-right] [A-C-S-right]] #'key::right_alt_control)
		(cons [[home] [S-home]] #'key::home)
		(cons [[end] [S-end]] #'key::end)
		(cons [[C-home] [C-S-home]] #'key::home_control)
		(cons [[C-end] [C-S-end]] #'key::end_control))
)
	(bind_many_keys rectangle::keymap (car keys_and_fn)
		(fn_symbol
			(concat "key::rectangle" (substring (symbol-name (cdr keys_and_fn)) 3))
			`(lambda () (interactive)
				(call-interactively #',(cdr keys_and_fn))
				(funcall after_move_hook_fn)))))

; These too.
(let (
	(get_binding
		(lambda (symbol_name binding)
			(fn_symbol symbol_name
				`(lambda () (interactive)
					; This can change buffer, so check rectangle::mode.
					(call-interactively #',binding)
					(when rectangle::mode (setq deactivate-mark nil))))))
)
	(define-key rectangle::keymap [C-up]
		(funcall get_binding "key::rectangle::up_control" #'jumpHistory::forward))
	(define-key rectangle::keymap [C-down]
		(funcall get_binding "key::rectangle::down_control" #'jumpHistory::backward))
	(dolist (
		symbol_name_and_keys
		'(
			("key::rectangle::down_mouse_5" down-mouse-5 S-down-mouse-5)
			("key::rectangle::down_mouse_4" down-mouse-4 S-down-mouse-4)
		)
	)
		(map_modify_list #'vector (cdr symbol_name_and_keys))
		(let (
			(binding
				(funcall get_binding
					(car symbol_name_and_keys)
					(lookup-key global-map (nth 1 symbol_name_and_keys))))
		)
			(dolist (key (cdr symbol_name_and_keys))
				(mouse::bind_in_fringes rectangle::keymap key binding)))))

; Commands that move mark, adjusted to work in rectangle mode.
; That is, if rectangle::mark_column would be inside a tab or after end of line, then any
; function that changes its column would need to update rectangle::mark_column to display
; mark correctly.
(dolist (
	key_symbol_name_fn
	'(
		([?\C-w] "key::rectangle::w_control" key::w_control)
		([?\C-\S-w] "key::rectangle::w_control_shift" key::w_control_shift)
	)
)
	(define-key rectangle::keymap (car key_symbol_name_fn)
		(fn_symbol (nth 1 key_symbol_name_fn)
			`(lambda () (interactive)
				(call-interactively #',(nth 2 key_symbol_name_fn))
				(setq rectangle::mark_column nil)
				(funcall after_move_hook_fn)))))

(provide 'myRectangle)
