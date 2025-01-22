; -*- lexical-binding:nil -*-

; event:
; 0 - window (maybe sometimes frame, maybe sometimes nil)
; 1 - buffer-char-position if on text area, otherwise area symbol
; 2 - x-y-pixel-position
; 3 - time
; 4 -
;	some special thing under mouse: tab-line-format if on tab-line,
;	something similar on mode-line;
;	image, probably some display string, or nil on text area.
; 5 - buffer-char-position or nil
; 6 - col-row-in-chars
; 7 - nil (idk)
; 8 - pixel-position-relative-to-glyph
; 9 - glyph-pixel-size

(setq mouse-highlight 0)

(defun mouse::set_window (window) "WINDOW can be nil."
	(when (framep window) (setq window (frame-selected-window window)))
	; Hmm, it's pretty bad, and useless 99% of time, it's possible to
	; just add binding #'ignore to minibuffer-inactive-map.
	(and
		(window-minibuffer-p window)
		(not (minibuffer-window-active-p window))
		(user-error "Minibuffer window is not active."))
	(run-hooks 'mouse-leave-buffer-hook)
	(select-window window))

; Cache

; For entire drag
(defvar mouse::window)
(defvar mouse::window_body_frame_pixel_top)
(defvar mouse::window_body_frame_pixel_left)
(defvar mouse::frame_char_width)
(defvar mouse::frame_char_height)

; For window scroll
(defvar mouse::window_start)
(defvar mouse::window_last_visible_line_start)
(defvar mouse::window_last_visible_line_end)

; For window hscroll
(defvar mouse::window_hscroll)

(defun mouse::cache_scroll () "Should be called after vertical scroll."
	(setq
		mouse::window_start (window-start)
		mouse::window_last_visible_line_start
			(save-excursion (goto_window_last_visible_line_start) (point))
		mouse::window_last_visible_line_end
			(save-excursion
				(goto-char mouse::window_last_visible_line_start)
				(goto_visual_line_end)
				(point)))
	nil)

(defun mouse::cache_hscroll () "Should be called after hscroll."
	(setq mouse::window_hscroll (window-hscroll))
	nil)

(defun mouse::cache_drag () "Should be called before drag."
	(setq
		mouse::window (selected-window)
		mouse::window_body_frame_pixel_top (get_window_body_frame_pixel_top)
		mouse::window_body_frame_pixel_left (get_window_body_frame_pixel_left)
		mouse::frame_char_width (frame-char-width)
		mouse::frame_char_height (frame-char-height))
	(mouse::cache_scroll)
	(mouse::cache_hscroll)
	nil)

; Some helpers, almost all of them use this cache from above.

(defun mouse::is_below_window ()
"Return t if caret is not (or partially) visible
and will cause window scroll on next redisplay."
	(> (point) mouse::window_last_visible_line_end))

(defun mouse::keep_caret_in_window ()
"If caret is above window, move it to first visible char,
if it's below window, move it to last visible char,
so it won't cause window scrolling."
	(cond
		((< (point) mouse::window_start) (goto-char mouse::window_start))
		((mouse::is_below_window) (goto-char mouse::window_last_visible_line_end)))
	nil)

(defun mouse::move_to_control_move_left_border (control_move_left_fn)
	(unless (eobp)
		(forward-char 1)
		(funcall control_move_left_fn))
	nil)

(defun mouse::move_to_control_move_right_border (control_move_right_fn)
	(unless (bobp)
		(forward-char -1)
		(funcall control_move_right_fn))
	nil)

(defun mouse::forward_line_unless_at_last_visible_line ()
	(let ((caret (point)))
		(vertical-motion 1)
		(when (mouse::is_below_window) (goto-char caret)))
	nil)

; Line relative to selected window's currently first visible line,
; based on mouse pixel y-position.

(defun mouse::set_line ()
"Move to line start in selected window that mouse points to,
or first/last visible line if mouse is above/below window."
	(goto-char mouse::window_start)
	(let (
		(mouse_line ; 0 if mouse is at first line.
			(/
				; Mouse pixel y position relative to window body top.
				(- (nthcdr 2 (mouse-pixel-position)) mouse::window_body_frame_pixel_top)
				mouse::frame_char_height))
	)
		(when (> mouse_line 0)
			(vertical-motion mouse_line)
			(when (mouse::is_below_window)
				(goto-char mouse::window_last_visible_line_start))))
	nil)

(defun mouse::set_line_precise (position_info)
"Like mouse::set_position, but only care about line - leave caret at line start.
Use if only line is needed - it's more efficient than mouse::set_position
and can be more precise than mouse::set_line."
	(let (position)
		(if
			(not
				(and
					(eq mouse::window (posn-window position_info))
					(setq position (nth 5 position_info))))
			(mouse::set_line)
			(goto-char position)
			(vertical-motion 0)))
	nil)

; Absolute column to use in move-to-column and similar functions,
; taking window's hscroll into account.
; Based on mouse pixel x-position.

(defunWithBase
	(lambda (float_to_int_fn)
		(+
			mouse::window_hscroll
			(funcall float_to_int_fn
				(/
					; Mouse pixel x position relative to window body left.
					(max
						; Limit to 0.0 - return mouse::window_hscroll
						; if mouse is on the left of window body.
						0.0
						(float
							(-
								(car (cdr (mouse-pixel-position)))
								mouse::window_body_frame_pixel_left)))
					mouse::frame_char_width))))

	(mouse::column () "Rounded column." (,base #'round))
	(mouse::column_left () (,base #'floor))
	(mouse::column_right () (,base #'ceiling))
	(mouse::column_float () "Raw float column." (,base #'identity)))

(defun mouse::relative_column ()
"Return column relative to window's left border - if mouse points at the first
visible character from the left, it's column 0, regardless of window-hscroll;
if it points at line-number-margin, column is < 0.
Useful for vertical-motion."
	(/
		(float
			(-
				(car (cdr (mouse-pixel-position)))
				mouse::window_body_frame_pixel_left))
		mouse::frame_char_width))

(defun mouse::move_to_column_recognize_tabs ()
	(vertical-motion (cons (mouse::relative_column) 0) nil nil t)
	nil)

(defun mouse::move_to_left_column ()
"If mouse points between chars, then move to the left side of this char,
don't check to which border mouse is closer to, like raw position in mouse-events."
	(vertical-motion (cons (mouse::relative_column) 0))
	nil)

; Position using events, falling back to mouse pixel position.

(defun mouse::set_position (position_info) "Slowest but most precise version."
	(if (eq mouse::window (posn-window position_info))
		(let (
			(position (nth 1 position_info))
			(do
				(lambda () ; Dynamic binding: position.
					(goto-char position)
					(when (mouse::is_below_window)
						(goto-char mouse::window_last_visible_line_start)
						(mouse::move_to_column_recognize_tabs)
						t)))
		)
			(cond
				((integerp position)
					(unless
						(or
							(funcall do)
							(= (point) (save-excursion (goto_visual_line_end) (point))))
						(setq position_info (nthcdr 8 position_info))
						; If mouse is closer to the right border of glyph
						; or exactly in the middle of glyph.
						(when
							(let ((glyph_pixel_width (car (nth 1 position_info))))
								(and
									; If it's 0, then just go to normal position.
									; Not sure why it can even be 0 (might have been
									; a bug in some older emacs version,
									; I haven't checked this for a long time).
									(> glyph_pixel_width 0)
									(>=
										; X coord relative to glyph.
										; Negative in line number margin.
										(car (car position_info))
										(/ glyph_pixel_width 2.0))))
							(forward-char 1)
							(skip_invisible_forward))))
				; Handle fringes and such differently, because glyphs recorded for them
				; are not buffer chars, so buffer position in them should be used
				; unconditionally.
				((setq position (nth 5 position_info)) (funcall do))
				(t
					(mouse::set_line)
					(mouse::move_to_column_recognize_tabs))))
		(mouse::set_line)
		(mouse::move_to_column_recognize_tabs)))

(defun mouse::set_position_left (position_info)
"Like mouse::set_position, but don't check if mouse points closer
to the right border of a glyph, just move to original position
in position_info, or if there is no valid position in position_info,
use mouse::column_left instead of mouse::column to move to column."
	(let (position)
		(if
			(and
				(eq mouse::window (posn-window position_info))
				(setq position (nth 5 position_info)))
			(progn
				(goto-char position)
				(when (mouse::is_below_window)
					(goto-char mouse::window_last_visible_line_start)
					(mouse::move_to_left_column)))
			(mouse::set_line)
			(mouse::move_to_left_column))))

; ==================================== Mouse tracking ====================================

(defvar mouse::vertical_scroll_fn)
(defvar mouse::horizontal_scroll_fn)

; Bind scrolling commands in all areas.
; Double and triple event must be bound too, because this-command-keys-vector
; still contains these modifiers, so lookup-key used by set-transient-map will
; not find these commands in transient map.
(defconst mouse::transient_map
	(let (
		(keymap (make-sparse-keymap))
		(bind
			(lambda (modifier key binding)
				(setq key (concat "wheel-" key))
				(dolist (
					key_area_vector
					'(
						nil ; Text area.
						[nil] ; Window system's title bar.
						[tab-line]
						[header-line]
						[mode-line]
						[vertical-line]
						; Some others: vertical-scroll-bar,
						; horizontal-scroll-bar, tool-bar, menu-bar.
					)
				)
					(dolist (multi_modifier '("" "double-" "triple-"))
						(define-key
							keymap
							(vconcat
								key_area_vector
								(vector (intern (concat modifier multi_modifier key))))
							binding)))))
		(wheel_base
			(lambda (scroll_fn)
				`(lambda (event) (interactive "e")
					(setq current_column
						(funcall mouse::vertical_scroll_fn
							(lambda ()
								(call-interactively #',scroll_fn)
								(mouse::cache_scroll)
								(mouse::set_line))
							(nth 1 event))))))
		(shift_wheel_base
			(lambda (scroll_fn)
				`(lambda (event) (interactive "e")
					(setq current_column
						(funcall mouse::horizontal_scroll_fn
							(lambda ()
								(call-interactively #',scroll_fn)
								(mouse::cache_hscroll))
							(nth 1 event))))))
	)
		(funcall bind "" "up" (funcall wheel_base #'scroll::down))
		(funcall bind "" "down" (funcall wheel_base #'scroll::up))
		(funcall bind "C-" "up" (funcall wheel_base #'scroll::screen_down))
		(funcall bind "C-" "down" (funcall wheel_base #'scroll::screen_up))
		(funcall bind "S-" "up" (funcall shift_wheel_base #'scroll::right))
		(funcall bind "S-" "down" (funcall shift_wheel_base #'scroll::left))
		(funcall bind "C-S-" "up" (funcall shift_wheel_base #'scroll::screen_right))
		(funcall bind "C-S-" "down" (funcall shift_wheel_base #'scroll::screen_left))
		keymap))

(defun mouse::get_transient_map (track_mouse_fn vertical_scroll_fn horizontal_scroll_fn)
"Return map for set-transient-map.
vertical_scroll_fn and horizontal_scroll_fn are called with function that will
do the scrolling and with position_info, they should set caret column
after that scrolling.
track_mouse_fn is called with position_info.
Every function should return current column (so current_column can be set).

This doesn't set mouse position initially,
because some functions want to set this together with mark.

Used by mouse::drag_region_base and rectangle::mouse_drag_region."
	(setq mouse::vertical_scroll_fn vertical_scroll_fn)
	(setq mouse::horizontal_scroll_fn horizontal_scroll_fn)
	(define-key mouse::transient_map [mouse-movement]
		`(lambda (event) (interactive "e")
			(setq current_column (,track_mouse_fn (nth 1 event)))))
	mouse::transient_map)

(defun mouse::drag_region_base (track_mouse_fn vertical_scroll_fn horizontal_scroll_fn)
"See mouse::get_transient_map for description of parameters."
	(set-transient-map
		(mouse::get_transient_map track_mouse_fn vertical_scroll_fn horizontal_scroll_fn)
		t
		`(lambda ()
			(setq
				scroll-margin ',scroll-margin
				auto-hscroll-mode ',auto-hscroll-mode
				track-mouse ',track-mouse)
			(when (= (point) (mark)) (deactivate-mark))
			(funcall after_move_hook_fn current_column)))
	; Suppress automatic scrolling near the edges while tracking movement,
	; as it interferes with the natural dragging behavior
	; (point will unexpectedly be moved beneath the pointer,
	; making selections in auto-scrolling margins impossible).
	(setq
		scroll-margin 0
		auto-hscroll-mode nil
		; Make events relate to start frame.
		; Not sure the fact of it being integer matters, but must be non-nil.
		track-mouse 0)
	nil)


(defun mouse::single_drag_region ()
	(let (
		(scroll
			(lambda (scroll_fn _)
				(funcall scroll_fn)
				(mouse::move_to_column_recognize_tabs)
				(current-column)))
	)
		(mouse::drag_region_base
			(lambda (position_info)
				(mouse::set_position position_info)
				(current-column))
			scroll
			scroll))
	nil)


(defun mouse::control_drag_track
	(position_before_move control_move_left_fn control_move_right_fn)
	(let ((mark (mark)))
		(if (< position_before_move mark)
			(if (< (point) mark)
				(mouse::move_to_control_move_left_border control_move_left_fn)
				(set-marker (mark-marker)
					(save-excursion
						(goto-char mark)
						(funcall control_move_left_fn)
						(point)))
				(funcall control_move_right_fn))
			(if (>= (point) mark)
				(funcall control_move_right_fn)
				(set-marker (mark-marker)
					(save-excursion
						(goto-char mark)
						(funcall control_move_right_fn)
						(point)))
				(mouse::move_to_control_move_left_border control_move_left_fn))))
	; Keep caret inside window - control-move can move caret outside window
	; and that will cause uncontroled scrolling, so prevent that.
	(mouse::keep_caret_in_window)
	(current-column))

(defun mouse::control_drag_region_base (control_move_left_fn control_move_right_fn)
	(let (
		(scroll
			`(lambda (scroll_fn position_info)
				; Go to position that mouse actually points to,
				; because now caret is after control move.
				(mouse::set_position_left position_info)
				(let ((position_before_move (point)))
					(funcall scroll_fn)
					(mouse::move_to_left_column)
					(mouse::control_drag_track
						position_before_move
						#',control_move_left_fn
						#',control_move_right_fn))))
	)
		(mouse::drag_region_base
			`(lambda (position_info)
				(let ((position_before_move (point)))
					(mouse::set_position_left position_info)
					(mouse::control_drag_track
						position_before_move
						#',control_move_left_fn
						#',control_move_right_fn)))
			scroll
			scroll))
	nil)

(defun mouse::double_drag_region ()
	(mouse::control_drag_region_base #'control_move_left #'control_move_right))

(defun mouse::control_drag_region ()
	(mouse::control_drag_region_base
		#'progressive_control_move_left #'progressive_control_move_right))


(defun mouse::line_drag_track (position_before_move)
"Mouse tracking function for line-drag (triple-click drag).
Caret must be at line start, probably just after mouse::set_line_precise call."
	(let ((mark (mark)))
		(if (< position_before_move mark)
			(if (< (point) mark)
				; If mark is on the next line.
				(when (= mark (save-excursion (vertical-motion 1) (point)))
					(set-marker (mark-marker) (point))
					(mouse::forward_line_unless_at_last_visible_line))
				(set-marker (mark-marker)
					(save-excursion
						(goto-char mark)
						(vertical-motion -1)
						(point)))
				(mouse::forward_line_unless_at_last_visible_line))
			(vertical-motion 1)
			(cond
				((<= (point) mark)
					(set-marker (mark-marker)
						(save-excursion
							(goto-char mark)
							(vertical-motion 1)
							(point)))
					(vertical-motion -1))
				((mouse::is_below_window)
					(goto-char mouse::window_last_visible_line_start)))))
	(current-column))

(defun mouse::triple_drag_region ()
	(mouse::drag_region_base
		(lambda (position_info)
			(let ((position_before_move (point)))
				(mouse::set_line_precise position_info)
				(mouse::line_drag_track position_before_move)))
		(lambda (scroll_fn _)
			(let ((position_before_move (point)))
				(funcall scroll_fn)
				(mouse::line_drag_track position_before_move)))
		(lambda (scroll_fn _)
			(let ((position_before_move (point)))
				(funcall scroll_fn)
				; Now line truncation could change, so move mark and caret
				; to possibly new starts of lines.
				; <= instead of < here because it works well with this one case
				; when mark = caret - mouse::forward_line_unless_at_last_visible_line
				; didn't move caret (or maybe also when we are at buffer end
				; or something).
				(vertical-motion (if (<= position_before_move (mark)) 0 -1))
				(set-marker (mark-marker)
					(save-excursion
						(goto-char (mark))
						(vertical-motion 0)
						(point)))
				(mouse::line_drag_track position_before_move)))))

; ========================================= Keys =========================================

(define-key global-map [A-down-mouse-1]
	(lambda (event) (interactive "e")
		(setq now_scrolling nil)
		(mc/add-cursor-on-click event)))

; Single

(defun mouse::down_mouse_command (event)
"Simple command to select window and set position of caret in it,
without starting mouse tracking (dragging).
Suitable for local mouse bindings that first move caret and then call
some function that acts on (point)."
	(interactive "e")
	(setq event (nth 1 event))
	(mouse::set_window (posn-window event))
	(deactivate-mark)
	(jumpHistory::add)
	(mouse::cache_drag)
	(mouse::set_position event)
	(funcall after_move_hook_fn)
	nil)

(defun mouse::bind_in_fringes (keymap key binding)
	(dolist (area '(nil [left-fringe] [right-fringe]))
		(define-key keymap (vconcat area key) binding)))

(mouse::bind_in_fringes global-map [down-mouse-1]
	(lambda (event) (interactive "e")
		(setq event (nth 1 event))
		(mouse::set_window (posn-window event))
		(jumpHistory::add)
		(mouse::cache_drag)
		(mouse::set_position event)
		(set-mark (point))
		(funcall after_move_hook_fn)
		(mouse::single_drag_region)))

(mouse::bind_in_fringes global-map [S-down-mouse-1]
	(lambda (event) (interactive "e")
		(setq event (nth 1 event))
		(mouse::set_window (posn-window event))
		(cond
			(mark-active (jumpHistory::add))
			(now_scrolling (set-mark now_scrolling))
			(t (set-mark (point)) (jumpHistory::add)))
		(mouse::cache_drag)
		(mouse::set_position event)
		(funcall after_move_hook_fn)
		(mouse::single_drag_region)))

; Double/control

(let (
	(mouse_control_move
		(lambda (control_move_left_fn control_move_right_fn drag_region_fn)
			`(lambda (event) (interactive "e")
				(setq event (nth 1 event))
				(mouse::set_window (posn-window event))
				(jumpHistory::add)
				(mouse::cache_drag)
				(mouse::set_position_left event)
				(mouse::move_to_control_move_left_border #',control_move_left_fn)
				(set-mark (point))
				(,control_move_right_fn)
				(mouse::keep_caret_in_window)
				(funcall after_move_hook_fn)
				(,drag_region_fn))))
)

	(mouse::bind_in_fringes global-map [double-down-mouse-1]
		(funcall mouse_control_move
			#'control_move_left
			#'control_move_right
			#'mouse::double_drag_region))
	(mouse::bind_in_fringes global-map [C-down-mouse-1]
		(funcall mouse_control_move
			#'progressive_control_move_left
			#'progressive_control_move_right
			#'mouse::control_drag_region)))

(let (
	(mouse_shift_control_move_setup
		; Caret and mark must already be at meaningful positions.
		(lambda (control_move_left_fn control_move_right_fn)
			(let ((mark (mark)))
				(if (< (point) mark)
					(progn
						(mouse::move_to_control_move_left_border control_move_left_fn)
						(set-marker (mark-marker)
							(save-excursion
								(goto-char mark)
								(mouse::move_to_control_move_right_border
									control_move_right_fn)
								(point))))
					(funcall control_move_right_fn)
					(set-marker (mark-marker)
						(save-excursion
							(goto-char mark)
							(mouse::move_to_control_move_left_border
								control_move_left_fn)
							(point)))))))
)

	(mouse::bind_in_fringes global-map [S-double-down-mouse-1]
		`(lambda (event) (interactive "e")
			(setq event (nth 1 event))
			(mouse::set_window (posn-window event))
			(mouse::cache_drag)
			(mouse::set_position_left event)
			; No need for handling now_scrolling, as it's already handled by single click.
			; Mark can be inactive, because different (or none) modifier keys don't start
			; new cycle of counting mouse clicks, e.g. first mouse click can be without
			; any modifier keys and second can be with shift, so we can't be sure
			; that down_mouse_1 was called before.
			(if mark-active
				(,mouse_shift_control_move_setup #'control_move_left #'control_move_right)
				(mouse::move_to_control_move_left_border #'control_move_left)
				(set-mark (point))
				(control_move_right))
			(mouse::keep_caret_in_window)
			(funcall after_move_hook_fn)
			(mouse::double_drag_region)))

	(mouse::bind_in_fringes global-map [C-S-down-mouse-1]
		`(lambda (event) (interactive "e")
			(setq event (nth 1 event))
			(mouse::set_window (posn-window event))
			(jumpHistory::add)
			(mouse::cache_drag)
			(cond
				(now_scrolling (set-mark now_scrolling))
				((not mark-active) (set-mark (point))))
			(mouse::set_position_left event)
			(,mouse_shift_control_move_setup
				#'progressive_control_move_left #'progressive_control_move_right)
			(mouse::keep_caret_in_window)
			(funcall after_move_hook_fn)
			(mouse::control_drag_region))))

; Triple/line

(let (
	(mouse_line_move
		(lambda (jump_history_add)
			`(lambda (event) (interactive "e")
				(setq event (nth 1 event))
				(mouse::set_window (posn-window event))
				,(when jump_history_add '(jumpHistory::add))
				(mouse::cache_drag)
				(mouse::set_line_precise event)
				(set-mark (point))
				(mouse::forward_line_unless_at_last_visible_line)
				(funcall after_move_hook_fn)
				(mouse::triple_drag_region))))
)
	(mouse::bind_in_fringes global-map [triple-down-mouse-1]
		(funcall mouse_line_move t))
	(mouse::bind_in_fringes global-map [A-C-down-mouse-1]
		(funcall mouse_line_move nil)))

(mouse::bind_in_fringes global-map [S-triple-down-mouse-1]
	(lambda (event) (interactive "e")
		; Mark will always be active here - it's a triple click.
		(setq event (nth 1 event))
		(mouse::set_window (posn-window event))
		(mouse::cache_drag)
		(mouse::set_line_precise event)
		(cond
			((is_caret_on_the_same_line (mark))
				(set-marker (mark-marker) (point))
				(mouse::forward_line_unless_at_last_visible_line))
			((< (point) (mark))
				(set-marker (mark-marker)
					(save-excursion (goto-char (mark)) (vertical-motion 1) (point))))
			(t
				(set-marker (mark-marker)
					(save-excursion (goto-char (mark)) (vertical-motion 0) (point)))
				(mouse::forward_line_unless_at_last_visible_line)))
		(funcall after_move_hook_fn)
		(mouse::triple_drag_region)))

(mouse::bind_in_fringes global-map [A-C-S-down-mouse-1]
	(lambda (event) (interactive "e")
		(setq event (nth 1 event))
		(mouse::set_window (posn-window event))
		(jumpHistory::add)
		(mouse::cache_drag)
		(cond
			(now_scrolling (set-mark now_scrolling))
			((not mark-active) (set-mark (point))))
		(set-marker (mark-marker)
			(save-excursion (goto-char (mark)) (vertical-motion 0) (point)))
		(let ((position_before_move (point)))
			(mouse::set_line_precise event)
			(funcall after_move_hook_fn (mouse::line_drag_track position_before_move)))
		(mouse::triple_drag_region)))

(provide 'myMouse)
