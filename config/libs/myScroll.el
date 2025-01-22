; -*- lexical-binding:nil -*-

; Scroll bar testing.
;(defun asd (event) (interactive "e")
;	(message "%S" event)
;	(let* (
;		(end-position (event-end event))
;		(window (posn-window end-position))
;		(part (nth 4 end-position))
;		before-scroll
;	)
;		(unless (eq part 'end-scroll)
;			(with-current-buffer (window-buffer window)
;				(setq before-scroll point-before-scroll))
;			(save-selected-window
;				(select-window window 'mark-for-redisplay)
;				(setq before-scroll (or before-scroll (point)))
;				(cond
;					((eq part 'above-handle) (scroll-down))
;					((eq part 'below-handle) (scroll-up))
;					((eq part 'ratio)
;						(scroll-up
;							(let* (
;								(portion-whole (nth 2 end-position))
;								(lines (scroll-bar-scale portion-whole (1- (window-total-height))))
;							)
;								(cond
;									((/= lines 0) lines)
;									((< (car portion-whole) 0) -1)
;									(t 1)))))
;					((eq part 'up) (scroll-up -1))
;					((eq part 'down) (scroll-up 1))
;					((eq part 'top) (set-window-start window (point-min)))
;					((eq part 'bottom) (goto-char (point-max)) (recenter))
;					((eq part 'handle)
;						(let* (
;							(start-position (event-start event))
;							(portion-whole (nth 2 start-position))
;						)
;							;; With 'scroll-bar-adjust-thumb-portion' nil and 'portion-whole'
;							;; indicating that the buffer is fully visible, do not scroll the
;							;; window since that might make it impossible to scroll it back
;							;; with GTK's thumb (Bug#32002).
;							(when
;								(or
;									scroll-bar-adjust-thumb-portion
;									(not (numberp (car portion-whole)))
;									(not (numberp (cdr portion-whole)))
;									(/= (car portion-whole) (cdr portion-whole)))
;								(save-excursion
;									; Calculate position relative to the accessible
;									; part of the buffer.
;									(goto-char
;										(+
;											(point-min)
;											(scroll-bar-scale
;												portion-whole (- (point-max) (point-min)))))
;									(vertical-motion 0)
;									(set-window-start window (point))))))))
;			(unless (input-pending-p t) (redisplay))
;			(with-current-buffer (window-buffer window)
;				(setq point-before-scroll before-scroll)))))
;
;(define-key global-map [vertical-scroll-bar mouse-1] #'asd)

(setq scroll-margin 3)
;(setq scroll-step 1)
(setq scroll-conservatively 5)
; I think it's nicer to have some scroll-conservatively, and still use
; scroll-aggressively as fallback, because using only scroll-conservatively
; means jumping far in the buffer shows only scroll-margin above/below cursor,
; which is a little too little.
(setq-default scroll-up-aggressively 0.25)
(setq-default scroll-down-aggressively 0.25)

(setq hscroll-margin 4)
(setq hscroll-step 1)

; next-screen-context-lines - "number of lines of continuity when scrolling by screenfuls"
(setq next-screen-context-lines 4)
(defconst scroll::NEXT_SCREEN_CONTEXT_COLUMN_COUNT 10
"`next-screen-context-lines' but for horizontal scrolling.")

(defconst scroll::LINE_COUNT 4 "Scroll step for vertical scrolling.")
(defconst scroll::COLUMN_COUNT 8 "Scroll step for horizontal scrolling.")

(defun scroll::with_now_scrolling_check (scroll_fn)
	(let ((caret (point)) (return_value (funcall scroll_fn)))
		(unless (or now_scrolling (= (point) caret))
			(save-excursion (goto-char caret) (jumpHistory::add))
			(setq now_scrolling caret))
		return_value))

(defun scroll::up (n) "Scroll up selected window by (* scroll::LINE_COUNT n) lines."
	(interactive "p")
	(scroll::with_now_scrolling_check
		(lambda ()
			(ignore-error (beginning-of-buffer end-of-buffer)
				(scroll-up (* scroll::LINE_COUNT n))))))

(fset 'scroll::down (get_reverse_command #'scroll::up))

(defun scroll::left (n)
"Scroll left selected window by (* scroll::COLUMN_COUNT n) columns.
Return new window-hscroll value."
	(interactive "p")
	(scroll::with_now_scrolling_check
		(lambda () (scroll-left (* scroll::COLUMN_COUNT n)))))

(fset 'scroll::right (get_reverse_command #'scroll::left))

; This is weird, but scroll-left and scroll-right just use window-body-width,
; but scroll-up and scroll-down use some more complicated thing, so just use
; loop here for now.
(defun scroll::screen_up (n)
"Scroll up selected window by (* (- window-height next-screen-context-lines) n)
or something close to that."
	(interactive "p")
	(scroll::with_now_scrolling_check
		(lambda ()
			(ignore-error (beginning-of-buffer end-of-buffer)
				(while (> n 0) (scroll-up) (-- n))
				(while (< n 0) (scroll-down) (++ n))))))

(fset 'scroll::screen_down (get_reverse_command #'scroll::screen_up))

(defun scroll::screen_left (n)
"Scroll left selected window by
(* (- window_last_column scroll::NEXT_SCREEN_CONTEXT_COLUMN_COUNT) n) columns.
Return new window-hscroll value."
	(interactive "p")
	(scroll::with_now_scrolling_check
		(lambda ()
			(scroll-left
				(*
					(- (get_window_last_column) scroll::NEXT_SCREEN_CONTEXT_COLUMN_COUNT)
					n)))))

(fset 'scroll::screen_right (get_reverse_command #'scroll::screen_left))


(defvar scroll::after_move_hook_fn
	(lambda ()
		(when now_scrolling
			(setq goal_column nil)
			(setq current_column (current-column)))
		nil))

(dolist (
	key_and_fn
	'(
		(A-C-up . scroll::down)
		(A-C-down . scroll::up)
		(A-C-S-up . scroll::right)
		(A-C-S-down . scroll::left)
		(prior . scroll::screen_down) ; page up
		(next . scroll::screen_up) ; page down
		(S-prior . scroll::screen_right)
		(S-next . scroll::screen_left)
	)
)
	(define-key global-map (vector (car key_and_fn))
		`(lambda () (interactive)
			(call-interactively #',(cdr key_and_fn))
			(funcall scroll::after_move_hook_fn))))

(dolist (
	key_and_fn
	'(
		(wheel-up . scroll::down)
		(wheel-down . scroll::up)
		(S-wheel-up . scroll::right)
		(S-wheel-down . scroll::left)
		(C-wheel-up . scroll::screen_down)
		(C-wheel-down . scroll::screen_up)
		(C-S-wheel-up . scroll::screen_right)
		(C-S-wheel-down . scroll::screen_left)
	)
)
	(bind_many_keys
		global-map
		(mapcar
			`(lambda (area_vector)
				(vconcat area_vector ,(vector (car key_and_fn))))
			[nil [nil] [header-line] [mode-line]])
		`(lambda (event) (interactive "e")
			(with-selected-window
				(let ((probably_window (posn-window (nth 1 event))))
					(cond
						((windowp probably_window) probably_window)
						((framep probably_window)
							(frame-selected-window probably_window))
						(t (selected-window))))
				(call-interactively #',(cdr key_and_fn))
				(funcall scroll::after_move_hook_fn)))))

(provide 'myScroll)
