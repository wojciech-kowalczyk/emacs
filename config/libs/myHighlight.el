; -*- lexical-binding:nil -*-

(setq blink-matching-paren nil)
(show-paren-mode -1)

; ================================ Highlight current line ================================

(define_face 'highlightLine::face '((t :extend t :background "#353535")))

(defconst highlightLine::overlay_priority -50)

; I tried to use visual lines, but it looks like shit - :extend face attribute
; doesn't work properly on "virtual newlines", e.g.
; < and > are highlighting bounds, | is a caret:
; lines not wrapped - correct:
; <li|ne1
; >line2
; or
; line1
; <li|ne2
; >
; lines wrapped - wrong:
; <loo|ooooooooooooooooooooooooo>
; oooooooooong line
; or
; looooooooooooooooooooooooooo<
; ooo|ooooooong line
; >
(defun highlightLine::highlight (window)
	(let ((overlay (window-parameter window 'highlightLine::overlay)))
		(if overlay
			(unless
				(and
					(eq (current-buffer) (overlay-buffer overlay))
					; This unfortunately probably isn't good enough guard,
					; because there can be overlays with 'field prop which
					; would affect constrain-to-field...
					; But there is no easy way to check OVERLAY_MODIFF
					; from lisp, so I'll stick with this for now.
					(= (buffer-modified-tick) (overlay-get overlay 'buffer-modified-tick))
					(in_range_inclusive
						(window-point window)
						(overlay-start overlay)
						(1- (overlay-end overlay))))
				(with-selected-window window
					(move-overlay overlay
						(constrain-to-field (pos-bol) (point))
						(constrain-to-field (pos-bol 2) (point))
						(current-buffer)))
				(overlay-put overlay 'buffer-modified-tick (buffer-modified-tick)))
			(setq overlay
				(with-selected-window window
					(make-overlay
						(constrain-to-field (pos-bol) (point))
						(constrain-to-field (pos-bol 2) (point)))))
			(overlay-put overlay 'face 'highlightLine::face)
			(overlay-put overlay 'priority highlightLine::overlay_priority)
			(overlay-put overlay 'window window)
			(overlay-put overlay 'buffer-modified-tick (buffer-modified-tick))
			(set-window-parameter window 'highlightLine::overlay overlay)))
	nil)

(defun highlightLine::unhighlight ()
"Delete highlightLine::overlay on windows displaying current buffer."
	(dolist (window (get-buffer-window-list nil nil t))
		(when-let ((overlay (window-parameter window 'highlightLine::overlay)))
			(delete-overlay overlay))))

(define-minor-mode highlightLine::mode
"Mode providing emotional support for people in south Pakistan.

When this mode is turned on, it doesn't actually highlight the line,
but only updates `pre-redisplay-functions' to do that before the next
redisplay, so if you are calling it from some window change hook like
`window-selection-change-functions', call `highlightLine::highlight'
after turning this mode on, because `pre-redisplay-function' runs before
window change hooks."
	:lighter nil

	(if highlightLine::mode
		(progn
			(setq-local pre-redisplay-functions
				(cons #'highlightLine::highlight pre-redisplay-functions))
			; In case kill-all-local-variables is called, because then
			; highlightLine::overlay is forever lost in active state.
			(add-hook 'change-major-mode-hook #'highlightLine::unhighlight nil t))
		(setq pre-redisplay-functions
			(delq #'highlightLine::highlight pre-redisplay-functions))
		(remove-hook 'change-major-mode-hook #'highlightLine::unhighlight t)
		(highlightLine::unhighlight)))

(define_globalized_minor_mode 'highlightLine::global_mode 'highlightLine::mode
	(lambda () (when (/= (aref (buffer-name) 0) ?\s) (highlightLine::mode))))

(highlightLine::global_mode)

; ========================== Highlight matching paren at caret ==========================

(define_face 'highlightParens::face
	'((t :foreground "#FFEF28" :background "#3B514D" :weight bold)))

(defvar-local highlightParens::auto::opening_overlay nil)
(defvar-local highlightParens::auto::closing_overlay nil)

; Turns out copy-overlay doesn't copy rear-advance and front-advance attributes.
(defun highlightParens::auto::make_overlay ()
	(let ((overlay (make-overlay 1 1 nil t))) ; Non-extendable overlay.
		; -1 priority to let region's background face property override highlighted
		; paren's one. Because of that, now when region covers highlighted paren,
		; then this paren is bold, has highlightParens::face's foreground and region's
		; background, so it is possible to differenciate between highlighted paren
		; covered in marked region and the one not covered.
		(overlay-put overlay 'priority -1)
		(overlay-put overlay 'face 'highlightParens::face)
		overlay))

(defun highlightParens::auto::post_command ()
	(if-let ((paren_at_caret_region_max (region::paren_at_caret_region_max)))
		(let (
			(opening_paren_position (car paren_at_caret_region_max))
			(closing_paren_position (1- (cdr paren_at_caret_region_max)))
			(current_buffer (current-buffer))
			(selected_window (selected-window))
		)
			(overlay-put highlightParens::auto::opening_overlay 'window selected_window)
			(overlay-put highlightParens::auto::closing_overlay 'window selected_window)
			(move-overlay highlightParens::auto::opening_overlay
				opening_paren_position (1+ opening_paren_position) current_buffer)
			(move-overlay highlightParens::auto::closing_overlay
				closing_paren_position (1+ closing_paren_position) current_buffer))
		(highlightParens::auto::unhighlight)))

(defun highlightParens::auto::unhighlight ()
	(delete-overlay highlightParens::auto::opening_overlay)
	(delete-overlay highlightParens::auto::closing_overlay))

(define-minor-mode highlightParens::auto::mode "Queen Ahri is the best" :lighter nil
	(if highlightParens::auto::mode
		(progn
			(unless highlightParens::auto::opening_overlay
				(setq highlightParens::auto::opening_overlay
					(highlightParens::auto::make_overlay))
				(setq highlightParens::auto::closing_overlay
					(highlightParens::auto::make_overlay)))
			(highlightParens::auto::post_command)
			(add-hook 'post-command-hook #'highlightParens::auto::post_command nil t)
			; In case kill-all-local-variables is called.
			(add-hook 'change-major-mode-hook
				#'highlightParens::auto::unhighlight nil t))
		(remove-hook 'post-command-hook #'highlightParens::auto::post_command t)
		(remove-hook 'change-major-mode-hook #'highlightParens::auto::unhighlight t)
		(highlightParens::auto::unhighlight)))

(define_globalized_minor_mode
	'highlightParens::auto::global_mode
	'highlightParens::auto::mode
	(lambda ()
		(when (or (/= ?\s (aref (buffer-name) 0)) (minibufferp))
			(highlightParens::auto::mode))))

(highlightParens::auto::global_mode)

; ========================= Show surroundings of matching paren =========================

; Probably won't work well with even number.
(defconst highlightParens::outOfView::ABOVE_LINE_COUNT 3)
(defconst highlightParens::outOfView::BELOW_LINE_COUNT 3)

(defconst highlightParens::outOfView::BACKGROUND "#323232")

(defvar highlightParens::outOfView::overlay (make-overlay 1 1 nil t))
(overlay-put highlightParens::outOfView::overlay 'display-line-numbers-disable t)
(overlay-put highlightParens::outOfView::overlay 'display "")
(delete-overlay highlightParens::outOfView::overlay)

(defvar highlightParens::outOfView::paren_at_caret_overlay
	(highlightParens::auto::make_overlay))
(delete-overlay highlightParens::outOfView::paren_at_caret_overlay)

(defun highlightParens::outOfView::pre_command ()
	(delete-overlay highlightParens::outOfView::overlay)
	(delete-overlay highlightParens::outOfView::paren_at_caret_overlay)
	(remove-hook 'pre-command-hook #'highlightParens::outOfView::pre_command)
	nil)

(defun highlightParens::outOfView::show ()
"Create overlay showing surroundings of matching paren to paren at caret,
if it exists, if there is enough space, etc.
Add pre-command-hook to delete this overlay after any command.
Also put highlightParens::outOfView::paren_at_caret_overlay on paren at caret.

Unfortunately this doesn't work very well when lines are truncated.
I mean it still works, but line-number-display won't be propertly simulated.
Also this uses logical lines, so they can cover more than
highlightParens::outOfView::ABOVE_LINE_COUNT or
highlightParens::outOfView::BELOW_LINE_COUNT visual lines."
	(interactive)
	(when-let ((paren_at_caret_region_info (region::paren_at_caret_region_info)))
		(let (
			(caret (point))
			(do
				(lambda (
					overlay_start
					overlay_end
					position_before_paren_at_caret
					position_before_paren
					line_count_above
					line_count_below
				)
					(overlay-put highlightParens::outOfView::overlay 'before-string
						(save-excursion
							(let (before_string)
								; Make before_string and put highlightParens::face on paren.
								; before_string will have (line_count_above + line_count_below
								; + 1 (caret line) + 1 (final newline)) lines,
								; appended/prepended if end/start of buffer encountered.
								(goto-char position_before_paren)
								(let (
									(prepended_newline_count
										(- (forward-line (- line_count_above))))
									(str_start (point))
								)
									(goto-char position_before_paren)
									; +1 because of additional final newline.
									(let (
										(appended_newline_count
											(forward-line (1+ line_count_below)))
										(str_end (point))
									)
										; If we are at buffer's end and it doesn't end with
										; a newline and position (calling position of
										; forward-line) wasn't at buffer's end, then add one
										; newline, because of weird behaviour of forward-line.
										(when
											(and
												(/= ?\n (preceding-char))
												(/= position_before_paren (point-max)))
											(++ appended_newline_count))
										(font-lock-ensure str_start str_end) ; Important.
										(setq before_string
											(concat
												(make-string prepended_newline_count ?\n)
												(buffer-substring str_start str_end)
												(make-string appended_newline_count ?\n))))
									(let (
										(index_before_paren
											(+
												prepended_newline_count
												(- position_before_paren str_start)))
									)
										; Put highlight face on closing paren in before_string
										; (overlay is used in buffer, so it's not copied
										; by buffer-substring).
										(put-text-property
											index_before_paren
											(1+ index_before_paren)
											'face
											'highlightParens::face
											before_string)))
								(when display-line-numbers-mode
									(setq
										before_string ; Turn tabs to spaces.
											; Take tab-width from current buffer.
											(let ((tab_width tab-width))
												(withTempBuffer
													(setq tab-width tab_width)
													(insert before_string)
													(goto-char 1)
													(while
														(progn
															(line_tabs_to_spaces)
															(unless (eobp)
																(forward-char 1)
																t)))
													(buffer-string)))
										before_string ; Add line-number-display margin.
											(let* (
												(line_number_display_width (line-number-display-width))
												(line_number_display_width_with_padding
													(+ 2 line_number_display_width))
												(line_number_display_offset (window-hscroll))
												; Absolute first line's number.
												(line_number
													(-
														(line-number-at-pos position_before_paren t)
														line_count_above))
												; Number of last line to add line-number-display to.
												(last_line_number
													(+ line_number line_count_above line_count_below))
												(line_start_index 0)
												line_end_index
												before_string_with_line_numbers
											)
												(while (<= line_number last_line_number)
													(setq line_end_index
														(cl-position ?\n before_string :start line_start_index))
													(setq before_string_with_line_numbers
														(concat
															before_string_with_line_numbers
															(let (
																(line_number_string (number-to-string line_number))
																; Index in before_string of first char
																; of line-number-display on current line.
																(line_number_display_start_index
																	(+ line_start_index line_number_display_offset))
																(get_line_number_margin
																	; Dynamic binding:
																	; line_number_string, line_number_display_width.
																	(lambda (maybe_last_space_string)
																		(propertize_no_copy
																			(concat
																				(get_space_string
																					(max ; Max 0 for safety.
																						0
																						(1+
																							(-
																								line_number_display_width
																								(length line_number_string)))))
																				line_number_string
																				maybe_last_space_string)
																			'face 'line-number)))
															)
																; If current line is shorter than or exactly window-hscroll.
																(if (<= line_end_index line_number_display_start_index)
																	(concat
																		; Full original line.
																		(substring before_string
																			line_start_index line_end_index)
																		; Padding to reach line_number_display_offset.
																		(get_space_string
																			(- line_number_display_start_index line_end_index))
																		; Don't append final space,
																		; to not trigger show-trailing-whitespace.
																		; (I think that show-trailing-whitespace
																		; don't work on display strings, so it
																		; doesn't matter.)
																		(funcall get_line_number_margin nil))
																	(concat
																		(substring before_string
																			line_start_index line_number_display_start_index)
																		(funcall get_line_number_margin " ")
																		(substring before_string
																			line_number_display_start_index line_end_index))))
															; Append newline.
															(substring before_string
																line_end_index (setq line_start_index (1+ line_end_index)))))
													(++ line_number))
												before_string_with_line_numbers)))
								; Append to not override 'line-number and 'highlightParens::face faces.
								; Inherit default face, because otherwise it seems like overlay inherits
								; face from it's position somehow, so for example if it starts on a text
								; with cursive on, then displayed 'before-string also has this cursive on,
								; so prevent that by adding this :inherit.
								(add-face-text-property
									0
									(length before_string)
									(list
										:inherit 'default
										:extend t
										:background highlightParens::outOfView::BACKGROUND)
									t
									before_string)
								before_string)))
					(let ((selected_window (selected-window)))
						(overlay-put highlightParens::outOfView::overlay
							'window selected_window)
						(overlay-put highlightParens::outOfView::paren_at_caret_overlay
							'window selected_window))
					(let ((current_buffer (current-buffer)))
						(move-overlay highlightParens::outOfView::overlay overlay_start
							overlay_end current_buffer)
						(move-overlay highlightParens::outOfView::paren_at_caret_overlay
							position_before_paren_at_caret
							(1+ position_before_paren_at_caret)
							current_buffer))
					(add-hook 'pre-command-hook #'highlightParens::outOfView::pre_command)
					nil))
		)
			; If caret is at opening paren.
			(if (= caret (car (cdr paren_at_caret_region_info)))
				(let ((position_before_closing_paren (1- (cdr (car paren_at_caret_region_info)))))
					(goto_window_last_visible_line_start)
					(forward-line 0)
					(let ((last_visible_line_end (pos-eol)))
						; If closing paren is not or is partially visible.
						(when (> position_before_closing_paren last_visible_line_end)
							(let (
								(overlay_start
									(pos-bol (- 2 highlightParens::outOfView::BELOW_LINE_COUNT)))
							)
								; If there is enough space for overlay.
								(when (< caret overlay_start)
									(let (
										(below_line_count_div_2
											(/ highlightParens::outOfView::BELOW_LINE_COUNT 2))
									)
										; Cover last visible line, one line after that and newline
										; after that, to apply :extend t to the last line,
										; for background one the last line to look nicely.
										(goto-char last_visible_line_end)
										(forward-line 2)
										(funcall do
											overlay_start
											(point)
											; Position before opening paren.
											(car (car paren_at_caret_region_info))
											position_before_closing_paren
											below_line_count_div_2
											(1+ below_line_count_div_2))))))))
				(let (
					(position_before_opening_paren (car (car paren_at_caret_region_info)))
					(first_visible_line_start (window-start))
				)
					; If opening paren is not visible.
					(when (< position_before_opening_paren first_visible_line_start)
						(goto-char first_visible_line_start)
						(forward-line highlightParens::outOfView::ABOVE_LINE_COUNT)
						(let ((overlay_end (point)))
							; If there is enough space for overlay.
							(when (>= caret overlay_end)
								(let (
									(above_line_count_div_2
										(/ highlightParens::outOfView::ABOVE_LINE_COUNT 2))
								)
									(funcall do
										first_visible_line_start
										overlay_end
										; Position before closing paren.
										(1- (cdr (car paren_at_caret_region_info)))
										position_before_opening_paren
										above_line_count_div_2
										above_line_count_div_2)))))))
			(goto-char caret)))
	nil)

(define-key global-map [?\C-\S-g] #'highlightParens::outOfView::show)

(defconst highlightParens::outOfView::IDLE_TIME 1.5
"After this many seconds of emacs' idle time, run `highlightParens::outOfView::show'
unless highlightParens::outOfView overlays are already used,
or unless caret has been moved by scrolling.
Used only by `highlightParens::outOfView::mode'.")

(defvar highlightParens::outOfView::timer nil)

(define-minor-mode highlightParens::outOfView::mode
"Call `highlightParens::outOfView::show' after every
`highlightParens::outOfView::IDLE_TIME' of idle time."
	:global t

	(if highlightParens::outOfView::mode
		(setq highlightParens::outOfView::timer
			(run-with-idle-timer highlightParens::outOfView::IDLE_TIME t
				(lambda ()
					(or
						now_scrolling
						(overlay-buffer highlightParens::outOfView::overlay)
						(highlightParens::outOfView::show)))))
		(cancel-timer highlightParens::outOfView::timer)
		; Yes this can delete overlays of manual call, not ideal.
		(highlightParens::outOfView::pre_command)))

(highlightParens::outOfView::mode)

(provide 'myHighlight)
