; -*- lexical-binding:nil -*-

; Based on myTab.el

; mode-line z minibuforem - jako że minibufor nie ma mode-lina, to ilość zaznaczonych
; charów itp. może być w last active window mode-line albo coś innego.
; W sumie to mało potrzebne, ale może coś innego fajnego wymyślić.

; ======================================== Faces ========================================

(define_face 'mode-line-active
	'((t
		:family "Segoe UI Short"
		:foreground "#d7d9da"
		:background "#3C3F41"
		:height 150
		:box (:line-width (-2 . -2) :color "grey40"))))

(define_face 'mode-line-inactive '((t :inherit mode-line-active :box nil)))

; ================================= Exclusion/inclusion =================================

(defvar mode-line-forced-buffer-hash-table (make-hash-table :test 'eq)
"Key - buffer, value - t to include, nil to exclude.
Buffers with forced exclusion or inclusion.")

(defun mode-line-force-buffer (buffer include)
"Forcefully include or exclude buffer in this mode-line system.
Inclusion should only be done for buffers that are excluded by rules
in `mode-line-should-exclude'."
	(puthash buffer include mode-line-forced-buffer-hash-table))

(defun mode-line-should-exclude (buffer)
"Non-nil if buffer should be excluded from this mode-line system -
it doesn't want its mode-line-format changed.
Rules meant for user customization."
	; Internal or temporary buffers, not meant for normal user interaction.
	(= (string-to-char (buffer-name buffer)) ?\s))

(defun mode-line-should-exclude-window ()
"Non-nil if selected window should be excluded from this mode-line system -
it doesn't want its mode-line-format changed.
Selected window's buffer is current.
Rules meant for user customization.
Only called after buffer already has been included in this mode-line system.
This is handy for excluding windows that are dedicated, or for temporarily
excluding windows that are too small, etc."
	(< (window-body-height) 8))

(defun mode-line-excluded (buffer)
"Non-nil if buffer is excluded from this mode-line system (forcefully or not)."
	(let ((value (gethash buffer mode-line-forced-buffer-hash-table 'none)))
		(or (null value) (and (eq value 'none) (mode-line-should-exclude buffer)))))

; ================================ Core mode-line system ================================

(setq-default mode-line-format nil)

(defvar mode-line-part-vector-length 0)

(defvar-window-local mode-line-part-vector nil
	(make-vector mode-line-part-vector-length nil))

(defvar-local mode-line-part-vector nil
"Vector of mode-line parts - following objects:
	Symbol \\='window, meaning to use the car of corresponding entry in
	window-local variable `mode-line-part-vector'.

	String to use in mode-line. It shouldn't have leading or trailing spaces,
	because they will be added.

	Nil, meaning to not add anything to the mode-line.")

(defvar mode-line-activation-functions nil
"List of functions to run with no args in a buffer being activated by this
mode-line system.

Functions here shouldn't call `force-mode-line-update', it has already
been called.

This has a global value default value, but it can be made buffer-local to
completely override the global value for a specific buffer.

All global mode-line parts are designed to work well buffer-locally too.")

(defun mode-line-format-function ()
"`mode-line-format' function, returns nil or string.
Concatenates all mode-line parts on selected window."
	(unless (mode-line-should-exclude-window)
		(concat
			" "
			(let (
				(window_local_value (mode-line-part-vector))
				(i -1)
			)
				(mapconcat
					(lambda (part)
						(++ i)
						(when-let (
							(str
								(if (eq part 'window)
									(car (aref window_local_value i))
									part))
						)
							(concat str "  ")))
					mode-line-part-vector))
			(format-mode-line global-mode-string))))

(defun mode-line-is-buffer-activated ()
"Non-nil if this mode-line system is activated for current buffer."
	(eq mode-line-format 'mode-line-format-function))

(defun mode-line-is-window-activated ()
"Non-nil if this mode-line system is activated for current buffer and selected window."
	(and (mode-line-is-buffer-activated) (not (mode-line-should-exclude-window))))

(defun mode-line-activate ()
"Activate this mode-line system for current buffer, unless it wants
to be excluded from it.

Calling this on an already activated buffer is safe.

This is the main function adding buffers to this mode-line system.

This is installed as a global `after-change-major-mode-hook' hook,
similar to globalized minor modes.
So if buffer won't call `fundamental-mode' in any way, then it won't be
registered by this system."
	(unless
		(or
			(mode-line-excluded (current-buffer))
			; Guard re-activation, because we don't want to run setup functions twice.
			(mode-line-is-buffer-activated))
		(setq mode-line-format #'mode-line-format-function)
		(setq mode-line-part-vector (make-vector mode-line-part-vector-length nil))
		(force-mode-line-update)
		(mapc #'funcall mode-line-activation-functions))
	nil)

; Initialization

(add-hook 'emacs-startup-hook
	(fn_symbol "mode-line-initialize"
		(lambda ()
			(add-hook 'after-change-major-mode-hook #'mode-line-activate)
			(dolist (buffer (buffer-list))
				(with-current-buffer buffer (mode-line-activate)))
			nil)))

; =================================== Mode-line parts ===================================

; Memory error, for now different, uses global-mode-string.

; Here I use actual mode-line formatting, because I decide when to call
; format-mode-line on it.
(setq global-mode-string '("" (memory-full "Memory full!  ")))

; Buffer identification

(defvar mode-line-buffer-identification-index mode-line-part-vector-length)
(++ mode-line-part-vector-length)

(advice-add 'rename-buffer :filter-return
	(fn_symbol "mode-line-buffer-identification-rename-buffer"
		(lambda (name)
			(let ((buffer (get-buffer name)))
				(or
					(mode-line-excluded buffer)
					(buffer-file-name buffer)
					(with-current-buffer buffer
						(aset mode-line-part-vector mode-line-buffer-identification-index
							(propertize name 'face 'bold))
						(force-mode-line-update))))
			name)))

; mode-line-buffer-identification-watcher is a buffer-local variable, so when
; kill-all-local-variables or kill-buffer are called, remove this watch.
(defun mode-line-buffer-identification-remove-watcher ()
	(file-notify-rm-watch mode-line-buffer-identification-watcher)
	; This function assumes that mode-line-buffer-identification-watcher and
	; local hooks will be cleared by kill-all-local-variables or kill-buffer.
	; Uncomment this if that assumption is incorrect.
;	(kill-local-variable 'mode-line-buffer-identification-remove-watcher)
;	(remove-hook
;		'change-major-mode-hook
;		#'mode-line-buffer-identification-remove-watcher
;		t)
;	(remove-hook
;		'kill-buffer-hook
;		#'mode-line-buffer-identification-remove-watcher
;		t)
	nil)

(defvar-local mode-line-buffer-identification-watcher nil)

(defun mode-line-buffer-identification-watcher (event buffer)
	(let ((action (nth 1 event)) (file (nth 2 event)))
		(when (memq action '(created deleted))
			(setq file
				(propertize (abbreviate-file-name file)
					'face
					(if (eq action 'create)
						'helm-ff-file
						'(helm-ff-nofile helm-ff-file))))
			(helm-files-add-extension-face file)
			(helm-files-add-basename-face file)
			(with-current-buffer buffer
				(cl-assert (equal buffer-file-name file))
				(aset mode-line-part-vector mode-line-buffer-identification-index file)
				(force-mode-line-update))))
	nil)

(defun mode-line-buffer-identification-after-set-visited-file-name ()
	(aset mode-line-part-vector mode-line-buffer-identification-index
		(if buffer-file-name
			(progn
				(if mode-line-buffer-identification-watcher
					(file-notify-rm-watch mode-line-buffer-identification-watcher)
					(add-hook
						'change-major-mode-hook
						#'mode-line-buffer-identification-remove-watcher
						nil
						t)
					(add-hook
						'kill-buffer-hook
						#'mode-line-buffer-identification-remove-watcher
						nil
						t))
				(setq mode-line-buffer-identification-watcher
					(file-notify-add-watch
						buffer-file-name
						'(change)
						`(lambda (event)
							(mode-line-buffer-identification-watcher
								event ,(current-buffer)))))
				(let (
					(file
						(propertize (abbreviate-file-name buffer-file-name)
							'face
							(if (file-exists-p buffer-file-name)
								'helm-ff-file
								'(helm-ff-nofile helm-ff-file))))
				)
					(helm-files-add-extension-face file)
					(helm-files-add-basename-face file)
					file))
			; Buffer now doesn't visit any file.
			; Cleanup mode-line-buffer-identification-watcher.
			(when mode-line-buffer-identification-watcher
				(file-notify-rm-watch mode-line-buffer-identification-watcher)
				(kill-local-variable 'mode-line-buffer-identification-remove-watcher)
				(remove-hook
					'change-major-mode-hook
					#'mode-line-buffer-identification-remove-watcher
					t)
				(remove-hook
					'kill-buffer-hook
					#'mode-line-buffer-identification-remove-watcher
					t))
			(propertize (buffer-name) 'face 'bold)))
	(force-mode-line-update)
	nil)

(push
	(lambda ()
		(aset mode-line-part-vector mode-line-buffer-identification-index
			(if (derived-mode-p 'dired-mode)
				; For now just simple one-time name.
				; Probably should watch for default-directory changes.
				(propertize (abbreviate-file-name default-directory)
					'face 'helm-ff-directory)
				; Watch for variable buffer-file-name changes.
				(add-hook
					'after-set-visited-file-name-hook
					#'mode-line-buffer-identification-after-set-visited-file-name
					nil
					t)
				(if (not buffer-file-name)
					(propertize (buffer-name) 'face 'bold)
					; Watch for buffer-file-name file changes.
					(setq mode-line-buffer-identification-watcher
						(file-notify-add-watch
							buffer-file-name
							'(change)
							`(lambda (event)
								(mode-line-buffer-identification-watcher
									event ,(current-buffer)))))
					; To not leave above watcher forever, remove it if
					; mode-line-buffer-identification-watcher variable would be lost,
					; by kill-all-local-variables or kill-buffer.
					(add-hook
						'change-major-mode-hook
						#'mode-line-buffer-identification-remove-watcher
						nil
						t)
					(add-hook
						'kill-buffer-hook
						#'mode-line-buffer-identification-remove-watcher
						nil
						t)
					(let (
						(file
							(propertize (abbreviate-file-name buffer-file-name)
								'face
								(if (file-exists-p buffer-file-name)
									'helm-ff-file
									'(helm-ff-nofile helm-ff-file))))
					)
						(helm-files-add-extension-face file)
						(helm-files-add-basename-face file)
						file))))
		nil)
	mode-line-activation-functions)

; Read only

(defvar mode-line-read-only-index mode-line-part-vector-length)
(++ mode-line-part-vector-length)

(defconst mode-line-read-only-pressed-str
	(let (
		(pressed_face
			'(:box (:line-width (-2 . -2) :style pressed-button :color "grey90")))
	)
		(propertize
			(all-the-icons-material "lock_outline" :v-adjust -0.12 :face pressed_face)
			'mouse-face pressed_face
			'pointer 'hand)))

(defconst mode-line-read-only-str
	(propertize
		(all-the-icons-material "lock_outline"
			:v-adjust -0.12
			:face '(:box (:line-width (-2 . -2) :style released-button :color "grey60")))
		'mouse-face
			'(:box (:line-width (-2 . -2) :style released-button :color "grey90"))
		'pointer 'hand
		'help-echo "mouse-1, f6: read only off"
		'keymap
			; In this keymap there should be a binding for down-mouse-1 to make
			; :box have :style pressed.
			; Probably do the same as widget packgage does - use read--mouse-event
			; or something. It's not ideal, but it'll be fine for now.
			(let ((keymap (make-sparse-keymap)))
				; Old.
;				(define-key keymap [mode-line mouse-1]
;					(lambda (event) (interactive "e")
;						(with-current-buffer (window-buffer (posn-window (nth 1 event)))
;							(read-only-mode 'toggle))))
				; New.
				(define-key keymap [mode-line down-mouse-1]
					(lambda (event) (interactive "e")
						(with-current-buffer (window-buffer (posn-window (nth 1 event)))
							; Show button as pressed.
							(aset mode-line-part-vector mode-line-read-only-index
								mode-line-read-only-pressed-str)
							(force-mode-line-update)
							(catch 'button-press-cancelled
								(let ((track-mouse t))
									(while
										(not
											(and
												(eventp event)
												(eq (event-basic-type event) mouse-1)))
										(setq event (read-event))
										(when (mouse-movement-p event)
											; TODO This should handle it like a real mouse click,
											; so allow some mouse movement (it's in some variable),
											; and check if this mouse movement hadn't moved us
											; out of box; not just abort on even the slightest
											; mouse movement.
											(push event unread-command-events)
											; Restore the previous button.
											(aset mode-line-part-vector mode-line-read-only-index
												mode-line-read-only-str)
											(force-mode-line-update)
											(throw 'button-press-cancelled nil))))
								; If we are here, then mouse click happened and we should
								; execute the mouse click command.
								; This will show the correct button.
								(read-only-mode -1)))
						nil))
				keymap)))

(defun mode-line-read-only-mode ()
	(aset mode-line-part-vector mode-line-read-only-index
		(when buffer-read-only mode-line-read-only-str))
	(force-mode-line-update)
	nil)

(push
	(lambda () (add-hook 'read-only-mode-hook #'mode-line-read-only-mode nil t) nil)
	mode-line-activation-functions)

; Caret position and column

; This should be (/ (line-number-at-pos) (buffer-line-count)) - only based on
; lines, not on char position.
; That would require some cache for buffer line count.
; Current line number can be obtained from line number margin, but that is available
; only after the redisplay... actually it's available from mode-line-format-function
; call site. But that needs some work, and well working scroll bars will be better
; anyway, so let's focus on scroll bars, not this.

(defvar mode-line-position-index mode-line-part-vector-length)
(++ mode-line-part-vector-length)

(defconst mode-line-position-props
	(cons
		(list
			'display
			(list
				'min-width
				(list
					(pixel_to_canonical_char_width
						(get_string_pixel_width
							(propertize "00%" 'face 'mode-line-active))))))
		(list
			'display
			(list
				'min-width
				(list
					(pixel_to_canonical_char_width
						(get_string_pixel_width
							(propertize "C 00" 'face 'mode-line-active))))))))

(push
	(lambda ()
		(aset mode-line-part-vector mode-line-position-index 'window)
		(setq-local pre-redisplay-functions
			(cons
				(lambda (window)
					(with-selected-window window
						(when (mode-line-is-window-activated)
							(let* (
								(entry
									(or
										(aref (mode-line-part-vector)
											mode-line-position-index)
										(aset (mode-line-part-vector)
											mode-line-position-index
											(cons nil (make-vector 3 0)))))
								(guard (cdr entry))
								(caret (point))
								(buffer_size (buffer-size))
								(column (current-column))
							)
								(unless
									(and
										(= (aref guard 0) caret)
										(= (aref guard 1) buffer_size)
										(= (aref guard 2) column))
									(aset guard 0 caret)
									(aset guard 1 buffer_size)
									(aset guard 2 column)
									(setcar entry
										(let* (
											(position_str
												(concat
													(number-to-string
														(round
															(*
																100
																(/
																	(float caret)
																	(1+ buffer_size)))))
													"%"))
											(column_str
												(concat "C " (number-to-string column)))
											(str (concat position_str column_str))
										)
											(set-text-properties
												0 (length position_str)
												(car mode-line-position-props)
												str)
											(set-text-properties
												(length position_str)
												(+ (length position_str) (length column_str))
												(cdr mode-line-position-props)
												str)
											str))
									(force-mode-line-update)))))
					nil)
				pre-redisplay-functions)))
	mode-line-activation-functions)

; Marked region

(defvar mode-line-marked-region-index mode-line-part-vector-length)
(++ mode-line-part-vector-length)

(push
	(lambda ()
		(aset mode-line-part-vector mode-line-marked-region-index 'window)
		(setq-local pre-redisplay-functions
			(cons
				(lambda (window)
					(with-selected-window window
						(when (mode-line-is-window-activated)
							(let* (
								(entry
									(or
										(aref (mode-line-part-vector)
											mode-line-marked-region-index)
										(aset (mode-line-part-vector)
											mode-line-marked-region-index
											(cons nil nil))))
								(guard (cdr entry))
								(caret (point))
								(mark (mark))
								(is_mark_active
									(and
										mark-active
										(not deactivate-mark)
										; This check is needed only for mouse dragging.
										(/= caret mark)))
								(new_guard (if is_mark_active (cons caret mark) t))
							)
								(unless (equal guard new_guard)
									(setcdr entry new_guard)
									(aset (mode-line-part-vector) mode-line-marked-region-index
										(when is_mark_active
											(concat
												"Chars: "
												(number-to-string (abs (- caret mark)))
												" Lines: "
												(number-to-string
													(1+
														(if (> caret mark)
															(line_count_backward mark)
															(line_count_forward mark)))))))
									(force-mode-line-update)))))
					nil)
				pre-redisplay-functions))
		nil)
	mode-line-activation-functions)

; Prefix arg

(defvar mode-line-prefix-index mode-line-part-vector-length)
(++ mode-line-part-vector-length)

(defun mode-line-prefix-post-command ()
	(when (mode-line-is-window-activated)
		(aset (mode-line-part-vector) mode-line-prefix-index
			(when prefix-arg (cons nil (format "P %S" prefix-arg))))
		(force-mode-line-update))
	nil)

(push
	(lambda ()
		(aset mode-line-part-vector mode-line-prefix-index 'window)
		(add-hook 'post-command-hook #'mode-line-prefix-post-command nil t)
		nil)
	mode-line-activation-functions)


(provide 'myModeLine)
