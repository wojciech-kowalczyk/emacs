; -*- lexical-binding:nil -*-

; ======================================== Saving ========================================

; Auto save on -
; .#file# as lock file, I guess created at the same time as making any change
; in buffer leading to buffer being out of sync with original file, so it marks
; a buffer for auto saving in a way.
; #file# as auto saved file, deleted after every explicit save and updated/created
; after default: 30 seconds of idle time as specified by 'auto-save-timeout',
; or after default: 300 input events as specified by 'auto-save-interval'.
;
; There is another option, namely 'auto-save-visited-mode', but for now I guess
; this can stay. Though lets switch to that when these auto save files become annoying.

(setq auto-save-no-message t)
(setq make-backup-files nil)
(setq save-silently t)
; Ignore -*- first line of files.
; I don't like putting such editor specific settings in files.
(setq enable-local-variables nil)

; save-place

(setq save-place-file (concat INIT_DIR "saved_places.el"))
; This mode requires some weird initialization.
(save-place-mode)
(kill-local-variable 'save-place-mode)
(setq-default save-place-mode t)

; Make save-place honor `save-silently'.
(fset 'save-place-alist-to-file
	`(lambda ()
		(let ((inhibit-message save-silently))
			(,(symbol-function 'save-place-alist-to-file)))))

; Add a hook making buffer read-only if it's file wasn't modified for at least 90 days.

(defconst auto_read_only_time (time-convert (* 90 86400)))

(add-hook 'find-file-hook
	(fn_symbol "find-file-make-read-only-if-old"
		(lambda ()
			; If file is read-only, buffer is made read-only too by `after-find-file'.
			(unless buffer-read-only
				(let (
					(mod_time
						(file-attribute-modification-time
							(file-attributes buffer-file-name)))
				)
					(and
						mod_time
						(time-less-p auto_read_only_time (time-subtract nil mod_time))
						(setq buffer-read-only t)))))))

; Add a hook saving all modified buffers visiting existing files
; when emacs loses focus.

(defun save-file-buffers ()
	(dolist (buffer (buffer-list))
		(when-let (
			(file (buffer-file-name buffer))
			((buffer-modified-p buffer))
			((file-exists-p file))
		)
			(with-current-buffer buffer (basic-save-buffer)))))

(defun save-file-buffers-when-emacs-lost-focus ()
	(unless (find_in_list (frame-list) #'frame-focus-state) (save-file-buffers)))

(add-function :after after-focus-change-function
	#'save-file-buffers-when-emacs-lost-focus)

; Interactive buffer saving

(defun whitespace_cleanup_command ()
	(unless buffer-read-only
		(ignore-error buffer-read-only (whitespace-cleanup))
		; Don't put this inside ignore-error because whitespace-cleanup
		; may find read-only text at the end of the buffer (after point).
		(funcall after_move_hook_fn nil t)))

(defun save_buffer ()
"Maybe save current buffer and perform some other actions.
For now this means that if buffer is visiting a file and that file is modified
or doesn't exist, call `whitespace-cleanup' and `basic-save-buffer' in it."
	(when-let ((file (buffer-file-name)))
		(let ((before-save-hook (cons #'whitespace_cleanup_command before-save-hook)))
			(basic-save-buffer)))
	nil)

(defun save_some_buffers (&optional pred)
"Similar to `save-some-buffers' but avoids asking user.

Return number of buffers saved.

This uses some things from `save-some-buffers', namely:
	`files--buffers-needing-to-be-saved', which honors `buffer-offer-save';
	it's called with `save-some-buffers-default-predicate'.

	`buffer-save-without-query' - if buffer has this non-nil,
	then `basic-save-buffer' is called.

	`save-some-buffers-functions' is called at the end."
	(unless pred (setq pred save-some-buffers-default-predicate))
	(let ((saved_buffer_count___ 0))
		(let (
			(buffers (files--buffers-needing-to-be-saved pred))
			(before-save-hook
				(cons
					(lambdaSymbol ()
						(++ saved_buffer_count___)
						(whitespace_cleanup_command))
					before-save-hook))
		)
			; Do this after scanning for buffers to be saved, in case basic-save-buffer
			; ask some questions and user cancells saving of some buffer, to not ask
			; twice about the same buffer.
			(dolist (buffer (buffer-list))
				(with-current-buffer buffer
					(when buffer-save-without-query
						(basic-save-buffer)
						(setq buffers (delq buffer buffers)))))
			(dolist (buffer buffers) (with-current-buffer buffer (basic-save-buffer))))
		(dolist (fn save-some-buffers-functions) (funcall fn nil t))
		saved_buffer_count___))

; Delete trailing whitespace. With prefix save some buffers.
(define-key global-map [?\C-h]
	(lambda (arg) (interactive "P")
		(cond
			(arg
				(let ((saved_buffer_count (save_some_buffers)))
					(message "%s buffer%s saved."
						(if (= saved_buffer_count 0)
							"No"
							(number-to-string saved_buffer_count))
						(if (= saved_buffer_count 1) "" "s"))))
			((not buffer-read-only)
				(ignore-error buffer-read-only (whitespace-cleanup))
				(setq deactivate-mark nil)
				; Don't put this inside ignore-error because whitespace-cleanup
				; may find read-only text at the end of the buffer (after point).
				(funcall after_move_hook_fn nil t)))))

; ======================================== Frames ========================================

(defun delete_frame (&optional frame)
"`handle-delete-frame' with saving all buffers before exiting emacs (w/o asking user)."
	(interactive)
	(unless frame (setq frame (selected-frame)))
	(if
		(find_in_list (frame-list)
			(lambda (frame_1)
				(not
					(or
						(eq frame_1 frame)
						(frame-parent frame_1)
						(frame-parameter frame_1 'delete-before)))))
		(delete-frame frame t)
		(save_some_buffers)
		(save-buffers-kill-emacs t)))


(define-key global-map [A-f4] #'delete_frame)

(define-key special-event-map [delete-frame]
	(lambda (event) (interactive "e") (delete_frame (posn-window (nth 1 event)))))

; ======================================= Windows =======================================

; Splitting

(setq split-height-threshold 15)
(setq split-width-threshold 35)

(defunWithBase
	(lambda (
		window symbol_that_must_be_in_window_size_fixed window_width_or_height_fn
		split_threshold window_min_width_or_height
	)
		(and
			(window-live-p window)
			(not (window-parameter window 'window-side))
			(with-current-buffer (window-buffer window)
				(and
					(memq
						window-size-fixed
						(list nil symbol_that_must_be_in_window_size_fixed))
					(>=
						(funcall window_width_or_height_fn window)
						(max split_threshold (* 2 window_min_width_or_height)))))))

	(window::is_vertically_splittable (window)
"Window can be split vertically when its width is not fixed and
it's at least twice as wide as window-min-width and wider than split-width-threshold."
		(,base window 'height #'window-width split-width-threshold window-min-width))

	(window::is_horizontally_splittable (window)
"Window can be split horizontally when its height is not fixed and
it's at least twice as high as window-min-height and higher than split-height-threshold."
		(,base window 'width #'window-height split-height-threshold window-min-height)))

(defconst window::PREFERRED_RATIO (/ 16.0 9.0))

(defun window::split_window_sensibly (&optional window)
"Like split-window-sensibly but based on window::PREFERRED_RATIO.
Return new window or nil."
	(unless window (setq window (selected-window)))
	(cond
		; If window is too wide and is vertically splittable.
		((and
				(>
					(/ (float (window-size window t t)) (window-size window nil t))
					window::PREFERRED_RATIO)
				(window::is_vertically_splittable window))
			(split-window window nil 'right))
		((window::is_horizontally_splittable window)
			(split-window window nil 'below))))

(setq split-window-preferred-function #'window::split_window_sensibly)

(define-key global-map [A-kp-left]
	(lambda () (interactive) (select-window (split-window nil nil 'left))))
(define-key global-map [A-kp-right]
	(lambda () (interactive) (select-window (split-window nil nil 'right))))
(define-key global-map [A-kp-up]
	(lambda () (interactive) (select-window (split-window nil nil 'above))))
(define-key global-map [A-kp-down]
	(lambda () (interactive) (select-window (split-window nil nil 'below))))

; Resizing

(defconst WINDOW_SIZE_CHANGE_STEP 2 "In lines or columns.")

(defun enlarge_window_base (n horizontal)
	(withDemotedErrors (enlarge-window (* n WINDOW_SIZE_CHANGE_STEP) horizontal)))

(defun enlarge_window_horizontal_command (arg) (interactive "p")
	(enlarge_window_base arg t))

(defun enlarge_window_horizontal_neg_command (arg) (interactive "p")
	(enlarge_window_horizontal_command (- arg)))

(defun enlarge_window_vertical_command (arg) (interactive "p")
	(enlarge_window_base arg nil))

(defun enlarge_window_vertical_neg_command (arg) (interactive "p")
	(enlarge_window_vertical_command (- arg)))

(define-key global-map [kp-add]			#'enlarge_window_horizontal_command)
(define-key global-map [kp-subtract]	#'enlarge_window_horizontal_neg_command)
(define-key global-map [S-kp-add]		#'enlarge_window_vertical_command)
(define-key global-map [S-kp-subtract]	#'enlarge_window_vertical_neg_command)

(define-key global-map [kp-space] #'fit-window-to-buffer)
(define-key global-map [S-kp-space] #'balance-windows)

; Scrolling

(define-key global-map [C-kp-space] #'recenter-top-bottom)

; Deleting

(define-key global-map [kp-delete]
	(lambda (arg) (interactive "P")
		(if arg
			(delete-other-windows nil t)
			(window--delete))))

; Selecting

(define-key global-map [kp-enter] #'other-window)

; Tabs

(define-key global-map [kp-left]		#'tab-backward-tab)
(define-key global-map [kp-right]		#'tab-forward-tab)
(define-key global-map [kp-home]		#'tab-first-tab)
(define-key global-map [kp-end]			#'tab-last-tab)

(define-key global-map [C-kp-left]		#'tab-move-tab-to-left)
(define-key global-map [C-kp-right]		#'tab-move-tab-to-right)
(define-key global-map [C-kp-home]		#'tab-move-tab-to-start)
(define-key global-map [C-kp-end]		#'tab-move-tab-to-end)

(define-key global-map [kp-insert]		#'tab-kill-buffer)
(define-key global-map [S-kp-insert]	#'tab-kill-other-buffers-in-tabset)
(define-key global-map [C-kp-insert]	#'tab-kill-all-buffers-in-tabset)

; Tabsets

(define-key global-map [S-kp-left]		#'tab-backward-tabset)
(define-key global-map [S-kp-right]		#'tab-forward-tabset)
(define-key global-map [C-S-kp-left]	#'tab-manual-scroll-right)
(define-key global-map [C-S-kp-right]	#'tab-manual-scroll-left)

; Mouse stuff

(define-key global-map [tab-line wheel-up]
	(lambda (event n) (interactive "e\np")
		(tab-manual-scroll-right n (posn-window (nth 1 event)))))
(define-key global-map [tab-line wheel-down]
	(lambda (event n) (interactive "e\np")
		(tab-manual-scroll-left n (posn-window (nth 1 event)))))

(bind_many_keys global-map [[tab-line down-mouse-2] [mode-line down-mouse-2]]
	(lambda (event) (interactive "e") (window--delete (posn-window (nth 1 event)))))

(provide 'myWindows)
