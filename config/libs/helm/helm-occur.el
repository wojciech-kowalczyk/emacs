; -*- lexical-binding:nil -*-

; Most of this file is copied from helm-grep and some setting variables from there
; are also used here: helm-grep-line-number, helm-grep-save-buffer-name-no-confirm.

; A good addition would be to check buffer size and then decide if should use text props.

(defvar helm-occur-source nil)
(defvar helm-occur-history nil)
(defvar helm-occur-buffer-and-tick-list nil)
(defvar helm-occur-initial-buffer-and-line-number)

(defconst helm-occur-default-use-text-props t
"If non-nil, use text properties.

Using text properties obviously makes things slower.

`helm-occur-use-text-props-mode-alist' overrides it per major mode.")

(defvar helm-occur-use-text-props-mode-alist nil
"Alist (mode . value), where VALUE is non-nil to use text properties, else nil.

It only makes sense to put modes here that want to do the opposite of what
the default is (`helm-occur-default-use-text-props').")

(defconst helm-occur-keep-closest-position t
"When non-nil select closest candidate from point after update.
This happen only in `helm-occur-source' which is always related to
`current-buffer'.")

(defvar helm-occur-always-search-in-current nil
"Always search in current buffer when non-nil.")


(defun helm-occur-get-line-number (cand)
	(string-match "^ +\\([0-9]+\\)" cand)
	(string-to-number (match-string-no-properties 1 cand)))

(defun helm-occur-resume-internal ()
"Update `helm-occur-buffer-and-tick-list', return t if update is necessary."
	(let ((previous_buffer_count (length helm-occur-buffer-and-tick-list)))
		(setq helm-occur-buffer-and-tick-list
			(cl-delete-if
				(lambda (buffer_and_tick)
					(not (buffer-live-p (car buffer_and_tick))))
				helm-occur-buffer-and-tick-list))
		(if helm-occur-buffer-and-tick-list
			(let (wrong_tick)
				; Update ticks and maybe find a wrong one.
				(dolist (buffer_and_tick helm-occur-buffer-and-tick-list)
					(setcdr buffer_and_tick
						(let (
							(tick
								(buffer-chars-modified-tick
									(car buffer_and_tick)))
						)
							(setq wrong_tick (/= (cdr buffer_and_tick) tick))
							tick)))
				; Update if something changed.
				; This obviously could be optimized to only update modified buffers.
				(or
					wrong_tick
					(/=
						previous_buffer_count
						(length helm-occur-buffer-and-tick-list))))
			(message "helm-occur: Cannot resume because all buffers were killed."))))

(defvar helm-occur-trim nil)

(defconst helm-occur-default-keymap
	(let ((keymap (make-sparse-keymap)))
		(set-keymap-parent keymap helm-map)
		(define-key keymap [?\C-b]
			(lambda () (interactive)
				(setq helm-occur-trim (not helm-occur-trim))
				(helm-force-update
					t
					(when-let ((cand (helm-get-selection)))
						(string-match "^ +[0-9]+ " cand)
						(match-string-no-properties 0 cand)))))
		keymap))

(defconst helm-occur-source
	(helm-source-sync-make nil
		:name "Occur"
		:candidates
			(lambda ()
				(let (group_list group line_number_max_length line_number_column_length)
					; Fill group_list with lists with elements (line . line_number_str).
					(dolist (buffer_and_tick helm-occur-buffer-and-tick-list)
						(with-current-buffer (car buffer_and_tick)
							(let ((line_number 1) fn)
								(if
									(if-let (
										(cell
											(find_in_list
												helm-occur-use-text-props-mode-alist
												(lambda (cell) (derived-mode-p (car cell)))))
									)
										(cdr cell)
										helm-occur-default-use-text-props)
									(progn ; Use text props.
										(font-lock-ensure)
										(setq fn #'buffer-substring))
									(setq fn #'buffer-substring-no-properties))
								(save-excursion
									(goto-char (point-min))
									(while
										(let ((line (funcall fn (point) (goto-char (pos-eol)))))
											(unless (string= line "")
												(push
													(cons line (number-to-string line_number))
													group))
											(++ line_number)
											(unless (eobp) (forward-char 1) t))))))
						(when group
							(push group group_list)
							(setq group nil)))
					(when group_list
						; Set line_number_max_length.
						(setq line_number_max_length
							(cl-loop
								for group in group_list
								maximize (length (cdr (car group)))))
						; Space, line number, space.
						(setq line_number_column_length (+ line_number_max_length 2))
						; Nreverse everything to correct order.
						(map_modify_list #'nreverse group_list)
						(setq group_list (nreverse group_list))
						; Transform every candidate to it's final form
						; and add separator lines.
						; Copied from helm-grep-transform-to-display.
						(let (
							(buffer_and_tick_list helm-occur-buffer-and-tick-list)
							(trim helm-occur-trim)
						)
							(with-temp-buffer
								(map_modify_list
									(lambda (group)
										(let ((buffer (car (pop buffer_and_tick_list))))
											(map_modify_list
												(lambda (cand)
													(erase-buffer)
													(insert (car cand))
													(goto-char 1)
													(when trim (skip-chars-forward " \t") (delete-region 1 (point)))
													(line_tabs_to_spaces)
													(propertize_no_copy
														(let ((line_number_str (cdr cand)))
															(concat
																(get_space_string
																	(1+
																		(-
																			line_number_max_length
																			(length line_number_str))))
																(propertize_no_copy line_number_str
																	'face 'helm-grep-line-number)
																" "
																(buffer-string)))
														'match-part
														(list
															(cons
																line_number_column_length
																(+ line_number_column_length (buffer-size))))
														'helm-occur-buffer buffer))
												group)
											(cons
												(propertize_no_copy
													(concat (buffer-name buffer) "\n")
													'face 'helm-source-header
													'helm-noncandidate t)
												group)))
									group_list)))
						group_list)))
		:match-part t
		:sort nil
		:noncandidate 'header
		:grouped 'separators
		:action
			(list
				(cons "Find file" #'helm-occur-jump)
				(cons "Find file other window" #'helm-occur-jump-other-window)
				(cons "Find file other frame" #'helm-occur-jump-other-frame)
				(cons "Save results" #'helm-occur-save-results))
		:persistent-action #'helm-occur-jump
		:keymap
			(let ((keymap (make-sparse-keymap)))
				(set-keymap-parent keymap helm-occur-default-keymap)
				; Goto next/previous buffer.
				(helm-add-goto-bindings
					keymap (lambda () (get-text-property (point) 'helm-occur-buffer)))
				(define-key keymap [?\C-\S-s]
					(helm-make-action-command #'helm-occur-save-results))
				keymap)
		:candidate-number-limit 9999
		:follow t
		:nomark t
		:mode-line
			(lambda ()
				(concat
					; Display current buffer and it's index.
					(when-let ((buffer (get-text-property (point) 'helm-occur-buffer)))
						(concat
							(number-to-string
								(1+
									(cl-position-if
										(lambda (buffer_and_tick)
											(eq buffer (car buffer_and_tick)))
										helm-occur-buffer-and-tick-list)))
							"/"
							(number-to-string (length helm-occur-buffer-and-tick-list))
							" "
							(buffer-name buffer)
							"  "))
					(when helm-occur-trim "trim  ")))
		:resume
			(lambda ()
				(when (eq (helm-occur-resume-internal) t)
					(helm-force-update t)))))

(defun helm-occur--select-closest-candidate ()
"`helm-default-preselect' function for helm-occur."
	(let ((buffer (car helm-occur-initial-buffer-and-line-number)) found)
		(while
			(not
				(or
					(eobp)
					(setq found
						(eq buffer (get-text-property (point) 'helm-occur-buffer)))))
			(forward-line 1))
		(when found
			(let ((start (point)))
				(while
					(progn
						(forward-line 1)
						(eq buffer (get-text-property (point) 'helm-occur-buffer))))
				(helm-goto-prev-candidate)
				(let ((end (pos-eol)) line_number_list)
					(goto-char start)
					(while (re-search-forward "^ +\\([0-9]+\\)" end t)
						(push
							(string-to-number (match-string-no-properties 1))
							line_number_list))
					(when-let (
						; Closest number to initial line number found in line_number_list.
						(closest
							(cl-loop
								with initial_line_number =
									(cdr helm-occur-initial-buffer-and-line-number)
								for i in line_number_list
								for diff = (abs (- initial_line_number i))
								collect (cons diff i) into res
								minimize diff into min
								finally return (cdr (assq min res))))
					)
						(goto-char start)
						; Return non-nil on success.
						(re-search-forward
							(concat "^ +" (number-to-string closest)) end t)))))))

(defun helm-occur (&optional buffer_list input resume)
"Symbol at point is searched at startup.

With no args, search only in current buffer and narrow buffer to region
if mark is active.

BUFFER_LIST must contain only live buffers.
BUFFER_LIST is modified."
	(interactive)
	(save-restriction
		(unless resume
			(if buffer_list
				(progn
					(when helm-occur-always-search-in-current
						(setq buffer_list
							(cons (current-buffer) (delq (current-buffer) buffer_list))))
					(setq helm-occur-buffer-and-tick-list
						(map_modify_list
							(lambda (buffer)
								(cons buffer (buffer-chars-modified-tick buffer)))
							buffer_list)))
				(setq helm-occur-buffer-and-tick-list
					(list (cons (current-buffer) (buffer-chars-modified-tick))))
				(when mark-active (narrow-to-region (region-beginning) (region-end)))))
		(helm
			:sources (list helm-occur-source)
			:history 'helm-occur-history
			:input input
			:default
				(delq
					nil
					(list
						input
						(when-let ((str (thing-at-point 'symbol)))
							(regexp-quote str))))
			:use-default-as-input
				(unless input
					; Don't use initial input if that could cause a substantial lag.
					(cl-loop
						with total_size = 0
						for buffer_and_tick in helm-occur-buffer-and-tick-list
						do (+= total_size (buffer-size (car buffer_and_tick)))
						finally return (<= total_size 2000000)))
			:resume "*helm-occur*"
			:helm-delay t
			:helm-default-keymap helm-occur-default-keymap
			:helm-default-preselect
				(when
					(and
						(not resume)
						helm-occur-keep-closest-position
						(or
							(not buffer_list)
							helm-occur-always-search-in-current))
					(setq helm-occur-initial-buffer-and-line-number
						(cons (current-buffer) (line-number-at-pos)))
					#'helm-occur--select-closest-candidate))))

; Actions

(defun helm-occur-jump (_cand &optional method)
	(let* (
		(cand (helm-get-selection 'withprop))
		(buffer (get-text-property 0 'helm-occur-buffer cand))
	)
		(cl-case method
			(other-window (helm-window-show-buffers (list buffer) t))
			(other-frame (switch-to-buffer-other-frame buffer))
			(t (switch-to-buffer buffer)))
		; Handle use-default-as-input.
		(let ((helm-pattern (or helm-use-default-as-input-pattern helm-pattern)))
			(with-selected-window (get-buffer-window buffer 0)
				; Move to line and mark nearest matching regex from bol.
				(helm-goto-line (helm-occur-get-line-number cand))
				(helm-highlight-current-line t t)))))

(defun helm-occur-jump-other-window (_cand)
"Go to CANDIDATE line in other window.
Same as `helm-occur-jump' but go in other window."
	(helm-occur-jump nil 'other-window))

(defun helm-occur-jump-other-window (_cand)
"Go to CANDIDATE line in new frame.
Same as `helm-occur-jump' but go in new frame."
	(helm-occur-jump nil 'other-frame))

; Mode

(defvar helm-occur-mode-resume-info nil)

(defconst helm-occur-mode-map
	(let ((keymap (make-sparse-keymap)))
		; Jump in selected window.
		(define-key keymap [C-down-mouse-3]
			(lambda (event) (interactive "e")
				(mouse::down_mouse_command event)
				(helm-occur-mode-jump)))
		; Jump in other window w/o selecting it.
		(define-key keymap [down-mouse-3]
			(lambda (event) (interactive "e")
				(mouse::down_mouse_command event)
				(save-selected-window (helm-occur-mode-jump 'other-window))))
		; Jump in selected window.
		(define-key keymap [return] #'helm-occur-mode-jump)
		; Jump in other window and select it.
		(define-key keymap [S-return]
			(lambda () (interactive) (helm-occur-mode-jump 'other-window)))
		; Jump in other window w/o selecting it.
		(define-key keymap [C-up] #'helm-occur-mode-jump-other-window-backward)
		(define-key keymap [C-down] #'helm-occur-mode-jump-other-window-forward)

		; Navigate in helm-occur-buffer.
		(define-key keymap [C-S-down] #'helm-occur-mode-goto-next-buffer)
		(define-key keymap [C-S-up] #'helm-occur-mode-goto-previous-buffer)

		; Resume helm session.
		(define-key keymap [?\C-g]
			(lambda () (interactive)
				(setq helm-occur-buffer-and-tick-list (cdr helm-occur-mode-resume-info))
				(when (memq (helm-occur-resume-internal) '(t nil))
					(helm-occur nil (car helm-occur-mode-resume-info) t))))
		keymap))

(define-derived-mode helm-occur-mode fundamental-mode "helm-occur-mode" "moden")

(defun helm-occur-mode-jump-base (buffer method)
	(let (
		(helm-pattern (car helm-occur-mode-resume-info))
		(helm-current-source helm-occur-source)
		(helm-buffer "*helm-occur*")
		(line_number
			(helm-occur-get-line-number
				(buffer-substring-no-properties (pos-bol) (pos-eol))))
	)
		(cl-case method
			(other-window (helm-window-show-buffers (list buffer) t))
			(other-frame (switch-to-buffer-other-frame buffer))
			(t (switch-to-buffer buffer)))
		(with-selected-window (get-buffer-window buffer 0)
			(helm-goto-line line_number)
			(helm-highlight-current-line t t))))

(defun helm-occur-mode-jump (&optional method) (interactive)
	(when-let ((buffer (get-text-property (pos-bol) 'helm-occur-buffer)))
		(helm-occur-mode-jump-base buffer method)))

(defun helm-occur-mode-jump-other-window-base (arg)
	(let ((start (point)))
		(when
			(memq last-command
				'(
					helm-occur-mode-jump-other-window-forward
					helm-occur-mode-jump-other-window-backward
				))
			(forward-line arg))
		(if-let ((buffer (get-text-property (pos-bol) 'helm-occur-buffer)))
			(save-selected-window (helm-occur-mode-jump-base buffer 'other-window))
			(goto-char start))))

(defun helm-occur-mode-jump-other-window-forward (arg) (interactive "p")
	(helm-occur-mode-jump-other-window-base arg))
(defun helm-occur-mode-jump-other-window-backward (arg) (interactive "p")
	(helm-occur-mode-jump-other-window-base (- arg)))

(defun helm-occur-mode-goto-buffer-base (n)
	(when (/= n 0)
		(let* (
			(get_buffer (lambda () (get-text-property (pos-bol) 'helm-occur-buffer)))
			(buffer (funcall get_buffer))
			(end_fn (if (> n 0) #'eobp #'bobp))
		)
			(when buffer
				(while
					(and
						(not (funcall end_fn))
						(progn
							(forward-line n)
							(let ((this_buffer (funcall get_buffer)))
								; Until found different buffer.
								(or
									(not this_buffer)
									(eq buffer this_buffer))))))
				; Go back to something if we ended up at start/end of buffer.
				(cond
					((and (> n 0) (eobp))
						(re-search-backward ".")
						(goto-char (pos-bol)))
					; Give some reasonable limit.
					((and (< n 0) (bobp) (re-search-forward "^ +[0-9]+" 1000 t))
						(goto-char (pos-bol))))))))

(defun helm-occur-mode-goto-next-buffer (n) (interactive "p")
	(helm-occur-mode-goto-buffer-base n))
(defun helm-occur-mode-goto-previous-buffer (n) (interactive "p")
	(helm-occur-mode-goto-buffer-base (- n)))

(defun helm-occur-save-results (_cand)
	; Save occur result in a `helm-occur-mode' buffer.
	(let ((buffer "*helm-occur-result*"))
		(when (get-buffer buffer)
			(setq buffer
				(if helm-grep-save-buffer-name-no-confirm
					(concat
						"*helm-occur-|"
						helm-pattern
						"|-"
						(format-time-string "%H:%M:%S")
						"*")
					(let (helm-pattern)
						(helm-read-buffer
							:prompt "Helm occur buffer: "
							:input buffer
							:must-match 'confirm-existing)))))
		(with-current-buffer (get-buffer-create buffer)
			(helm-occur-mode)
			; Save some vars locally for resuming this helm session.
			(setq-local
				helm-occur-mode-resume-info
					(cons helm-pattern helm-occur-buffer-and-tick-list))
			(setq buffer-read-only t)
			(let ((inhibit-read-only t))
				(erase-buffer)
				(insert
					"Results from helm-occur for pattern \""
					helm-pattern
					"\":\n")
				(save-excursion
					(insert (with-current-buffer "*helm-occur*" (buffer-string))))
				; Remove useless or problematic props.
				(remove-text-properties (point) (point-max)
					'(
						keymap nil
						helm-source nil
						helm-header nil
						helm-index nil
						helm-noncandidate nil
					))
				; Move to the first candidate line.
				(forward-line 1)))
		(pop-to-buffer buffer)))

; Helm occur from isearch

(defun helm-occur-from-isearch ()
"Invoke `helm-occur' from isearch.

To use this bind it to a key in `isearch-mode-map'.

With prefix arg choose buffers."
	(interactive)
	(let ((input isearch-string))
		(unwind-protect
			(isearch-exit)
			(helm-occur
				(when current-prefix-arg
					; Mark current buffer.
					(helm::call_after_next_update
						`(lambda ()
							(helm-map-candidates-in-source
								(lambda ()
									(when
										(eq
											,(current-buffer)
											(get-buffer
												(get-text-property (point) 'helm-real)))
										(helm-make-visible-mark))))))
					(map_modify_list
						#'get-buffer
						(helm-read-buffer
							:prompt "Occur buffers: "
							:must-match t
							:marked-candidates t)))
				input))))

(provide 'helm-occur)
