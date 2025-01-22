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

(defvar modeLine::forced_buffer_hash_table (make-hash-table :test 'eq)
"Key - buffer, value - t to include, nil to exclude.
Buffers with forced exclusion or inclusion.")

(defun modeLine::force_buffer (buffer include)
"Forcefully include or exclude buffer in this mode-line system.
Inclusion should only be done for buffers that are excluded by rules
in `modeLine::should_exclude'."
	(puthash buffer include modeLine::forced_buffer_hash_table))

(defun modeLine::should_exclude (buffer)
"Non-nil if buffer should be excluded from this mode-line system -
it doesn't want its mode-line-format changed.
Rules meant for user customization."
	; Internal or temporary buffers, not meant for normal user interaction.
	(= (string-to-char (buffer-name buffer)) ?\s))

(defun modeLine::should_exclude_window ()
"Non-nil if selected window should be excluded from this mode-line system -
it doesn't want its mode-line-format changed.
Selected window's buffer is current.
Rules meant for user customization.
Only called after buffer already has been included in this mode-line system.
This is handy for excluding windows that are dedicated, or for temporarily
excluding windows that are too small, etc."
	(< (window-body-height) 8))

(defun modeLine::excluded (buffer)
"Non-nil if buffer is excluded from this mode-line system (forcefully or not)."
	(let ((value (gethash buffer modeLine::forced_buffer_hash_table 'none)))
		(or (null value) (and (eq value 'none) (modeLine::should_exclude buffer)))))

; ================================ Core mode-line system ================================

(defvar modeLine::symbol_list nil
"List of symbols, whose value should be a string to use in mode-line, or nil.
String shouldn't have leading or trailing spaces, they will be added by this system.
Symbol's function definition should be a function with no args
called to calculate value for symbol.
Symbol can have 'mode-line-setup-fn property, that should be a function
called with no args to setup this mode-line part for current buffer.

At least one symbol in here should have non-nil value, because otherwise
global-mode-line appended to it will start with leading spaces, which will look bad.")

(defun modeLine::define_part (symbol fn &optional setup_fn set_on_activation)
"Define new mode-line part and append it to `modeLine::symbol_list'.
FN is a function with no args that should compute and return value for SYMBOL.
SETUP_FN can be a function with no args run on mode-line's activation.
SET_ON_ACTIVATION non-nil mean to set SYMBOL on mode-line's activation."
	(make-variable-buffer-local symbol)
	(setq fn `(lambda () (setq ,symbol (,fn))))
	(fset symbol fn)
	(when set_on_activation
		(setq setup_fn `(lambda () ,(when setup_fn (list setup_fn)) (,fn))))
	(when setup_fn (put symbol 'mode-line-setup-fn setup_fn))
	(setq modeLine::symbol_list (nconc modeLine::symbol_list (list symbol)))
	nil)

(setq-default mode-line-format nil)

; This is to not have to use double %.
; Also now every command wanting to update mode-line must call mode-line-update.
(defvar-local modeLine::format "" "Should always be a string.")
(put 'modeLine::format 'risky-local-variable t)

(defun mode-line-update (&optional all)
"Set `modeLine::format' by concating all mode-line parts.
ALL, like in `force-mode-line-update', updates mode-line of every window."
	(let (
		(update_fn
			(lambda ()
				(setq modeLine::format
					(concat
						" "
						(mapconcat
							(lambda (symbol)
								(when-let ((str (symbol-value symbol)))
									(concat str "  ")))
							modeLine::symbol_list)
						(format-mode-line global-mode-string)))
				(force-mode-line-update)))
	)
		(if all
			(let (updated_buffer_list)
				(dolist (window (window-list-1 nil 0 t))
					(let ((buffer (window-buffer window)))
						(unless (memq buffer updated_buffer_list)
							(push buffer updated_buffer_list)
							(with-selected-window window (funcall update_fn))))))
			(funcall update_fn)))
	nil)

(defun modeLine::maybe_activate (buffer)
"Activate this mode-line system for BUFFER, unless it is excluded.
Setup some buffer-local hooks."
	(or
		(modeLine::excluded buffer)
		; Don't accidentally activate twice.
		(eq (buffer-local-value 'mode-line-format buffer) 'modeLine::format)
		(with-current-buffer buffer
			; Run setup functions.
			(dolist (symbol modeLine::symbol_list)
				(when-let ((setup_fn (get symbol 'mode-line-setup-fn)))
					(funcall setup_fn)))
			(setq mode-line-format 'modeLine::format)
			; Call for the first time.
			; Useful when showing other windows.
			(mode-line-update)
			; Setup main update hook.
			; Low priority, because mode-line parts are usually done
			; with normal priority.
			(add-hook 'post-command-hook #'mode-line-update 1 t)))
	nil)

; Hooks that should activate this mode-line system in buffers that are not excluded
; (or are temp or internal buffers).

; Main hook.
(defun modeLine::after_change_major_mode_hook ()
	(modeLine::maybe_activate (current-buffer))
	nil)

; For now modeLine::should_exclude depends only on buffer name, so if buffer name changes,
; buffer may no longer be excluded, so then activate mode-line system.
(defun modeLine::rename_buffer_hook (name)
	(modeLine::maybe_activate (get-buffer name))
	name)

(add-hook 'emacs-startup-hook
	(fn_symbol "modeLine::initialize"
		(lambda ()
			(advice-add 'rename-buffer :filter-return #'modeLine::rename_buffer_hook)
			(advice-add 'rename-buffer :filter-return
				#'modeLine::buffer_identification_rename_buffer_hook)
			(add-hook 'after-change-major-mode-hook
				#'modeLine::after_change_major_mode_hook)
			(mapc #'modeLine::maybe_activate (buffer-list))
			nil)))

; =============================== Standard mode-line parts ===============================

; Memory error

; Here I use actual mode-line formatting, because I decide when to call
; 'format-mode-line' on it.
(setq global-mode-string '("" (memory-full "Memory full!  ")))

; Buffer identification

(defun modeLine::buffer_identification_rename_buffer_hook (name)
	(let ((buffer (get-buffer name)))
		(unless (or (modeLine::excluded buffer) (buffer-file-name buffer))
			(setq modeLine::buffer_identification (propertize name 'face 'bold))
			(dolist (window (get-buffer-window-list buffer 0 t))
				(with-current-buffer (window-buffer window) (mode-line-update)))))
	name)

(defun modeLine::buffer_identification_after_save_hook ()
	(remove-hook 'after-save-hook #'modeLine::buffer_identification_after_save_hook t)
	(setq modeLine::buffer_identification
		; Calculate it once more, because I think it might have changed
		; (if it really could change, there should be some hook added to detect that!).
		(let (
			(file
				(propertize (abbreviate-file-name buffer-file-name)
					'face 'helm-ff-file))
		)
			(helm-files-add-extension-face file)
			(helm-files-add-basename-face file)
			file))
	(when (get-buffer-window nil 'visible) (mode-line-update)))

(let (
	(fn
		(lambda ()
			(if (derived-mode-p 'dired-mode)
				(propertize (abbreviate-file-name default-directory)
					'face 'helm-ff-directory)
				(if-let ((file (buffer-file-name)))
					(let (face)
						(if (file-exists-p file)
							(setq face 'helm-ff-file)
							(setq face '(helm-ff-nofile helm-ff-file))
							(add-hook
								'after-save-hook
								#'modeLine::buffer_identification_after_save_hook
								nil
								t))
						; TODO there should be a watcher added to change mode-line
						; when file is deleted.
						; Also visiting file in a buffer should also have some hook
						; to update mode-line.
						(setq file (propertize (abbreviate-file-name file) 'face face))
						(helm-files-add-extension-face file)
						(helm-files-add-basename-face file)
						file)
					(propertize (buffer-name) 'face 'bold)))))
)
	(modeLine::define_part 'modeLine::buffer_identification fn nil t))

; Buffer narrowing

(modeLine::define_part
	'modeLine::narrowing
	(lambda ()
		(when (buffer-narrowed-p) ; This ensures buffer-size != 0.
			(concat
				"Narrow: "
				(number-to-string
					(round
						(*
							100
							(/
								(float (- (point-max) (point-min)))
								(buffer-size)))))
				"%")))
	(lambda () (add-hook 'post-command-hook #'modeLine::narrowing nil t))
	t)

; Read only

(defconst modeLine::read_only_str
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
			; Probably do the same as widget packgage does - use read--mouse-event or
			; something. It's not ideal, but it'll be fine for now.
			(let ((keymap (make-sparse-keymap)))
				(define-key keymap [mode-line mouse-1]
					(lambda (event) (interactive "e")
						(with-current-buffer (window-buffer (posn-window (nth 1 event)))
							(setq buffer-read-only nil))))
				keymap)))

(defconst modeLine::read_only_local_str
	(all-the-icons-material "lock_outline" :v-adjust -0.12 :face 'shadow))

(modeLine::define_part
	'modeLine::read_only
	(lambda ()
		; Don't consider inhibit-read-only, it's always used temporarily,
		; and it would make it more complicated.
		(cond
			(buffer-read-only modeLine::read_only_str)
			; Maybe there is no point to check this - for example Custom-mode
			; enforces read-onlyness not through any text or overlay properties,
			; but through before-change-functions, so there is no reasonable
			; way of detecting if region is supposed to be read-only or not
			; with any certainty. Well, just (when (ignore-errors (insert ?a) t) ...)
			; would work, but it's far too invasive - undo, syntax processing, etc.
			((get-text-property (point) 'read-only) modeLine::read_only_local_str)))
	(lambda () (add-hook 'post-command-hook #'modeLine::read_only nil t))
	t)

; Caret position

(modeLine::define_part
	'modeLine::position
	; Hmm, it seems like if at least two windows with the same buffer were to be
	; redisplayed at the same time, the later redisplayed one would take priority,
	; which is bad. So maybe every mode-line function should be called with one arg -
	; window; and selected window should not be changed. This would allow for comparison
	; (eq window (selected-window)) and then selected window would take priority.
	; But that obviously wouldn't be a problem if emacs hasn't tried to push everything
	; into being buffer-local, then mode-line-format could be not buffer-local, but
	; window-local, and then it wouldn't be a problem at all.
	`(lambda ()
		(propertize_no_copy
			(concat
				(number-to-string (round (* 100 (/ (float (point)) (1+ (buffer-size))))))
				"%")
			'display
			'(min-width
				(
					,(pixel_to_canonical_char_width
						(get_string_pixel_width
							(propertize "00%" 'face 'mode-line-active)))
				))))
	(lambda () (add-hook 'post-command-hook #'modeLine::position nil t))
	t)

; Column

(modeLine::define_part
	'modeLine::column
	`(lambda ()
		(propertize_no_copy (concat "C " (number-to-string current_column))
			'display
			'(min-width
				(
					,(pixel_to_canonical_char_width
						(get_string_pixel_width
							(propertize "C 00" 'face 'mode-line-active)))
				))))
	(lambda () (add-hook 'post-command-hook #'modeLine::column nil t))
	t)

; Marked region

; Variable because rectangle mode needs to change this.
(defvar mode-line-marked-region-fn
	(lambda ()
		(and
			mark-active
			(not deactivate-mark)
			(let ((caret (point)) (mark (mark)))
				(when (/= caret mark) ; This check is needed only for mouse dragging.
					(concat
						"Chars: "
						(number-to-string (abs (- caret mark)))
						" Lines: "
						(number-to-string
							(1+
								(if (> caret mark)
									(line_count_backward mark)
									(line_count_forward mark))))))))))

(modeLine::define_part
	'modeLine::marked_region
	(lambda () (funcall mode-line-marked-region-fn))
	(lambda () (add-hook 'post-command-hook #'modeLine::marked_region nil t))
	t)

; Prefix arg

(modeLine::define_part
	'modeLine::prefix_arg
	(lambda () (when prefix-arg (format "P %S" prefix-arg)))
	(lambda ()
		; Could be hooked to just specific prefix arg setting commands,
		; like universal-argument, but it's just one "if" so it's fine.
		(add-hook 'post-command-hook #'modeLine::prefix_arg nil t))
	t)

(provide 'myModeLine)
