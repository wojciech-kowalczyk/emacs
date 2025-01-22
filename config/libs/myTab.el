; -*- lexical-binding:nil -*-

; Subsystem tracking live buffers and ignoring them (if tab-should-exclude says so)
; or assigning them to a tabset chosen by tab-get-buffer-tabset-name and managing
; their tab-line-format.
;
; The display of tab-line works window-locally. E.g. if ignored buffer is displayed
; in a window, then tab-line-format of that window is left unchanged by this system,
; so this buffer can manage it itself; if assigned buffer is displayed in a window
; (and that window isn't ignored according to tab-should-exclude-window), then
; tab-line-format of that window is set to display this buffer's tabset
; (if necessary, scrolled in a way to make this buffer's tab visible).
;
; The exclusion/inclusion and assignment of buffers to tabsets can be overriden
; per-buffer by tab-force-buffer and helm-buffers-move-to-tabset from helm-buffers.
;
; Unfortunately some stuff is now in helm-buffers, which doesn't fully make sense...

; Some info on what in this file is called "tab" and "tabset".
; Tab is a vector [buffer format_unselected format_selected format_pixel_width],
; where format_(un)selected is a propertized string used in tab-line-format,
; and format_pixel_width is width of format_(un)selected string.
; format_unselected and format_selected are assumed to have the same pixel width.
; Tabset is a list, element of tab-tabset-list: (tabset_name . tab_list).
; Tabsets' names must be unique.
; Every tabset has a window-local variable called tab-tabset-name-and-scroll -
; (tabset_name . first_visible_tab_index).

(defconst tab-DEFAULT-TABSET-STRING "Default"
"If buffer doesn't belong to any project and there is no rule that assingns
it to a tabset, then it is assigned to this tabset.")

(defconst tab-BUFFER-NAME-FIXED-LENGTH nil
"Fixed length of buffer name shown in tab.
Overrides `tab-BUFFER-NAME-MAX-LENGTH' if non-nil.
Nil - off.")
(defconst tab-BUFFER-NAME-MAX-LENGTH 30
"Max length of buffer name shown in tab.
Nil - no limit.")

(define_face 'tab-line-active
	'((t :family "Segoe UI Short" :background "#35383A" :height 149)))
(define_face 'tab-line-inactive '((t :inherit tab-line-active)))

(define_face 'tab-unselected-tab-face '((t :foreground "#B9CBDE" :background "#171717")))
(define_face 'tab-selected-tab-face '((t :foreground "#FFC800" :background "black")))

(defconst tab-UNDER-MOUSE-UNSELECTED-TAB-FOREGROUND-COLOR nil
"Nil if dynamic - same color as icon.")
(defconst tab-UNDER-MOUSE-SELECTED-TAB-FOREGROUND-COLOR nil
"Nil if dynamic - same color as icon.")

(defconst tab-FALLBACK-UNDER-MOUSE-COLOR "#C9DCEF")
(defconst tab-FALLBACK-ICON-COLOR tab-FALLBACK-UNDER-MOUSE-COLOR
"If all-the-icons-color-icons is nil, or on some rare icons that don't have
assigned foreground, this will be icon's color.")

(defvar tab-forced-buffer-hash-table (make-hash-table :test 'eq)
"Key - buffer, value - t to include, nil to exclude.
Buffers with forced exclusion or inclusion.")

(defun tab-force-buffer (buffer include)
"Forcefully include or exclude buffer in this tab-line system.
Inclusion should only be done for buffers that are excluded by rules
in `tab-should-exclude'."
	(puthash buffer include tab-forced-buffer-hash-table))

(defun tab-should-exclude (buffer)
"Non-nil if buffer should be excluded from this tab-line system -
it doesn't want its tab-line-format changed.
Rules for user customization."
	; Internal or temporary buffers, not meant for normal user interaction.
	(= (string-to-char (buffer-name buffer)) ?\s))

(defun tab-should-exclude-window ()
"Non-nil if selected window should be excluded from this tab-line system -
it doesn't want its tab-line-format changed.
Selected window's buffer is current.
Rules for user customization.
Only called after buffer already has been included in this tab-line system.
This is handy for excluding windows that are dedicated, or for temporarily
excluding windows that are too small, etc."
	(or (window-dedicated-p) (< (window-body-height) 8)))

(defun tab-get-buffer-tabset-name ()
"Return tabset name for current buffer.
Always has to be a string.
Rules for user customization."
	(cond
		((string-prefix-p "*" (buffer-name)) "Emacs")
		((derived-mode-p 'shell-mode 'eshell-mode) "Shell")
		((derived-mode-p 'dired-mode) "Dired")
		((derived-mode-p 'org-mode 'org-agenda-mode 'diary-mode) "Org")
		((derived-mode-p 'eaf-mode) "EAF")
		((derived-mode-p 'emacs-lisp-mode) "Elisp")
		((derived-mode-p 'c-mode 'c++-mode) "C")
		(t tab-DEFAULT-TABSET-STRING)))

; Prevent killing messages buffer. Instead of killing it, clear it and mark as unmodified.
; Reasons for this:
;	1) Need to have a reliable, always alive, displayable on a tab-line buffer,
;	to fall back to when all other buffers are killed;
;
;	2) messages buffer is unique, because after it's death, almost immediately a new
;	messages buffer is created, but not using get-buffer-create like every other buffer,
;	but Fget_buffer_create in C code (by this line in xdisp.c:
;	Fset_buffer (Fget_buffer_create (Vmessages_buffer_name, Qnil));).
;	In consequence, my hook to get-buffer-create, doesn't run for messages.
;	UPDATE: I don't hook to get-buffer-create anymore, I use after-change-major-mode-hook
;	instead, so maybe this second point doesn't matter now. Anyway it doesn't hurt
;	to leave it unchanged I think.
; This is a source of problems, because this whole tab system assumes that no buffer
; can be created undetected.
; So disallowing killing messages buffer solves two problems in a simple way.
(push
	(fn_symbol "tab-prevent-messages-buffer-kill"
		(lambda ()
			(or
				(not (string= messages-buffer-name (buffer-name)))
				(let ((inhibit-read-only t))
					(erase-buffer)
					(set-buffer-modified-p nil)
					nil)))) ; Return nil to not kill buffer.
	kill-buffer-query-functions)

; Converting a buffer to a propertized string representing a tab in tab-line.

(defconst tab-UNSELECTED-TAB-SPACE-STRING
	(propertize " " 'face 'tab-unselected-tab-face))
(defconst tab-SELECTED-TAB-SPACE-STRING
	(propertize " " 'face 'tab-selected-tab-face))
(defconst tab-ELLIPSIS-STRING "..." "For too long tab names.")
(defconst tab-UNSELECTED-TAB-FACE-BACKGROUND
	(face-attribute 'tab-unselected-tab-face :background nil '(tab-line-active default)))
(defconst tab-SELECTED-TAB-FACE-BACKGROUND
	(face-attribute 'tab-selected-tab-face :background nil '(tab-line-active default)))

(defun tab-format-tab (buffer)
"Return [format_unselected format_selected format_pixel_width], where
format_(un)selected is display representation of a tab -
propertized string used in `tab-line-format',
and format_pixel_width is width of format_(un)selected.
format_unselected format_selected are assumed to have the same pixel width."
	(let* (
		(buffer_name (buffer-name buffer))
		is_name_trimmed
		(formatted_buffer_name ; Buffer name surrounded with spaces.
			(concat
				" "
				(let* (
					(buffer_name_length (length buffer_name))
					(get_trimmed_buffer_name_if_too_long
						(lambda (max_length)
							(when (> buffer_name_length max_length)
								(setq is_name_trimmed t)
								(concat
									(substring-no-properties buffer_name
										0 (- max_length (length tab-ELLIPSIS-STRING)))
									tab-ELLIPSIS-STRING))))
				)
					(cond
						(tab-BUFFER-NAME-FIXED-LENGTH
							(or
								(funcall get_trimmed_buffer_name_if_too_long
									tab-BUFFER-NAME-FIXED-LENGTH)
								(concat
									buffer_name
									(get_space_string
										(-
											tab-BUFFER-NAME-FIXED-LENGTH
											buffer_name_length)))))
						(tab-BUFFER-NAME-MAX-LENGTH
							(or
								(funcall get_trimmed_buffer_name_if_too_long
									tab-BUFFER-NAME-MAX-LENGTH)
								buffer_name))
						(t buffer_name)))
				" "))
		(base_icon (with-current-buffer buffer (all-the-icons-icon-for-buffer)))
		(keymap (make-sparse-keymap))
	)
		; Setup keymap.
		(define-key keymap [tab-line down-mouse-1]
			`(lambda (event) (interactive "e")
				(select-window (posn-window (nth 1 event)))
				(switch-to-buffer ,buffer nil t)))
		(define-key keymap [tab-line down-mouse-3]
			`(lambda (arg) (interactive "P") (tab-kill-buffer ,buffer arg)))
		(define-key keymap [tab-line C-down-mouse-1]
			`(lambda (event) (interactive "e")
				(when-let (
					(new_window
						(window::split_window_sensibly (posn-window (nth 1 event))))
				)
					(select-window new_window)
					(switch-to-buffer ,buffer nil t))))
		(let (
			(base_icon_face_no_background
				(let ((icon_face_plist (get-text-property 0 'face base_icon)))
					(list
						:family (plist-get icon_face_plist :family)
						:height (plist-get icon_face_plist :height)
						:foreground
							(if-let (
								(icon_inherited_face (plist-get icon_face_plist :inherit))
							)
								; This supports only the default all-the-icons' way -
								; :inherit, if it exists, is a symbol (a face), that
								; always has :foreground specified.
								; Could be changed in the future.
								(face-attribute icon_inherited_face :foreground)
								tab-FALLBACK-ICON-COLOR))))
			; Copy icon to not change cached entry in all-the-icons.
			(unselected_icon (copy-sequence base_icon))
			(selected_icon (copy-sequence base_icon))
			(format_tab ; Dynamic binding: keymap, formatted_buffer_name.
				(lambda
					(tab_face under_mouse_tab_foreground_color first_space_string icon)
					(propertize_no_copy
						(concat
							first_space_string
							icon
							(propertize formatted_buffer_name 'face tab_face))
						'keymap keymap
						'mouse-face
							(list
								:inherit tab_face
								:foreground
									(or
										under_mouse_tab_foreground_color
										(plist-get
											(get-text-property 0 'face icon)
											:foreground))))))
		)
			(put-text-property 0 1 'face
				; Not nconc to not change base_icon_face_no_background for selected_icon.
				(append
					base_icon_face_no_background
					(list :background tab-UNSELECTED-TAB-FACE-BACKGROUND))
				unselected_icon)
			(put-text-property 0 1 'face
				(nconc
					base_icon_face_no_background
					(list :background tab-SELECTED-TAB-FACE-BACKGROUND))
				selected_icon)
			(let (
				(unselected_format
					(funcall format_tab
						'tab-unselected-tab-face
						tab-UNDER-MOUSE-UNSELECTED-TAB-FOREGROUND-COLOR
						tab-UNSELECTED-TAB-SPACE-STRING
						unselected_icon))
				(selected_format
					(funcall format_tab
						'tab-selected-tab-face
						tab-UNDER-MOUSE-SELECTED-TAB-FOREGROUND-COLOR
						tab-SELECTED-TAB-SPACE-STRING
						selected_icon))
			)
				; If name was trimmed, add help-echo with full buffer name.
				(when is_name_trimmed
					(propertize_no_copy unselected_format 'help-echo buffer_name)
					(propertize_no_copy selected_format 'help-echo buffer_name))
				(vector
					unselected_format
					selected_format
					(get_string_pixel_width
						(let ((unselected_format_copy (copy-sequence unselected_format)))
							(add-face-text-property 0 (length unselected_format_copy)
								'tab-line-active t unselected_format_copy)
							unselected_format_copy)))))))

; Basic stuff for storing, creating and deleting tabs and tabsets.

(defun tab-excluded (buffer)
"Non-nil if buffer is excluded from this tab-line system (forcefully or not)."
	(let ((value (gethash buffer tab-forced-buffer-hash-table 'none)))
		(or (null value) (and (eq value 'none) (tab-should-exclude buffer)))))

(defvar tab-buffer-tabset-name-hash-table (make-hash-table :test 'eq)
"Key - buffer, value - key's tabset_name.")

(defun tab-buffer-tabset-name (buffer)
"Return buffer tabset's name or nil if entry in `tab-buffer-tabset-name-hash-table'
doesn't exist."
	(gethash buffer tab-buffer-tabset-name-hash-table))

(defun tab-set-buffer-tabset-name (buffer &optional value)
"Update/add buffer's entry in `tab-buffer-tabset-name-hash-table' and return
it's new value."
	(puthash
		buffer
		(or value (with-current-buffer buffer (tab-get-buffer-tabset-name)))
		tab-buffer-tabset-name-hash-table))

(defun tab-tab-buffer (tab) "Access buffer of TAB." (aref tab 0))

(defun tab-tab (buffer tab_list) "Return tab or nil."
	(find_in_list tab_list (lambda (tab) (eq buffer (tab-tab-buffer tab)))))

(defun tab-create-tab (buffer)
"Return new tab - [buffer format_unselected format_selected format_pixel_width]."
	(vconcat (vector buffer) (tab-format-tab buffer)))

(defun tab-delete-tab-internal (tab)
	(remhash (tab-tab-buffer tab) tab-buffer-tabset-name-hash-table)
	(remhash (tab-tab-buffer tab) tab-forced-buffer-hash-table)
	nil)

(defun tab-delete-tab (tab tabset)
"Delete tab from tabset and remove record of buffer in
`tab-buffer-tabset-name-hash-table'."
	(tab-delete-tab-internal tab)
	(setcdr tabset (delq tab (cdr tabset)))
	nil)

(defvar tab-tabset-list nil
"Global list of tabsets.
Tabset is a cons (tabset_name . tab_list).")

(defun tab-tabset (tabset_name) "Return tabset or nil."
	(assoc tabset_name tab-tabset-list))

(defun tab-create-tabset (tabset_name)
"Create tabset, append it to `tab-tabset-list' and return it.
Remember to add tabs to it before displaying tabset."
	(let ((tabset (list tabset_name)))
		(setq tab-tabset-list (nconc tab-tabset-list (list tabset)))
		tabset))

(defun tab-delete-tabset (tabset)
"Delete tabset from `tab-tabset-list' and remove records
of tabset's buffers in `tab-buffer-tabset-name-hash-table'."
	(mapc #'tab-delete-tab-internal (cdr tabset))
	(setq tab-tabset-list (delq tabset tab-tabset-list))
	nil)

; Calculating value of tab-line-format.

(defconst tab-LEFT-ARROW-ICON-AND-PIXEL-WIDTH
	(let* (
		(icon (all-the-icons-alltheicon "arrow-left"))
		(icon_face
			(let ((icon_face_plist (get-text-property 0 'face icon)))
				(list
					:family (plist-get icon_face_plist :family)
					:height (plist-get icon_face_plist :height)
					:background tab-UNSELECTED-TAB-FACE-BACKGROUND
					:foreground "grey65")))
	)
		(setq icon (propertize icon 'face icon_face))
		(cons
			icon
			(get_string_pixel_width
				(propertize icon 'face (append '(:inherit tab-line-active) icon_face)))))
"Used to indicate that there are invisible tabs on the left.")

(defvar-window-local tab-tabset-name-and-scroll
"(tabset_name . first_visible_tab_index).")

(defun tab-tab-line-format ()
"Return `tab-line-format' for selected window and its buffer, which is assumed
to be current.
Current buffer must be in current tabset."
	(unless (tab-should-exclude-window)
		(let* (
			(tabset_name (tab-buffer-tabset-name (current-buffer)))
			(tab_list (cdr (tab-tabset tabset_name)))
			(tab (tab-tab (current-buffer) tab_list))
			(tabset_name_and_scroll
				; Get or create tab-tabset-name-and-scroll.
				(if-let ((tabset_name_and_scroll (tab-tabset-name-and-scroll)))
					(progn
						(unless (string= tabset_name (car tabset_name_and_scroll))
							(setcar tabset_name_and_scroll tabset_name)
							(setcdr tabset_name_and_scroll 0))
						tabset_name_and_scroll)
					(setf (tab-tabset-name-and-scroll) (cons tabset_name 0))))
			(scroll (cdr tabset_name_and_scroll))
			(visible_tab_list (nthcdr scroll tab_list))
			(window_pixel_width (window-pixel-width))
		)
			(if (memq tab visible_tab_list)
				; Selected tab is further to the right than the first visible tab.
				(let (
					(visible_width
						(cl-loop for tab_1 in visible_tab_list sum (aref tab_1 3)))
					(tab_x
						(cl-loop
							for tab_1 in visible_tab_list
							until (eq tab tab_1)
							sum (aref tab_1 3)))
				)
					; Add left arrow width if appropriate.
					(when (/= scroll 0)
						(+= visible_width (cdr tab-LEFT-ARROW-ICON-AND-PIXEL-WIDTH)))
					; If tabs in visible_tab_list are all fully visible
					; (the most common case).
					(if (<= visible_width window_pixel_width)
						; Scroll to the left until tabs are no longer fully visible,
						; or until max scrolled to the left.
						; In other words, when selected tab is truly visible and there is
						; free space on the right side of last tab, try to scroll
						; to the left to cover that space with tabs.
						(cl-loop
							; Until tabset is max scrolled to the left or everything
							; is perfectly visible.
							until (or (= scroll 0) (= visible_width window_pixel_width))
							do
							; Scroll one tab to the left.
							(setq visible_tab_list (nthcdr (-- scroll) tab_list))
							(+= visible_width (aref (car visible_tab_list) 3))
							(when (= scroll 0)
								(-= visible_width
									(cdr tab-LEFT-ARROW-ICON-AND-PIXEL-WIDTH)))
							; If tabs in visible_tab_list are no longer fully visible.
							when (> visible_width window_pixel_width)
								return
									; If selected tab is the last one, it is now not
									; fully visible, so scroll back one tab to the right.
									(when (eq tab (car (last visible_tab_list)))
										(setq visible_tab_list (cdr visible_tab_list))
										(++ scroll)))
						; Chop tab off from the right, until tabs in visible_tab_list will
						; be fully visible, then that means selected tab was visible,
						; or until we would have to chop selected tab, then that means
						; selected tab is out of view on the right, then chop tabs
						; from the left until tabs will be fully visible, leaving us
						; with selected tab being the mostright fully visible one.
						(setq visible_tab_list (reverse visible_tab_list))
						(cl-loop
							; If selected tab is now the first element in visible_tab_list
							; (so it's really the rightmost tab), meaning that it is still
							; not visible, chop tabs from the left = scroll to the right,
							; until selected tab will be visible.
							when (eq tab (car visible_tab_list))
								return
									(progn
										(setq visible_tab_list (reverse visible_tab_list))
										(cl-loop
											do
											(-= visible_width
												(aref (car visible_tab_list) 3))
											; Always show the selected tab (in case
											; selected tab alone is wider than window).
											while
												(setq visible_tab_list
													(cdr visible_tab_list))
											do (++ scroll)
											; While tabs in visible_tab_list are not all
											; fully visible.
											while (> visible_width window_pixel_width)))
							do
							(-= visible_width (aref (car visible_tab_list) 3))
							(setq visible_tab_list (cdr visible_tab_list))
							; While tabs in visible_tab_list are not all fully visible.
							while (> visible_width window_pixel_width))
						(setq visible_tab_list (nthcdr scroll tab_list))))
				; Selected tab is further to the left than the first visible tab.
				; Find selected tab, return list starting with it and save it's position
				; as the first visible tab in tabset.
				(setq visible_tab_list tab_list)
				(setq scroll 0)
				(while (not (eq tab (car visible_tab_list)))
					(setq visible_tab_list (cdr visible_tab_list))
					(++ scroll)))
			(setcdr tabset_name_and_scroll scroll) ; Save new scroll.
			(concat
				(when (/= scroll 0) (car tab-LEFT-ARROW-ICON-AND-PIXEL-WIDTH))
				(mapconcat
					(lambda (tab_1) (aref tab_1 (if (eq tab tab_1) 2 1)))
					visible_tab_list)))))

; Finding buffers

(defun tab-most-recent-buffer-in-tabset (tabset)
"Return most recent buffer from tabset."
	(or
		(find_in_list
			(buffer-list)
			`(lambda (buffer) (equal ,(car tabset) (tab-buffer-tabset-name buffer))))
		; If no buffer from tabset has been found, return first buffer from tabset.
		(nth 1 tabset)))

(defun tab-most-recent-buffer-in-next-tabset (tabset)
"Return most recent tab from next tabset.
There must be more than one existing tabsets."
	(tab-most-recent-buffer-in-tabset
		(let ((tabset_sub_alist (memq tabset tab-tabset-list)))
			(if (cdr tabset_sub_alist) ; If tabset isn't last.
				(nth 1 tabset_sub_alist) ; Next tabset.
				(car tab-tabset-list))))) ; First tabset.

(defun tab-most-recent-non-excluded-buffer ()
"Search starts from the second buffer in buffer-list,
because current buffer is the first (and we don't want it)."
	(find_in_list (cdr (buffer-list)) #'tab-buffer-tabset-name))

; Adding, deleting, moving tabs

(defun tab-create-tabset-with-buffer (buffer tabset_name)
	(setcdr (tab-create-tabset tabset_name) (list (tab-create-tab buffer)))
	nil)

(defun tab-set-tab-line-format (value)
	(set-window-parameter nil 'tab-line-format value)
	(force-mode-line-update)
	nil)

(defun tab-update-windows-displaying-tabset-tab-lines (tabset_name &optional omit_buffer)
"Update tab lines of windows displaying buffers whose tabset has name tabset_name.
Don't touch windows displaying OMIT_BUFFER if non-nil."
	; 0 to never include minibuffer window, t to include all frames.
	(dolist (window (window-list-1 nil 0 t))
		(and
			(equal tabset_name (tab-buffer-tabset-name (window-buffer window)))
			(not (eq (window-buffer window) omit_buffer))
			(with-selected-window window
				(tab-set-tab-line-format (tab-tab-line-format))))))

(defun tab-add-new-tab-to-maybe-new-tabset-and-update-tab-lines (buffer tabset_name)
"If tabset named TABSET_NAME doesn't exist, create it.
Create tab for BUFFER in this tabset.
Update tab lines of buffers that need it."
	(if-let ((tabset (tab-tabset tabset_name)))
		(setcdr tabset (nconc (cdr tabset) (list (tab-create-tab buffer))))
		(tab-create-tabset-with-buffer buffer tabset_name))
	(tab-update-windows-displaying-tabset-tab-lines tabset_name)
	nil)

(defun tab-add-window-size-change-hook (buffer)
"Add `tab-window-size-change-hook' to `window-size-change-functions' buffer-locally."
	(with-current-buffer buffer
		(make-local-variable 'window-size-change-functions)
		(cl-pushnew
			#'tab-window-size-change-hook window-size-change-functions :test #'eq))
	nil)

(defun tab-activate (buffer)
"Activate this tab-line system for BUFFER, unless it is excluded.

Activation of already activated buffer is safe.

Update or create BUFFER's tab and/or tabset.

This is the main function adding buffers to this tab-line system.
You can assign a buffer to some tabset (and force it's inclusion if necessary)
by using `tab-set-buffer-tabset-name' (and `tab-force-buffer' if
`tab-should-exclude' returns non-nil) before this function runs (it runs
in `after-change-major-mode-hook' and after `rename-buffer')."
	(unless (tab-excluded buffer)
		(tab-add-window-size-change-hook buffer)
		(if-let ((tabset_name (tab-buffer-tabset-name buffer)))
			(let* (
				(tabset (tab-tabset tabset_name))
				(tab_list (cdr tabset))
				(tab (tab-tab buffer tab_list))
			)
				; If tab actually existed.
				(if tab
					; Update existing tab.
					(let ((new_tabset_name (tab-set-buffer-tabset-name buffer)))
						; If buffer still belongs in it's old tabset.
						(if (string= tabset_name new_tabset_name)
							(let ((format (tab-format-tab buffer)))
								; Update buffer tab's format.
								(aset tab 1 (aref format 0))
								(aset tab 2 (aref format 1))
								(aset tab 3 (aref format 2))
								; Update tab line of all windows displaying buffer's old
								; tabset.
								(tab-update-windows-displaying-tabset-tab-lines
									tabset_name))
							; If buffer's old tabset contains only one tab.
							(if (not (cdr tab_list))
								; Delete old tabset.
								; No need to update tab lines of windows displaying it,
								; because that tabset had only one buffer in it, so all
								; windows displaying it will display buffer's new tabset
								; after this function.
								(setq tab-tabset-list (delq tabset tab-tabset-list))
								; Delete buffer's tab from it's old tabset.
								(setcdr tabset (delq tab tab_list))
								; Update tab line of all windows displaying buffer's old
								; tabset.
								(tab-update-windows-displaying-tabset-tab-lines
									tabset_name))
							(tab-add-new-tab-to-maybe-new-tabset-and-update-tab-lines
								buffer new_tabset_name)))
					(tab-add-new-tab-to-maybe-new-tabset-and-update-tab-lines
						buffer tabset_name)))
			(tab-add-new-tab-to-maybe-new-tabset-and-update-tab-lines
				buffer (tab-set-buffer-tabset-name buffer))))
	nil)

; Hooks

(defun tab-after-change-major-mode-hook ()
"The main hook that registers new buffers in this tab-line system.
This is like globalized minor modes work - they are turned on by this hook.
So if buffer won't call fundamental-mode in any way, then it won't be registered
by this system."
	(tab-activate (current-buffer))
	nil)

(defun tab-rename-buffer-hook (name)
	; It's filter-return, so NAME is an actual name given to buffer.
	(tab-activate (get-buffer name))
	name)

(defun tab-window-size-change-hook (window)
"Runs after WINDOW has changed its size or buffer.
Recomputes `tab-line-format' of WINDOW, to adjust tabset scroll if appropriate.
This is the buffer-local version of this hook.
It function is added buffer-locally to `window-size-change-functions' by
`tab-activate' in every buffer included by this tab system."
	(with-selected-window window (tab-set-tab-line-format (tab-tab-line-format)))
	nil)

; This function deletes BUFFER's tab, so if this is called not from kill-buffer
; and there is a possibility that BUFFER won't be killed, it needs to be accounted
; for with at least BUFFER re-registering with a call to tab-activate.
(defun tab-replace-buffer-in-windows-hook (buffer &optional new_buffer)
"NEW_BUFFER is the buffer to maybe switch to;
if nil, choose the most suitable default."
	; Every mention of "buffer" in this function's comments mean this function's
	; parameter.

	; Convert possible string to buffer.
	(when (stringp buffer) (setq buffer (get-buffer buffer)))
	(let (
		(window_delete ; Based on window--delete.
			(lambda (window)
				(let ((window_deletable_value (window-deletable-p window)))
					(cond
						((eq window_deletable_value 'frame)
							(delete-frame (window-frame window)))
						(window_deletable_value (delete-window window))))))
		; Windows that display buffer.
		(buffer_window_list
			; Find all buffer's windows and unrecord buffer in all windows.
			(cl-loop
				for window in (window-list-1 nil nil t)
				do (unrecord-window-buffer window buffer)
				when (eq buffer (window-buffer window)) collect window))
	)
		(if-let ((buffer_tabset_name (tab-buffer-tabset-name buffer)))
			; Still protect against possible calls straight to replace-buffer-in-windows
			; with message buffer, because it shouldn't ever be killed
			; (call to replace-buffer-in-windows basically always implies that it is
			; being killed).
			(unless (string= messages-buffer-name (buffer-name buffer))
				(let* (
					(buffer_tabset (tab-tabset buffer_tabset_name))
					(buffer_tab_list (cdr buffer_tabset))
					; [1] I'm very new to this quitting stuff so I might be wrong -
					; I think when we are called from kill-buffer, it's possible
					; to quit out of this function, and we are about to make important
					; changes to this tab-line system, which if after this point any
					; quit was to happen, tab-line would be out of sync with actual
					; state of buffers in emacs, so try to prevent that by inhibiting
					; quit from here and setting quit-flag to nil later.
					; I think this is the last place that can quit from kill-buffer.
					(inhibit-quit t)
				)
					(cond
						; If buffer is displayed in at least one window.
						(buffer_window_list
							; Get new buffer and delete old tab/tabset.
							; If buffer is not the only one in it's tabset.
							(if (cdr buffer_tab_list)
								(progn
									(unless new_buffer
										(setq new_buffer
											(tab-tab-buffer
												; Next (or previous, if buffer is the last one in buffer_tab_list)
												; tab in buffer_tab_list, treating buffer as the one in the middle.
												(if (eq buffer (tab-tab-buffer (car buffer_tab_list)))
													(nth 1 buffer_tab_list)
													(let ((buffer_tab_list_cdr (cdr buffer_tab_list)))
														(while
															(not
																(eq
																	buffer
																	(tab-tab-buffer (car buffer_tab_list_cdr))))
															(setq buffer_tab_list buffer_tab_list_cdr)
															(setq buffer_tab_list_cdr (cdr buffer_tab_list_cdr)))
														(or (nth 1 buffer_tab_list_cdr) (car buffer_tab_list)))))))
									(tab-delete-tab (tab-tab buffer buffer_tab_list) buffer_tabset))
								; This branch assumes that messages buffer will always be alive,
								; so it will only execute when there is more than one tabset.
								(unless new_buffer
									(setq new_buffer
										(tab-most-recent-buffer-in-next-tabset buffer_tabset)))
								(tab-delete-tabset buffer_tabset))
							; Try to find info to set window-start and window-point for
							; new_buffer by searching window-prev-buffers in all windows,
							; starting with selected window.
							; This should be similar to how switch-to-buffer does that,
							; so switching-to-buffer and then killing previous current
							; buffer be the same as killing current buffer and being
							; switched to other buffer automatically by
							; replace-buffer-in-windows.
							(let (
								(new_buffer_default_info
									; Search for new_buffer in windows' prev buffer lists.
									(cl-loop
										; Non-minibuffer windows on all frames.
										for window in (window-list-1 nil 0 t)
										for info = (assq new_buffer (window-prev-buffers window))
										when info return (cons window (cdr info))))
								(selected_window (selected-window))
							)
								; Try to switch to new_buffer in every window displaying old buffer.
								(dolist (buffer_window buffer_window_list)
									(let ((window_dedicated_value (window-dedicated-p buffer_window)))
										; If window is strongly dedicated, or window's frame doesn't
										; accept buffer, then delete window/frame, else switch to
										; new_buffer in this window.
										(if
											(or
												(eq window_dedicated_value t)
												(let (
													(window_frame_buffer_predicate
														(frame-parameter
															(window-frame buffer_window) 'buffer-predicate))
												)
													(and
														window_frame_buffer_predicate
														(not
															(funcall window_frame_buffer_predicate
																new_buffer)))))
											(funcall window_delete buffer_window)
											(set-window-buffer buffer_window new_buffer)
											; Preserve window's dedicated value.
											(set-window-dedicated-p buffer_window window_dedicated_value)
											; If window is selected, set current buffer.
											(when (eq buffer_window selected_window) (set-buffer new_buffer))
											(let (
												(new_buffer_info
													; If new_buffer was found in prev-buffer list of at least
													; one window.
													(if new_buffer_default_info
														(cdr
															(or
																(and
																	; If it wasn't this window.
																	(not
																		(eq
																			buffer_window
																			(car new_buffer_default_info)))
																	; Try to find new_buffer in this window.
																	(assq
																		new_buffer
																		(window-prev-buffers buffer_window)))
																; Else use the default.
																new_buffer_default_info))
														'(1 1)))
											)
												(set-window-start buffer_window (car new_buffer_info) t)
												(set-window-point buffer_window (nth 1 new_buffer_info)))))))
							; Now every window displaying old buffer is deleted or displays new buffer.
							; Update windows displaying old buffer's tabset, maybe excluding new buffer,
							; because it will be updated by tab-window-size-change-hook if needed.
							(tab-update-windows-displaying-tabset-tab-lines
								buffer_tabset_name new_buffer))
						; If buffer is not the only one in it's tabset.
						((cdr buffer_tab_list)
							; Delete buffer's tab from it's tabset.
							(tab-delete-tab (tab-tab buffer buffer_tab_list) buffer_tabset)
							(tab-update-windows-displaying-tabset-tab-lines buffer_tabset_name))
						(t (tab-delete-tabset buffer_tabset)))
					; See [1].
					(setq quit-flag nil)))
			; Branch handling buffers excluded from this tab-line system.
			; It's much simpler than this above, so some functions in this file will
			; delete tab of buffer to be killed and only then call kill-buffer, which
			; will call this function, for this function to run this branch.
			;
			; Delete all windows displaying excluded buffer.
			; This behaviour differs from emacs' default one - this always just
			; deletes window instead of showing previous buffer in it.
			(mapc window_delete buffer_window_list)))
	nil)

; New behaviour in switch-to-buffer -
; if switch-to-buffer-preserve-window-point = 'search-all-windows
; then search in window-prev-buffers' of all windows,
; instead of just selected window like with t or 'already-displayed.
(setq switch-to-buffer-preserve-window-point 'search-all-windows)

; Implement this in this :override hook.
(defun tab-switch-to-buffer-hook (buffer &optional norecord force_same_window)
	(interactive
		(let (
			(force-same-window
				(cond
					((or switch-to-buffer-obey-display-actions (window-minibuffer-p))
						nil)
					((not (eq (window-dedicated-p) t)) t)
					((eq switch-to-buffer-in-dedicated-window 'pop) nil)
					((if (eq switch-to-buffer-in-dedicated-window 'prompt)
							(y-or-n-p
								(format "Window is dedicated to %s; undedicate it?"
									(window-buffer)))
							switch-to-buffer-in-dedicated-window)
						(set-window-dedicated-p nil nil)
						t)
					(t (user-error "Cannot switch buffers in a dedicated window"))))
		)
			(list (read-buffer-to-switch "Switch to buffer: ") nil force-same-window)))

	(setq buffer (window-normalize-buffer-to-switch-to buffer))
	(let (
		(do
			(lambda ()
				(let* (
					(preserve-win-point
						(buffer-local-value
							'switch-to-buffer-preserve-window-point buffer))
					(get_entry
						(lambda (&optional window)
							(assq buffer (window-prev-buffers window))))
					(entry
						(cond
							((not preserve-win-point) nil)
							((eq preserve-win-point t) (funcall get_entry))
							((eq preserve-win-point 'already-displayed)
								(when (get-buffer-window buffer 0) (funcall get_entry)))
							(preserve-win-point ; 'search-all-windows
								; Start searching from the selected window,
								; so it will have the highest priority.
								(cl-loop
									for window in (window-list-1 nil 0 t)
									thereis (funcall get_entry window)))))
				)
					(set-window-buffer nil buffer)
					(when entry
						(set-window-start nil (nth 1 entry) t)
						(set-window-point nil (nth 2 entry)))
					(with-current-buffer buffer (funcall after_move_hook_fn nil t)))))
	)
		(cond
			(switch-to-buffer-obey-display-actions
				(when
					(eq
						(selected-window)
						(progn
							(pop-to-buffer-same-window buffer norecord)
							(selected-window)))
					(funcall do)))
			; Don't call set-window-buffer if it's not needed since it
			; might signal an error (e.g. if the window is dedicated).
			((eq buffer (window-buffer)))
			((window-minibuffer-p)
				(if force_same_window
					(user-error "Cannot switch buffers in minibuffer window")
					(pop-to-buffer buffer norecord)))
			((eq (window-dedicated-p) t)
				(if force_same_window
					(user-error "Cannot switch buffers in a dedicated window")
					(pop-to-buffer buffer norecord)))
			(t (funcall do))))
	(unless norecord (select-window (selected-window)))
	(set-buffer buffer))

; Not a place for this, but also add :after pop-to-buffer call to after_move_hook_fn.

(defun tab-pop-to-buffer-hook (&rest _) (funcall after_move_hook_fn nil t))

; Again not the place for this but for now at least keep it in the same place.

(push
	(lambda (frame) (with-selected-frame frame (funcall after_move_hook_fn nil t)))
	window-selection-change-functions)

; Interactive

(defun tab-kill-buffer (&optional buffer arg)
"Interactive way of killing buffers.
That means - first, replace-buffer-in-windows and redisplay, so user gets
quick visual response; then save-buffer, which can take some time, then kill-buffer.
If buffer isn't actually killed, then restore it's tab (actually, don't restore it,
but treat is as a newly created buffer; I think it's a good way of doing this).
If buffer is killed, return t, else nil.

With prefix arg don't do any of this and don't save, just kill-buffer."
	(interactive "i\nP")
	(unless buffer (setq buffer (current-buffer)))
	(if arg
		; This is already called with prefix, so use short answers.
		(let ((use-short-answers t)) (kill-buffer buffer))
		; Similar to the comment [1] from tab-replace-buffer-in-windows-hook,
		; prevent quitting after changing tab-line system state, to ensure
		; buffer will be killed.
		; This should be more robust, checking all conditions where kill-buffer
		; aborts killing buffer, and if there is even a slight suspicion any
		; of them might be met, just call save_buffer and kill-buffer, to not
		; make tab-line system out of sync.
		; I noticed this quitting problem only recently, so there might be more
		; problems like this in this file.
		(let ((inhibit-quit t))
			(replace-buffer-in-windows buffer)
			(redisplay)
			(with-current-buffer buffer (save_buffer))
			(or
				(kill-buffer buffer)
				(progn (tab-activate buffer) nil)))))

(defvar tab-MESSAGES-TABSET nil
"Messages buffer's tabset.
This should be treated as const, it's initialized by tab-initialize.")

(defun tab-kill-all-buffers-in-tabset (&optional other arg)
"Save and kill all buffers in current tabset.
With prefix arg don't save, just kill-buffer."
	(interactive "i\nP")
	(if-let ((tabset_name (tab-buffer-tabset-name (current-buffer))))
		(let* (
			(tabset (tab-tabset tabset_name))
			(buffer_to_kill_list (mapcar #'tab-tab-buffer (cdr tabset)))
			new_buffer
		)
			(if other
				(setq
					new_buffer (current-buffer)
					buffer_to_kill_list (delq new_buffer buffer_to_kill_list))
				(setq new_buffer
					; If there are other tabsets, select different tabset,
					; else select messages buffer, because every time there is
					; only one tabset, it is tab-MESSAGES-TABSET.
					(if (cdr tab-tabset-list)
						(tab-most-recent-buffer-in-next-tabset tabset)
						(get-buffer messages-buffer-name))))
			(if arg
				(let ((use-short-answers t)) (mapc #'kill-buffer buffer_to_kill_list))
				(let ((inhibit-quit t)) ; Same as in tab-kill-buffer.
					(dolist (buffer buffer_to_kill_list)
						; There might be a couple useless calls to
						; tab-update-windows-displaying-tabset-tab-lines affecting only
						; buffers to be killed, but that's acceptable I guess.
						; I could have some let-bounded var to not do anything
						; in tab-update-windows-displaying-tabset-tab-lines to prevent
						; that.
						(replace-buffer-in-windows buffer new_buffer))
					(redisplay)
					(dolist (buffer buffer_to_kill_list)
						(with-current-buffer buffer (save_buffer))
						(unless (kill-buffer buffer)
							(tab-activate buffer))))))
		(unless other (tab-kill-buffer nil arg))))

(defun tab-kill-other-buffers-in-tabset (arg)
"Save and kill other buffers in current buffer's tabset.
With prefix arg don't save, just kill-buffer."
	(interactive "P")
	(tab-kill-all-buffers-in-tabset t arg))

(defun tab-forward-tabset (n) (interactive "p")
	(when (/= n 0)
		(let ((tabset_name (tab-buffer-tabset-name (current-buffer))))
			(unless tabset_name
				(if (> n 0) (-- n) (++ n))
				(if (or (= n 0) (not (cdr tab-tabset-list)))
					(progn
						(setq n 0)
						(switch-to-buffer (tab-most-recent-non-excluded-buffer) nil t))
					(setq tabset_name
						(tab-buffer-tabset-name
							(tab-most-recent-non-excluded-buffer)))))
			(when (/= n 0)
				; If there is more than one tabset.
				(if (cdr tab-tabset-list)
					(switch-to-buffer
						(tab-most-recent-buffer-in-tabset
							(nth
								; Calculate index.
								; mod instead of % because it's always in range
								; [0, (length tab-tabset-list)), so for example
								; (mod -7 10) = 3 and (% -7 10) = -7.
								(mod
									(+
										(index_in_list_eq
											tab-tabset-list (tab-tabset tabset_name))
										n)
									(length tab-tabset-list))
								tab-tabset-list))
						nil
						t)
					(message "It's the only tabset."))))))

(fset 'tab-backward-tabset (get_reverse_command #'tab-forward-tabset))

(defun tab-forward-tab (n) (interactive "p")
	(when (/= n 0)
		(let ((tabset_name (tab-buffer-tabset-name (current-buffer))))
			; If current buffer is excluded, select last shown buffer.
			(unless tabset_name
				(if (> n 0) (-- n) (++ n))
				(if (or (= n 0) (not (cdr tab-tabset-list)))
					(progn
						(setq n 0)
						(switch-to-buffer (tab-most-recent-non-excluded-buffer) nil t))
					(setq tabset_name
						(tab-buffer-tabset-name
							(tab-most-recent-non-excluded-buffer)))))
			(when (/= n 0)
				(switch-to-buffer
					(tab-tab-buffer
						(let ((tab_list (cdr (tab-tabset tabset_name))))
							(nth
								; Calculate index.
								(max
									0
									(min
										(+
											(index_in_list tab_list
												(lambda (tab)
													(eq
														(tab-tab-buffer tab)
														(current-buffer))))
											n)
										(1- (length tab_list))))
								tab_list)))
					nil
					t)))))

(fset 'tab-backward-tab (get_reverse_command #'tab-forward-tab))

(defunWithBase
	(lambda (get_tab_to_switch_to_fn)
		(switch-to-buffer
			; If current buffer is excluded, select last shown buffer.
			(if-let ((tabset_name (tab-buffer-tabset-name (current-buffer))))
				(tab-tab-buffer
					(funcall get_tab_to_switch_to_fn
						(cdr (tab-tabset tabset_name))))
				(tab-most-recent-non-excluded-buffer))
			nil
			t))

	(tab-first-tab () "Switch to first tab in current tabset." (interactive)
		(,base #'car))
	(tab-last-tab () "Switch to last tab in current tabset." (interactive)
		(,base (lambda (tab_list) (car (last tab_list))))))

(defun tab-move-tab-to-right (n) "Move current tab N places right." (interactive "p")
	(when-let (
		((/= n 0))
		(tabset_name (tab-buffer-tabset-name (current-buffer)))
	)
		(let* ((tabset (tab-tabset tabset_name)) (tab_list (cdr tabset)))
			(when (cdr tab_list)
				(let* (
					(index -1)
					(tab
						(find_in_list tab_list
							(lambda (tab)
								(++ index)
								(eq (tab-tab-buffer tab) (current-buffer)))))
					new_index
				)
					(if (= index 0)
						(setq tab_list (cdr tab_list))
						(delete_from_list_by_index tab_list index))
					(setq new_index (max 0 (min (+ index n) (length tab_list))))
					(if (= new_index 0)
						(setq tab_list (cons tab tab_list))
						(insert_in_list_by_index tab_list new_index tab))
					(setcdr tabset tab_list)
					(when (/= index new_index)
						(tab-update-windows-displaying-tabset-tab-lines
							tabset_name)))))))

(fset 'tab-move-tab-to-left (get_reverse_command #'tab-move-tab-to-right))

(defunWithBase
	(lambda (fn)
		(when-let ((buffer_tabset_name (tab-buffer-tabset-name (current-buffer))))
			(let (
				(tabset (tab-tabset buffer_tabset_name))
				current_buffer_tab
				current_buffer_new_tab_list
			)
				; Create tab list without current tab and find current tab and save it
				; for later.
				(dolist (tabset_tab (cdr tabset))
					(if (eq (current-buffer) (tab-tab-buffer tabset_tab))
						(setq current_buffer_tab tabset_tab)
						(push tabset_tab current_buffer_new_tab_list)))
				(funcall fn)
				(setcdr tabset current_buffer_new_tab_list)
				(tab-update-windows-displaying-tabset-tab-lines (car tabset)))))

	(tab-move-tab-to-start () "Move current tab to the first position." (interactive)
		(,base
			(lambda ()
				(setq current_buffer_new_tab_list (nreverse current_buffer_new_tab_list))
				(push current_buffer_tab current_buffer_new_tab_list))))

	(tab-move-tab-to-end () "Move current tab to the last position." (interactive)
		(,base
			(lambda ()
				(push current_buffer_tab current_buffer_new_tab_list)
				(setq current_buffer_new_tab_list
					(nreverse current_buffer_new_tab_list))))))

(defun tab-manual-scroll-left (n &optional window)
"Scroll WINDOW's tabset N tabs to the left.
That means show tab(s) that are now on the right side."
	(interactive "p")
	(when (/= n 0)
		(unless window (setq window (selected-window)))
		(let ((buffer (window-buffer window)))
			(when (tab-buffer-tabset-name buffer)
				(with-selected-window window
					(unless (tab-should-exclude-window)
						; At this point tab-tabset-name-and-scroll should always
						; be valid for window's tabset.
						(let* (
							(tabset_name_and_scroll (tab-tabset-name-and-scroll))
							(tab_list (cdr (tab-tabset (car tabset_name_and_scroll))))
							(scroll (cdr tabset_name_and_scroll))
							(new_scroll scroll)
						)
							(if (> n 0)
								(let* (
									(visible_tab_list (nthcdr scroll tab_list))
									(visible_width
										(cl-loop
											for tab in visible_tab_list
											sum (aref tab 3)))
									(window_pixel_width (window-pixel-width))
								)
									; Add left arrow width if appropriate.
									(when (/= scroll 0)
										(+= visible_width
											(cdr tab-LEFT-ARROW-ICON-AND-PIXEL-WIDTH)))
									(while
										(and
											(/= n 0)
											(> visible_width window_pixel_width)
											(progn
												(-= visible_width
													(aref (car visible_tab_list) 3))
												; If last tab is wider than the window,
												; show last tab.
												(setq visible_tab_list
													(cdr visible_tab_list))))
										; Add left arrow width if it wasn't already.
										(when (= scroll new_scroll 0)
											(+= visible_width
												(cdr tab-LEFT-ARROW-ICON-AND-PIXEL-WIDTH)))
										(++ new_scroll)
										(-- n)))
								(setq new_scroll (max 0 (+ scroll n))))
							(when (/= new_scroll scroll)
								; Save new scroll.
								(setcdr tabset_name_and_scroll new_scroll)
								; Update tab-line-format.
								(tab-set-tab-line-format
									(concat
										(when (/= new_scroll 0)
											(car tab-LEFT-ARROW-ICON-AND-PIXEL-WIDTH))
										(mapconcat
											(lambda (tab)
												(aref
													tab
													(if (eq buffer (tab-tab-buffer tab))
														2 1)))
											(nthcdr new_scroll tab_list))))))))))))

(defun tab-manual-scroll-right (n &optional window) (interactive "p")
	(tab-manual-scroll-left (- n) window))

; Helm interface

(defun tab-helm-candidates ()
"Return list (display . tabset_name).
If `helm-read-tabset-current-tabset-name' is non-nil, then don't include tabset
saved there.
Source should set :match-part to non-nil.
Display string has format \"tabset_name tab_count propertized_tab_list\"."
	(when-let (
		(tabset_list
			(if helm-read-tabset-current-tabset-name
				(cl-delete-if
					(lambda (tabset)
						(string= helm-read-tabset-current-tabset-name (car tabset)))
					(copy-sequence tab-tabset-list))
				(copy-sequence tab-tabset-list)))
	)
		(let (
			tabset_name_max_length
			tab_count_str_max_length
			tab_count_str_list
		)
			(cl-loop
				for tabset in tabset_list
				maximize (length (car tabset)) into temp_tabset_name_max_length
				for tabset_count_str = (number-to-string (length (cdr tabset)))
				maximize (length tabset_count_str) into temp_tab_count_str_max_length
				collect tabset_count_str into temp_tab_count_str_list
				finally
				(setq tabset_name_max_length temp_tabset_name_max_length)
				(setq tab_count_str_max_length temp_tab_count_str_max_length)
				(setq tab_count_str_list temp_tab_count_str_list))
			(map_modify_list
				(lambda (tabset)
					(let ((tabset_name (car tabset)))
						(cons
							(let (
								(tab_list (cdr tabset))
								(tab_count_str (pop tab_count_str_list))
								(most_recent_buffer_in_tabset
									(tab-most-recent-buffer-in-tabset tabset))
							)
								(propertize_no_copy
									(concat
										tabset_name
										(get_space_string
											(1+ (- tabset_name_max_length (length tabset_name))))
										tab_count_str
										(get_space_string
											(+ (- tab_count_str_max_length (length tab_count_str)) 3))
										(mapconcat
											(lambda (tab)
												(let (index face)
													; If tab is the selected tab in this tabset.
													(if
														(eq
															most_recent_buffer_in_tabset
															(tab-tab-buffer tab))
														(setq index 2 face 'tab-selected-tab-face)
														(setq index 1 face 'tab-unselected-tab-face))
													(setq face (list face 'tab-line-active))
													(let ((str (copy-sequence (aref tab index))))
														; Clear mainly keymap prop - we don't want
														; users to accidentally invoke some command
														; from there.
														(remove-text-properties 0 (length str)
															'(keymap nil mouse-face nil font-lock-face)
															str)
														; Add base face.
														(add-face-text-property 0 (length str)
															'tab-line-active t str)
														str)))
											tab_list))
									'match-part (list (cons 0 (length tabset_name)))))
							tabset_name)))
				tabset_list))))

; Read tabset(s)

; Based on helm-read-buffer.

(defvar helm-read-tabset-mode-line nil)
(defvar helm-read-tabset-current-tabset-name nil)

(defconst helm-read-tabset-source
	(helm-source-sync-make nil
		:candidates #'tab-helm-candidates
		:match-part t
		:mode-line (lambda () helm-read-tabset-mode-line)))

(defun helm-read-tabset-candidate-transformer (candidates)
	(if (string= helm-pattern helm-read-tabset-current-tabset-name)
		candidates
		(helm-maybe-add-new-candidate candidates)))

(cl-defun helm-read-tabset
	(&key
		no-current
		marked-candidates
		(must-match 'confirm-new)
		(prompt helm-prompt)
		input
		default
		preselect
		history)
"Return tabset(s) name(s).

MUST-MATCH can be:
	t - strict match - only existing tabsets
	nil - existing or not, no confirmation
	\\='confirm-new - confirm non-existing tabsets
	\\='confirm-existing - confirm existing tabsets
	function with 2 args - candidate in (display . tabset_name) form and action.

PRESELECT can be a tabset name or t to preselect current tabset, if any.

NO-CURRENT non-nil means to exclude current tabset from candidates and also
don't allow it as a new candidate if applicable."
	; Interactive just to browse tabsets.
	; There could be a separate helm command for that with some actions like
	; switch to tabset, delete tabset, etc. but that's pretty much all covered
	; by helm-buffers, so it's much more practical to just use that.
	; But still if someone wants to see the state of this tab line system,
	; it's possible to do execute-extended-command -> helm-read-tabset.
	(interactive)
	(setq helm-read-tabset-current-tabset-name nil)
	(let (action nomark)
		(if marked-candidates
			(setq action (lambda (_) (helm-marked-candidates)))
			(setq action #'identity)
			(setq nomark t))
		(setf (helm-source-action helm-read-tabset-source) action)
		(setf (helm-source-nomark helm-read-tabset-source) nomark))
	(let (name confirm candidate_transformer)
		(if (eq must-match t)
			(setq name "existing ")
			(setq candidate_transformer
				(if
					(and
						no-current
						(setq helm-read-tabset-current-tabset-name
							(tab-buffer-tabset-name (current-buffer))))
					#'helm-read-tabset-candidate-transformer
					#'helm-maybe-add-new-candidate))
			(when must-match
				(setq confirm
					(cond
						((eq must-match 'confirm-new) t)
						((eq must-match 'confirm-existing)
							(lambda (cand _action)
								(unless (get-text-property 0 'helm-new (car cand))
									"confirm override")))
						(t must-match)))))
		(setf (helm-source-confirm helm-read-tabset-source) confirm)
		(setf (helm-source-candidate-transformer helm-read-tabset-source)
			candidate_transformer)
		(setq name
			(concat
				"Read "
				(when no-current "other ")
				name
				"tabset"
				(when marked-candidates "s")))
		(setq helm-read-tabset-mode-line (concat "  " name))
		(setf (helm-source-name helm-read-tabset-source) name))
	(when (eq preselect t)
		(setq preselect
			(or
				helm-read-tabset-current-tabset-name
				(tab-buffer-tabset-name (current-buffer)))))
	(when preselect (setq preselect (concat "^" (regexp-quote preselect))))
	(helm
		:sources (list helm-read-tabset-source)
		:prompt prompt
		:input input
		:default default
		:preselect preselect
		:history history
		:allow-nest t
		:resume 'noresume))

; Saving tabsets and tabs in desktop file.

(defvar tab-desktop-saved-state nil)

(desktop::add_to_globals 'tab-desktop-saved-state)

(add-hook 'desktop::save_hook
	(fn_symbol "tab-save-tabsets-and-tabs"
		(lambda ()
			(setq tab-desktop-saved-state
				(mapcar
					(lambda (tabset)
						(cons
							(car tabset) ; Tabset name.
							(mapcar ; List of buffer names.
								(lambda (tab) (buffer-name (tab-tab-buffer tab)))
								(cdr tabset))))
					tab-tabset-list))
			nil)))

; Initialization

(add-hook 'emacs-startup-hook
	(fn_symbol "tab-initialize"
		(lambda ()
			; Add hooks now, because it's faster to load buffers that are now in their
			; final form, than to load every buffer separately and later update
			; most of them when they change their major modes.
			(advice-add 'replace-buffer-in-windows :override
				#'tab-replace-buffer-in-windows-hook)
			(advice-add 'switch-to-buffer :override #'tab-switch-to-buffer-hook)
			(advice-add 'pop-to-buffer :after #'tab-pop-to-buffer-hook)
			(advice-add 'rename-buffer :filter-return #'tab-rename-buffer-hook)
			(add-hook 'after-change-major-mode-hook #'tab-after-change-major-mode-hook)

			; Create tabsets and tabs.

			; Firstly, as a special case, create tabset and tab for messages buffer.
			(let ((messages_buffer (get-buffer messages-buffer-name)))
				(setcdr
					(setq tab-MESSAGES-TABSET
						(tab-create-tabset
							(tab-set-buffer-tabset-name messages_buffer)))
					(list (tab-create-tab messages_buffer)))
				(tab-add-window-size-change-hook messages_buffer))

			; Tabsets and tabs in tab-desktop-saved-state have priority,
			; because they have well defined position (from left to right on tab-line),
			; and can have overriden exclusion and manually changed tabset.
			; Also don't act on messages buffer, because it should always
			; be in it's default tabset.
			(dolist (saved_tabset tab-desktop-saved-state)
				(let (
					(tabset
						(if (string= (car tab-MESSAGES-TABSET) (car saved_tabset))
							tab-MESSAGES-TABSET
							(tab-create-tabset (car saved_tabset))))
				)
					(dolist (buffer_name (cdr saved_tabset))
						(when-let (
							((not (string= buffer_name messages-buffer-name)))
							(buffer (get-buffer buffer_name))
						)
							(tab-add-window-size-change-hook buffer)
							; Override exclusion if necessary.
							(when (tab-excluded buffer) (tab-force-buffer buffer t))
							(tab-set-buffer-tabset-name buffer (car tabset))
							(nconc tabset (list (tab-create-tab buffer)))))
					; If all buffers in saved_tabset don't exist, delete tabset.
					(unless (cdr tabset) (tab-delete-tabset tabset))))

			; Append the rest.
			(dolist (buffer (buffer-list))
				; Unless buffer should be excluded or it has already been added.
				(unless (or (tab-buffer-tabset-name buffer) (tab-excluded buffer))
					(tab-add-window-size-change-hook buffer)
					(nconc
						(let ((buffer_tabset_name (tab-set-buffer-tabset-name buffer)))
							(or
								(tab-tabset buffer_tabset_name)
								(tab-create-tabset buffer_tabset_name)))
						(list (tab-create-tab buffer)))))

			; Also call after_move_hook in every window on startup.
			; Again, this isn't the place for this.
			(dolist (window (window-list-1 nil 0 t))
				(with-selected-window window (funcall after_move_hook_fn)))

			nil)))

(provide 'myTab)
