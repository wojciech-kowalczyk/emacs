; -*- lexical-binding:nil -*-

(require 'cl-lib)
(require 'color)
(require 'help-mode)
(require 'isearch)

(defconst INIT_DIR (file-name-directory user-init-file)
"Directory of emacs init file.")
(defconst LIBS_DIR (concat INIT_DIR "libs/")
"Directory containing many init.el parts. Just INIT_DIR/libs/.")
(push (substring-no-properties LIBS_DIR 0 -1) load-path)

(require 'myUtility)

; Get rid of that annoying message (also now don't support passing prefix-arg
; to a mouse/drag-mouse command after down-mouse command, as I don't use this now,
; but maybe it should be supported in the future).
(fset 'undefined #'ignore)

; ?\C-\[ = ?\e = ?\M, so, to my knowledge, it's impossible to use
; meta modifier key while escape or ctrl-[ is bound, without changing C code.
; Fortunately only meta modifier is done this bad, any other modifier doesn't have this problem.
(setq w32-alt-is-meta nil)
; Windows doesn't support it, plus it probably slows rectangle::mode,
; because when this is non-nil, then rectangle::extract_region is called after every command.
(setq select-active-regions nil)

; I don't use mark-ring, I use jumpHistory instead.
(setq global-mark-ring-max 0)
(setq mark-ring-max 0)

(setq shift-select-mode nil)

(setq undo-limit 1000000)
(setq undo-strong-limit (* undo-limit 2))

(setq-default show-trailing-whitespace t)
(setq-default word-wrap t)

(setplist 'erase-buffer nil) ; Remove 'disable t.

(setq make-cursor-line-fully-visible nil)

; Don't echo prefix args, they are wrong anyway (always show C-u for some reason).
(setq echo-keystrokes nil)

(setq input-method-verbose-flag nil)

; Disable this crap blank space at the end of tooltips.
(setq w32-tooltip-extra-pixels 0)

(setq inhibit-startup-screen t)
(fset 'display-startup-echo-area-message #'ignore)

(setq highlight-nonselected-windows t)
(setq window-resize-pixelwise t)
(setq fit-window-to-buffer-horizontally t)

(setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows 'hollow)
(setq cursor-type-over-image 'hollow)
(setq cursor-color "#BDBDBD")
(setq blink-cursor-alist nil)
(setq blink-cursor-delay 1.5)

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-trailing-separator-p t)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

; Remove continuation fringe indicators.
(setq-default fringe-indicator-alist
	(assq-delete-all 'continuation fringe-indicator-alist))

(when (featurep 'ispell) (setq ispell-highlight-face 'flyspell-incorrect))

; Loading edebug with this set to non-nil will not set keybinds in emacs-lisp-mode.
(setq edebug-inhibit-emacs-lisp-mode-bindings t)

; I never used this, may be bad.
(setq register-preview-delay 0)

; Nowhere this is documented, but nil value is safe.
(setq help-char nil)
(setq help-event-list '(f1))

(setq ring-bell-function #'ignore)

(setq-default tab-width 4)

; Because some melon took reference intead of copy, to later compare
; original with reference which obviously always match...
(setq initial-frame-alist (delete '(visibility . icon) initial-frame-alist))
(push '(visibility . t) initial-frame-alist)
(push '(fullscreen . maximized) initial-frame-alist)

; Modes

(auto-encryption-mode -1)
(temp-buffer-resize-mode)

; Faces

; Default face can't use define_face (it can't have default spec defined I guess).
(face-spec-set 'default
	'((t
		:family "JBM_NL_Full_1,15"
		:background "#2B2B2B"
		:foreground "#A9B7C6"
		:height 126
		:foundry "outline"
		:inherit nil
		:extend nil
		:stipple nil
		:inverse-video nil
		:box nil
		:strike-through nil
		:overline nil
		:underline nil
		:slant normal
		:weight regular
		:width normal)))

(define_face 'border '((t :background "black" :foreground "black")))
(define_face 'region '((t :extend t :background "#1E3E80")))
(define_face 'trailing-whitespace '((t :underline t)))
(define_face 'line-number '((t :foreground "#626669")))
(define_face 'line-number-current-line '((t :foreground "#A4A3A3")))
; Maybe I should move this underline a little bit lower, as now it basically
; covers '_' char with current default face.
; Wow it turns out this is now impossible - :position only matters for :style line.
; Change it in C.
;(define_face 'error '((t :underline (:color "#FF0000" :style wave :position nil))))
;(define_face 'warning '((t :underline (:color "#BC3F3C" :style wave :position nil))))
(define_face 'error '((t :underline (:color "#BC3F3C" :style wave))))
(define_face 'warning '((t :underline (:color "#AEAE80" :style wave))))

(define_face 'font-lock-variable-name-face '((t)))
(define_face 'font-lock-function-call-face '((t)))
(define_face 'font-lock-function-name-face '((t :foreground "#FFC66D")))
(define_face 'font-lock-keyword-face '((t :foreground "#CC7832")))
(define_face 'font-lock-delimiter-face '((t :inherit font-lock-keyword-face)))
(define_face 'font-lock-punctuation-face '((t :inherit font-lock-keyword-face)))
(define_face 'font-lock-escape-face '((t :inherit font-lock-keyword-face)))
(define_face 'font-lock-bracket-face '((t)))
(define_face 'font-lock-warning-face '((t :inherit warning)))
(define_face 'font-lock-comment-face '((t :foreground "#808080")))
(define_face 'font-lock-doc-face '((t :foreground "#68AD52" :slant italic)))
(define_face 'font-lock-doc-markup-face '((t :foreground "#77B767" :slant italic)))
(define_face 'font-lock-string-face '((t :foreground "#6A8759")))
(define_face 'font-lock-number-face '((t :foreground "#6897BB")))
(define_face 'font-lock-operator-face '((t :foreground "#5F8C8A")))
(define_face 'font-lock-preprocessor-face '((t :foreground "#BBB529")))
(define_face 'font-lock-constant-face '((t :foreground "#9876AA")))
(define_face 'font-lock-type-face '((t :foreground "#B5B6E3")))

(define_face 'lazy-highlight '((t :extend t :background "#32593D")))
(define_face 'isearch '((t :inherit region)))

; ===================================== Keybindings =====================================

; setcdr instead of setq because setcdr also changes keymaps that are already used.

(setcdr input-decode-map nil)

; Another discovery - many keys are mapped in key-translation-map to chars like ¢ or ©.
; I remove them here.
; Also for some reason some keys have different values for -UPPERCASE and -\S-lowercase,
; for example ?\A-C and ?\A-\S-c. Seems like ?\A-C is proritized, or maybe even is
; the only valid event.
; Ok so I think now I know how it works - \C-x sequences must have \S,
; case of x doesn't matter; in other sequences case matters, seems like the original
; events if pressed with shift have uppercase, but if that event have no binding,
; it is changed to \S-downcased_x (maybe?, I'm not sure, maybe I'll clear it in C someday).

(setcdr key-translation-map nil)
; Right now I'm using right click to follow links/push buttons, so it's useless for me.
;(define-key key-translation-map [down-mouse-1] #'mouse--down-1-maybe-follows-link)
;(define-key key-translation-map [mouse-1] #'mouse--click-1-maybe-follows-link)
(require 'bug-reference)
(setcdr bug-reference-map nil)
(define-key bug-reference-map [C-return] #'bug-reference-push-button)
(define-key bug-reference-map [down-mouse-3] #'bug-reference-push-button)
(require 'button)
(setcdr button-buffer-map nil)
(define-key button-buffer-map [tab] #'forward-button)
(define-key button-buffer-map [S-tab] #'backward-button)
(setcdr button-map nil)
(set-keymap-parent button-map button-buffer-map)
(define-key button-map [C-return] #'push-button)
(define-key button-map [down-mouse-3] #'push-button)
(define-key button-map [mode-line down-mouse-3] #'push-button)
(define-key button-map [header-line down-mouse-3] #'push-button)
(define-key button-map [follow-link] 'mouse-face)
(require 'info)
(define-key Info-mode-map [C-return] #'Info-follow-nearest-node)
(define-key Info-mode-map [down-mouse-3] #'Info-mouse-follow-nearest-node)

; Unbind some default keybinds that break other keybinds for no reason.
(define-key global-map [?\t] nil t) ; tab
(define-key global-map [?\v] nil t) ; vertical tab
(define-key global-map [?\e] nil t) ; escape
(define-key global-map [?\r] nil t) ; enter
(define-key global-map [?\b] nil t) ; backspace
(define-key global-map [?\d] nil t) ; delete

(define-key help-mode-map [?\C-c] nil t)

; Other useless stuff from global-map.
(define-key global-map [?\C-@] nil t)
(define-key global-map [?\C-j] nil t)
(define-key global-map [?\C-n] nil t)
(define-key global-map [?\C-o] nil t)
(define-key global-map [?\C-q] nil t)
(define-key global-map [?\C-u] nil t)
(define-key global-map [?\C-y] nil t)
(define-key global-map [?\C-\]] nil t)
(define-key global-map [?\C-_] nil t)

(define-key global-map [f10] nil t)
(define-key global-map [C-f10] nil t)
(define-key global-map [paste] nil t)
(define-key global-map [copy] nil t)
(define-key global-map [cut] nil t)
(define-key global-map [f18] nil t)
(define-key global-map [f16] nil t)
(define-key global-map [f20] nil t)
(define-key global-map [C-M-mouse-5] nil t)
(define-key global-map [C-M-mouse-4] nil t)
(define-key global-map [C-M-wheel-down] nil t)
(define-key global-map [C-M-wheel-up] nil t)
(define-key global-map [M-mouse-7] nil t)
(define-key global-map [M-mouse-6] nil t)
(define-key global-map [M-mouse-5] nil t)
(define-key global-map [M-mouse-4] nil t)
(define-key global-map [M-wheel-right] nil t)
(define-key global-map [M-wheel-left] nil t)
(define-key global-map [M-wheel-down] nil t)
(define-key global-map [M-wheel-up] nil t)
(define-key global-map [S-wheel-right] nil t)
(define-key global-map [S-wheel-left] nil t)
(define-key global-map [wheel-right] nil t)
(define-key global-map [wheel-left] nil t)
(define-key global-map [C-M-mouse-1] nil t)
(define-key global-map [C-M-drag-mouse-1] nil t)
(define-key global-map [C-M-down-mouse-1] nil t)
(define-key global-map [M-mouse-2] nil t)
(define-key global-map [M-mouse-3] nil t)
(define-key global-map [M-down-mouse-1] nil t)
(define-key global-map [M-drag-mouse-1] nil t)
(define-key global-map [M-mouse-1] nil t)
(define-key global-map [S-f10] nil t)
(define-key global-map [M-f10] nil t)
(define-key global-map [compose-last-chars] nil t)
(define-key global-map [help] nil t)
(define-key global-map [f4] nil t)
(define-key global-map [f3] nil t)
(define-key global-map [pinch] nil t)
(define-key global-map [C-M-backspace] nil t)
(define-key global-map [C-M-delete] nil t)
(define-key global-map [C-M-end] nil t)
(define-key global-map [C-M-home] nil t)
(define-key global-map [C-M-down] nil t)
(define-key global-map [C-M-up] nil t)
(define-key global-map [C-M-right] nil t)
(define-key global-map [C-M-left] nil t)
(define-key global-map [M-left] nil t)
(define-key global-map [M-right] nil t)
(define-key global-map [mouse-movement] nil t)
(define-key global-map [touch-end] nil t)
(define-key global-map [deletechar] nil t)
(define-key global-map [deleteline] nil t)
(define-key global-map [insertline] nil t)
(define-key global-map [open] nil t)
(define-key global-map [again] nil t)
(define-key global-map [redo] nil t)
(define-key global-map [undo] nil t)
(define-key global-map [S-insertchar] nil t)
(define-key global-map [C-insertchar] nil t)
(define-key global-map [insertchar] nil t)
(define-key global-map [S-insert] nil t)
(define-key global-map [C-insert] nil t)
(define-key global-map [execute] nil t)
(define-key global-map [M-begin] nil t)
(define-key global-map [begin] nil t)
(define-key global-map [M-end] nil t)
(define-key global-map [M-prior] nil t)
(define-key global-map [M-next] nil t)
(define-key global-map [C-next] nil t)
(define-key global-map [C-prior] nil t)
(define-key global-map [M-home] nil t)
(define-key global-map [scroll] nil t)
(define-key global-map [find] nil t)
(define-key global-map [menu] nil t)
(define-key global-map [XF86Back] nil t)
(define-key global-map [XF86Forward] nil t)

; There are still some things in mode-line and header-line keymap, but I'm not
; sure how useful they are yet, so I'll deal with them later.

(define-key global-map [tab-line] nil t)
(define-key global-map [left-fringe] nil t)
(define-key global-map [right-fringe] nil t)
(define-key global-map [left-margin] nil t)
(define-key global-map [right-margin] nil t)

; Clear and fill function-key-map with "better" bindings.

(setcdr function-key-map nil)

; Translate keypad keys.
(let (
	(fn
		(lambda (str binding)
			(setq str (concat "kp-" str))
			(mapc
				(lambda (key_modifier)
					(when-let (
						(new_binding
							(if (integerp binding)
								; Omit shift bindings for chars, because they don't have
								; them - there is no way to type S-*, S-/, etc.
								(unless (string-search "S" key_modifier)
									(key-parse
										(concat key_modifier (char-to-string binding))))
								(vector
									(intern
										(concat key_modifier (symbol-name binding))))))
					)
						(define-key
							function-key-map
							(vector (intern (concat key_modifier str)))
							new_binding)))
				KEY_MODIFIER_VECTOR)))
)
	(dotimes (i 10) (funcall fn (number-to-string i) (+ i ?0)))
	(funcall fn "divide" ?/)
	(funcall fn "multiply" ?*)
	(funcall fn "subtract" ?-)
	(funcall fn "add" ?+)
	(funcall fn "decimal" ?.)
	(funcall fn "enter" 'return)
	(dolist (event '(delete insert home end prior next left right up down))
		(funcall fn (symbol-name event) event)))

; Strip fringes from mouse commands.
(let (
	(keymap (make-sparse-keymap))
	(binding (lambda (_) (substring (this-single-command-keys) 1)))
	(bind
		(lambda (symbol_name)
			(define-key keymap (vector (intern symbol_name)) binding)))
)
	(mapc
		(lambda (key_modifier)
			(dolist (multi_modifier '("" "double-" "triple-"))
				; No "down" because it seems like it doesn't work -
				; if a prefixed event like [left-fringe down-mouse-1] doesn't have
				; a binding without any (well, idk about input-decode-map) remapping,
				; then no remapping is even tried.
				; I think than this only works like that with down-mouse events.
				(dolist (type_modifier '("" "drag-"))
					; Normally used mouse-i are [1, 7].
					(dotimes (i 7)
						(funcall bind
							(concat
								key_modifier
								multi_modifier
								type_modifier
								"mouse-"
								(number-to-string (1+ i))))))
				(dolist (type_modifier '("up" "down"))
					(funcall bind
						(concat
							key_modifier
							multi_modifier
							"wheel-"
							type_modifier)))))
		KEY_MODIFIER_VECTOR)
	(bind_many_keys function-key-map [[left-fringe] [right-fringe]] keymap))

; Also deal with local-function-key-map.
(setcdr local-function-key-map nil)
(set-keymap-parent local-function-key-map function-key-map)

; Clean global-map from all mouse bindings.
; When I'll build my emacs, all places adding mouse bindings to global-map should
; be deleted, so it won't be necessary.
(mapc
	(lambda (key_modifier)
		(dolist (multi_modifier '("" "double-" "triple-"))
			(dolist (type_modifier '("" "down-" "drag-"))
				; Normally used mouse-i are [1, 7].
				(dotimes (i 7)
					; Turns out (define-key keymap key nil t) adds binding nil if there
					; is no binding (idk why).
					(let (
						(key
							(intern
								(concat
									key_modifier
									multi_modifier
									type_modifier
									"mouse-"
									(number-to-string (1+ i)))))
					)
						(when (assq key (nthcdr 2 global-map))
							(define-key global-map (vector key) nil t)))))
			(dolist (type_modifier '("up" "down"))
				(let (
					(key
						(intern
							(concat
								key_modifier
								multi_modifier
								"wheel-"
								type_modifier)))
				)
					(when (assq key (nthcdr 2 global-map))
						(define-key global-map (vector key) nil t))))))
	KEY_MODIFIER_VECTOR)

; Clear some annoying maps (temporary, they will be filled with some good
; bindings someday).

(setcdr special-mode-map nil)
(setcdr messages-buffer-mode-map nil)
(setcdr prog-mode-map nil)
(setcdr lisp-mode-shared-map nil)
(setcdr lisp-mode-map nil)
(setcdr lisp-data-mode-map nil)
(setcdr emacs-lisp-mode-map nil)
(setcdr lisp-interaction-mode-map nil)
(setcdr text-mode-map nil)

; For now remove ispell-completion-at-point from completion-at-point-functions
; because it errors every time it's called.
(add-hook 'text-mode-hook
	(fn_symbol "disable_ispell_in_text_mode"
		(lambda ()
			(remove-hook 'completion-at-point-functions 'ispell-completion-at-point t))))


; Windows drives info.
(when (eq system-type 'windows-nt)
	(cl-defstruct (w32-drive-info (:constructor w32-drive-info-make) (:copier nil))
		(char nil :type char :documentation "Drive letter.")
		(size nil :type integer :documentation "Size of drive.")
		(free-space nil :type integer :documentation "Free space left.")
		(volume-name nil
			:type string
			:documentation
"Long name, like \"System\".
May be an empty string (or maybe not? who knows)."))

	(defvar w32-drive-info-list)

	(defun w32-drive-info-list-set ()
		(setq w32-drive-info-list nil)
		(withTempBuffer
			(call-process
				"wmic" nil t nil "logicaldisk" "get" "FreeSpace,Name,Size,VolumeName")
			; Output is formatted like this (order of arguments in process call doesn't
			; change anything):
			; FreeSpace Name Size VolumeName\r
			; 000000    C:   000  System\r
			; \r\n
			; Where \r and \n are special.
			; I have no idea why there are \r chars, but it seems
			; like this program indeed ends it's output lines with "\r\r\n",
			; so after eol conversion it's "\r\n". Idk why it does that.
			(goto-char 1)
			(while (progn (forward-line 1) (/= (following-char) ?\r))
				(push
					(let (
						(get_str_and_move
							(lambda ()
								(prog1
									(buffer-substring-no-properties
										(point)
										(progn (skip-chars-forward "^ ") (point)))
									(skip-chars-forward " "))))
						free-space char size volume-name
					)
						(setq free-space (string-to-number (funcall get_str_and_move)))
						(setq char (following-char))
						(forward-char 2)
						(skip-chars-forward " ")
						(setq size (string-to-number (funcall get_str_and_move)))
						(setq volume-name
							; I'm not sure if every drive must have VolumeName.
							(if (= (following-char) ?\r) "" (funcall get_str_and_move)))
						(w32-drive-info-make
							:char char
							:size size
							:free-space free-space
							:volume-name volume-name))
					w32-drive-info-list)))
		(setq w32-drive-info-list (nreverse w32-drive-info-list)))

	(w32-drive-info-list-set))

(push (concat LIBS_DIR "all-the-icons") load-path)
(require 'all-the-icons)

(require 'pcre2el)

; Contains this giant variable with file name regexes and associated modes.
(require 'myFileAssociations)
(require 'myDesktop)
(require 'myModeLine)
(require 'myMultipleCursors)
(require 'myMovement)
(require 'myMouse)
(push (concat LIBS_DIR "helm") load-path)
(require 'helm)
(require 'myTab)
(require 'myWindows)
(require 'myComment)
(require 'myRegion)
(require 'myJumpHistory)
(require 'myExpandRegion)
(require 'myRectangle)
(require 'myScroll)
(require 'myDuplicate)
(require 'myClipboard)
(require 'myInsert)
(require 'myHighlight)
(require 'myDelete)
(require 'myIndent)
(require 'myMoveLine)
(require 'mySearchReplace)
(require 'myUndo)
(require 'myMisc)
(require 'myShell)
(require 'myCompletion)
(require 'myC)

(setq open-paren-in-column-0-is-defun-start nil)

; Show all props.
(setq describe-char-unidata-list t)

; It's used in some places by default anyway, make minibuffer messages more visible.
; This only matters when in the minibuffer.
(setq minibuffer-message-properties '(face minibuffer-prompt))

; Don't do this crap - e.g. sometimes when in eval-expression I want to switch
; buffer for some reference, then when exiting or quitting, this kicks in.
(setq read-minibuffer-restore-windows nil)

; Set this as the default coding system.
; AFAIK this is used when the final result of choosing a coding system
; is 'undecided, then emacs proceeds to try to guess what coding to use
; based on file contents. Category of the coding system below will
; be called first and on most files it will match. I guess if the coding
; system assigned to this category have unspecified eol, then another check
; is done to determine eols to use. utf-8-dos always enforces \r\n, never
; checks for eols.
; I'm still not sure exactly how it works because of how save-buffer works when
; buffer-file-name has been changed (it may or may not change buffer-file-coding-system
; which is more important than any coding category).
(set-coding-system-priority 'utf-8-dos)

; Default value is iso-latin-2-dos, so change it.
; This is used only for new buffers I think.
(setq-default buffer-file-coding-system 'utf-8-dos)

; Default value of file-coding-system-alist is this:
; ("\\.tzst\\'" no-conversion . no-conversion)
; ("\\.zst\\'" no-conversion . no-conversion)
; ("\\.dz\\'" no-conversion . no-conversion)
; ("\\.txz\\'" no-conversion . no-conversion)
; ("\\.xz\\'" no-conversion . no-conversion)
; ("\\.lzma\\'" no-conversion . no-conversion)
; ("\\.lz\\'" no-conversion . no-conversion)
; ("\\.g?z\\'" no-conversion . no-conversion)
; ("\\.\\(?:tgz\\|svgz\\|sifz\\)\\'" no-conversion . no-conversion)
; ("\\.tbz2?\\'" no-conversion . no-conversion)
; ("\\.bz2\\'" no-conversion . no-conversion)
; ("\\.Z\\'" no-conversion . no-conversion)
; ("\\.elc\\'" . utf-8-emacs)
; ("\\.el\\'" . prefer-utf-8)
; ("\\.utf\\(-8\\)?\\'" . utf-8)
; ("\\.xml\\'" . xml-find-file-coding-system)
; ("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix)
; ("\\.tar\\'" no-conversion . no-conversion)
; ("\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system)
; ("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'" . latexenc-find-file-coding-system)
; ("" undecided)
; Delete some entries I think are useless.
(setq file-coding-system-alist (assoc-delete-all "\\.el\\'" file-coding-system-alist))
(setq file-coding-system-alist (assoc-delete-all "\\.elc\\'" file-coding-system-alist))
(setq file-coding-system-alist
	(assoc-delete-all "\\(\\`\\|/\\)loaddefs.el\\'" file-coding-system-alist))

(setq history-delete-duplicates t)

(setq delete-by-moving-to-trash t)

(setq dired-create-destination-dirs 'always)

; Allow minibuffer and echo-area to shrink,
; because sometimes when I accidentally paste something with \n to minibuffer,
; then I need to exit and reenter it, instead of just deleting what I want.
(setq resize-mini-windows t)

; Because someone forgot to use :inherit attribute (I guess).
(define_face 'tooltip
	`((t
		:family "Segoe UI Short"
		:foreground ,(face-attribute 'default :foreground nil t)
		:background ,(face-attribute 'default :background nil t))))

; Measuring runtime of commands.
;(defvar command_time nil)
;(defun before_command_time_hook () (setq command_time (current-time)))
;(defun after_command_time_hook ()
;	(let ((time_passed (float-time (time-subtract nil command_time))))
;		(when (>= time_passed 0.001)
;			(message "command: %s, time: %s, keys: %s"
;				(if (symbolp this-command)
;					this-command
;					"lambda")
;				time_passed
;				(this-command-keys)))))
;(add-hook 'pre-command-hook 'before_command_time_hook -99)
;(add-hook 'post-command-hook 'after_command_time_hook 99)

; this-command-keys and alike testing.
;(defvar asd 0)
;(defun asd_1 ()
;	(let* (
;		(list
;			(list
;				(cons "raw" (copy-sequence (this-single-command-raw-keys)))
;				(cons "single" (copy-sequence  (this-single-command-keys)))
;				(cons "keys" (copy-sequence (this-command-keys)))
;				(cons "vector" (copy-sequence (this-command-keys-vector)))))
;		(list_1 (delete-dups (mapcar #'cdr list)))
;	)
;		(map_modify_list `(lambda (i) (rassoc i ',list)) list_1)
;		(message "%S %S %s"
;			asd
;			(if (consp this-command) "lambda" this-command)
;			(mapconcat
;				(lambda (i) (format "%s: %S" (car i) (cdr i)))
;				list_1
;				"\n"))))
;(defun asd_2 ()
;	(let* (
;		(list
;			(list
;				(cons "raw" (copy-sequence (this-single-command-raw-keys)))
;				(cons "single" (copy-sequence (this-single-command-keys)))
;				(cons "keys" (copy-sequence (this-command-keys)))
;				(cons "vector" (copy-sequence (this-command-keys-vector)))))
;		(list_1 (delete-dups (mapcar #'cdr list)))
;	)
;		(map_modify_list `(lambda (i) (rassoc i ',list)) list_1)
;		(message "%S %S %s"
;			asd
;			(if (consp this-command) "lambda" this-command)
;			(mapconcat
;				(lambda (i) (format "%s: %S" (car i) (cdr i)))
;				list_1
;				"\n")))
;	(++ asd))
;
;(add-hook 'pre-command-hook #'asd_1)
;(add-hook 'post-command-hook #'asd_2)
;
;(defun asd_3 () (interactive) (error "asd"))
;(define-key global-map [?\A-j] #'asd_3)

; Remove all keybindings that use meta modifier, as they break ?\C-\[ keybinding.
;(let ((recursive_map_keymap (make-symbol "")))
;	(set recursive_map_keymap
;		`(lambda (key def)
;			(cond
;				((keymapp def)
;					(let ((keymap def))
;						(map-keymap ,recursive_map_keymap def)))
;				((memq 'meta (event-modifiers key))
;					(define-key keymap (vector key) nil t)))))
;	(mapc
;		`(lambda (keymap)
;			(define-key keymap [?\e] nil t)
;			(map-keymap ,recursive_map_keymap keymap))
;		(vector
;			global-map
;			lisp-mode-map
;			emacs-lisp-mode-map
;			lisp-interaction-mode-map
;			help-mode-map)))
