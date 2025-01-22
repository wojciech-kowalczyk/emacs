; -*- lexical-binding:nil -*-

(declare-function markdown-show-entry "ext:markdown-mode.el")
(declare-function outline-show-subtree "outline")
(declare-function org-reveal "org")
(declare-function hs-show-all "hideshow.el")

(defvar hs-minor-mode)
(defvar hs-show-hook)
(defvar winner-boring-buffers)

(defconst helm-su-or-sudo "sudo"
"What command to use for root access - a string.")

(defconst helm-default-kbsize 1024.0
"Default Kbsize to use for showing files size.
It is a float, usually 1024.0 but could be 1000.0 on some systems.")

(defconst helm-highlight-matches-around-point-max-lines '(18 . 18)
"Number of lines around point where matched items are highlighted.
A cons cell (x . y) - match x lines before point and y lines after point.
(0 . 0) = only match on current line.
Probably best to set this to cons with half of `window-screen-lines' in full-screen.")

(define_face 'helm-match-item '((t :inherit lazy-highlight))
"Face used to highlight matched items.")

; Utils functions

(defconst helm-window-show-buffers-function #'helm-window-decide-split-fn
"The default function to use when opening several buffers at once.
It is typically used to rearrange windows.
Options:
	helm-window-decide-split-fn - Decide how to split according to number of candidates
	helm-window-default-split-fn - Split windows vertically or horizontally
	helm-window-alternate-split-fn - Split in alternate windows
	helm-window-mosaic-fn - Split windows in mosaic.")

(defun helm-window-show-buffers (buffers &optional other-window)
"Show BUFFERS.

With more than one buffer marked switch to these buffers in separate windows.
If OTHER-WINDOW is non-nil, keep current buffer and switch to other buffers
in separate windows.

Prefix arg can matter."
	(let (
		(initial-ow-fn
			(if (one-window-p t)
				#'helm-window-other-window
				#'switch-to-buffer-other-window))
	)
		(cond
			((cdr buffers)
				(funcall helm-window-show-buffers-function buffers
					(and other-window initial-ow-fn)))
			(other-window (funcall initial-ow-fn (car buffers)))
			(t (switch-to-buffer (car buffers))))))

(defun helm-window-decide-split-fn (candidates &optional other-window-fn)
"Try to find the best split window fn according to the number of CANDIDATES.

This function is suitable for `helm-window-show-buffers-function'."
	(funcall
		(cond
			((length> candidates 3) #'helm-window-mosaic-fn)
			((length= candidates 3) #'helm-window-alternate-split-fn)
			(t #'helm-window-default-split-fn))
		candidates other-window-fn))

(defun helm-window-default-split-fn (candidates &optional other-window-fn)
"Split windows vertically (with prefix horizontally) and balance them.

This function is suitable for `helm-window-show-buffers-function'."
	(funcall (or other-window-fn #'switch-to-buffer) (car candidates))
	(let ((vertical (not helm-current-prefix-arg)))
		(save-selected-window
			(cl-loop
				for buffer in (cdr candidates)
				while
					(condition-case nil
						(progn
							(select-window (split-window nil nil vertical))
							(balance-windows)
							(switch-to-buffer buffer)
							t)
						(error
							(message "Too many buffers to visit simultaneously.")
							nil))))))

(defun helm-window-alternate-split-fn (candidates &optional other-window-fn)
"Split windows vertically and horizontally in alternate fashion.

With prefix arg start with horizontal split.

This function is suitable for `helm-window-show-buffers-function'."
	(funcall (or other-window-fn #'switch-to-buffer) (car candidates))
	(let ((vertical (not helm-current-prefix-arg)))
		(save-selected-window
			(cl-loop
				for buffer in (cdr candidates)
				while
					(condition-case nil
						(progn
							(select-window (split-window nil nil vertical))
							(switch-to-buffer buffer)
							(setq vertical (not vertical))
							t)
						(error
							(message "Too many buffers to visit simultaneously.")
							nil))))))

(defun helm-window-mosaic-fn (candidates &optional other-window-fn)
"Make an as-square-as-possible window mosaic of the CANDIDATES buffers.

If rectangular, the long side is in the direction given by `split-width-threshold'.
If OTHER-WINDOW-FN is non-nil, current windows are included in the mosaic.

This function is suitable for `helm-window-show-buffers-function'."
	(when other-window-fn
		(setq candidates (nconc (mapcar #'window-buffer (window-list)) candidates)))
	(delete-other-windows)
	(let* (
		(prefer-horizontal-split
			(and
				(numberp split-width-threshold)
				(>= (window-body-width) split-width-threshold)))
		mosaic-length-tile-count
		mosaic-width-tile-count
		mosaic-length-tile-size
		mosaic-width-tile-size
		next-window
	)
		; If 4 tiles, make 2x2 mosaic.
		; If 5-6 tiles, make 2x3 mosaic with direction depending on
		; `prefer-horizontal-split'.
		; If 7-9 tiles, make 3x3 mosaic. And so on.
		(setq mosaic-length-tile-count (ceiling (sqrt (length candidates))))
		(setq mosaic-width-tile-count
			(if (length> candidates (* mosaic-length-tile-count (1- mosaic-length-tile-count)))
				mosaic-length-tile-count
				(1- mosaic-length-tile-count)))
		; We lower-bound the tile size, otherwise the function would
		; fail during the first inner split.
		; There is consequently no need to check for errors when splitting.
		(let (
			(frame-mosaic-length-direction-size (frame-height))
			(frame-mosaic-width-direction-size (frame-width))
			(window-mosaic-length-direction-min-size window-min-height)
			(window-mosaic-width-direction-min-size window-min-width)
		)
			(when prefer-horizontal-split
				(setq
					frame-mosaic-length-direction-size (frame-width)
					frame-mosaic-width-direction-size (frame-height)
					window-mosaic-length-direction-min-size window-min-width
					window-mosaic-width-direction-min-size window-min-height))
			(setq mosaic-length-tile-size
				(max
					(/ frame-mosaic-length-direction-size mosaic-length-tile-count)
					window-mosaic-length-direction-min-size))
			(setq mosaic-width-tile-size
				(max
					(/ frame-mosaic-width-direction-size mosaic-width-tile-count)
					window-mosaic-width-direction-min-size))
			; Shorten `candidates' to `max-tiles' elements.
			(let (
				(max-tiles
					(*
						(/ frame-mosaic-length-direction-size mosaic-length-tile-size)
						(/ frame-mosaic-width-direction-size mosaic-width-tile-size)))
			)
				(when (length> candidates max-tiles)
					(message "Too many buffers to visit simultaneously.")
					(setcdr (nthcdr (1- max-tiles) candidates) nil))))
		; Make the mosaic.
		(while candidates
			(when (length> candidates mosaic-length-tile-count)
				(setq next-window
					(split-window nil
						mosaic-width-tile-size
						(not prefer-horizontal-split))))
			(switch-to-buffer (pop candidates))
			(dotimes (_ (min (1- mosaic-length-tile-count) (length candidates)))
				(select-window
					(split-window nil
						mosaic-length-tile-size
						prefer-horizontal-split))
				(switch-to-buffer (pop candidates)))
			(when next-window (select-window next-window)))))

(defun helm-window-other-window (buffer-or-name)
"Switch to BUFFER-OR-NAME in other window.
1 prefix - split vertically,
2 prefix - split horizontally,
no prefix / other prefix - split-window-sensibly."
	(select-window
		(cond
			((equal helm-current-prefix-arg '(4)) (split-window nil nil 'right))
			((equal helm-current-prefix-arg '(16)) (split-window nil nil 'below))
			(t (window::split_window_sensibly))))
	(switch-to-buffer buffer-or-name))


; Add `helm-buffer' to `winner-boring-buffers' when quitting/exiting helm.
; Uncomment this if you don't want to see helm buffers
; after running winner-undo/redo.
;(require 'winner)
;(add-hook 'helm-cleanup-hook
;	(fn_symbol "helm-handle-winner-boring-buffers"
;		(lambda ()
;			(cl-pushnew helm-buffer winner-boring-buffers :test 'equal))))

; Persistent action helpers

(defun helm-file-human-size (size)
"Return a string showing SIZE of a file in human readable form.
SIZE can be an integer or a float depending on it's value.
`file-attributes' will take care of that to avoid overflow error.
Set returned string's face to `helm-ff-face'."
	(propertize_no_copy
		(cl-loop
			with char = ?B
			for i across "kMGTPEZY"
			while (>= size helm-default-kbsize)
			do
			(setq char i)
			(setq size (/ size helm-default-kbsize))
			finally return (format "%.1f%c" size char))
		'face 'helm-ff-size))


(defvar helm-highlight-current-line-overlay-list nil)

(defvar helm-highlight-current-line-cleanup-timer nil)

(defun helm-highlight-current-line-cleanup ()
"Cleanup overlays after `helm-highlight-current-line'."
	(mapc #'delete-overlay helm-highlight-current-line-overlay-list)
	(setq helm-highlight-current-line-overlay-list nil))

(defun helm-highlight-current-line-cleanup-hook ()
"Call `helm-highlight-current-line-cleanup'. Should be used in a timer or hook."
	(helm-highlight-current-line-cleanup)
	(remove-hook 'pre-command-hook #'helm-highlight-current-line-cleanup-hook)
	; If this function runs, that means timer cleanup haven't run yet (if any).
	(when helm-highlight-current-line-cleanup-timer
		(cancel-timer helm-highlight-current-line-cleanup-timer)
		(setq helm-highlight-current-line-cleanup-timer nil)))

(defun helm-highlight-current-line
	(&optional mark_first_match no_timer_cleanup no_pre_command_cleanup)
"Highlight all matching items around current line.

The number of lines around matched line where the matching items are
highlighted are defined by `helm-highlight-matches-around-point-max-lines'.

This uses helm-pattern and helm-current-source.

If MARK_FIRST_MATCH mark first highlighted matched part on current line,
if there is any.

If NO_PRE_COMMAND_CLEANUP is nil, add pre-command-hook to cleanup highlighting
overlays (don't do anything with region though).
NO_TIMER_CLEANUP is similar but run timer with 2 seconds.

At least one type of cleanup should be used."
	; Make sure that everything is cleaned up.
	(helm-highlight-current-line-cleanup-hook)
	(unless (string= helm-pattern "")
		(let (
			(start (pos-bol (- 1 (car helm-highlight-matches-around-point-max-lines))))
			(end (pos-eol (1+ (cdr helm-highlight-matches-around-point-max-lines))))
		)
			(when-let (
				(match_list
					(helm-match
						(split-string
							(buffer-substring-no-properties start end)
							"\n"
							t)))
			)
				(save-excursion
					(goto-char start)
					(dolist (match match_list)
						; Jump to non-empty line.
						(skip-chars-forward "\n")
						(unless (eq match t)
							(dolist (range match)
								(let (
									(ov
										; Non-advancing overlay.
										(make-overlay
											(+ (point) (car range))
											(+ (point) (cdr range))
											nil
											t))
								)
									(overlay-put ov 'face 'helm-match-item)
									(overlay-put ov 'priority 1)
									(push ov helm-highlight-current-line-overlay-list))))
						(forward-line 1)))
				(when-let* (
					mark_first_match
					; Mark first match on current line and delete this overlay.
					(overlay_on_current_line_list
						(cl-loop
							with bol = (pos-bol)
							with eol = (pos-eol)
							for ov in helm-highlight-current-line-overlay-list
							when (in_range_inclusive (overlay-start ov) bol eol)
								collect ov))
				)
					(let* (
						(min_ov (car overlay_on_current_line_list))
						(min (overlay-start min_ov))
					)
						(dolist (ov overlay_on_current_line_list)
							(when (> min (overlay-start ov))
								(setq min (overlay-start ov))
								(setq min_ov ov)))
						(goto-char (overlay-end min_ov))
						(set-mark min)
						(delete-overlay min_ov)
						(setq helm-highlight-current-line-overlay-list
							(delq min_ov helm-highlight-current-line-overlay-list))))
				; Maybe add cleanup hooks.
				(when helm-highlight-current-line-overlay-list
					(unless no_pre_command_cleanup
						(add-hook
							'pre-command-hook #'helm-highlight-current-line-cleanup-hook))
					(unless no_timer_cleanup
						(run-at-time 2 nil #'helm-highlight-current-line-cleanup-hook))))))
	(recenter))


(defun helm-goto-char (loc) "Go to char, revealing if necessary."
	(goto-char loc)
	(let (
		(fn
			(cond
				((eq major-mode 'org-mode) #'org-reveal)
				((and (boundp 'outline-minor-mode) outline-minor-mode)
					#'outline-show-subtree)
				((and (boundp 'hs-minor-mode) hs-minor-mode)
					#'hs-show-all)
				((and (boundp 'markdown-mode-map) (derived-mode-p 'markdown-mode))
					#'markdown-show-entry)))
		(hs-show-hook `((lambda () (goto-char ,loc))))
	)
		; outline may fail in some conditions e.g. with markdown enabled (Bug#1919).
		(when fn (funcall fn))))

(defun helm-goto-line (lineno)
	(goto-char (point-min))
	(helm-goto-char (pos-bol lineno)))

; Decode html entities

(defconst helm-html-entities-alist
	'(
		("&quot;"   . 34)   ; "
		("&gt;"     . 62)   ; >
		("&lt;"     . 60)   ; <
		("&amp;"    . 38)   ; &
		("&euro;"   . 8364) ; €
		("&Yuml;"   . 89)   ; Y
		("&iexcl;"  . 161)  ; ¡
		("&cent;"   . 162)  ; ¢
		("&pound;"  . 163)  ; £
		("&curren;" . 164)  ; ¤
		("&yen"     . 165)  ; ¥
		("&brvbar;" . 166)  ; ¦
		("&sect;"   . 167)  ; §
		("&uml;"    . 32)   ; SPC
		("&nbsp;"   . 160)  ;   (non breaking space)
		("&copy;"   . 169)  ; ©
		("&ordf;"   . 97)   ; a
		("&laquo;"  . 171)  ; «
		("&not;"    . 172)  ; ¬
		("&masr;"   . 174)  ; ®
		("&deg;"    . 176)  ; °
		("&plusmn;" . 177)  ; ±
		("&sup2;"   . 50)   ; 2
		("&sup3;"   . 51)   ; 3
		("&acute;"  . 39)   ; '
		("&micro;"  . 956)  ; μ
		("&para;"   . 182)  ; ¶
		("&middot;" . 183)  ; ·
		("&cedil;"  . 32)   ; SPC
		("&sup1;"   . 49)   ; 1
		("&ordm;"   . 111)  ; o
		("&raquo;"  . 187)  ; »
		("&frac14;" . 49)   ; 1
		("&frac12;" . 49)   ; 1
		("&frac34;" . 51)   ; 3
		("&iquest;" . 191)  ; ¿
		("&Agrave;" . 192)  ; À
		("&Aacute;" . 193)  ; Á
		("&Acirc;"  . 194)  ; Â
		("&Atilde;" . 195)  ; Ã
		("&Auml;"   . 196)  ; Ä
		("&Aring;"  . 197)  ; Å
		("&Aelig"   . 198)  ; Æ
		("&Ccedil;" . 199)  ; Ç
		("&Egrave;" . 200)  ; È
		("&Eacute;" . 201)  ; É
		("&Ecirc;"  . 202)  ; Ê
		("&Euml;"   . 203)  ; Ë
		("&Igrave;" . 204)  ; Ì
		("&Iacute;" . 205)  ; Í
		("&Icirc;"  . 206)  ; Î
		("&Iuml;"   . 207)  ; Ï
		("&eth;"    . 208)  ; Ð
		("&Ntilde;" . 209)  ; Ñ
		("&Ograve;" . 210)  ; Ò
		("&Oacute;" . 211)  ; Ó
		("&Ocirc;"  . 212)  ; Ô
		("&Otilde;" . 213)  ; Õ
		("&Ouml;"   . 214)  ; Ö
		("&times;"  . 215)  ; ×
		("&Oslash;" . 216)  ; Ø
		("&Ugrave;" . 217)  ; Ù
		("&Uacute;" . 218)  ; Ú
		("&Ucirc;"  . 219)  ; Û
		("&Uuml;"   . 220)  ; Ü
		("&Yacute;" . 221)  ; Ý
		("&thorn;"  . 222)  ; Þ
		("&szlig;"  . 223)  ; ß
		("&agrave;" . 224)  ; à
		("&aacute;" . 225)  ; á
		("&acirc;"  . 226)  ; â
		("&atilde;" . 227)  ; ã
		("&auml;"   . 228)  ; ä
		("&aring;"  . 229)  ; å
		("&aelig;"  . 230)  ; æ
		("&ccedil;" . 231)  ; ç
		("&egrave;" . 232)  ; è
		("&eacute;" . 233)  ; é
		("&ecirc;"  . 234)  ; ê
		("&euml;"   . 235)  ; ë
		("&igrave;" . 236)  ; ì
		("&iacute;" . 237)  ; í
		("&icirc;"  . 238)  ; î
		("&iuml;"   . 239)  ; ï
		("&eth;"    . 240)  ; ð
		("&ntilde;" . 241)  ; ñ
		("&ograve;" . 242)  ; ò
		("&oacute;" . 243)  ; ó
		("&ocirc;"  . 244)  ; ô
		("&otilde;" . 245)  ; õ
		("&ouml;"   . 246)  ; ö
		("&divide;" . 247)  ; ÷
		("&oslash;" . 248)  ; ø
		("&ugrave;" . 249)  ; ù
		("&uacute;" . 250)  ; ú
		("&ucirc;"  . 251)  ; û
		("&uuml;"   . 252)  ; ü
		("&yacute;" . 253)  ; ý
		("&thorn;"  . 254)  ; þ
		("&yuml;"   . 255)  ; ÿ
		("&reg;"    . 174)  ; ®
		("&shy;"    . 173)  ; ­
	)
"Table of html character entities and values.
See https://www.freeformatter.com/html-entities.html")

(defun helm-html-decode-entities-string (str) "Decode entities in the string STR."
	(with-temp-buffer
		(insert str)
		(goto-char 1)
		(while (re-search-forward "&#?\\([^;]*\\);" nil t)
			; Convert html entity to string.
			(let ((entity (match-string 0)))
				(when-let (
					(new_str
						(if-let ((cell (assoc entity helm-html-entities-alist)))
							(char-to-string (cdr cell))
							(save-match-data
								(when (string-match "[0-9]+" entity)
									(char-to-string
										(string-to-number
											(match-string 0 entity)))))))
				)
					(replace-match new_str))))
		(buffer-string)))

(provide 'helm-utils)
