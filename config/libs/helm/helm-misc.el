; -*- lexical-binding:nil -*-

; Some simple helm commands.

(defvar display-time-world-list)
(declare-function display-time-world-display "time.el")
(declare-function LaTeX-math-mode "ext:latex.el")
(declare-function jabber-chat-with "ext:jabber.el")
(declare-function jabber-read-account "ext:jabber.el")

(defconst helm-time-zone-home-location "Paris"
"The time zone of your home. String.")

(define_face 'helm-time-zone-current '((t :extend t :foreground "green"))
"Face used to colorize current time in `helm-world-time'.")

(define_face 'helm-time-zone-home '((t :extend t :foreground "red"))
"Face used to colorize home time in `helm-world-time'.")

; Latex completion
;
; Test
; (setq LaTeX-math-menu '("Math"
; ["foo" val0 t]
; ("bar"
; ["baz" val1 t])
; ("aze"
; ["zer" val2 t])
; ("AMS"
; ("rec"
; ["fer" val3 t])
; ("rty"
; ["der" val4 t]))
; ("ABC"
; ("xcv"
; ["sdf" val5 t])
; ("dfg"
; ["fgh" val6 t]))))
; (helm-latex-math-candidates)
; =>
; (("foo" . val0)
; ("baz" . val1)
; ("zer" . val2)
; ("fer" . val3)
; ("der" . val4)
; ("sdf" . val5)
; ("fgh" . val6))

(defvar LaTeX-math-menu)

(defconst helm-latex-math-source
	(helm-source-sync-make nil
		:name "Latex Math Menu"
		:init
			(lambda ()
				(with-current-buffer helm-current-buffer
					(unless LaTeX-math-mode (LaTeX-math-mode))))
		:candidate-number-limit 9999
		:candidates
			(lambda ()
				(cl-labels (
					(helm-latex--math-collect (l)
						(cond
							((vectorp l) (list (cons (aref l 0) (aref l 1))))
							((listp l) (mapcan #'helm-latex--math-collect l))))
				)
					(helm-latex--math-collect LaTeX-math-menu)))
		:action #'call-interactively))

(defun helm-insert-latex-math () "helm for latex math symbols completion."
	(interactive)
	(helm :sources (list helm-latex-math-source)))

; Jabber Contacts (jabber.el)

(defun helm-jabber-online-contacts () "List online Jabber contacts."
	(with-no-warnings
		(cl-loop
			for item in (jabber-concat-rosters)
			when (get item 'connected)
				collect
					(cons
						(or (get item 'name) (symbol-name item))
						item))))

(defconst helm-jabber-contacts-source
	(helm-source-sync-make nil
		:name "Jabber Contacts"
		:candidates
			(lambda ()
				(require 'jabber)
				(mapcar #'car (helm-jabber-online-contacts)))
		:action
			(lambda (x)
				(jabber-chat-with
					(jabber-read-account)
					(symbol-name (cdr (assoc x (helm-jabber-online-contacts))))))))

; World time

(defconst helm-world-time-source
	(helm-source-sync-make nil
		:name "Time World List"
		:candidates
			(lambda ()
				(require 'time)
				(setq world-clock-list (time--display-world-list))
				(cl-loop
					with current_time = (format-time-string "%H:%M")
					for i in
						(split-string
							(with-temp-buffer
								(world-clock-display world-clock-list)
								(buffer-string))
							"\n")
					for (z . p) in world-clock-list
					collect
						(cons
							(cond
								((string-match current_time i)
									(propertize i 'face 'helm-time-zone-current))
								((string-match helm-time-zone-home-location i)
									(propertize i 'face 'helm-time-zone-home))
								(t i))
							z)))
		:action
			(list
				(cons
					"Set timezone env (TZ)"
					(lambda (candidate) (setenv "TZ" candidate))))))

(defun helm-world-time ()
"helm to show world time.
Default action change TZ environment variable locally to emacs."
	(interactive)
	(helm :sources (list helm-world-time-source)))

; Helm ratpoison UI

(defconst helm-ratpoison-commands-source
	(helm-source-sync-make nil
		:name "Ratpoison Commands"
		:candidates
			(lambda ()
				(split-string
					(with-temp-buffer
						; with ratpoison prefix key
						(call-process "ratpoison" nil t nil "-c" "help")
						(goto-char 1)
						(while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
							(replace-match "<ratpoison> \\1: \\2"))
						(goto-char (point-max))
						; direct binding
						(save-excursion (call-process "ratpoison" nil t nil "-c" "help top"))
						(while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
							(replace-match "\\1: \\2"))
						(buffer-string))
					"\n"))
		:action
			(list
				(cons
					"Execute the command"
					(lambda (candidate)
						(call-process "ratpoison" nil nil nil "-ic" candidate))))
		:display-to-real
			(lambda (display)
				(when-let ((start (string-search ": " display)))
					(substring display (+ start 2))))
		:candidate-number-limit 9999))

(defun helm-ratpoison-commands () (interactive)
	(helm :sources (list helm-ratpoison-commands-source)))

; Helm stumpwm UI

(defconst helm-stumpwm-commands-source
	(helm-source-sync-make nil
		:name "Stumpwm Commands"
		:candidates
			(lambda ()
				(split-string
					(with-temp-buffer
						(call-process "stumpish" nil t nil "commands")
						(goto-char 1)
						(while (re-search-forward "[ ]*\\([^ ]+\\)[ ]*\n?" nil t)
							(replace-match "\n\\1\n"))
						(delete-blank-lines)
						(sort-lines nil (point-min) (point-max))
						(buffer-string))
					"\n"))
		:action
			(list
				(cons
					"Execute the command"
					(lambda (candidate)
						(call-process "stumpish" nil nil nil candidate))))
		:candidate-number-limit 9999))

(defun helm-stumpwm-commands () (interactive)
	(helm :sources (list helm-stumpwm-commands-source)))

; X RandR resolution change

(defun helm-xrandr-info ()
"Return a pair with current X screen number and current X display name."
	(with-temp-buffer
		(call-process "xrandr" nil t nil "--current")
		(goto-char 1)
		(let (screen output)
			(save-excursion
				(when (re-search-forward "\\(^Screen \\)\\([0-9]\\):" nil t)
					(setq screen (match-string 2))))
			(when (re-search-forward "^\\(.*\\) connected" nil t)
				(setq output (match-string 1)))
			(cons screen output))))

(defun helm-xrandr-screen () "Return current X screen number." (car (helm-xrandr-info)))

(defconst helm-xrandr-change-resolution-source
	(helm-source-sync-make nil
		:name "Change resolution"
		:candidates
			(lambda ()
				(with-temp-buffer
					(call-process "xrandr" nil t nil "--screen" (helm-xrandr-screen) "-q")
					(goto-char 1)
					(cl-loop
						while (re-search-forward "   \\([0-9]+x[0-9]+\\)" nil t)
						for mode = (match-string 1)
						unless (member mode modes)
							collect mode into modes
						finally return modes)))
		:action
			(list
				(cons
					"Change Resolution"
					(lambda (mode)
						(call-process "xrandr" nil nil nil
							"--screen" (helm-xrandr-screen)
							"--output" (cdr (helm-xrandr-info)) ; Current X display name.
							"--mode" mode))))))

(defun helm-xrandr () (interactive)
	(helm :sources (list helm-xrandr-change-resolution-source)))

(provide 'helm-misc)
