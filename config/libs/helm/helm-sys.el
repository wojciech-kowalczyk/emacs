; -*- lexical-binding:nil -*-

; Top (process)
; Not for Windows.
; Broken - some top line that is not a candidate line (I guess),
; that helm doesn't support now, also weird hooks that now don't work.

;(define_face 'helm-top-columns '((t :inherit helm-header :extend t))
;"Face for helm help string in minibuffer.")
;
;(defconst helm-top-command
;	(cl-case system-type
;		(darwin "env COLUMNS=%s ps -axo pid,user,pri,nice,ucomm,tty,start_time,vsz,%%cpu,%%mem,etime,command")
;		(t      "env COLUMNS=%s top -b -n 1"))
;"Top command used to display output of top.
;A format string where %s will be replaced with `frame-width'.
;
;To use top command, a version supporting batch mode (-b option)
;is needed. On Mac OSX top command doesn't support this, so the
;ps command is used instead by default.
;
;Normally top command output have 12 columns, but in some
;versions you may have less than this, so you can either customize
;top to use 12 columns with the interactives f and W commands
;of top, or modify `helm-top-sort-columns-alist' to fit with the
;number of columns your top command is using.
;
;If you modify ps command be sure that pid comes in first and
;\"env COLUMNS=%s\" is specified at beginning of command. Ensure
;also that no elements contain spaces (e.g., use start_time and
;not start). Same as for top: you can customize
;`helm-top-sort-columns-alist' to make sort commands working
;properly according to your settings.")
;
;(defconst helm-top-sort-columns-alist
;	'(
;		(com . 11)
;		(mem . 9)
;		(cpu . 8)
;		(user . 1)
;	)
;"Allow defining which column to use when sorting output of top/ps command.
;Only com, mem, cpu and user are sorted, so no need to put something
;else there, it will have no effect.
;Note that column numbers are counted from zero, i.e. column 1 is the
;nth 0 column.
;Key - symbol, value - int - column number.")
;
;(defconst helm-top-poll-delay 1.5
;"Helm top poll after this delay when `helm-top-poll-mode' is enabled.
;The minimal delay allowed is 1.5.
;Number.")
;
;(defconst helm-top-poll-delay-post-command 1.0
;"Helm top stop polling during this delay.
;This delay is added to `helm-top-poll-delay' after Emacs stops being idle.
;Number.")
;
;(defconst helm-top-poll-preselection 'linum
;"Stay on same line or follow candidate when `helm-top-poll' updates display.
;Possible values are \\='candidate or \\='linum.
;This affects also sorting functions in the same way.")
;
;(defvar helm-top-sort-fn nil)
;(defvar helm-top--poll-timer nil)
;(defvar helm-top-after-init-hook nil "Local hook for helm-top.")
;
;(defun helm-top-poll (&optional no-update delay)
;	(when helm-top--poll-timer
;		(cancel-timer helm-top--poll-timer)
;		(setq helm-top--poll-timer nil))
;	(condition-case nil
;		(progn
;			(and
;				helm-alive-p
;				(not no-update)
;				(helm-force-update
;					t
;					(if (eq helm-top-poll-preselection 'candidate)
;						(replace-regexp-in-string
;							"[0-9]+" "[0-9]+"
;							(regexp-quote (helm-get-selection t)))
;						(with-helm-window (helm-get-index)))))
;			(setq helm-top--poll-timer
;				(run-with-idle-timer
;					(if-let ((idle_time (current-idle-time)))
;						(time-add
;							idle_time (seconds-to-time (or delay helm-top-poll-delay)))
;						(or delay helm-top-poll-delay))
;					nil
;					#'helm-top-poll)))
;		(quit
;			(cancel-timer helm-top--poll-timer)
;			(setq helm-top--poll-timer nil))))
;
;(defun helm-top-poll-no-update ()
;	(helm-top-poll t (+ helm-top-poll-delay helm-top-poll-delay-post-command)))
;
;(defun helm-top-initialize-poll-hooks ()
;	; When Emacs is idle during say 20s
;	; the idle timer will run in 20+1.5 s.
;	; This is fine when Emacs stays idle, because the next timer
;	; will run at 21.5+1.5 etc... so the display will be updated
;	; at every 1.5 seconds.
;	; But as soon as emacs looses its idleness, the next update
;	; will occur at say 21+1.5 s, so we have to reinitialize
;	; the timer at 0+1.5.
;	(add-hook 'post-command-hook #'helm-top-poll-no-update)
;	(add-hook 'focus-in-hook #'helm-top-poll-no-update))
;
;(define-minor-mode helm-top-poll-mode
;"Refresh automatically helm top buffer once enabled."
;	:global t
;	(if helm-top-poll-mode
;		(progn
;			(add-hook 'helm-top-after-init-hook #'helm-top-poll-no-update)
;			(add-hook 'helm-top-after-init-hook #'helm-top-initialize-poll-hooks))
;		(remove-hook 'helm-top-after-init-hook #'helm-top-poll-no-update)
;		(remove-hook 'helm-top-after-init-hook #'helm-top-initialize-poll-hooks)))
;
;(defconst helm-top-source
;	(helm-source-sync-make nil
;		:name
;			(lambda ()
;				(if helm-top-poll-mode
;					"Top (auto updating)"
;					"Top"))
;		:candidates
;			(lambda ()
;				(with-local-quit
;					(unless helm-top-sort-fn (helm-top-set-mode-line "CPU"))
;					(split-string
;						(with-temp-buffer
;							(call-process-shell-command
;								(format helm-top-command (frame-width))
;								nil
;								t)
;							(buffer-string))
;						"\n")))
;		:after-init (lambda () (run-hooks 'helm-top-after-init-hook))
;		:candidate-number-limit 9999
;		:cleanup
;			(lambda ()
;				(when helm-top--poll-timer
;					(cancel-timer helm-top--poll-timer)
;					(setq helm-top--poll-timer nil))
;				(remove-hook 'post-command-hook #'helm-top-poll-no-update)
;				(remove-hook 'focus-in-hook #'helm-top-poll-no-update))
;		:display-to-real ; Return pid only from line.
;			(lambda (line) (car (split-string line)))
;		:keymap
;			(let ((map (make-sparse-keymap)))
;				(set-keymap-parent map helm-map)
;				(define-key map (kbd "M-P") #'helm-top-run-sort-by-cpu)
;				(define-key map (kbd "M-C") #'helm-top-run-sort-by-com)
;				(define-key map (kbd "M-M") #'helm-top-run-sort-by-mem)
;				(define-key map (kbd "M-U") #'helm-top-run-sort-by-user)
;				(helm-add-delete-binds-force
;					map (lambda (cand) (helm-top-sh "TERM" (list cand))))
;				map)
;		:candidate-transformer #'helm-top-sort-transformer
;		:action-transformer
;			; Show actions only on line starting by a PID.
;			`(lambda (actions)
;				(if (string-match "\\` *[0-9]+" (helm-get-selection t))
;					',(list
;						(cons
;							"Kill (SIGTERM)"
;							(lambda (_pid)
;								(helm-top-sh "TERM" (helm-top--marked-pids))))
;						(cons
;							"Kill (SIGKILL)"
;							(lambda (_pid)
;								(helm-top-sh "KILL" (helm-top--marked-pids))))
;						(cons
;							"Kill (SIGINT)"
;							(lambda (_pid)
;								(helm-top-sh "INT" (helm-top--marked-pids))))
;						(cons
;							"Kill (Choose signal)"
;							(lambda (_pid)
;								(let ((pids (helm-top--marked-pids)))
;									(helm-top-sh
;										(helm-comp-read
;											(format "Kill %d pids with signal: " (length pids))
;											'(
;												"ALRM" "HUP" "INT" "KILL" "PIPE" "POLL"
;												"PROF" "TERM" "USR1" "USR2" "VTALRM"
;												"STKFLT" "PWR" "WINCH" "CHLD" "URG"
;												"TSTP" "TTIN" "TTOU" "STOP" "CONT"
;												"ABRT" "FPE" "ILL" "QUIT" "SEGV"
;												"TRAP" "SYS" "EMT" "BUS" "XCPU" "XFSZ"
;											)
;											:must-match t)
;										pids)))))
;					actions))))
;
;(defun helm-top--skip-top-line ()
;	; This seems completely wrong - persistent action call and generally
;	; skipping some line (it doesn't look like a candidate line, so it should
;	; be in header, not in text area).
;	(and
;		(eq (helm-get-current-source) helm-top-source)
;		(string-match-p "^ *PID" (helm-get-selection t))
;		(helm-next-line)))
;
;(defun helm-top--marked-pids ()
;	(cl-delete-if
;		(lambda (cand)
;			(not
;				(string-match-p
;					"\\`[0-9]+\\'"
;					(cond
;						((symbolp cand) (symbol-name cand))
;						((consp cand) (car cand))
;						(t cand)))))
;		(helm-marked-candidates)))
;
;(defun helm-top-sh (sig pids)
;"Run kill shell command with signal SIG on PIDS for `helm-top'."
;	(message "kill -%s %s exited with status %s."
;		sig
;		(mapconcat #'identity pids " ")
;		(apply #'call-process "kill" nil nil nil (format "-%s" sig) pids)))
;
;; Sort top command
;
;(defun helm-top-set-mode-line (str)
;	(setf (helm-source-mode-line helm-top-source) (concat "  Sort by " str)))
;
;(defvar helm-top--line nil)
;(defun helm-top-sort-transformer (candidates)
;	; Transformer for `helm-top'.
;	; Return empty string for invalid candidates. ??? Why.
;	(setq candidates
;		(mapcar
;			(lambda (disp)
;				(cond
;					((string-match "^ *[0-9]+" disp) disp)
;					((string-match "^ *PID" disp)
;						(setq helm-top--line
;							(cons (propertize disp 'face 'helm-top-columns) "")))
;					(t (cons disp ""))))
;			(if helm-top-sort-fn
;				(cl-loop
;					for c in candidates
;					if (string-match "^ *[0-9]+" c)
;						collect c into pid-cands
;					else
;						collect c into header-cands
;					finally
;						return
;							(append
;								header-cands
;								(sort pid-cands helm-top-sort-fn)))
;				candidates)))
;	(or
;		(member helm-top--line candidates)
;		(if helm-top--line
;			(cons helm-top--line candidates)
;			candidates)))
;
;(defmacro tempSortLet (thing_to_search &rest body) (declare (debug t))
;	`(let (
;		(split-1 (split-string s1))
;		(split-2 (split-string s2))
;		(col (cdr (assq ,thing_to_search helm-top-sort-columns-alist)))
;	)
;		,@body))
;
;(defun helm-top-sort-by-com (s1 s2)
;	(tempSortLet 'com
;		(string< (nth col split-1) (nth col split-2))))
;
;(defun helm-top-sort-by-mem (s1 s2)
;	(tempSortLet 'mem
;		(> (string-to-number (nth col split-1)) (string-to-number (nth col split-2)))))
;
;(defun helm-top-sort-by-cpu (s1 s2)
;	(tempSortLet 'cpu
;		(> (string-to-number (nth col split-1)) (string-to-number (nth col split-2)))))
;
;(defun helm-top-sort-by-user (s1 s2)
;	(tempSortLet 'user
;		(string< (nth col split-1) (nth col split-2))))
;
;(unintern 'tempSortLet nil)
;
;(defun helm-top-run-sort-base (mode_line sort_fn)
;	(helm-top-set-mode-line mode_line)
;	(setq helm-top-sort-fn sort_fn)
;	(helm-force-update
;		nil
;		(if (eq helm-top-poll-preselection 'linum)
;			(with-helm-window (helm-get-index))
;			(replace-regexp-in-string
;				"[0-9]+" "[0-9]+" (regexp-quote (helm-get-selection t))))))
;
;(defun helm-top-run-sort-by-com () (interactive)
;	(helm-top-run-sort-base "COM" #'helm-top-sort-by-com))
;
;(defun helm-top-run-sort-by-cpu () (interactive)
;	; Force sorting by CPU even if some versions of top are using by
;	; default CPU sorting (Bug#1908).
;	(helm-top-run-sort-base "CPU" #'helm-top-sort-by-cpu))
;
;(defun helm-top-run-sort-by-mem () (interactive)
;	(helm-top-run-sort-base "MEM" #'helm-top-sort-by-mem))
;
;(defun helm-top-run-sort-by-user () (interactive)
;	(helm-top-run-sort-base "USER" #'helm-top-sort-by-user))
;
;(defun helm-top () (interactive)
;	(add-hook 'helm-after-update-hook #'helm-top--skip-top-line)
;	(helm
;		:sources (list helm-top-source)
;		:preselect "^\\s-*[0-9]+"
;		:helm-full-frame t
;		:helm-truncate-lines nil)
;	(remove-hook 'helm-after-update-hook #'helm-top--skip-top-line))

; Emacs process

(defconst helm-emacs-process-source
	(helm-source-sync-make nil
		:name "Emacs process"
		:candidates
			(lambda ()
				(let (tabulated-list-use-header-line)
					(list-processes--refresh))
				(mapcar
					(lambda (process)
						(let ((process_name (process-name process)))
							(if-let (
								(command
									(mapconcat
										#'identity
										(process-command process)
										" "))
								((not (string= command "")))
							)
								(cons
									(concat
										process_name
										" --> "
										command)
									process_name)
								process_name)))
					(process-list)))
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-map)
				(helm-add-delete-binds
					map
					(lambda (cand cache)
						(delete-process (get-process cand))
						(cl-delete-if
							(lambda (c) (eq (if (consp c) (cdr c) c) cand))
							cache))
					t)
				map)
		:action
			(list
				(cons
					"Kill process"
					(lambda (_candidate)
						(dolist (p (helm-marked-candidates))
							(delete-process (get-process p))))))
		:multiline t))

(defun helm-emacs-process () (interactive)
	(helm :sources (list helm-emacs-process-source) :helm-truncate-lines t))

(provide 'helm-sys)
