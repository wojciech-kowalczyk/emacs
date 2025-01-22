; -*- lexical-binding:nil -*-

(defconst helm-raise-command nil
"A shell command to jump to a window running specific program.
Need external program wmctrl.
This will be use with `format', so use something like \"wmctrl -xa %s\".
Can be nil.")

(defconst helm-external-programs-associations nil
"Alist of strings to store externals programs associated with file extension.
This variable overhide setting in .mailcap file.
E.g. \\='((\"jpg\" . \"gqview\") (\"pdf\" . \"xpdf\")).")

(defconst helm-default-external-file-browser "explorer.exe"
"Default external file browser for your system.
Directories will be opened externally with it when opening file
externally in `helm-ff'.
Set to nil if you do not have an external file browser or do not
want to use it.
Windows users should set that to \"explorer.exe\".")

(defvar helm-ff-open-file-externally-after-hook nil
"Hook that run after opening a file with external program.")

(defvar helm-ff-open-file-externally-after-finish-hook nil
"Hook that run after external program finish.")

(defvar helm-external-command-history nil
"History variable for helm-external-command.")

(defvar helm-external-commands-list
	(cl-loop
		for dir in (split-string (getenv "PATH") path-separator)
		when (and (file-exists-p dir) (file-accessible-directory-p dir))
			for lsdir =
				(cl-loop
					for i in (directory-files dir t)
					for bn = (file-name-nondirectory i)
					when
						(and
							(not (member bn completions))
							(not (file-directory-p i))
							(file-executable-p i))
						collect bn)
		append lsdir into completions
		finally return (sort completions #'string<))
"A list of all external commands the user can execute.")

(defun helm-run-or-raise (exe &optional files)
"Run asynchronously EXE or jump to the application window.
If EXE is already running just jump to his window if
`helm-raise-command' is non-nil.
When FILES argument is provided run EXE with FILES.
Change FILES only if system is windows-nt, else pass them as they are,
so they probably should be absolute."
	(when (member exe helm-external-commands-list)
		; Allow adding more files to the current process if it is
		; already running (i.e. Don't just raise it without sending files)
		; we assume program doesn't start a new
		; process (like firefox, transmission etc...).
		; Just jump to the already running program instance or start
		; a new process.
		(if (and (not files) (get-process exe))
			(if helm-raise-command
				(run-at-time 0.1 nil
					#'shell-command
					(format helm-raise-command exe))
				(error "Error: %s is already running" exe))
			(and
				files
				(eq system-type 'windows-nt)
				(setq files (mapcar #'helm-w32-prepare-filename files)))
			(make-process
				:name exe
				:command (cons exe files)
				:sentinel
					`(lambda (process event)
						(when
							(and
								(string= event "finished\n")
								helm-raise-command
								(not
									; Pid from running process exe.
									; Protect system processes calls
									; Ensure `list-system-processes' and `process-attributes'
									; don't run on remote.
									(cl-loop
										with default-directory = temporary-file-directory
										for pid in (list-system-processes)
										for process = (assoc-default 'comm (process-attributes pid))
										when (and process (string-search ,exe process))
											return pid)))
							(run-hooks 'helm-ff-open-file-externally-after-finish-hook)
							(shell-command (format helm-raise-command "emacs")))
						(message "%s process finished." process))))
		; Move command on top list.
		(setq helm-external-commands-list
			(cons exe (delete exe helm-external-commands-list)))))

(defun helm-get-default-program-for-file (filename)
"Try to find a default program to open FILENAME.
Try first in `helm-external-programs-associations' and then in
mailcap file. If nothing found return nil."
	(let* (
		(ext (file-name-extension filename))
		(def-prog (assoc-default ext helm-external-programs-associations))
	)
		(cond
			((and def-prog (not (string= def-prog ""))) def-prog)
			((and helm-default-external-file-browser (file-directory-p filename))
				helm-default-external-file-browser)
			(ext
				; Get the command to use for filename from mailcap files.
				(mailcap-parse-mailcaps)
				(when-let ((mime (mailcap-extension-to-mime ext)))
					(let ((result (mailcap-mime-info mime)))
						; If elisp file have no associations in .mailcap
						; `mailcap-maybe-eval' is returned, in this case
						; just return nil.
						(when (stringp result) (helm-basename result))))))))

(defconst helm-external-sources
	(let (
		(action
			(lambda (candidate)
				(helm-run-or-raise candidate)
				(add-to-history 'helm-external-command-history candidate)))
	)
		(map_modify_list
			(lambda (name_and_symbol)
				(helm-source-sync-make nil
					:name (car name_and_symbol)
					:candidates
						`(lambda ()
							(mapcar
								(lambda (c)
									(if (get-process c)
										(propertize c 'face 'font-lock-type-face)
										c))
								,(cdr name_and_symbol)))
					:nomark t
					:action action))
			(list
				(cons "External commands history" 'helm-external-command-history)
				(cons "External commands" 'helm-external-commands-list)))))

(defun helm-run-external-command ()
"helm to run external program asynchronously from Emacs.
If program is already running try to run `helm-raise-command' if
defined otherwise exit with error. You can set your own list of
commands with `helm-external-commands-list'."
	(interactive)
	(helm :sources helm-external-sources)
	; Remove from history no more valid executables.
	(setq helm-external-command-history
		(cl-delete-if-not #'executable-find helm-external-command-history)))

(provide 'helm-external)
