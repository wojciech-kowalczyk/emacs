; -*- lexical-binding:nil -*-

(require 'epg)
(require 'epa)

(declare-function epg-list-keys             "epg")
(declare-function epg-make-context          "epg")
(declare-function epg-key-sub-key-list      "epg")
(declare-function epg-sub-key-id            "epg")
(declare-function epg-key-user-id-list      "epg")
(declare-function epg-user-id-string        "epg")
(declare-function epg-user-id-validity      "epg")
(declare-function epa-sign-region           "epa")
(declare-function epa--read-signature-type  "epa")
(declare-function epa-display-error         "epa")
(declare-function epg-export-keys-to-string "epg")
(declare-function epg-context-armor         "epg")
(declare-function epg-context-set-armor     "epg")
(declare-function epg-delete-keys           "epg")

(defvar epa-protocol)
(defvar epa-last-coding-system-specified)
(defvar epg-key-validity-alist)
(defvar mail-header-separator)

(defvar helm-epa--list-only-secrets nil
"[INTERNAL] Used to pass MODE argument to `epg-list-keys'.")

(defun helm-epa-get-key-list (&optional keys)
"Build candidate list for `helm-epa-list-keys'."
	(mapcar
		(lambda (key)
			(let (
				(uid_raw (car (epg-key-user-id-list key)))
				(validity (epg-user-id-validity uid_raw))
			)
				(cons
					(format " %s %s %s"
						(if-let ((bomb (rassq validity epg-key-validity-alist)))
							(string (car bomb))
							"?")
						(propertize (epg-sub-key-id (car (epg-key-sub-key-list key)))
							'face
							(cl-case validity
								(none 'epa-validity-medium)
								((revoked expired) 'epa-validity-disabled)
								(t 'epa-validity-high)))
						(propertize (epg-user-id-string uid_raw)
							'face 'font-lock-warning-face))
					key)))
		(or
			keys
			(epg-list-keys (epg-make-context epa-protocol)
				nil helm-epa--list-only-secrets))))

(defun helm-epa--select-keys (prompt keys)
"A helm replacement for `epa--select-keys'."
	(let (
		(result
			(helm
				:sources
					(list
						(helm-source-sync-make nil
							:name "Epa select keys"
							:candidates `(lambda () (helm-epa-get-key-list ',keys))
							:action (lambda (_candidate) (helm-marked-candidates))))
				:prompt
					(and
						prompt
						(let ((split (split-string prompt "\n")))
							(if (cdr split)
								(format "%s\n(%s): "
									(replace-regexp-in-string "\\.[\t ]*\\'" "" (car split))
									(replace-regexp-in-string "\\.[\t ]*\\'" "" (nth 1 split)))
								(format "%s: "
									(replace-regexp-in-string "\\.[\t ]*\\'" "" (car split))))))))
	)
		(if (or (equal result "") (null result))
			(error "No keys selected, aborting")
			result)))

(defun helm-epa--read-signature-type ()
"A helm replacement for `epa--read-signature-type'."
	(cl-case
		(helm-read-answer
"Signature type:
(n - Create a normal signature)
(c - Create a cleartext signature)
(d - Create a detached signature)"
			"ncd")
		(?n 'normal)
		(?c 'clear)
		(t 'detached)))

(defun helm-epa-collect-keys-from-candidates (candidates)
	(mapcar
		(lambda (cand) (epg-sub-key-id (car (epg-key-sub-key-list cand))))
		candidates))

(defun helm-epa-collect-id-from-candidates (candidates)
	(mapcar
		(lambda (cand) (epg-user-id-string (car (epg-key-user-id-list cand))))
		candidates))

(defun helm-epa-success-message (str keys ids)
	(message str
		(mapconcat
			(lambda (pair) (concat (car pair) " " (cdr pair)))
			(cl-loop
				for k in keys
				for i in ids
				collect (cons k i))
			"\n")))

(define-minor-mode helm-epa-mode
"Enable helm completion on gpg keys in epa functions."
	:global t
	(if helm-epa-mode
		(progn
			(advice-add 'epa--select-keys :override #'helm-epa--select-keys)
			(advice-add 'epa--read-signature-type :override #'helm-epa--read-signature-type))
		(advice-remove 'epa--select-keys #'helm-epa--select-keys)
		(advice-remove 'epa--read-signature-type #'helm-epa--read-signature-type)))

(defun helm-epa-list-keys ()
"List all gpg keys.
This is the helm interface for `epa-list-keys'."
	(interactive)
	(helm
		:sources
			(list
				(helm-source-sync-make nil
					:name "Epg list keys"
					:candidates #'helm-epa-get-key-list
					:action-transformer
						(lambda (actions)
							(if (with-current-buffer helm-current-buffer (derived-mode-p 'message-mode 'mail-mode))
								(helm-append-at-nth actions
									(list
										(cons
											"Sign mail with key"
											(lambda (candidate)
												(let (
													(key (epg-sub-key-id (car (epg-key-sub-key-list candidate))))
													(id (epg-user-id-string (car (epg-key-user-id-list candidate))))
													start end mode
												)
													(save-excursion
														(goto-char (point-min))
														(when (search-forward mail-header-separator nil t)
															(forward-line 1))
														(setq epa-last-coding-system-specified
															(or
																coding-system-for-write
																(select-safe-coding-system (point) (point-max))))
														(setq
															start (point)
															end (point-max)
															mode
																(if current-prefix-arg ; verbose
																	(epa--read-signature-type)
																	'clear)))
													; TODO Make non-interactive functions to replace
													; epa-sign-region and epa-encrypt-region and inline them.
													(with-no-warnings
														(epa-sign-region start end candidate mode))
													(message "Mail signed with key `%s %s'." key id))))
										(cons
											"Encrypt mail with key"
											(lambda (_candidate)
												(let ((cands (helm-marked-candidates)) start end)
													(save-excursion
														(goto-char (point-min))
														(when (search-forward mail-header-separator nil t)
															(forward-line 1))
														(setq
															start (point)
															end (point-max))
														(setq epa-last-coding-system-specified
															(or
																coding-system-for-write
																(select-safe-coding-system start end))))
													; Don't let some read-only text stop us from encrypting.
													(let (
														(inhibit-read-only t)
														(keys (helm-epa-collect-keys-from-candidates cands))
														(ids (helm-epa-collect-id-from-candidates cands))
													)
														(with-no-warnings
															(epa-encrypt-region start end cands nil nil))
														(helm-epa-success-message
															"Mail encrypted with key(s):\n%s."
															keys ids))))))
									3)
								actions))
					:action
						(list
							(cons "Show key" #'epa--show-key)
							(cons
								"Encrypt file with key"
								(lambda (_candidate)
									(let* (
										(cands (helm-marked-candidates))
										(file (helm-read-file-name "Encrypt file: "))
										(keys (helm-epa-collect-keys-from-candidates cands))
										(ids (helm-epa-collect-id-from-candidates cands))
									)
										(epa-encrypt-file file cands)
										(helm-epa-success-message
											"File encrypted with key(s):\n%s." keys ids))))
							(cons
								"Copy keys"
								(lambda (_candidate)
									(let (
										(keys (helm-marked-candidates))
										(context (epg-make-context epa-protocol))
									)
										(with-no-warnings
											(setf (epg-context-armor context) t))
										(condition-case err
											(clipboard-add
												(epg-export-keys-to-string context keys))
											(error
												(epa-display-error context)
												(signal (car err) (cdr err)))))))
							(cons
								"Delete keys"
								; Delete gpg marked keys from helm-epa.
								(lambda (_candidate)
									(let (
										(context (epg-make-context epa-protocol))
										(keys (helm-marked-candidates))
									)
										(message "Deleting gpg keys...")
										(condition-case err
											(epg-delete-keys context keys)
											(error
												(epa-display-error context)
												(signal (car err) (cdr err))))
										(message "Deleting gpg keys done.")))))))))

(provide 'helm-epa)
