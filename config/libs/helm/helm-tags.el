; -*- lexical-binding:nil -*-

(require 'etags)

(declare-function xref-push-marker-stack "xref")

(defconst helm-etags-tag-file-name "TAGS"
"Etags tag file name. String.")

(defconst helm-etags-tag-file-search-limit 10
"The limit level of directory to search tag file.
Don't search tag file deeply if outside this value.
Int.")

(defconst helm-etags-match-part-only t
"Match only against the tag part if non-nil in `helm-etags-source'.
A tag looks like this: filename: (defun foo
You can choose matching against the tag part (i.e \"(defun foo\"),
or against the whole candidate (i.e \"(filename:5:(defun foo\").")

(defconst helm-etags-execute-action-at-once-if-one t
"Whether to jump straight to the selected tag if there's only one match.")

(define_face 'helm-etags-file
	'((t :extend t :foreground "Lightgoldenrod4" :underline t))
"Face used to highlight etags filenames.")

; Etags

(defun helm-etags-find-file (candidate)
"Find file CANDIDATE from helm etags buffer."
	(helm-etags-action-goto #'find-file candidate))

(defun helm-etags-find-file-other-window (candidate)
"Find file other window from helm etags buffer."
	(helm-etags-action-goto #'find-file-other-window candidate))

(defun helm-etags-find-file-other-frame (candidate)
"Find file other frame from helm etags buffer."
	(helm-etags-action-goto #'find-file-other-frame candidate))

(defvar helm-etags-mtime-alist nil
"Store the last modification time of etags files here.")
(defvar helm-etags-cache (make-hash-table :test 'equal)
"Cache content of etags files used here for faster access.")

(defun helm-etags-get-tag-file (&optional directory)
"Return the path of etags file if found in DIRECTORY.
Look recursively in parents directories for a `helm-etags-tag-file-name' file."
	; Get tag file from `default-directory' or upper directory.
	(cl-loop
		with current-dir = (or directory default-directory)
		for i from 0
		until ; file readable
			(let (
				(tag-path
					(expand-file-name helm-etags-tag-file-name current-dir))
			)
				(and
					(stringp tag-path)
					(file-regular-p tag-path)
					(file-readable-p tag-path)))
		; Return nil if outside the value of
		; `helm-etags-tag-file-search-limit'.
		when (= i helm-etags-tag-file-search-limit)
			return nil
		; Or search upper directories.
		do
		(++ i)
		(setq current-dir (expand-file-name (concat current-dir "../")))
		finally return (expand-file-name helm-etags-tag-file-name current-dir)))

(defun helm-etags-all-tag-files ()
"Find Etags files.
Return files from the following sources:
  1) An automatically located file in the parent directories,
     by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command."
	(helm-fast-remove-dups
		(delq
			nil
			(cons
				(helm-etags-get-tag-file)
				(cons tags-file-name tags-table-list)))))

(defun helm-etags-action-goto (switcher candidate)
"Helm default action to jump to an etags entry in other window."
	(when mark-active (deactivate-mark))
	(let* (
		(split (helm-grep-split-line candidate))
		(fname
			(cl-loop
				for tagf being the hash-keys of helm-etags-cache
				for f = (expand-file-name (car split) (file-name-directory tagf))
				thereis
					(cl-loop
						for ext in (cons "" (remove "" tags-compression-info-list))
						for file = (concat f ext)
						when (file-exists-p file)
							return file)))
	)
		(if (null fname)
			(error "File %s not found" fname)
			(xref-push-marker-stack)
			(funcall switcher fname)
			(goto_line (string-to-number (nth 1 split)))
			(recenter))))

(defun helm-etags-mtime (file) "Last modification time of etags tag FILE."
	(car (cdr (nth 5 (file-attributes file)))))

; name will be set before every helm call.
(defconst helm-etags-source
	(helm-source-sync-make nil
		:candidates
			; Use `helm-etags-cache' or tag file.
			; If there is no entry in cache, create one.
			(lambda ()
				(when-let ((tagfiles (helm-etags-all-tag-files)))
					(let (candidates)
						(dolist (file tagfiles)
							(if-let ((cache (gethash file helm-etags-cache)))
								(setq candidates (append candidates cache))
								(let* (
									max
									(split
										(with-temp-buffer
											(insert-file-contents file)
											(setq max (line-number-at-pos (point-max)))
											(split-string (buffer-string) "\n" t)))
									(progress-reporter
										(make-progress-reporter "Loading tag file..." 0 max))
									(candidates_from_current_file
										(cl-loop
											with fname = nil
											with cand = nil
											for i in split
											for count from 0
											for elm =
												(unless (string-match "^\x0c" i) ; "^L"
													(if-let ((match (string-match "\177" i))) ; "^?"
														(substring i 0 match)
														i))
											for linum =
												(when (string-match "[0-9]+,?[0-9]*$" i)
													(car (split-string (match-string 0 i) ",")))
											do
											(cond
												((and elm (string-match "^\\([^,]+\\),[0-9]+$" elm))
													(setq fname
														(propertize_no_copy (match-string 1 elm)
															'face 'helm-etags-file)))
												(elm (setq cand (concat fname ":" linum ":" elm)))
												(t (setq cand nil)))
											when cand
												collect (propertize_no_copy cand 'linum linum)
												and do (progress-reporter-update progress-reporter count)))
								)
									(puthash file candidates_from_current_file helm-etags-cache)
									(setq candidates (append candidates candidates_from_current_file)))
								; Store or set the last modification of tag file.
								(if-let ((cell (assoc file helm-etags-mtime-alist)))
									; If an entry exists modify it.
									(setcdr cell (helm-etags-mtime file))
									; No entry - create a new one.
									(cl-pushnew (cons file (helm-etags-mtime file))
										helm-etags-mtime-alist
										:test #'equal))))
						candidates)))
		:keymap helm-grep-keymap
		:match-part
			(when helm-etags-match-part-only
				(lambda (candidate)
					(string-match helm-grep-split-line-regex candidate)
					; Match only the tag part of candidate and not the filename.
					(list (cons (match-beginning 3) (match-end 3)))))
		:action
			(list
				(cons "Go to tag" #'helm-etags-find-file)
				(cons "Go to tag in other window" #'helm-etags-find-file-other-window)
				(cons "Go to tag in other frame" #'helm-etags-find-file-other-frame))
		:persistent-action
			(lambda (candidate)
				(helm-etags-find-file candidate)
				(helm-highlight-current-line t t))))

(defun helm-etags (&optional reinit)
"helm for etags.
If called with a prefix argument REINIT
or if any of the tag files have been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories,
     by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command."
	(interactive "P")
	(let (
		(tag-files (helm-etags-all-tag-files))
		(str
			(if mark-active
				(buffer-substring-no-properties (region-beginning) (region-end))
				(thing-at-point 'symbol)))
	)
		(if (cl-notany #'file-exists-p tag-files) ; If no file exists (in the universe).
			(message "Error: No tag file found. Create with etags shell command, or visit with `find-tag' or `visit-tags-table'.")
			(cl-loop
				for k being the hash-keys of helm-etags-cache
				unless (member k tag-files) do (remhash k helm-etags-cache))
			(dolist (f tag-files)
				(when
					(or
						(equal reinit '(4))
						(and
							helm-etags-mtime-alist
							; If file has been modified in this session.
							f
							(let ((last-modif (assoc-default f helm-etags-mtime-alist)))
								(and
									last-modif
									(/= last-modif (helm-etags-mtime f))))))
					(remhash f helm-etags-cache)))
			; Create header name for this helm etags session.
			(setf (helm-source-name helm-etags-source) (concat "Etags in " (helm-etags-get-tag-file)))
			(helm
				:sources (list helm-etags-source)
				:default
					(or
						(and
							(stringp str)
							(if
								(and
									(eq major-mode 'haskell-mode)
									(string-match "[']\\'" str))
								str
								(list (concat "\\_<" str "\\_>") str)))
						(thing-at-point 'symbol))
				:use-default-as-input t
				:execute-action-at-once-if-one helm-etags-execute-action-at-once-if-one))))

(provide 'helm-tags)
