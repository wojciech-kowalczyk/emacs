; -*- lexical-binding:nil -*-

; company-mode completion backend for file names

(defconst company::files::exclusion_list nil
"A list of file name extensions and directory names to ignore.
The values should use the same format as `completion-ignored-extensions'.")

(defconst company::files::chop_trailing_slash t
"Non-nil to remove the trailing slash after inserting directory name.
This way it's easy to continue completion by typing `/' again.")

(defconst company::files::regex_vector
	(let ((begin
			(concat
				"\\(?:\\.\\{1,2\\}/\\|~/\\|"
				(if (eq system-type 'windows-nt)
					"[a-zA-Z]:/"
					"/")
				"\\)")))
		(vector
			(concat "\"\\(" begin "[^\"\n]*\\)")
			(concat "'\\(" begin "[^'\n]*\\)")
			(concat "\\(?:[ \t=[]\\|^\\)\\(" begin "[^ \t\n]*\\)"))))

(defvar company::files::completion_cache nil)

(defun company-files (command &optional arg)
"`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings."
(interactive '(interactive))
	(let ((is_trailing_slash
			(lambda (file)
				; `file-directory-p' is very expensive on remotes. We are relying on
				; `file-name-all-completions' returning directories with trailing / instead.
				(let ((len (length file)))
					(and (> len 0) (= ?/ (aref file (1- len)))))))
		  (is_connected
			(lambda (file)
				(or (not (file-remote-p file)) (file-remote-p file nil t)))))
		(cl-case command
			(interactive (company-begin-backend 'company-files))
			(prefix
				; Grab existing file name.
				; When surrounded with quotes, it can include spaces.
				(let (file dir)
					(and
						(cl-loop
							for regex across company::files::regex_vector
							when (setq file (company-grab-line regex 1))
								return file)
						(funcall is_connected file)
						(setq dir (file-name-directory file))
						(not (string-match "//" dir))
						(file-exists-p dir)
						file)))
			(candidates
				(let* ((dir (file-name-directory arg))
					   (file (file-name-nondirectory arg))
					   (key (list file (expand-file-name dir) (nth 5 (file-attributes dir))))
					   (completion-ignore-case read-file-name-completion-ignore-case))
					; Unless keys match.
					(unless (and
							(equal (cdr (car company::files::completion_cache)) (cdr key))
							(string-prefix-p (car (car company::files::completion_cache)) (car key)))
						(let* ((get_directory_files
								(lambda (dir prefix)
									; Don't use directory-files. It produces directories without trailing /.
									(ignore-error file-error
										(let ((completion_list
												(sort
													(file-name-all-completions prefix dir)
													(lambda (s1 s2) (string-lessp (downcase s1) (downcase s2))))))
											(if company::files::exclusion_list
												; Filter out exclusions.
												(setq completion_list
													(let* ((dir-exclusions
															(cl-remove-if-not
																is_trailing_slash
																company::files::exclusion_list))
														   (file-exclusions
															(cl-set-difference
																company::files::exclusion_list
																dir-exclusions)))
														(cl-loop
															for completion in completion_list
															unless (if (funcall is_trailing_slash completion)
																	(member completion dir-exclusions)
																	(cl-find-if
																		(lambda (exclusion) (string-suffix-p exclusion completion))
																		file-exclusions))
																collect completion))))
											(if (equal prefix "")
												(delete "../" (delete "./" completion_list))
												completion_list)))))
							   (candidates
								(mapcar
									(lambda (f) (concat dir f))
									(funcall get_directory_files dir file)))
							   (directories
								(unless (file-remote-p dir)
									(cl-remove-if-not
										(lambda (file_)
											(and
												(funcall is_trailing_slash file_)
												(not (file-remote-p file_))
												(funcall is_connected file_)))
										candidates)))
							   (children
								(and
									directories
									(mapcan
										(lambda (directory)
											(mapcar
												(lambda (c) (concat directory c))
												(funcall get_directory_files directory "")))
										directories))))
							(setq company::files::completion_cache
								(cons key (append candidates children)))))
					(all-completions arg (cdr company::files::completion_cache))))
			(location (cons (dired-noselect (file-name-directory (directory-file-name arg))) 1))
			(post-completion
				(if (and company::files::chop_trailing_slash (funcall is_trailing_slash arg))
					(delete-char -1)))
			(kind (if (string-suffix-p "/" arg) 'folder 'file))
			(sorted t)
			(no-cache t))))

(provide 'company-files)
