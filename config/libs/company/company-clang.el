; -*- lexical-binding:nil -*-

; company-mode completion backend for Clang

(require 'company-template)

(defconst company::clang::executable (executable-find "clang")
"Location of clang executable.")

(unless company::clang::executable
	(error "Company found no clang executable"))

(defconst company::clang::begin_after_member_access t
"When non-nil, start automatic completion after member access operators.

Automatic completion starts whenever the current symbol is preceded by
\".\", \"->\" or \"::\", ignoring `company-minimum-prefix-length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and \":\".")

(defconst company::clang::use_compile_flags_txt nil
"When non-nil, use flags from compile_flags.txt if present.

The lines from that files will be appended to `company::clang::argument_list'.

And if such file is found, Clang is called from the directory containing it.
That allows the flags use relative file names within the project.")

(defconst company::clang::argument_list nil
"A list of additional argument strings to pass to clang when completing.
Prefix files (-include ...) can be selected with company::clang::prefix.")

(defvar company::clang::mode_list '(c-mode c++-mode objc-mode)
"Major modes which clang may complete.")

(defconst company::clang::insert_arguments t
"When non-nil, insert function arguments as a template after completion.")


(defvar company::clang::prefix nil "Filepath to clang completion prefix file.")

; Do we ever see OVERLOAD (or OVERRIDE)?
(defconst company::clang::completion_pattern
	"^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\|Pattern\\)\\(?:\\(?: (InBase)\\)? : \\(.*\\)$\\)?$")

(defconst company::clang::error_buffer_name "*clang-error*")

(defun company-clang (command &optional arg)
"`company-mode' completion backend for Clang.
Clang version 1.1 or newer is required.

Additional command line arguments can be specified in `company::clang::argument_list'.
Prefix files (-include ...) can be selected with `company::clang::prefix'.

With Clang versions before 2.9, we have to save the buffer before performing completion.
With Clang 2.9 and later, buffer contents are passed via standard input."
(interactive '(interactive))
	(let ((get_candidate_annotation
			(lambda (candidate)
				(let ((ann
						; TODO: Parse the original formatting here, rather than guess.
						; Strip it every time in the `meta' handler instead.
						(let ((meta (get-text-property 0 'meta candidate)))
							(cond
								((null meta) nil)
								((string-match "[^:]:[^:]" meta) (substring meta (1+ (match-beginning 0))))
								((string-match "(anonymous)" meta) nil)
								((string-match "\\((.*)[ a-z]*\\'\\)" meta)
									(let ((paren (match-beginning 1)))
										(if (not (eq (aref meta (1- paren)) ?>))
											(match-string 1 meta)
											(with-temp-buffer
												(insert meta)
												(goto-char paren)
												(substring meta (1- (search-backward "<")))))))))))
					(if (not (and ann (string-prefix-p "(*)" ann)))
						ann
						(with-temp-buffer
							(insert ann)
							(search-backward ")")
							(let ((pt (1+ (point))))
								(re-search-forward ".\\_>" nil t)
								(delete-region pt (point)))
							(buffer-string)))))))
		(cl-case command
			(interactive (company-begin-backend 'company-clang))
			(prefix
				(and
					(memq major-mode company::clang::mode_list)
					buffer-file-name
					(inside_code)
					(if company::clang::begin_after_member_access
						(company::get_prefix_after_member_access)
						(company-grab-symbol))))
			(candidates
				(cons
					:async
					(lambda (callback)
						(if (buffer-modified-p)
							(basic-save-buffer))
						(unless company::clang::prefix
							(setq company::clang::prefix 'none))
						(let* ((args ; Clang arguments.
								(let ((default-directory default-directory))
									(append
										'("-fsyntax-only" "-Xclang" "-code-completion-macros")
										(let ((fname "compile_flags.txt")
											  (argument_list_copy company::clang::argument_list)
											  current-dir-rel)
											(when-let* (company::clang::use_compile_flags_txt
														(dir (locate-dominating-file default-directory fname)))
												(setq current-dir-rel (file-relative-name default-directory dir)
													  default-directory dir)
												(with-temp-buffer
													(insert-file-contents fname)
													(setq argument_list_copy
														(append
															argument_list_copy
															(split-string
																(buffer-substring-no-properties (point-min) (point-max))
																"[\n\r]+"
																t
																"[ \t]+"))))
												(unless (equal current-dir-rel "./")
													(push (concat "-I" current-dir-rel) argument_list_copy)))
											argument_list_copy)
										(unless (eq 'none company::clang::prefix)
											(list "-include" (expand-file-name company::clang::prefix)))
										(list
											"-Xclang"
											(concat
												"-code-completion-at="
												(save-excursion
													(goto-char (- (point) (length arg)))
													(format "%s:%d:%d"
														buffer-file-name
														(line-number-at-pos)
														(1+ (length (encode-coding-region (line-beginning-position) (point) 'utf-8 t)))))))
										(list buffer-file-name))))
							   (objc (derived-mode-p 'objc-mode))
							   (buf (get-buffer-create "*clang-output*"))
							   ; Looks unnecessary in Emacs 25.1 and later.
							   ; (Inconclusive, needs more testing)
							   ; github.com/company-mode/company-mode/pull/288#issuecomment-72491808
							   (process-adaptive-read-buffering nil)
							   (existing-process (get-buffer-process buf)))
							(if existing-process
								(kill-process existing-process))
							(with-current-buffer buf
								(erase-buffer)
								(setq buffer-undo-list t))
							(let* ((process-connection-type nil)
								   (process (apply #'start-file-process "company-clang" buf company::clang::executable args)))
								(set-process-sentinel
									process
									(lambda (proc status)
										(unless (string-match-p "hangup\\|killed" status)
											(funcall callback
												(let ((res (process-exit-status proc)))
													(with-current-buffer buf
														(unless (eq 0 res)
															(goto-char (point-min))
															(let* ((buf (get-buffer-create company::clang::error_buffer_name))
																   (cmd (concat company::clang::executable " " (mapconcat #'identity args " ")))
																   (pattern (format company::clang::completion_pattern ""))
																   (message-truncate-lines t)
																   (err
																	(if (and
																			(re-search-forward pattern nil t)
																			; Something in the Windows build?
																			; Looks like Clang doesn't always include the error text
																			; before completions (even if exited with error).
																			(> (match-beginning 0) (point-min)))
																		(buffer-substring-no-properties (point-min) (1- (match-beginning 0)))
																		; Warn the user more aggressively if no match was found.
																		(message "clang failed with error %d: %s" res cmd)
																		(buffer-string))))
																(with-current-buffer buf
																	(let ((inhibit-read-only t))
																		(erase-buffer)
																		(insert
																			(current-time-string)
																			"\nclang failed with error "
																			(number-to-string res)
																			":\n"
																			cmd
																			"\n\n"
																			err)
																		(setq buffer-read-only t)
																		(goto-char (point-min))))))
														; Still try to get any useful input.
														; Parse output.
														(goto-char (point-min))
														(let ((pattern (format company::clang::completion_pattern (regexp-quote arg)))
															  (case-fold-search nil)
															  (results (make-hash-table :test 'equal :size (/ (point-max) 100)))
															  lines)
															(while (re-search-forward pattern nil t)
																(let ((match (match-string-no-properties 1))
																	  (meta (match-string-no-properties 2)))
																	(if (equal match "Pattern")
																		(setq match
																			(let ((start 0) end)
																				(if (string-match "#]" match)
																					(setq start (match-end 0)))
																				(if (string-match "[ (]<#" match start)
																					(setq end (match-beginning 0)))
																				(substring match start end))))
																	(if (string-match ":" match)
																		(setq match (substring match 0 (match-beginning 0))))
																	; Avoiding duplicates:
																	; github.com/company-mode/company-mode/issues/841
																	(cond
																		; Either meta != completion (not a macro)
																		((not (equal match meta)) (puthash match meta results))
																		; Or it's the first time we see this completion
																		((eq (gethash match results 'none) 'none) (puthash match nil results)))))
															(maphash
																(lambda (match meta)
																	(if meta
																		(put-text-property 0 1
																			'meta
																			(replace-regexp-in-string ; Strip formatting.
																				"#]"
																				" "
																				(replace-regexp-in-string "[<{[]#\\|#[>}]" "" meta t)
																				t)
																			match))
																	(push match lines))
																results)
															lines))))))))))))
			(meta (get-text-property 0 'meta arg))
			(kind
				; XXX: Not very precise.
				; E.g. it will say that an arg-less ObjC method is a variable (perhaps we
				; could look around for brackets, etc, if there any actual users who's bothered by it).
				; And we can't distinguish between local vars and struct fields.
				; Or between keywords and macros.
				(let ((meta (get-text-property 0 'meta arg)))
					(cond
						((null meta) 'keyword)
						((string-match "(" meta)
							(if (string-match-p
									(format "\\`%s *\\'" (regexp-quote arg))
									(substring meta 0 (match-beginning 0)))
								'keyword ; Also macro, actually (no return type).
								'function))
						(t 'variable))))
			(annotation (funcall get_candidate_annotation arg))
			(post-completion
				(when-let* (company::clang::insert_arguments (annotation (funcall get_candidate_annotation arg)))
					(insert annotation)
					(if (string-match "\\`:[^:]" annotation)
						(company::template::objc_templatify annotation) ; From company-template.el
						(company::template::c_like_templatify (concat arg annotation))))))))

(provide 'company-clang)
