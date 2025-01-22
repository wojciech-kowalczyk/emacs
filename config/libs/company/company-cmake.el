; -*- lexical-binding:nil -*-

; company-mode completion backend for CMake

(defcustom company::cmake::executable (executable-find "cmake")
"Location of cmake executable.")

(unless company::cmake::executable
	(error "Company found no cmake executable"))

(defconst company::cmake::executable_argument_list
	'("--help-command-list"
	  "--help-module-list"
	  "--help-property-list"
	  "--help-variable-list")
"The arguments we pass to cmake, separately.
They affect which types of symbols we get completion candidates for.")

(defconst company::cmake::completion_pattern "^\\(%s[a-zA-Z0-9_<>]%s\\)$"
"Regexp to match the candidates.")

(defconst company::cmake::mode_list '(cmake-mode)
"Major modes in which cmake may complete.")

(defvar company::cmake::candidates_cache (make-hash-table :test 'equal)
"Cache for the raw candidates.")

(defvar company::cmake::meta_command_cache (make-hash-table :test 'equal)
"Cache for command arguments to retrieve descriptions for the candidates.")

(defun company-cmake (command &optional arg)
"`company-mode' completion backend for CMake."
(interactive '(interactive))
	(let ((call_cmake_goto_3_line_return_string
			(lambda (get_end_position_fn) ; Dynamic binding: arg.
				(with-temp-buffer
					(call-process company::cmake::executable
						nil
						t
						nil
						(gethash arg company::cmake::meta_command_cache)
						; Unexpand candidate.
						(cond
							((string-match "^CMAKE_\\(C\\|CXX\\|Fortran\\)\\(_.*\\)$" arg)
								(concat "CMAKE_<LANG>" (match-string 2 arg)))
							; C flags
							((string-match "^\\(.*_\\)IS_GNU\\(C\\|CXX\\|G77\\)$" arg)
								(concat (match-string 1 arg) "IS_GNU<LANG>"))
							; C flags
							((string-match "^\\(.*_\\)OVERRIDE_\\(C\\|CXX\\|Fortran\\)$" arg)
								(concat (match-string 1 arg) "OVERRIDE_<LANG>"))
							((string-match "^\\(.*\\)\\(_DEBUG\\|_RELEASE\\|_RELWITHDEBINFO\\|_MINSIZEREL\\)\\(.*\\)$" arg)
								(concat (match-string 1 arg) "_<CONFIG>" (match-string 3 arg)))
							(t arg)))
					; Go to the third line, and return the result.
					; Tested with cmake 2.8.9.
					(goto-char (point-min))
					(forward-line 2)
					(buffer-substring-no-properties (point) (funcall get_end_position_fn))))))
		(cl-case command
			(interactive (company-begin-backend 'company-cmake))
			(prefix
				(and
					(memq major-mode company::cmake::mode_list)
					(or
						(inside_code)
						; If the current symbol follows ${.
						(save-excursion
							(skip-syntax-backward "w_")
							(and (eq (char-before) ?\{) (eq (char-before (1- (point))) ?$))))
					(company-grab-symbol)))
			(candidates
				(let (results cmd-opts str)
					(dolist (arg company::cmake::executable_argument_list)
						; If hash is empty, fill it.
						(unless (gethash arg company::cmake::candidates_cache)
							(puthash
								arg
								; Replace tags.
								(replace-regexp-in-string
									"\\(.*\\)<CONFIG>\\(.*\\)"
									(mapconcat #'identity
										'("\\1DEBUG\\2" "\\1RELEASE\\2" "\\1RELWITHDEBINFO\\2" "\\1MINSIZEREL\\2")
										"\n")
									(replace-regexp-in-string
										"\\(.*?\\(IS_GNU\\)?\\)<LANG>\\(.*\\)"
										(lambda (_match)
											(mapconcat #'identity
												(if (match-beginning 2)
													'("\\1CXX\\3" "\\1C\\3" "\\1G77\\3")
													'("\\1CXX\\3" "\\1C\\3" "\\1Fortran\\3"))
												"\n"))
										(with-temp-buffer
											(let ((res (call-process company::cmake::executable nil t nil arg)))
												(if (/= 0 res)
													(message "cmake executable exited with error=%d" res)))
											(buffer-string))
										t))
								company::cmake::candidates_cache))
						(setq cmd-opts (replace-regexp-in-string "-list$" "" arg)
							  str (gethash arg company::cmake::candidates_cache))
						(if str
							(setq results
								(nconc
									results
									(let ((start 0)
										  (pattern
											(format company::cmake::completion_pattern
												(regexp-quote arg) (if (= 0 (length arg)) "+" "*")))
										  (lines (split-string str "\n"))
										  match
										  rlt)
										(dolist (line lines)
											(when-let (((string-match pattern line)) (match (match-string 1 line)))
												(puthash match cmd-opts company::cmake::meta_command_cache)
												(push match rlt)))
										rlt)))))
					results))
			(meta
				; Don't cache the documentation of every candidate (command)
				; Cache in this case will cost too much memory.

				; Go to the third line, trim it and return the result.
				; Tested with cmake 2.8.9.
				(replace-regexp-in-string "^[ \t\n\r]+" ""
					(funcall call_cmake_goto_3_line_return_string #'line-end-position)))
			(doc-buffer
				; Go to the third line, trim it and return the doc buffer.
				; Tested with cmake 2.8.9.
				(company-doc-buffer (funall call_cmake_goto_3_line_return_string #'point-max))))))

(provide 'company-cmake)
