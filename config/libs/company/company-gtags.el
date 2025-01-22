; -*- lexical-binding:nil -*-

; company-mode completion backend for GNU Global

(require 'company-template)

(defconst company::gtags::executable (executable-find "global")
"Location of GNU global executable.")

(defconst company::gtags::insert_arguments t
"When non-nil, insert function arguments as a template after completion.")

(defcustom company::gtags::mode_list '(prog-mode jde-mode)
"Modes that use `company-gtags'.
In all these modes (and their derivatives) `company-gtags' will perform completion.")


(defvar-local company::gtags::are_tags_available 'unknown)

(defun company-gtags (command &optional arg)
"`company-mode' completion backend for GNU Global."
(interactive '(interactive))
	(let ((get_annotation
			(lambda () ; Dynamic binding: arg.
				(let ((meta (get-text-property 0 'meta arg)))
					(if (string-match (concat (regexp-quote arg) " *(") meta)
						(with-temp-buffer
							(let ((start (match-end 0)))
								(insert meta)
								(goto-char start)
								(condition-case nil
									(forward-sexp)
									(scan-error (goto-char (point-max))))
								(buffer-substring-no-properties start (point)))))))))
		(cl-case command
			(interactive (company-begin-backend 'company-gtags))
			(prefix
				(and
					buffer-file-name
					(apply #'derived-mode-p company::gtags::mode_list)
					(inside_code)
					; If tags are available.
					(if (eq company::gtags::are_tags_available 'unknown)
						(setq company::gtags::are_tags_available
							(locate-dominating-file buffer-file-name "GTAGS"))
						company::gtags::are_tags_available)
					(company-grab-symbol)))
			(candidates
				(with-temp-buffer
					(let (tags) ; ???
						; For some reason Global v 6.6.3 is prone to returning exit status 1
						; even on successful searches when '-T' is used.
						(when (/=
								3
								(process-file
									company::gtags::executable
									nil
									; "-T" goes through all the tag files listed in GTAGSLIBPATH
									(list (current-buffer) nil)
									nil
									"-xGqT"
									(concat "^" arg)))
							(goto-char (point-min))
							(cl-loop
								while
									(re-search-forward
										(concat
											"^"
											"\\([^ ]*\\)" ; completion
											"[ \t]+\\([[:digit:]]+\\)" ; linum
											"[ \t]+\\([^ \t]+\\)" ; file
											"[ \t]+\\(.*\\)" ; definition
											"$")
										nil
										t)
								collect
									(propertize (match-string 1)
										'meta (match-string 4)
										'location
											(cons
												(expand-file-name (match-string 3))
												(string-to-number (match-string 2)))))))))
			(annotation (funcall get_annotation))
			(meta (get-text-property 0 'meta arg))
			(sorted t)
			(duplicates t)
			(location (get-text-property 0 'location arg))
			(post-completion
				(when-let* (company::gtags::insert_arguments (annotation (funcall get_annotation)))
					(insert annotation)
					(company::template::c_like_templatify annotation))))))

(provide 'company-gtags)
