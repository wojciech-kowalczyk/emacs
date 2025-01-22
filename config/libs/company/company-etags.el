; -*- lexical-binding:nil -*-

; company-mode completion backend for etags

(require 'etags)

(defconst company::etags::use_main_table_list t
"Always search `tags-table-list' if set.
If this is disabled, `company-etags' will try to find the one table for each
buffer automatically.")

(defconst company::etags::ignore_case nil
"Non-nil to ignore case in completion candidates.")

(defconst company::etags::everywhere t
"Non-nil to offer completions in comments and strings.
nil - off,
t - for company::etags::mode_list,
list of major modes.")

(defconst company::etags::mode_list '(prog-mode))

(defvar-local company::etags::buffer_table 'unknown)

(defun company-etags (command &optional arg)
"`company-mode' completion backend for etags."
(interactive '(interactive))
	(let ((get_etags_buffer_table
			(lambda ()
				(or
					(if company::etags::use_main_table_list
						tags-table-list)
					(if (eq company::etags::buffer_table 'unknown)
						(setq company::etags::buffer_table
							; Find table.
							(if-let ((file
										(expand-file-name
											"TAGS"
											(locate-dominating-file
												(or buffer-file-name default-directory)
												"TAGS")))
									 ((file-regular-p file)))
								(list file)))
						company::etags::buffer_table)))))
		(cl-case command
			(interactive (company-begin-backend 'company-etags))
			(prefix
				(and
					(apply #'derived-mode-p company::etags::mode_list)
					(or
						(eq t company::etags::everywhere)
						(apply #'derived-mode-p company::etags::everywhere)
						(inside_code))
					(funcall get_etags_buffer_table)
					(company-grab-symbol)))
			(candidates
				(let ((tags-table-list (funcall get_etags_buffer_table))
					  (tags-file-name tags-file-name)
					  (completion-ignore-case company-etags-ignore-case))
					(and
						(or tags-file-name tags-table-list)
						(fboundp 'tags-completion-table)
						(save-excursion
							(visit-tags-table-buffer)
							(all-completions arg (tags-completion-table))))))
			(location
				(let ((tags-table-list (funcall get_etags_buffer_table)))
					(if (fboundp 'find-tag-noselect)
						(save-excursion
							(let ((buffer (find-tag-noselect arg)))
								(cons buffer (with-current-buffer buffer (point))))))))
			(ignore-case company::etags::ignore_case))))

(provide 'company-etags)
