; -*- lexical-binding:nil -*-

(require 'abbrev)

(defun company-abbrev (command &optional arg)
"`company-mode' completion backend for abbrev."
(interactive '(interactive))
	(cl-case command
		(interactive
			(company-begin-backend
				'company-abbrev
				; Replace match with the expanded abbrev.
				(lambdaSymbol (_) (expand-abbrev))))
		(prefix (company-grab-symbol))
		(candidates
			(nconc
				(delete "" (all-completions arg global-abbrev-table))
				(delete "" (all-completions arg local-abbrev-table))))
		(kind 'snippet)
		(meta (abbrev-expansion arg))
		(post-completion (expand-abbrev))))

(provide 'company-abbrev)
