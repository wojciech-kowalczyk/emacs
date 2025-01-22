; -*- lexical-binding:nil -*-

; company-mode completion backend for oddmuse-mode

(eval-when-compile (require 'yaoddmuse nil t) (require 'oddmuse nil t))

(defun company-oddmuse (command &optional arg)
"`company-mode' completion backend for `oddmuse-mode'."
(interactive '(interactive))
	(cl-case command
		(interactive (company-begin-backend 'company-oddmuse))
		(prefix
			(let ((case-fold-search nil))
				(and
					(memq major-mode '(oddmuse-mode yaoddmuse-mode))
					(looking-back
						"\\(\\<[A-Z][[:alnum:]]*\\>\\)\\|\\[\\[\\([[:alnum:]]+\\>\\|\\)"
						(line-beginning-position))
					(or (match-string 1) (match-string 2)))))
		(candidates
			(all-completions
				arg
				(cl-case major-mode
					(yaoddmuse-mode
						(with-no-warnings
							(yaoddmuse-get-pagename-table yaoddmuse-wikiname)))
					(oddmuse-mode
						(with-no-warnings
							(oddmuse-make-completion-table oddmuse-wiki))))))))

(provide 'company-oddmuse)
