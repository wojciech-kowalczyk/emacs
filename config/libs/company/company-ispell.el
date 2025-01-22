; -*- lexical-binding:nil -*-

; company-mode completion backend using Ispell

(require 'ispell)
(ispell-lookup-words "TEST") ; Error if ispell don't work.

(defconst company::ispell::all_words
	; Dictionary (filepath) to use for `company-ispell'.
	(let ((dictionary (or ispell-complete-word-dict ispell-alternate-dictionary)))
		(ispell-lookup-words "" dictionary))
"All completion candidates for company-ispell.")

(defun company-ispell (command &optional arg)
"`company-mode' completion backend using Ispell."
(interactive '(interactive))
	(cl-case command
		(interactive (company-begin-backend 'company-ispell))
		(prefix (buffer-substring (point) (save-excursion (skip-syntax-backward "w") (point))))
		(candidates
			(if (= (length arg) 0) ; Small optimization.
				company::ispell::all_words
				(company::substitute_prefix
					arg
					(let ((completion-ignore-case t))
						; Workaround issue #284.
						(all-completions arg company::ispell::all_words)))))
		(kind 'text)
		(no-cache t)
		(sorted t)
		(ignore-case t)))

(provide 'company-ispell)
