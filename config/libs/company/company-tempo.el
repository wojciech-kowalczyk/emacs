; -*- lexical-binding:nil -*-

; company-mode completion backend for tempo

(require 'tempo)

(defconst company::tempo::expand nil
"Whether to expand a tempo tag after completion.")

(defun company-tempo (command &optional arg)
"`company-mode' completion backend for tempo."
(interactive '(interactive))
	(let ((tempo_lookup
			(lambda (match)
				(cdr (assoc match (tempo-build-collection))))))
		(cl-case command
			(interactive (company-begin-backend 'company-tempo))
			(prefix (or (car (tempo-find-match-string tempo-match-finder)) ""))
			(candidates (all-completions arg (tempo-build-collection)))
			(kind 'snippet)
			(meta
				(let ((templ (funcall tempo_lookup arg)) doc)
					(and
						templ
						(setq doc (documentation templ t))
						(car (split-string doc "\n" t)))))
			(post-completion
				(when company::tempo::expand
					; Replace candidate with the expanded tempo template.
					(search-backward arg)
					(goto-char (match-beginning 0))
					(replace-match "")
					(call-interactively (funcall tempo_lookup arg))))
			(sorted t))))

(provide 'company-tempo)
