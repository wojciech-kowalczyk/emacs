; -*- lexical-binding:nil -*-

(setq isearch-allow-scroll 'unlimited)
(setq isearch-lazy-count t)
(setq query-replace-skip-read-only t)

(define-key global-map [?\C-f]
	(lambda () (interactive) (isearch-forward) (funcall after_move_hook_fn)))
(define-key global-map [?\C-\S-f]
	(lambda () (interactive) (isearch-backward) (funcall after_move_hook_fn)))
;(keymap-global-set "M-f" 'mc/mark-all-like-this)
;(keymap-global-set "M-F" 'mc/mark-all-in-region)
(define-key isearch-mode-map [?\C-f]
	(lambda () (interactive) (isearch-repeat-forward) (funcall after_move_hook_fn)))
(define-key isearch-mode-map [?\C-\S-f]
	(lambda () (interactive) (isearch-repeat-backward) (funcall after_move_hook_fn)))

(define-key global-map [?\C-r] #'query-replace)
(define-key global-map [?\C-\S-r] #'query-replace-regexp)

(define-key isearch-mode-map [?\C-r] #'isearch-query-replace)
(define-key isearch-mode-map [?\C-\S-r] #'isearch-query-replace-regexp)

(define-key isearch-mode-map [?\C-s] #'isearch-toggle-case-fold)
(define-key isearch-mode-map [?\C-\S-s] #'isearch-toggle-regexp)

(define-key isearch-mode-map [?\C-e] #'isearch-edit-string)

(define-key isearch-mode-map [?\C-o] #'helm-occur-from-isearch)

(define-key isearch-mode-map [escape] #'isearch-cancel)

(define-key isearch-mode-map [return] #'isearch-exit)

(define-key isearch-mode-map [S-return]
	(lambda () (interactive) (isearch-printing-char ?\n)))

(define-key isearch-mode-map [?\C-\s] #'isearch-complete)

(define-key isearch-mode-map [backspace] #'isearch-delete-char)

(define-key isearch-mode-map [?\C-v]
	(lambda () (interactive)
		(when-let ((clipboard_entry (clipboard-get-entry-to-paste)))
			(isearch-yank-string (car clipboard_entry)))))

(define-key query-replace-map [?\C-\[] nil t)

(provide 'mySearchReplace)
