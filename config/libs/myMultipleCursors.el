; -*- lexical-binding:nil -*-

(push (concat LIBS_DIR "multiple-cursors") load-path)
(require 'multiple-cursors)

(add-hook 'multiple-cursors-mode-hook
	(fn_symbol "highlight_line_mode_off"
		(lambda () (highlightLine::mode -1))))
(add-hook 'multiple-cursors-mode-disabled-hook #'highlightLine::mode)

(setq mc/edit-lines-empty-lines 'ignore)
(setq mc/list-file (concat LIBS_DIR "multipleCursorsSettings.el"))
(setq mc/mode-line '(" cursors: " (:eval (format "%d" (mc/num-cursors)))))

(define_face 'mc/cursor-bar-face '((t :background "#BDBDBD" :height 87 :family "Segoe UI")))
(define_face 'mc/cursor-face '((t)))

(define-key mc/keymap [escape] #'mc/keyboard-quit)

(define-key mc/keymap [return] nil t)

(define-key global-map [A-down] #'mc/mark-next-like-this)
(define-key global-map [A-S-down] #'mc/unmark-next-like-this)
(define-key global-map [A-up] #'mc/mark-previous-like-this)
(define-key global-map [A-S-up] #'mc/unmark-previous-like-this)

(define-key global-map [A-home] #'mc/edit-beginnings-of-lines)
(define-key global-map [A-end] #'mc/edit-ends-of-lines)

(provide 'myMultipleCursors)
