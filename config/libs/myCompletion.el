; -*- lexical-binding:nil -*-

(setq completion-styles '(emacs22 substring))
(setq completions-detailed t)
(setq completions-format 'one-column)

(push (concat LIBS_DIR "company") load-path)
(require 'company)
(global-company-mode)

; Set minibuffer-local-map

(setcdr minibuffer-local-map nil)

; This is basically exit-recursive-edit (just throws exit nil),
; but also sets deactivate-mark to nil.
(define-key minibuffer-local-map [return] #'exit-minibuffer)

; No special bind for escape, it's handled by escape-key-hook-abort-recursive-edit.

; Show full history.
(define-key minibuffer-local-map [?\C-h] #'helm-minibuffer-history)

; Completion (helm if helm handler is specified).
(define-key minibuffer-local-map [?\C-\s] #'minibuffer-complete)

(define-key minibuffer-local-map [?\C-\[] #'previous-history-element)
(define-key minibuffer-local-map [?\C-\]] #'next-history-element)

; Remove/rearrange binds in some keymaps that break eval-expression.

(define-key read-expression-map "\t" nil t)
(define-key read-expression-map [?\C-\M-i] nil t)
(define-key read-expression-map [tab] nil t)
; I'm not sure, but for some reason minibuffer-complete here just says
; that there are no matches, and read-expression-map binds [tab] by default
; to this function, so maybe that's expected to use this and not
; minibuffer-complete here.
(define-key read-expression-map [?\C-\s] #'completion-at-point)

(define-key read-expression-map "\e" nil t)
(define-key read-expression-map [?\A-g] #'read-expression-switch-to-completions)

(define-key read--expression-map "\r" nil t)
(define-key read--expression-map [?\C-j] nil t)
(define-key read--expression-map [return] #'read--expression-try-read)

(provide 'myCompletion)
