; -*- lexical-binding:nil -*-

(require 'shell)

(define-key comint-mode-map [return] #'comint-send-input)
(define-key comint-mode-map [?\r] nil t)

(provide 'myShell)
