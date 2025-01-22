; -*- lexical-binding:nil -*-

;; The functions `highlight-symbol-next', `highlight-symbol-prev',
;; `highlight-symbol-next-in-defun' and `highlight-symbol-prev-in-defun' allow
;; for cycling through the locations of any symbol at point.  Use
;; `highlight-symbol-nav-mode' to enable key bindings for
;; navigation. When `highlight-symbol-on-navigation-p' is set, highlighting is
;; triggered regardless of `highlight-symbol-idle-delay'.
;; `highlight-symbol-query-replace' can be used to replace the symbol.

(push "^No symbol at point$" debug-ignored-errors)

(defface highlightSymbolAutoReadFace '((t (:background "#344134"))) nil)
(defface highlightSymbolAutoWriteFace '((t (:background "#40332B"))) nil)

(defvar highlightSymbolAutoReadOverlay (make-overlay 1 1))
(overlay-put highlightSymbolAutoReadOverlay 'priority 50)
(overlay-put highlightSymbolAutoReadOverlay 'face 'highlightSymbolAutoReadFace)

(defvar highlightSymbolAutoWriteOverlay (make-overlay 1 1))
(overlay-put highlightSymbolAutoWriteOverlay 'priority 50)
(overlay-put highlightSymbolAutoWriteOverlay 'face 'highlightSymbolAutoWriteFace)

(defvar highlightSymbolMode nil)

(defvar highlightSymbol nil)
(make-variable-buffer-local 'highlightSymbol)

(defvar highlight-symbol-keyword-alist nil)
(make-variable-buffer-local 'highlight-symbol-keyword-alist)

(defconst highlight-symbol-border-pattern '("\\_<" . "\\_>"))

(defvar highlight-symbol-foreground-color "black"
  "Foreground color of highlighted symbols. Nil means to keep original text color.")

(defvar highlight-symbol-occurrence-message '(explicit navigation)
  "When to print the occurrence count of the current symbol.
A list.
If containing `explicit',
message after `highlight-symbol' is called explicitly.
If containing `temporary',
message after the symbol under point is temporarily highlighted by
`highlight-symbol-mode'.
If containing `navigation',
message after navigation commands.

Message after explicit highlighting `explicit'
Message after temporary highlighting `temporary'
Message after navigation commands `navigation'")

;;;###autoload
(define-minor-mode highlight-symbol-mode
  "Minor mode that highlights the symbol under point throughout the buffer.
Highlighting takes place after `highlight-symbol-idle-delay'."
  :init-value nil :lighter " Highlight symbol" :keymap nil
  (if highlight-symbol-mode
      ;; on
      (progn
        (highlight-symbol-update-timer highlight-symbol-idle-delay)
        (add-hook 'post-command-hook 'highlight-symbol-mode-post-command nil t))
    ;; off
    (remove-hook 'post-command-hook 'highlight-symbol-mode-post-command t)
    (highlight-symbol-mode-remove-temp)
    (kill-local-variable 'highlight-symbol)))

;;;###autoload
(defalias 'highlight-symbol-at-point 'highlight-symbol)

;;;###autoload
(defun highlight-symbol (&optional symbol)
  "Toggle highlighting of the symbol at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-faces'."
  (interactive)
  (let ((symbol (or symbol
                    (highlight-symbol-get-symbol)
                    (error "No symbol at point"))))
    (if (highlight-symbol-symbol-highlighted-p symbol)
        (highlight-symbol-remove-symbol symbol)
      (highlight-symbol-add-symbol symbol)
      (when (member 'explicit highlight-symbol-occurrence-message)
        (highlight-symbol-count symbol t)))))

(defun highlight-symbol-symbol-highlighted-p (symbol)
  "Test if the a symbol regexp is currently highlighted."
  (assoc symbol highlight-symbol-keyword-alist))

(defun highlight-symbol-should-auto-highlight-p (symbol)
  "Test if SYMBOL should be highlighted automatically."
  (or highlight-symbol-highlight-single-occurrence
      (> (highlight-symbol-count symbol) 1)))

(defun highlight-symbol-add-symbol (symbol &optional color)
  (unless (highlight-symbol-symbol-highlighted-p symbol)
    (when (equal symbol highlight-symbol)
      (highlight-symbol-mode-remove-temp))
    (let ((color (or color (highlight-symbol-next-color))))
      (unless (facep color)
        (setq color `((background-color . ,color)
                      (foreground-color . ,highlight-symbol-foreground-color))))
      ;; highlight
      (highlight-symbol-add-symbol-with-face symbol color))))

(defun highlight-symbol-add-symbol-with-face (symbol face)
  (let ((keywords `(,symbol 0 ',face prepend)))
    (push keywords highlight-symbol-keyword-alist)
    (font-lock-add-keywords nil (list keywords) 'append)
    (font-lock-flush)))

(defun highlight-symbol-remove-symbol (symbol)
  (let ((keywords (assoc symbol highlight-symbol-keyword-alist)))
    (setq highlight-symbol-keyword-alist
          (delq keywords highlight-symbol-keyword-alist))
    (font-lock-remove-keywords nil (list keywords))
    (font-lock-flush)))

;;;###autoload
(defun highlight-symbol-remove-all ()
  "Remove symbol highlighting in buffer."
  (interactive)
  (mapc 'highlight-symbol-remove-symbol
        (mapcar 'car highlight-symbol-keyword-alist)))

;;;###autoload
(defun highlight-symbol-list-all ()
  "List all symbols highlighted in the buffer."
  (interactive)
  (message "%s" (mapconcat 'highlight-symbol-fontify-symbol
                           (mapcar 'car highlight-symbol-keyword-alist) ", ")))

(defun highlight-symbol-fontify-symbol (symbol)
  (let ((prefix-length (length (car highlight-symbol-border-pattern)))
        (suffix-length (length (cdr highlight-symbol-border-pattern))))
    (propertize (substring symbol prefix-length
                           (- (length symbol) suffix-length))
                'face (assoc symbol highlight-symbol-keyword-alist))))

;;;###autoload
(defun highlight-symbol-count (&optional symbol message-p)
  "Print the number of occurrences of symbol at point."
  (interactive '(nil t))
  (let* ((symbol (or symbol
                     (highlight-symbol-get-symbol)
                     (error "No symbol at point")))
         (case-fold-search nil)
         (count (how-many symbol (point-min) (point-max))))
    (when message-p
      (if (= count 0)
          (message "Only occurrence in buffer")
        (message "Occurrence %d/%d in buffer"
                 (1+ (how-many symbol (point-min) (1- (point))))
                 count)))
    count))

;;;###autoload
(defun highlight-symbol-next ()
  "Jump to the next location of the symbol at point within the buffer."
  (interactive)
  (highlight-symbol-jump 1))

;;;###autoload
(defun highlight-symbol-prev ()
  "Jump to the previous location of the symbol at point within the buffer."
  (interactive)
  (highlight-symbol-jump -1))

;;;###autoload
(defun highlight-symbol-next-in-defun ()
  "Jump to the next location of the symbol at point within the defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol-jump 1)))

;;;###autoload
(defun highlight-symbol-prev-in-defun ()
  "Jump to the previous location of the symbol at point within the defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol-jump -1)))

(defvar highlight-symbol-nav-mode-map (make-sparse-keymap)
  "Keymap for `highlight-symbol-nav-mode'.")

;;;###autoload
(define-minor-mode highlight-symbol-nav-mode
  "Navigate occurrences of the symbol at point.

When called interactively, toggle `highlight-symbol-nav-mode'.
With prefix ARG, enable `highlight-symbol-nav-mode' if ARG is
positive, otherwise disable it.

When called from Lisp, enable `highlight-symbol-nav-mode' if ARG
is omitted, nil or positive.  If ARG is `toggle', toggle
`highlight-symbol-nav-mode'.  Otherwise behave as if called
interactively.

In `highlight-symbol-nav-mode' provide the following key bindings
to navigate between occurrences of the symbol at point in the
current buffer.

\\{highlight-symbol-nav-mode-map}")

;;;###autoload
(defun highlight-symbol-query-replace (replacement)
  "Replace the symbol at point with REPLACEMENT."
  (interactive (let ((symbol (or (thing-at-point 'symbol)
                                 (error "No symbol at point"))))
                 (highlight-symbol-temp-highlight)
                 (set query-replace-to-history-variable
                      (cons (substring-no-properties symbol)
                            (eval query-replace-to-history-variable)))
                 (list
                  (read-from-minibuffer "Replacement: " nil nil nil
                                        query-replace-to-history-variable))))
  (goto-char (beginning-of-thing 'symbol))
  (query-replace-regexp (highlight-symbol-get-symbol) replacement))

;;;###autoload
(defun highlight-symbol-occur (&optional nlines)
  "Call `occur' with the symbol at point.
Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative."
  (interactive "P")
  (if (thing-at-point 'symbol)
      (occur (highlight-symbol-get-symbol) nlines)
    (error "No symbol at point")))

(defun highlight-symbol-get-symbol ()
  "Return a regular expression identifying the symbol at point."
  (let ((symbol (thing-at-point 'symbol)))
    (when (and symbol
               (not (member 0 (mapcar
                               (lambda (e) (string-match e symbol))
                               highlight-symbol-ignore-list))))
      (concat (car highlight-symbol-border-pattern)
              (regexp-quote symbol)
              (cdr highlight-symbol-border-pattern)))))

(defun highlight-symbol-temp-highlight ()
  "Highlight the current symbol until a command is executed."
  (when highlight-symbol-mode
    (let ((symbol (highlight-symbol-get-symbol)))
      (unless (or (equal symbol highlight-symbol)
                  (highlight-symbol-symbol-highlighted-p symbol))
        (highlight-symbol-mode-remove-temp)
        (when (and symbol (highlight-symbol-should-auto-highlight-p symbol))
          (setq highlight-symbol symbol)
          (highlight-symbol-add-symbol-with-face symbol 'highlight-symbol-face) ; TODO zmienić to na overlay
          (font-lock-flush)
          (when (member 'temporary highlight-symbol-occurrence-message)
            (highlight-symbol-count symbol t)))))))

(defun highlight-symbol-mode-remove-temp ()
  "Remove the temporary symbol highlighting."
  (when highlight-symbol
    (highlight-symbol-remove-symbol highlight-symbol)
    (setq highlight-symbol nil)))

(defun highlight-symbol-mode-post-command ()
  "After a command, change the temporary highlighting.
Remove the temporary symbol highlighting and, unless a timeout is specified,
create the new one."
  (if (eq this-command 'highlight-symbol-jump)
      (when highlight-symbol-on-navigation-p
        (highlight-symbol-temp-highlight))
    (if (eql highlight-symbol-idle-delay 0)
        (highlight-symbol-temp-highlight)
      (unless (equal highlight-symbol (highlight-symbol-get-symbol))
        (highlight-symbol-mode-remove-temp)))))

(defun highlight-symbol-jump (dir)
  "Jump to the next or previous occurence of the symbol at point.
DIR has to be 1 or -1."
  (let ((symbol (highlight-symbol-get-symbol)))
    (if symbol
        (let* ((case-fold-search nil)
               (msg (member 'navigation highlight-symbol-occurrence-message))
               (bounds (bounds-of-thing-at-point 'symbol))
               (offset (- (point) (if (< 0 dir) (cdr bounds) (car bounds)))))
          (unless (eq last-command 'highlight-symbol-jump)
            (push-mark))
          ;; move a little, so we don't find the same instance again
          (goto-char (- (point) offset))
          (let ((target (re-search-forward symbol nil t dir)))
            (unless target
              (goto-char (if (< 0 dir) (point-min) (point-max)))
              (unless msg
                (message "Continued from beginning of buffer"))
              (setq target (re-search-forward symbol nil nil dir)))
            (goto-char (+ target offset)))
          (when msg
            (highlight-symbol-count symbol t))
          (setq this-command 'highlight-symbol-jump))
      (error "No symbol at point"))))

(provide 'highlightSymbol)
