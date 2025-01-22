; -*- lexical-binding:nil -*-

; company-mode completion backend for Yasnippet

(defvar yas-key-syntaxes)
(defvar yas-prompt-functions)
(declare-function yas--table-hash "yasnippet")
(declare-function yas--get-snippet-tables "yasnippet")
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas--template-content "yasnippet")
(declare-function yas--template-expand-env "yasnippet")
(declare-function yas--warning "yasnippet")
(declare-function yas-minor-mode "yasnippet")
(declare-function yas--require-template-specific-condition-p "yasnippet")
(declare-function yas--template-can-expand-p "yasnippet")
(declare-function yas--template-condition "yasnippet")

(defun company-yasnippet (command &optional arg)
"`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.
Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook \\='c-mode-hook
            (lambda ()
              (set (make-local-variable \\='company-backends)
                   \\='((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push \\='(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") \\='company-yasnippet)"
(interactive '(interactive))
	(cl-case command
		(interactive (company-begin-backend 'company-yasnippet))
		(prefix
			; Should probably use `yas--current-key', but that's bound to be slower.
			; How many trigger keys start with non-symbol characters anyway?
			(if (bound-and-true-p yas-minor-mode)
				(company-grab-symbol)))
		(annotation
			(if company::popup::align_annotations
				(concat " -> " (get-text-property 0 'yas-annotation arg))
				(get-text-property 0 'yas-annotation arg)))
		(candidates
			; Process the prefixes in reverse: unlike Yasnippet, we look for prefix
			; matches, so the longest prefix with any matches should be the most useful.
			(cl-loop
				with tables = (yas--get-snippet-tables)
				with arg_length = (length arg) ; Prefix length.
				for key-prefix in
					; Mostly copied from `yas--templates-for-key-at-point'.
					(save-excursion
						(let ((original (point))
							  (methods yas-key-syntaxes)
							  prefixes
							  method)
							(while methods
								(unless (eq method (car methods))
									(goto-char original))
								(setq method (car methods))
								(cond
									((stringp method)
										(skip-syntax-backward method)
										(setq methods (cdr methods)))
									((functionp method)
										(unless (eq (funcall method original) 'again)
											(setq methods (cdr methods))))
									(t
										(setq methods (cdr methods))
										(yas--warning "Invalid element `%s' in `yas-key-syntaxes'" method)))
								(let ((prefix (buffer-substring-no-properties (point) original)))
									(unless (equal prefix (car prefixes))
										(push prefix prefixes))))
							prefixes))
				for key_prefix_length = (length key-prefix)
				; Only consider keys at least as long as the symbol at point.
				when (>= key_prefix_length arg_length)
					thereis
						(mapcan
							(lambda (table)
								(let ((keyhash (yas--table-hash table))
									  (requirement (yas--require-template-specific-condition-p))
									  res)
									(if keyhash
										(maphash
											(lambda (key value)
												(if (and (stringp key) (string-prefix-p key-prefix key))
													(maphash
														(lambda (name template)
															(if (yas--template-can-expand-p
																	(yas--template-condition template)
																	requirement)
																(push
																	(propertize key
																		'yas-annotation name
																		'yas-template template
																		'yas-prefix-offset
																			(- key_prefix_length arg_length))
																	res)))
														value)))
											keyhash))
									res))
							tables)))
		(doc-buffer
			(let ((template (get-text-property 0 'yas-template arg))
				  (mode major-mode)
				  (file-name (buffer-file-name)))
				(with-current-buffer (company-doc-buffer)
					(let ((buffer-file-name file-name))
						(yas-minor-mode 1)
						(setq-local yas-prompt-functions '(yas-no-prompt))
						; This can error. Originally it converted errors to messages.
						(yas-expand-snippet (yas--template-content template))
						(if (eq mode 'web-mode)
							(setq mode 'html-mode))
						(delay-mode-hooks
							(let ((inhibit-message t)) (funcall mode))
							(ignore-errors (font-lock-ensure))))
					(current-buffer))))
		(no-cache t)
		(kind 'snippet)
		(post-completion
			(let ((template (get-text-property 0 'yas-template arg))
				  (prefix-offset (get-text-property 0 'yas-prefix-offset arg)))
				(yas-expand-snippet
					(yas--template-content template)
					(- (point) (length arg) prefix-offset)
					(point)
					(yas--template-expand-env template))))))

(provide 'company-yasnippet)
