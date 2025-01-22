; -*- lexical-binding:nil -*-

(define_face 'helm-command-active-mode '((t :inherit bold))
"Face used by `helm-command' for activated modes.")

(define_face 'helm-command-short-doc '((t :inherit helm-completions-detailed))
"Face used by `helm-command' for short docstring.")

; This file doesn't use helm-adaptive, because obarray, that is used to get commands,
; is too big to quickly sort. So this file's main helm call uses
; separate history source, using 'extended-command-history'.
(desktop::add_to_globals 'extended-command-history)

(defvar helm-command-input-history nil)

(defun helm-command-transformer (candidates &optional symbol_as_real)
	(with-current-buffer helm-current-buffer
		(let ((max_length (helm-get-max-length candidates)))
			(map_modify_list
				(lambda (cand)
					(let ((symbol (intern cand)))
						(when
							(or
								(eq symbol major-mode)
								(and
									(memq symbol minor-mode-list)
									(boundp symbol)
									symbol))
							(add-face-text-property
								0
								(length cand)
								'helm-command-active-mode
								t ; Let helm-match face take priority.
								cand))
						(cons
							(propertize_no_copy
								(concat
									cand
									(get_space_string (1+ (- max_length (length cand))))
									(propertize (helm-get-first-line-documentation symbol)
										'face 'helm-command-short-doc))
								'match-part (list (cons 0 (length cand))))
							(if symbol_as_real symbol cand))))
				candidates))))

(defconst helm-command-sources
	(let (
		(persistent_action
			(lambda (candidate)
				(when (setq candidate (intern-soft candidate))
					(helm-elisp--persistent-help
						candidate
						#'helm-describe-function))))
	)
		(list
			(helm-source-history-make 'extended-command-history nil
				:name "Emacs Commands history"
				:candidate-transformer #'helm-command-transformer
				:action #'helm-command-execute-command
				:persistent-action persistent_action)
			(helm-source-sync-make nil
				:name "Emacs Commands"
				:candidates
					(lambda ()
						(with-current-buffer helm-current-buffer
							(all-completions "" obarray #'commandp)))
				:candidate-transformer #'helm-command-transformer
				:action #'helm-command-execute-command
				:persistent-action persistent_action
				:nomark t))))

(defun helm-command-execute-command (command)
"Execute COMMAND as an editor command.
COMMAND must be a symbol that satisfies the `commandp' predicate.
Save COMMAND to `extended-command-history'."
	(when (setq command (intern-soft command))
		; Avoid having `this-command' set to *exit-minibuffer.
		(setq this-command command)
		; Handle repeat.
		(setq real-this-command command)
		(let ((prefix-arg helm-current-prefix-arg))
			; This will often throw 'exit.
			(command-execute command 'record))
		(add-to-history 'extended-command-history (symbol-name command))))

(defun helm-command ()
"helm replacement of regular 'execute-extended-command'.

You can get help on each command by persistent action."
	(interactive)
	; Validate history.
	(setq extended-command-history
		(cl-delete-if
			(lambda (str) (not (commandp (intern-soft str))))
			extended-command-history))
	(if (or defining-kbd-macro executing-kbd-macro)
		(helm-command-execute-command
			(if (not helm-mode)
				(read-extended-command)
				(helm-mode -1)
				(unwind-protect
					(read-extended-command)
					(helm-mode))))
		(helm
			:sources helm-command-sources
			:history 'helm-command-input-history)))

; Idk what it does.
(put 'helm-command 'interactive-only 'command-execute)

(define-key global-map [?\C-l] #'helm-command)

(provide 'helm-command)
