; -*- lexical-binding:nil -*-

(define-key global-map [?\C-e] #'eval-expression) ; TEMP

; Toggle truncate-lines.
(define-key global-map [?\C-\\]
	(fn_symbol "key::truncate_lines"
		(lambda () (interactive)
			(setq truncate-lines (not truncate-lines))
			(message "Truncate lines %s." (if truncate-lines "on" "off")))))

(require 'repeat)
(define-key global-map [?\C-\S-t] #'repeat)

; This is not very often used and C-s is a very accessible key, so it should
; definitely be changed, also C-S-s should also be used and also prefix.
(define-key global-map [?\C-s]
	(fn_symbol "key::swap_caret_and_mark"
		(lambda () (interactive)
			(when mark-active (swap_caret_and_mark) (funcall after_move_hook_fn)))))

(define-key global-map [?\C-p] #'helm-ff)
(define-key global-map [?\C-\S-p] #'helm-file-name-history)

(define-key global-map [f2]
	(fn_symbol "key::rename_visited_file"
		; Based on rename-visited-file and helm-ff-rename.
		(lambda () (interactive)
			(let (
				(helm-inhibit-move-to-first-candidate t)
				(dired-create-destination-dirs 'always)
				new_location
			)
				(if (not buffer-file-name)
					(setq new_location
						(let (
							(default
								(expand-file-name
									(file-name-nondirectory (buffer-name))
									default-directory))
						)
							(helm-read-file-name "Set visited file name: "
								:type 'file
								:preselect (helm-ff-regex-for-preselection default)
								:default default)))
					(setq new_location
						(helm-read-file-name "Rename visited file to: "
							:must-match #'helm-ff-confirm-override
							:preselect
								(helm-ff-regex-for-preselection buffer-file-name)
							:default buffer-file-name))
					; If the user has given a directory name, the file should be
					; moved there (under the same file name).
					(when (string-suffix-p "/" new_location)
						(setq new_location
							(expand-file-name
								(file-name-nondirectory buffer-file-name)
								new_location)))
					(let (
						overwrite
						dired-overwrite-confirmed ; for dired-handle-overwrite
					)
						(cond
							(helm-ff-confirm-override-done
								(setq overwrite t)
								(setq dired-overwrite-confirmed t))
							((file-exists-p new_location)
								(setq overwrite t)
								(setq dired-overwrite-confirmed
									(let (
										(help-form
											(format-message
												(substitute-command-keys
"Type \\`SPC' or \\`y' to overwrite file `%s',
\\`DEL' or \\`n' to skip to next,
\\`ESC' or \\`q' to not overwrite any of the remaining files,
\\`!' to overwrite all remaining files with no more questions.")
												new_location))
									)
										(dired-query
											'overwrite-query
											"Overwrite `%s'?"
											new_location)))))
						(dired-handle-overwrite new_location)
						(dired-maybe-create-dirs (file-name-directory new_location))
						(rename-file
							buffer-file-name new_location dired-overwrite-confirmed)))
				(set-visited-file-name new_location nil t)))))

(defvar escape-key-hook nil
"Hook run until success with no args, as an [escape] key bind.

Order is obviously very important, here are default depths:
	clear prefix arg - 10
	deactivate mark - 20
	abort recursive edit - 30
	exit recursive edit - 40.

Clearing prefix arg basically always should be first.
Deactivating mark is locally temporarily overriden by expandRegion.
Quitting temporary windows sometimes overrides aborting recursive edit
(or at least if minibuffer depth is higher than when temporary window
was created).

This isn't emacs' default hook variable, it instead uses my new implementation
of hooks in myUtility.el, because this hook really need proper support for mixed
local-global depths; so add-hook, remove-hook, run-hooks and others won't
work with this.")

(define-key global-map [escape]
	(fn_symbol "key::escape"
		(lambda () (interactive) (run_hook_until_success 'escape-key-hook))))

; Based on `keyboard-escape-quit'.

(add_hook
	'escape-key-hook
	(fn_symbol "escape-key-hook-clear-prefix-arg"
		(lambda ()
			; Since prefix-mode uses set-transient-map, invoking [escape] keybind
			; ends prefix-mode.
			(when current-prefix-arg
				(message "Prefix cleared.")
				t)))
	10)

(add_hook 'escape-key-hook #'deactivate-mark 20)

(add_hook
	'escape-key-hook
	(fn_symbol "escape-key-hook-abort-recursive-edit"
		(lambda ()
			(when (> (minibuffer-depth) 0)
				(abort-recursive-edit) ; throw exit t
				t)))
	30)

(add_hook
	'escape-key-hook
	(fn_symbol "escape-key-hook-exit-recursive-edit"
		(lambda ()
			(when (> (recursion-depth) 0)
				(exit-recursive-edit) ; throw exit nil
				t)))
	40)


; Read only toggle.

(define-key global-map [f6]
	(lambda () (interactive)
		(read-only-mode 'toggle)
		; When this info won't be shown in the mode-line, show a message.
		(unless (mode-line-is-window-activated)
			(message "Read only %s." (if buffer-read-only "enabled" "disabled")))))

; Prefix arg

(define-key global-map [?\C-t] #'universal-argument)

(define-key universal-argument-map [?\C-t] #'universal-argument-more)

(dolist (
	key
	'(
		kp-0
		kp-1
		kp-2
		kp-3
		kp-4
		kp-5
		kp-6
		kp-7
		kp-8
		kp-9
		kp-subtract
	)
)
	(define-key universal-argument-map (vector key) nil t))

; Display line numbers

(require 'display-line-numbers)

(define_globalized_minor_mode
	'global-display-line-numbers-mode
	'display-line-numbers-mode
	#'display-line-numbers--turn-on
	nil
	t)

(global-display-line-numbers-mode)

(provide 'myMisc)
