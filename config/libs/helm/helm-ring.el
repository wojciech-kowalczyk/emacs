; -*- lexical-binding:nil -*-

; Register

(declare-function undo-tree-restore-state-from-register "ext:undo-tree.el" (register))

(defconst helm-register-max-offset 160
"Max size of string register entries before truncating.
Must be int.")

(defconst helm-register-source
	(helm-source-sync-make nil
		:name "Registers"
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-map)
				(helm-add-delete-binds
					map
					(lambda (cand cache)
						(setq cand (car cand)) ; Get char.
						(setq register-alist (assq-delete-all cand register-alist))
						(cl-delete-if (lambda (c) (= (nth 1 c) cand)) cache))
					t)
				map)
		:candidates
			(lambda ()
				(cl-loop
					for (char . rval) in register-alist
					for key = (single-key-description char)
					for is_registerv = (registerv-p rval)
					for val = (if is_registerv (registerv-data rval) rval)
					for string-actions =
						(cond
							((numberp val)
								(list
									(number-to-string val)
									'insert-register
									'increment-register))
							((markerp val)
								(if-let ((buf (marker-buffer val)))
									(list
										(concat
											"marker - "
											(buffer-name buf)
											", "
											(number-to-string (marker-position val))
											".")
										'jump-to-register
										'insert-register)
									(list "marker in no buffer.")))
							((and (consp val) (window-configuration-p (car val)))
								(list "window configuration." 'jump-to-register))
							((and
									(vectorp val)
									(fboundp 'undo-tree-register-data-p)
									(undo-tree-register-data-p
										(if is_registerv val (aref val 1))))
								(list
									"undo-tree entry."
									'undo-tree-restore-state-from-register))
							((or
									(and (vectorp val) (eq 'registerv (aref val 0)))
									(and (consp val) (frame-configuration-p (car val))))
								(list "frame configuration." 'jump-to-register))
							((and (consp val) (eq (car val) 'file))
								(list
									(concat "file: " (prin1-to-string (cdr val)) ".")
									'jump-to-register))
							((and (consp val) (eq (car val) 'file-query))
								(list
									(concat
										"file-query reference: file "
										(nth 1 val)
										", position "
										(number-to-string (nth 2 val))
										".")
									'jump-to-register))
							((consp val)
								(list
									(concat
										(format "%4d" (length val))
										": "
										(mapconcat #'identity val
											(propertize "<n>"
												'face
												'clipboard-helm-short-representation-face)))
									'insert-register))
							((stringp val)
								(setq val
									(string-replace
										"\n"
										(propertize "<n>"
											'face
											'clipboard-helm-short-representation-face)
										val))
								(list
									(if (> (length val) helm-register-max-offset)
										(concat
											val
											(propertize "..."
												'face
												'clipboard-helm-short-representation-face))
										val)
									'insert-register
									'clipboard-add
									'append-to-register
									'prepend-to-register)))
					when string-actions
						collect
							(cons
								(format "Register %3s: %s" key (car string-actions))
								(cons char (cdr string-actions)))))
		:action-transformer
			(lambda (_actions)
				(cl-loop
					with func-actions =
						(list
							(cons
								'insert-register
								(cons
									"Insert"
									(lambda (c) (insert-register (car c)))))
							(cons
								'clipboard-add
								(cons
									"Copy to clipboard"
									(lambda (c)
										(with-temp-buffer
											(insert-register (car c))
											(clipboard-add (buffer-string) "Register")))))
							(cons
								'jump-to-register
								(cons
									"Jump to"
									(lambda (c) (jump-to-register (car c)))))
							(cons
								'append-to-register
								(cons
									"Append region"
									(lambda (c)
										(append-to-register
											(car c) (region-beginning) (region-end)))))
							(cons
								'prepend-to-register
								(cons
									"Prepend region"
									(lambda (c)
										(prepend-to-register
											(car c) (region-beginning) (region-end)))))
							(cons
								'increment-register
								(cons
									"Increment prefix arg"
									(lambda (c)
										(increment-register helm-current-prefix-arg (car c)))))
							(cons
								'undo-tree-restore-state-from-register
								(cons
									"Restore undo-tree"
									(lambda (c)
										(when (fboundp 'undo-tree-restore-state-from-register)
											(undo-tree-restore-state-from-register (car c)))))))
					for func in (cdr (helm-get-selection))
					when (assq func func-actions)
						collect (cdr it)))))

(defun helm-register () (interactive) (helm :sources (list helm-register-source)))

; Keybord macros

(require 'kmacro)

(defun helm-kmacro-make-current (candidate) "Make CANDIDATE macro the current one."
	(setq kmacro-ring (delete candidate kmacro-ring))
	(kmacro-push-ring)
	(kmacro-split-ring-element candidate))

(defconst helm-kmacro-source
	(helm-source-sync-make nil
		:name "Kmacro"
		:candidates
			(lambda ()
				(map_modify_list
					(lambda (cand)
						(cons
							(help-key-description
								(if (functionp cand)
									; Emacs-29+ (Oclosure).
									(kmacro--keys cand)
									; Emacs-28 and below (list).
									(car cand))
								nil)
							cand))
					(helm-fast-remove-dups (cons (kmacro-ring-head) kmacro-ring))))
		:multiline t
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-map)
				(helm-add-delete-binds
					map
					(lambda (cand cache)
						(let ((head (kmacro-ring-head)))
							(setq kmacro-ring (delete cand kmacro-ring))
							(when (equal head cand) (kmacro-delete-ring-head)))
						(rassq-delete-all cand cache))
					t)
				map)
		:action
			(list
				(cons
					"Execute kmacro (prefix arg to execute arg times)"
					(lambda (candidate)
						; Move candidate on top of list for next use.
						(helm-kmacro-make-current candidate)
						(kmacro-exec-ring-item candidate helm-current-prefix-arg)))
				(cons
					"Concat macros"
					(lambda (_candidate)
						(let ((mkd (helm-marked-candidates)))
							(when (cdr mkd)
								(kmacro-push-ring)
								(setq last-kbd-macro
									(vconcat
										(cl-loop
											for km in mkd
											for keys =
												(cond
													((vectorp km) km)
													((eq (oclosure-type km) 'kmacro)
														(kmacro--keys km))
													(t (car km)))
											if (vectorp keys)
												vconcat keys into result
											else
												collect keys into result
											finally return result)))))))
				(cons
					"Edit macro"
					(lambda (candidate)
						(kmacro-push-ring)
						(setq kmacro-ring (delete candidate kmacro-ring))
						(kmacro-split-ring-element candidate)
						(kmacro-edit-macro)))
				(cons
					"Insert macro"
					; Insert macro at point in `helm-current-buffer'.
					(lambda (candidate)
						(let (
							(desc (read-string "Describe macro briefly: "))
							name key
						)
							(while (fboundp (setq name (intern (read-string "New name for macro: "))))
								(message "Symbol `%s' already exists, choose another name" name)
								(sit-for 1.5))
							(helm-kmacro-make-current candidate)
							(kmacro-name-last-macro name)
							(when (y-or-n-p "Bind macro to a new key?")
								(let (key_binding)
									(while
										(setq key_binding
											(key-binding
												(setq key
													(read-key-sequence-vector "Bind macro to key: "))))
										(message "`%s' already run command `%s', choose another one"
											(help-key-description key nil) key_binding)
										(sit-for 1.5)))
								(global-set-key key name))
							(insert
								(format "; %s%s\n"
									desc
									(and
										key
										(format " (bound to `%s')"
											(help-key-description key nil)))))
							(insert-kbd-macro name (not (null key)))))))))

(defun helm-kmacro () (interactive)
	(helm
		:sources (list helm-kmacro-source)
		:quit-if-no-candidate (lambda () (message "No macro has been defined."))))

(provide 'helm-ring)
