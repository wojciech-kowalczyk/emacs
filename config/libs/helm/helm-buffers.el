; -*- lexical-binding:nil -*-

; Typing new buffer name could show its default tabset and mode.

(defconst helm-buffers-boring-regex "\\` "
"Regex matching boring buffers.
Don't put regexes matching helm buffers here, as they are excluded by default.
This is done mainly for safety, as renaming them, deleting them, etc.
can easily break helm such that it would require emacs' reset for helm to work again.
Also helm buffers are very temporary and there are
little meaningful actions that could be performed on them.")

(defconst helm-buffers-favorite-modes
	'("lisp-interaction-mode" "emacs-lisp-mode" "text-mode" "org-mode")
"List of preferred modes to open new buffers with.
List of strings (mode names, to be interned).")

(defvar helm-buffer-max-length nil
"Max length of buffer names before truncate.
When nil use the longest `buffer-name' length found.")

(defconst helm-buffers-skip-remote-checking nil
"Ignore checking for `file-exists-p' on remote files.")

(defconst helm-buffers-end-truncated-string "..."
"The string to display at end of truncated buffer names.")

(defconst helm-buffers-pretty-names
	'((dired-mode . "Dired") (lisp-interaction-mode . "Lisp Inter"))
"An alist specifying pretty names for modes. (major-mode . string)
Most of the time buffer's `mode-name' is a string so no need to
add it here as there is no need to compute it, but sometimes it
may be a mode-line specification which may be costly to compute,
in this case add here the pretty name as a string to avoid this
costly computation. Also if some pretty names are too long you
can add your own abbreviation here.")


(define_face 'helm-buffers-no-file '((t :foreground "grey60"))
"Face used for non-file buffers.")

(define_face 'helm-buffer-file '((t :inherit helm-ff-file))
"Face for saved, not modified, file buffers.")

(define_face 'helm-buffers-modified '((t :foreground "grey70"))
"Face used for modified (not outside of emacs) file buffers.")

(define_face 'helm-buffers-not-saved '((t :foreground "Indianred2"))
"Face used for file buffers whose files don't exist.")

(define_face 'helm-buffers-modified-externally '((t :foreground "red"))
"Face used for file buffers modfied outside of emacs.")

(define_face 'helm-buffers-dired '((t :foreground "DarkRed"))
"Face used for dired buffers.")

(define_face 'helm-buffers-tramp-archive '((t :foreground "Gold"))
"Face for buffers that are tramp archives.")


(defun helm-buffers-validate-buffer (buffer_name)
"Most action functions should use this to check if buffer still exists.

Return buffer object if buffer_name names alive buffer.

Even if we try we may not be able to prevent every buffer from being killed,
so don't assume that every buffer must live since assembling candidate list
until using it in action."
	(or
		(get-buffer buffer_name)
		(progn
			(message "Buffer \"%s\" no longer exists." buffer_name)
			nil)))

(defun helm-buffers-get-valid-marked ()
	(delq nil (map_modify_list #'helm-buffers-validate-buffer (helm-marked-candidates))))

(defun helm-buffers-create-new-buffer (candidate switch_fn)
	(let ((buffer (get-buffer-create candidate)))
		(if-let* (
			helm-current-prefix-arg
			(mode
				(intern-soft
					(helm-comp-read "Major mode: " helm-buffers-favorite-modes)))
		)
			(with-current-buffer buffer (funcall mode))
			(set-buffer-major-mode buffer))
		(funcall switch_fn buffer)))

(defconst helm-buffers-new-buffer-actions
	(list
		(cons
			"Create buffer (prefix - choose mode)"
			(lambda (candidate)
				(helm-buffers-create-new-buffer
					candidate #'switch-to-buffer)))
		(cons
			"Create buffer other window (prefix - choose mode)"
			(lambda (candidate)
				(helm-buffers-create-new-buffer
					candidate #'switch-to-buffer-other-window)))
		(cons
			"Create buffer other frame (prefix - choose mode)"
			(lambda (candidate)
				(helm-buffers-create-new-buffer
					candidate #'switch-to-buffer-other-frame)))))

(defconst helm-buffers-actions
	(list
		(cons "Switch to buffer(s)" #'helm-buffers-switch-buffers)
		(cons "Switch to buffer(s) other window" #'helm-buffers-switch-buffers-other-window)
		(cons "Switch to buffer(s) other frame" #'helm-buffers-switch-buffers-other-frame)
		(cons "Switch to shell" #'helm-buffers-switch-to-shell)
		(cons "Rename buffer" #'helm-buffers-rename-buffer)
		(cons "Query replace" #'helm-buffers-query-replace)
		(cons "Query replace regex" #'helm-buffers-query-replace-regexp)
		(cons "Grep buffer(s) (prefix - grep all buffers)" #'helm-zgrep-buffers)
		(cons "Occur buffer(s) (prefix - search also in current)" #'helm-buffers-occur)
		(cons "Browse project" #'helm-buffers-browse-project)
		(cons "Move to tabset" #'helm-buffers-move-to-tabset)
		(cons "Clear tab assignment" #'helm-buffers-clear-tab)
		(cons "Ediff marked buffers" #'helm-buffers-ediff-marked)
		(cons "Ediff merge marked buffers" #'helm-buffers-ediff-merge-marked)
		(cons
			"Diff with file"
			(lambda (candidate)
				(when (setq candidate (helm-buffers-validate-buffer candidate))
					(diff-buffer-with-file candidate))))
		(cons "Revert buffer(s)" #'helm-buffers-revert-marked)
		(cons
			"Insert buffer"
			(lambda (candidate)
				(when (setq candidate (helm-buffers-validate-buffer candidate))
					(insert-buffer candidate))))))

(defconst helm-buffers-default-keymap
	(let ((keymap (make-sparse-keymap)))
		(set-keymap-parent keymap helm-map)
		(define-key keymap [?\C-b] #'helm-buffers-toggle-show-hidden-buffers)
		keymap))

(defconst helm-buffers-keymap
	(let ((keymap (make-sparse-keymap)))
		(set-keymap-parent keymap helm-buffers-default-keymap)
		; No need to have separate command for grep and zgrep
		; as we don't use recursivity for buffers.
		; So use zgrep for both as it is capable to handle non-compressed files.
		(define-key keymap [?\C-\S-o]
			(helm-make-action-command #'helm-zgrep-buffers))
		(define-key keymap [?\C-o]
			(helm-make-action-command #'helm-buffers-occur))
		(define-key keymap [?\C-p]
			(helm-make-action-command #'helm-buffers-browse-project))
		(define-key keymap [?\C-d]
			(helm-make-action-command #'helm-buffers-ediff-marked))
		(define-key keymap [?\C-\S-d]
			(helm-make-action-command #'helm-buffers-ediff-merge-marked))
		(define-key keymap [?\C-r]
			(helm-make-action-command #'helm-buffers-query-replace))
		(define-key keymap [?\C-\S-r]
			(helm-make-action-command #'helm-buffers-query-replace-regexp))
		(define-key keymap [f2]
			(helm-make-action-command #'helm-buffers-rename-buffer))
		(define-key keymap [?\C-l]
			(helm-make-action-command #'helm-buffers-switch-to-shell))
		; Goto next/previous tabset.
		(helm-add-goto-bindings
			keymap (lambda () (get-text-property (point) 'helm-buffers-tabset-name)))
		; Revert buffer.
		(define-key keymap [?\C-\S-g]
			(lambda () (interactive)
				(with-helm-buffer
					(helm-buffers-revert-marked)
					(when helm-marked-candidates (helm-unmark-all))
					(helm-force-update t))))
		; Save and kill buffer. With prefix don't save.
		(helm-add-delete-binds
			keymap
			(lambda (cand cache)
				(when-let (
					(buffer (helm-buffers-validate-buffer cand))
					((not (eq buffer helm-current-buffer)))
					((if current-prefix-arg
						(let ((use-short-answers t)) (kill-buffer buffer))
						(with-current-buffer buffer (save_buffer))
						(kill-buffer buffer)))
				)
					(setq cache (rassq-delete-all cand cache)))
				cache)
			t)
		; Save buffer.
		(define-key keymap [?\C-h]
			(lambda () (interactive)
				(with-helm-buffer
					(dolist (buf (helm-marked-candidates))
						(when (setq buf (helm-buffers-validate-buffer buf))
							(with-current-buffer buf (save_buffer))))
					(when helm-marked-candidates (helm-unmark-all))
					(helm-force-update t))))
		(define-key keymap [?\C-\S-\s] #'helm-buffers-mark-similar)
		keymap))


(defun helm-buffers-candidate-transformer-skip (candidates)
	(cl-delete-if
		(lambda (cand) (string-match-p helm-buffers-boring-regex cand))
		(copy-sequence candidates)))

(defun helm-buffers-candidate-transformer-shadow (candidates)
	(mapcar
		(lambda (i)
			(if (string-match helm-buffers-boring-regex i)
				(propertize i 'face 'italic)
				i))
		candidates))

(defvar helm-buffers-candidate-transformer-fn #'helm-buffers-candidate-transformer-skip)
(defvar helm-buffers-mode-line
	(concat
		(if
			(eq
				helm-buffers-candidate-transformer-fn
				#'helm-buffers-candidate-transformer-skip)
			"Skip" "Show")
		" boring  "))

(defun helm-buffers-mode-line () helm-buffers-mode-line)

(defun helm-buffers-persistent-action (candidate)
	(when (setq candidate (helm-buffers-validate-buffer candidate))
		(let ((current (window-buffer helm-persistent-action-display-window)))
			(cond
				((or
						(helm-source-follow helm-current-source)
						(eq current helm-current-buffer)
						(not (eq current candidate)))
					(switch-to-buffer candidate))
				((window-dedicated-p
						(next-window helm-persistent-action-display-window 0))
					(delete-window helm-persistent-action-display-window))
				(t (switch-to-buffer helm-current-buffer))))))

(defun helm-buffers-action-transformer (actions)
	(cond
		((helm-with-window-or-buffer (get-text-property (point) 'helm-new))
			helm-buffers-new-buffer-actions)
		((string= (helm-get-selection) messages-buffer-name)
			(rassq-delete-all 'helm-buffers-move-to-tabset (copy-sequence actions)))
		(t actions)))

(defun helm-buffers-candidate-transformer ()
"Obtain raw candidates (buffer names) from buffer-list slot of helm-current-source
and return a new list of (display . real) cells.
This is not actually helm-source-candidate-transformer, but is used almost exactly
like one."
	(let* (
		(candidates
			; This function creates a new list.
			(funcall helm-buffers-candidate-transformer-fn
				(helm-source-buffers-buffer-list helm-current-source)))
		(candidates_length (length candidates))
		mode_name_max_length
		(mode_name_vector (make-vector candidates_length nil))
		tabset_name_max_length
		(tabset_name_vector (make-vector candidates_length nil))
		(dir_vector (make-vector candidates_length nil))
		remote_buffer_list
		is_remote_buffer ; If there is at least 1 remote buffer.
	)
		(cl-loop
			for buffer_name in candidates
			for i from 0
			for buffer = (get-buffer buffer_name)
			for mode_name =
				; Prevent using `format-mode-line' as much as possible.
				(with-current-buffer buffer
					(or
						(cdr (assq major-mode helm-buffers-pretty-names))
						(and (stringp mode-name) mode-name)
						(format-mode-line mode-name nil nil buffer)))
			for tabset_name = (tab-buffer-tabset-name buffer)
			maximize (length buffer_name) into temp_buffer_name_max_length
			maximize (length mode_name) into temp_mode_name_max_length
			maximize (length tabset_name) into temp_tabset_name_max_length
			do
			(aset mode_name_vector i mode_name)
			(aset tabset_name_vector i tabset_name)
			(when-let (
				(dir
					(with-current-buffer buffer
						(when default-directory
							(abbreviate-file-name default-directory))))
			)
				(aset dir_vector i dir)
				(when (file-remote-p dir) (push buffer_name remote_buffer_list)))
			finally
			(unless (default-value 'helm-buffer-max-length)
				(setq-local helm-buffer-max-length temp_buffer_name_max_length))
			(setq mode_name_max_length temp_mode_name_max_length)
			(setq tabset_name_max_length temp_tabset_name_max_length))
		(when remote_buffer_list
			(setq remote_buffer_list (nreverse remote_buffer_list))
			(setq is_remote_buffer t))
		; Transform each candidate to (display . real).
		(let ((i -1))
			(map_modify_list
				(lambda (buffer_name)
					(++ i)
					(cons
						(let* (
							(buffer (get-buffer buffer_name))
							(filename (buffer-file-name buffer))
							(file_abbrev (when filename (abbreviate-file-name filename)))
							(ext (when filename (helm-file-name-extension filename)))
							(dir (aref dir_vector i))
							(prefix
								; If this buffer is remote.
								(when (eq buffer_name (car remote_buffer_list))
									(setq remote_buffer_list (cdr remote_buffer_list))
									(concat (propertize "@" 'face 'helm-prefix) " ")))
							(archive-p
								(and
									(fboundp 'tramp-archive-file-name-p)
									(tramp-archive-file-name-p dir)))
							(help-echo file_abbrev)
							readable_buffer_name face type proc
						)
							(if prefix
								; Remote tramp buffer names may be hexified,
								; make them more readable.
								(setq
									dir (helm-url-unhex-string dir)
									readable_buffer_name
										(helm-url-unhex-string buffer_name))
								(setq readable_buffer_name (copy-sequence buffer_name)))
							; Handle tramp archive buffers specially.
							(cond
								(archive-p
									(setq
										face 'helm-buffers-tramp-archive
										type 'filebuf))
								; No fancy things on remote buffers.
								((and prefix helm-buffers-skip-remote-checking)
									(setq
										face 'helm-buffer-file
										type 'filebuf))
								((rassoc buffer dired-buffers) ; Dired buffer.
									(setq
										helm-echo dir
										face 'helm-buffers-dired
										type 'dired))
								(file_abbrev ; File buffer.
									(cond
										; A new buffer file not already saved on disk
										; (or a deleted file).
										((not (file-exists-p filename))
											(setq
												face 'helm-buffers-not-saved
												type 'notsaved))
										; A buffer file modified outside of emacs.
										((not (verify-visited-file-modtime buffer))
											(setq
												face 'helm-buffers-modified-externally
												type 'modout))
										; A buffer file modified and not saved on disk.
										((buffer-modified-p buffer)
											(setq
												face 'helm-buffers-modified
												type 'mod))
										; A buffer file not modified and saved on disk.
										(t
											(setq
												face 'helm-buffer-file
												type 'filebuf))))
								(t ; Non-file buffer.
									(setq help-echo dir)
									(setq proc (get-buffer-process buffer))
									(unless proc (setq prefix nil))
									(setq
										face 'helm-buffers-no-file
										type 'nofile)))
							(propertize_no_copy readable_buffer_name
								'help-echo help-echo)
							; readable_buffer_name may already have shadow face
							; from helm-buffers-candidate-transformer-shadow.
							(add-face-text-property 0 (length readable_buffer_name)
								face t readable_buffer_name)
							(when-let* (
								ext
								(dot_and_ext_start
									; Use string search, to support names like
									; "foo.txt<1>".
									(string-search (concat "." (downcase ext))
										(downcase readable_buffer_name)))
							)
								(add-face-text-property
									(1+ dot_and_ext_start)
									(+ dot_and_ext_start 1 (length ext))
									'helm-ff-extension nil readable_buffer_name))
							(let (
								(size (helm-buffer-size buffer))
								(mode_name (aref mode_name_vector i))
								(tabset_name (aref tabset_name_vector i))
								readable_buffer_name_truncated_length
								display
							)
								(propertize_no_copy
									(setq display
										(concat
											(all-the-icons-align
												(with-current-buffer buffer
													(all-the-icons-icon-for-buffer)))
											(or prefix (and is_remote_buffer "  "))
											(if (> (length readable_buffer_name) helm-buffer-max-length)
												; This branch will only execute when
												; (default-value 'helm-buffer-max-length) != nil.
												(concat
													(substring readable_buffer_name
														0
														(setq readable_buffer_name_truncated_length
															(-
																helm-buffer-max-length
																(length helm-buffers-end-truncated-string))))
													helm-buffers-end-truncated-string)
												(setq readable_buffer_name_truncated_length
													(length readable_buffer_name))
												(concat
													readable_buffer_name
													(get_space_string
														(-
															helm-buffer-max-length
															(length readable_buffer_name)))))
											(get_space_string (- 9 (length size)))
											size
											"  "
											mode_name
											(when tabset_name
												(concat
													(get_space_string
														(+ (- mode_name_max_length (length mode_name)) 2))
													tabset_name))
											(let (
												(get_spacing
													(lambda ()
														(get_space_string
															(+
																(-
																	tabset_name_max_length
																	(length tabset_name))
																(if tabset_name
																	2
																	(+
																		(-
																			mode_name_max_length
																			(length mode_name))
																		4))))))
											)
												(cond
													((eq type 'dired)
														(concat
															(funcall get_spacing)
															(propertize dir 'face 'helm-ff-directory)))
													(proc
														(concat
															(funcall get_spacing)
															"("
															(format "%s %s"
																(process-name proc)
																(process-status proc))
															" in "
															(propertize dir 'face 'helm-ff-directory)
															")"))
													(file_abbrev
														(setq file_abbrev
															(propertize file_abbrev
																'face
																; Non existing file.
																(if (eq type 'notsaved)
																	'(helm-ff-nofile helm-ff-file)
																	'helm-ff-file)))
														(helm-files-add-extension-face file_abbrev)
														(helm-files-add-basename-face file_abbrev)
														(concat (funcall get_spacing) file_abbrev))))))
									'helm-buffers-type type
									'helm-buffers-tabset-name tabset_name
									'match-part
										; Reasonable parts to match.
										; [1] Unfortunately this needs to add spaces in
										; between to accidentally not match these parts
										; as one string with no space in between.
										; There should be some better way to do that,
										; probably multimatch should know which parts are
										; separate and match them accordingly, not just
										; concat parts and pass it to multimatch like now.
										(let* (
											(last_part_start
												; 3 for icon, maybe 2 for prefix,
												; buffer-max-length, 11 for size
												; and -1 as prepended space, see [1].
												(+ 13 (if is_remote_buffer 2 0) helm-buffer-max-length))
											(last_part_end
												(+ 1 last_part_start (length mode_name)))
											(match_part_list
												(list
													; Buffer name maybe with prefix.
													(cons
														(if (or (not is_remote_buffer) prefix) 3 5)
														(+
															(if is_remote_buffer 5 3)
															readable_buffer_name_truncated_length))
													; Major mode name.
													(cons last_part_start last_part_end)))
										)
											(when tabset_name
												(+= last_part_start 2 mode_name_max_length)
												(setq last_part_end
													(+ 1 last_part_start (length tabset_name)))
												(nconc
													match_part_list
													(list (cons last_part_start last_part_end))))
											(when (> (length display) last_part_end)
												(nconc
													match_part_list
													(list
														; Associated file.
														(cons
															(+
																2
																last_part_start
																(if tabset_name
																	tabset_name_max_length
																	mode_name_max_length))
															(length display)))))
											match_part_list))))
						buffer_name))
				candidates))))

(cl-defstruct
	(helm-source-buffers
		(:copier nil)
		(:constructor helm-source-buffers--make)
		(:include helm-source-sync
			(candidates #'helm-buffers-list)
			(action helm-buffers-actions)
			(action-transformer #'helm-buffers-action-transformer)
			(keymap helm-buffers-keymap)
			(candidate-transformer #'helm-maybe-add-new-candidate)
			(persistent-action #'helm-buffers-persistent-action)
			(match-part t)
			(confirm t)
			(mode-line #'helm-buffers-mode-line)))

	(buffer-list nil
		:type list
		:documentation "For internal use only. Saved raw candidates."))

(helm-source-define-constructor "helm-source-buffers" "helm-source-sync" nil
	(lambda ()
		(setf (helm-source-candidates source)
			`(lambda ()
				; Save raw candidates (buffer names) in source's buffer-list slot,
				; so it can be later accessed by function toggling skipping and
				; shadowing boring buffers. It could just rerun :candidates
				; function entirely, but it wouldn't really be toggling then,
				; it would be a whole reset.
				(setf (helm-source-buffers-buffer-list ',source)
					(,(helm-source-candidates source)))
				(helm-buffers-candidate-transformer)))))

(defconst helm-buffers-source (helm-source-buffers-make nil :name "Buffers"))

(defun helm-buffers-list ()
"Return the list of buffer names, sorted by their tabset and their
place in them.

This list doesn't include internal helm buffers, like \"*helm*\",
because renaming them and other actions are very unsafe."
	(let* (
		(buffer_list
			(mapcan
				(lambda (tabset) (mapcar #'tab-tab-buffer (cdr tabset)))
				tab-tabset-list))
		(buffer_no_tabset_list
			; Delete buffers from tabsets and internal helm buffers.
			; Internal helm buffers should never be in any tabset.
			(cl-delete-if
				(lambda (buffer)
					(or
						(memq buffer buffer_list)
						(buffer-local-value 'helm-sources buffer)))
				(buffer-list)))
	)
		(nconc
			(map_modify_list #'buffer-name buffer_list)
			(sort
				(map_modify_list #'buffer-name buffer_no_tabset_list)
				#'helm-sort-alpha))))

(defun helm-buffer-size (buffer)
	(with-current-buffer buffer
		(save-restriction
			(widen)
			(helm-file-human-size
				(- (position-bytes (point-max)) (position-bytes (point-min)))))))

(defun helm-buffers-get-regex-for-preselection (buffer_name)
	; " . " for all-the-icons-align format.
	(concat "^ . " (regexp-quote buffer_name)))

(defun helm-buffers-mark-similar ()
"Mark or unmark all buffers that have the same type (the same color),
or tabset if with prefix.
Type/tabset defaults to this of current selection.
If selection is on marked cand, unmark, else mark."
	(interactive)
	(with-helm-window
		(let* (
			(get_thing
				`(lambda ()
					(get-text-property
						(point)
						',(if current-prefix-arg
							'helm-buffers-type 'helm-buffers-tabset-name))))
			(thing (funcall get_thing))
		)
			(helm-mark-some-toggle `(lambda () (equal ',thing (,get_thing)))))))

; Tabset stuff from myTab.el

(defun helm-buffers-move-to-tabset (_cand)
"Move marked buffers to tabset read by `helm-read-tabset'."
	(let ((marked (helm-marked-candidates nil t)))
		(map_modify_list
			(lambda (cand)
				(cons
					cand
					(helm-buffers-validate-buffer (get-text-property 0 'helm-real cand))))
			marked)
		(when (rassq (get-buffer messages-buffer-name) marked)
			(message "%s buffer is unmovable." messages-buffer-name)
			(setq marked (rassq-delete-all (get-buffer messages-buffer-name) marked)))
		(when-let (
			((setq marked (rassq-delete-all nil marked)))
			(tabset_name
				(helm-with-display-candidates (mapcar #'car marked)
					(helm-read-tabset
						:prompt
							(format "Move %d buffer%s to tabset: "
								(length marked)
								(if (length= marked 1) "" "s"))
						:preselect t)))
		)
			(map_modify_list #'cdr marked)
			(let (tabset_to_update_list)
				; Deal with old tabs - delete them and their tabsets if needed.
				; Collect old tabsets that still exist, to update tab-lines of windows
				; displaying them.
				; Transform MARKED to be a list of tabs - preserve old ones if any,
				; else create new ones.
				(map_modify_list
					(lambda (buffer)
						(if (not (tab-excluded buffer))
							(let* (
								(old_tabset
									(tab-tabset (tab-buffer-tabset-name buffer)))
								(tab (tab-tab buffer (cdr old_tabset)))
							)
								; Add or update buffer's entry in
								; tab-buffer-tabset-name-hash-table to associate buffer
								; with it's new tabset.
								(tab-set-buffer-tabset-name buffer tabset_name)
								; Delete buffer's tab from it's old tabset.
								(setcdr old_tabset (delq tab (cdr old_tabset)))
								; Save old tabset to later update tab-lines of windows
								; displaying it.
								(cl-pushnew old_tabset tabset_to_update_list :test #'eq)
								tab)
							; This buffer was excluded (forcefully or not),
							; so it didn't belong to any tabset,
							; so don't add anything to tabset_to_update_list.
							; Override buffer's exclusion.
							(tab-force-buffer buffer t)
							(tab-set-buffer-tabset-name buffer tabset_name)
							(tab-create-tab buffer)))
					marked)
				; Delete old empty tabsets.
				; No need to update tab lines of windows displaying old
				; tabsets, because all windows displaying them will display
				; the new tabset after this function.
				(setq tab-tabset-list (cl-delete-if-not #'cdr tab-tabset-list))
				(setq tabset_to_update_list
					(cl-delete-if-not #'cdr tabset_to_update_list))
				(let (
					(tabset ; Get or create new tabset.
						(or (tab-tabset tabset_name) (tab-create-tabset tabset_name)))
				)
					; Add tabs to the new tabset.
					(setcdr tabset (nconc (cdr tabset) marked))
					; Update tab line of all windows displaying new tabset.
					(tab-update-windows-displaying-tabset-tab-lines tabset_name)
					; And remove this buffer from tabset_to_update_list to
					; not update the same tabset twice.
					(setq tabset_to_update_list (delq tabset tabset_to_update_list)))
				; Update tab line of all windows displaying old tabsets.
				(dolist (tabset tabset_to_update_list)
					(tab-update-windows-displaying-tabset-tab-lines (car tabset)))))))

(defun helm-buffers-clear-tab (_cand)
"Register or re-register marked buffers in tab system, as if they were killed
and then created again."
	(when-let ((marked (helm-buffers-get-valid-marked)))
		; Could be much more optimized, like helm-buffers-move-to-tabset,
		; but here it's a little bit more complicated.
		(let (tabset_to_update_list)
			(dolist (buffer marked)
				(remhash buffer tab-forced-buffer-hash-table)
				(when-let ((tabset_name (tab-buffer-tabset-name buffer)))
					(remhash buffer tab-buffer-tabset-name-hash-table)
					(let ((tabset (tab-tabset tabset_name)))
						; Delete buffer's tab from it's old tabset.
						(setcdr tabset
							(cl-delete-if
								(lambda (tab) (eq buffer (tab-tab-buffer tab)))
								(cdr tabset)))
						; Save old tabset to later update tab-lines of windows
						; displaying it.
						(cl-pushnew tabset tabset_to_update_list :test #'eq))))
			; Delete empty buffers.
			(setq tab-tabset-list (cl-delete-if-not #'cdr tab-tabset-list))
			(setq tabset_to_update_list
				(cl-delete-if-not #'cdr tabset_to_update_list))
			; Update old tabsets.
			(dolist (tabset tabset_to_update_list)
				(tab-update-windows-displaying-tabset-tab-lines (car tabset))))
		; Add buffers to tab system as if they were never there.
		; This is the part that could use some optimization, because now there
		; are possible multiple wasted recomputations of tab-line-format.
		(dolist (buffer marked)
			(unless (tab-should-exclude buffer)
				(tab-add-new-tab-to-maybe-new-tabset-and-update-tab-lines
					buffer (tab-set-buffer-tabset-name buffer))))))


(defun helm-buffers-query-replace-1 (&optional regexp-flag buffers)
"Query replace in marked buffers.
If REGEXP-FLAG is given use `query-replace-regexp'."
	(unless buffers (setq buffers (helm-marked-candidates)))
	(let ((prompt (if regexp-flag "Query replace regex" "Query replace")))
		(let ((args (query-replace-read-args prompt regexp-flag t)))
			(dolist (buf buffers)
				(when (setq buf (helm-buffers-validate-buffer buf))
					(save-window-excursion
						(switch-to-buffer buf)
						(save-excursion
							(let ((case-fold-search t))
								(goto-char (point-min))
								(perform-replace
									(nth 0 args)
									(nth 1 args)
									t
									regexp-flag
									(nth 2 args)
									nil
									multi-query-replace-map)))))))))

(defun helm-buffers-query-replace (_candidate) (helm-buffers-query-replace-1))
(defun helm-buffers-query-replace-regexp (_candidate)
	(helm-buffers-query-replace-1 'regexp))

(defun helm-buffers-revert-marked (_candidate)
	(let ((i 0))
		(dolist (candidate (helm-marked-candidates))
			(when (setq candidate (helm-buffers-validate-buffer candidate))
				(with-current-buffer candidate
					(when (and buffer-file-name (file-exists-p buffer-file-name))
						(++ i)
						(revert-buffer t t)))))
		(when (> i 0) (message "Reverted %d buffer%s." i (if (= i 1) "" "s")))))

(defun helm-buffers-rename-buffer (candidate)
	(when (setq candidate (helm-buffers-validate-buffer candidate))
		(with-current-buffer candidate
			(rename-buffer
				(read-string
					(concat "New name (default " (buffer-name) "): ")
					nil nil (buffer-name))
				t))))

(defun helm-buffers-switch-to-shell (candidate)
	(when (setq candidate (helm-buffers-validate-buffer candidate))
		(let (
			(helm-ff-default-directory (buffer-local-value 'default-directory candidate))
		)
			(helm-ff-switch-to-shell nil))))

(defun helm-buffers-switch-buffers (_candidate)
"Switch to marked buffers and replace current buffer."
	(when-let ((marked (helm-buffers-get-valid-marked)))
		(helm-window-show-buffers marked)))

(defun helm-buffers-switch-buffers-other-window (_candidate)
"Switch to marked buffers in other windows."
	(when-let ((marked (helm-buffers-get-valid-marked)))
		(helm-window-show-buffers marked t)))

(defun helm-buffers-switch-buffers-other-frame (_candidate)
"Display marked buffers in other frame."
	(when-let ((marked (helm-buffers-get-valid-marked)))
		(select-frame (make-frame))
		(helm-window-show-buffers marked)))

(defun helm-buffers-ediff-marked (_candidate &optional merge)
"Ediff 2 marked buffers or CANDIDATE and `helm-current-buffer'.
With optional arg MERGE call `ediff-merge-buffers'."
	(let* (
		(mkd (helm-marked-candidates))
		(lg-lst (length mkd))
		buf1 buf2
	)
		(if (not (memq lg-lst '(1 2)))
			(message "One or two buffers must be marked.")
			(if (= lg-lst 1)
				(setq
					buf1 helm-current-buffer
					buf2 (helm-buffers-validate-buffer (car mkd)))
				(setq
					buf1 (helm-buffers-validate-buffer (car mkd))
					buf2 (helm-buffers-validate-buffer (nth 1 mkd))))
			(and
				buf1
				buf2
				(if merge
					(ediff-merge-buffers buf1 buf2)
					(ediff-buffers buf1 buf2))))))

(defun helm-buffers-ediff-merge-marked (_candidate)
"Ediff merge `helm-current-buffer' with CANDIDATE.
See `helm-buffers-ediff-marked'."
	(helm-buffers-ediff-marked nil t))

; Grep from buffer list

(defun helm-grep-buffers-1 (candidate zgrep)
"Run grep on all file buffers or CANDIDATE if it is a file buffer.
If one of selected buffers is not a file buffer, it is ignored
and grep will run on all other file buffers.
If only one candidate is selected and it is not a file buffer,
switch to this buffer and run `helm-occur'.
If a prefix arg is given run grep on all buffers ignoring
non-file buffers."
	(if-let (
		(files
			; Non-file and remote buffers are ignored.
			(cl-loop
				for buf in
					(if (or current-prefix-arg helm-current-prefix-arg)
						(buffer-list)
						(helm-buffers-get-valid-marked))
				for fname = (buffer-file-name (get-buffer buf))
				when (and fname (not (file-remote-p fname)))
					collect (expand-file-name fname)))
	)
		(let (
			(helm-ff-default-directory
				(if
					(and
						helm-ff-default-directory
						(file-remote-p helm-ff-default-directory))
					default-directory
					helm-ff-default-directory))
		)
			(helm-grep files zgrep))
		(when (setq candidate (helm-buffers-validate-buffer candidate))
			(let ((win-conf (current-window-configuration)))
				; files is empty, that means we have only CANDIDATE
				; and it is not a file buffer, fallback to occur.
				(switch-to-buffer candidate)
				(helm-occur)
				(when (= helm-exit-status 1) (set-window-configuration win-conf))))))

(defun helm-grep-buffers (candidate) "Action to grep buffers."
	(helm-grep-buffers-1 candidate nil))
(defun helm-zgrep-buffers (candidate) "Action to zgrep buffers."
	(helm-grep-buffers-1 candidate 'zgrep))


(defun helm-buffers-occur (_candidate)
"Cccur action for `helm-buffers-source'.
Can be used by any source that list buffers.
With prefix arg search also in current buffer."
	(when-let ((marked (helm-buffers-get-valid-marked)))
		(let ((helm-occur-always-search-in-current helm-current-prefix-arg))
			(helm-occur marked))))

(defun helm-buffers-toggle-show-hidden-buffers () (interactive)
	(if
		(eq
			helm-buffers-candidate-transformer-fn
			#'helm-buffers-candidate-transformer-shadow)
		(setq
			helm-buffers-candidate-transformer-fn
				#'helm-buffers-candidate-transformer-skip
			helm-buffers-mode-line "Skip boring  ")
		(setq
			helm-buffers-candidate-transformer-fn
				#'helm-buffers-candidate-transformer-shadow
			helm-buffers-mode-line "Show boring  "))
	(with-helm-buffer
		(puthash
			helm-default-source
			; Emulate normal call site of :candidates function.
			(let* (
				(helm-current-source helm-default-source)
				(helm-pattern
					(if-let ((fn (helm-source-pattern-transformer helm-current-source)))
						(funcall fn helm-pattern)
						helm-pattern))
			)
				(helm-buffers-candidate-transformer))
			helm-candidate-cache))
	(helm-force-update))

(defun helm-buffers-browse-project (candidate) "Browse project from buffer."
	(when (setq candidate (helm-buffers-validate-buffer candidate))
		(with-current-buffer candidate (helm-browse-project helm-current-prefix-arg))))


(defun helm-buffers () (interactive)
	(helm
		:sources (list helm-buffers-source)
		:preselect (helm-buffers-get-regex-for-preselection (buffer-name))
		:helm-default-keymap helm-buffers-default-keymap))

(define-key global-map [?\C-b] #'helm-buffers)

; Read buffer(s)

(defvar helm-read-buffer-mode-line nil)

(defconst helm-read-buffer-source
	(helm-source-buffers-make nil
		:keymap
			(let ((keymap (make-sparse-keymap)))
				(set-keymap-parent keymap helm-buffers-default-keymap)
				(define-key keymap [?\C-\S-\s] #'helm-buffers-mark-similar)
				keymap)
		:mode-line
			(lambda ()
				(concat
					helm-read-buffer-mode-line
					"  "
					(helm-buffers-mode-line)))
		:action nil
		:action-transformer nil))

(cl-defun helm-read-buffer
	(&key
		marked-candidates
		(must-match 'confirm-new)
		(prompt helm-prompt)
		input
		default
		preselect
		history)
"Return buffer name(s).

MUST-MATCH can be:
	t - strict match - only existing buffers
	nil - existing or not, no confirmation
	\\='confirm-new - confirm non-existing buffers
	\\='confirm-existing - confirm existing buffers
	function with 2 args - candidate in (display . buffer_name) form and action.

PRESELECT can be a buffer name or t to preselect current buffer."
	(let (action nomark)
		(if marked-candidates
			(setq action (lambda (_) (helm-marked-candidates)))
			(setq action #'identity)
			(setq nomark t))
		(setf (helm-source-action helm-read-buffer-source) action)
		(setf (helm-source-nomark helm-read-buffer-source) nomark))
	(let (name confirm candidate_transformer)
		(if (eq must-match t)
			(setq name "existing ")
			(setq candidate_transformer #'helm-maybe-add-new-candidate)
			(when must-match
				(setq confirm
					(cond
						((eq must-match 'confirm-new) t)
						((eq must-match 'confirm-existing)
							(lambda (cand _action)
								(unless (get-text-property 0 'helm-new (car cand))
									"confirm override")))
						(t must-match)))))
		(setf (helm-source-confirm helm-read-buffer-source) confirm)
		(setf (helm-source-candidate-transformer helm-read-buffer-source)
			candidate_transformer)
		(setq name (concat "Read " name "buffer" (when marked-candidates "s")))
		(setq helm-read-buffer-mode-line name)
		(setf (helm-source-name helm-read-buffer-source) name))
	(when preselect
		(when (eq preselect t) (setq preselect (buffer-name)))
		(setq preselect (helm-buffers-get-regex-for-preselection preselect)))
	(helm
		:sources (list helm-read-buffer-source)
		:prompt prompt
		:input input
		:default default
		:preselect preselect
		:history history
		:allow-nest t
		:resume 'noresume
		:helm-default-keymap helm-buffers-default-keymap))

(provide 'helm-buffers)
