; -*- lexical-binding:nil -*-

; No warnings in Emacs built --without-x
(declare-function x-list-fonts "xfaces.c")

; Xfont selection

(defvar helm-previous-font nil)
(defvar helm-xfonts-cache nil)
(defconst helm-xfonts-source
	(helm-source-sync-make nil
		:name "X Fonts"
		:init
			(lambda ()
				(unless helm-xfonts-cache (setq helm-xfonts-cache (x-list-fonts "*")))
				; Save current font so it can be restored in cleanup.
				(setq helm-previous-font (cdr (assq 'font (frame-parameters)))))
		:update (lambda () (setq helm-xfonts-cache (x-list-fonts "*")))
		:candidates (lambda () helm-xfonts-cache)
		:volatile t
		:action #'set-frame-font
		:persistent-action #'set-frame-font
		:follow 'never
		:nomark t
		:cleanup
			(lambda ()
				; When font has changed.
				(unless (eq helm-previous-font (cdr (assq 'font (frame-parameters))))
					; Restore previous font
					(set-frame-font helm-previous-font)))))

(defun helm-select-xfont () (interactive) (helm :sources (list helm-xfonts-source)))

; Ucs symbol completion

(defvar helm-ucs-history nil)

(desktop::add_to_globals 'helm-ucs-history)

(defconst helm-ucs-actions
	(list
		(cons "Insert character" #'helm-ucs-insert-char)
		(cons "Insert character name" #'helm-ucs-insert-name)
		(cons "Insert character code in hex" #'helm-ucs-insert-code)
		(cons "Copy char" #'helm-ucs-copy-char)
		(cons "Copy name" #'helm-ucs-copy-name)
		(cons "Copy code" #'helm-ucs-copy-code)
		(cons "Describe char" #'helm-ucs-describe-char)))

(defconst helm-ucs-map
	(let ((map (make-sparse-keymap)))
		(set-keymap-parent map helm-map)
		; Bind other-window key to easily switch between minibuffer and
		; helm-current-buffer's window.
		; Maybe it would be better to remove 'no-other-window prop from
		; helm-current-buffer's window instead or making this binding?
		(define-key map [kp-enter]
			(lambda () (interactive)
				(select-window (get-buffer-window helm-current-buffer))))
		; Move normal copy action to C-S-c and define prefix map C-c.
		(define-key map [?\C-\S-c] #'helm-copy-candidate)
		(define-key map [?\C-c ?1]
			(lambda () (interactive)
				(helm-ucs-copy-char (helm-get-selection))
				(message "Char copied.")))
		(define-key map [?\C-c ?2]
			(lambda () (interactive)
				(helm-ucs-copy-name (helm-get-selection))
				(message "Char name copied.")))
		(define-key map [?\C-c ?3]
			(lambda () (interactive)
				(helm-ucs-copy-code (helm-get-selection))
				(message "Char code copied.")))
		map))

(define_face 'helm-ucs-char '((t :foreground "gold"))
"Face used to display ucs characters.")

(defvar helm-ucs-cache nil)

(defun helm-ucs-match (candidate n)
"Return the N part of an ucs CANDIDATE.
Where N=1 is the ucs code, N=2 the ucs char and N=3 the ucs name."
	(when (string-match "^(\\(#x[a-f0-9]+\\)): *\\(.\\) *\\([^:]+\\)+" candidate)
		(match-string n candidate)))

(let ((i 1))
	(dolist (name '("code" "char" "name"))
		(fset (intern (concat "helm-ucs-insert-" name))
			`(lambda (candidate)
				(add-to-history 'helm-ucs-history candidate)
				(insert (helm-ucs-match candidate ,i))))
		(++ i)))

(let ((i 1))
	(dolist (name '("code" "char" "name"))
		(fset (intern (concat "helm-ucs-copy-" name))
			`(lambda (candidate)
				(add-to-history 'helm-ucs-history candidate)
				(clipboard-add (helm-ucs-match candidate ,i) helm-buffer)))
		(++ i)))

(defun helm-ucs-describe-char (candidate)
	(with-temp-buffer
		(insert (helm-ucs-match candidate 2))
		(describe-char (point-min)))
	(when-let ((window (get-buffer-window "*Help*")))
		(fit-window-to-buffer window)))

(defun helm-ucs-persistent-action (candidate)
	(with-current-buffer helm-current-buffer (helm-ucs-insert-char candidate))
	(helm-force-update t))

(defconst helm-ucs-history-source
	(helm-source-sync-make nil
		:name "UCS history"
		:candidates
			(lambda ()
				(mapcar
					(lambda (cand)
						(string-match
							"^(\\(#x[a-f0-9]+\\)): *\\(.\\) *\\([^:]+\\)+"
							cand)
						(concat
							(substring cand 0 (match-beginning 2))
							(propertize_no_copy (match-string 2 cand)
								'face 'helm-ucs-char)
							(substring cand (match-end 2))))
					helm-ucs-history))
		:persistent-action #'helm-ucs-persistent-action
		:follow 'never
		:action helm-ucs-actions
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-ucs-map)
				(helm-add-delete-binds
					map
					(lambda (cand cache)
						(setq helm-ucs-history (delete cand helm-ucs-history))
						(delete cand cache))
					t)
				map)))

; Only math* symbols are collected.
(defun helm-ucs-candidates ()
	(setq helm-ucs-cache
		(let* (
			(names (ucs-names))
			(progress_reporter
				(make-progress-reporter
					"helm-ucs: calculating candidates"
					0
					(* (hash-table-count names) 2)))
			code_max_length
			char_str_max_length
			code_and_char_str_list
		)
			(cl-loop
				for _name being the hash-keys of names using (hash-values char)
				for i from 1
				for code = (concat "#x" (format "%x" char))
				for char_str = (char-to-string char)
				maximize (length code) into temp_code_max_length
				maximize (string-width char_str) into temp_char_str_max_length
				collect (cons code char_str) into temp_code_and_char_str_list
				do (progress-reporter-update progress_reporter i)
				finally
				(setq code_max_length temp_code_max_length)
				(setq char_str_max_length temp_char_str_max_length)
				(setq code_and_char_str_list temp_code_and_char_str_list))
			(cl-loop
				for name being the hash-keys of names
				for i from (1+ (hash-table-count names))
				for (code . char_str) = (pop code_and_char_str_list)
				unless
					(or
						(string= name "")
						(not (fontp (char-displayable-p (read code)))))
					collect
						(progn
							(progress-reporter-update progress_reporter i)
							(concat
								"(" code "): "
								(get_space_string (- code_max_length (length code)))
								(propertize_no_copy char_str 'face 'helm-ucs-char)
								(get_space_string
									(- char_str_max_length (string-width char_str)))
								name))))))

(defconst helm-ucs-source
	(helm-source-sync-make nil
		:name "UCS"
		:candidates (lambda () helm-ucs-cache)
		:volatile t
		:update #'helm-ucs-candidates
		:sort #'helm-sort-length
		:action helm-ucs-actions
		:persistent-action #'helm-ucs-persistent-action
		:follow 'never
		:keymap helm-ucs-map
		:nomark t)
"Source for collecting `ucs-names' math symbols.")

(defun helm-ucs (arg)
"Called with a prefix arg (or `helm-force-update' in helm) force reloading cache."
	(interactive "P")
	(when arg (setq helm-ucs-cache nil ucs-names nil))
	(unless helm-ucs-cache (helm-ucs-candidates))
	(helm
		:sources (list helm-ucs-history-source helm-ucs-source)
		:input
			(when-let (
				(char (char-after))
				((multibyte-string-p (setq char (char-to-string char))))
			)
				char)))

(provide 'helm-font)
