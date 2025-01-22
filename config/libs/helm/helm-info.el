; -*- lexical-binding:nil -*-

(require 'info)

(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info" (filename nodename &optional no-going-back))
(declare-function ring-insert "ring")
(declare-function ring-empty-p "ring")
(declare-function ring-ref "ring")
(defvar Info-history)
(defvar Info-directory-list)
(defvar Info-minibuf-history nil)

; Build info-index sources with `helm-source-info' class.

(defvar helm-info-cache (make-hash-table :test 'eq)
"Hash table mapping sources to their list of candidates.")

(defun helm-info-candidates ()
"Calculate or get from cache candidates for info file of current source.
If file have nodes, loop through all nodes and accumulate candidates
found in each node, otherwise scan only the current info buffer."
	(or
		(gethash helm-current-source helm-info-cache)
		(puthash
			helm-current-source
			(save-window-excursion
				(with-temp-buffer
					(info
						(helm-source-info-file helm-current-source)
						(current-buffer))
					(let (
						Info-history
						candidates
						; Scan current info buffer and add candidates.
						(scan_current_buffer
							(lambda ()
								(goto-char (point-min))
								(while (search-forward "\n* " nil t)
									(unless (search-forward "Menu:\n" (1+ (pos-eol)) t)
										(let* (
											(start (pos-bol))
											; Fix Bug#1503 by getting the invisible
											; info displayed on next line in long strings.
											; e.g "* Foo.\n   (line 12)" instead of
											;     "* Foo.(line 12)"
											(end
												(or
													(save-excursion
														(goto-char start)
														(re-search-forward "(line +[0-9]+)" nil t))
													(pos-eol)))
										)
											(push
												; Long string have a new line inserted before
												; the invisible spec, remove it.
												(string-replace
													"\n"
													""
													(buffer-substring start end))
												candidates))))))
					)
						(if-let ((nodes (Info-index-nodes)))
							(dolist (node nodes)
								(Info-goto-node node)
								(funcall scan_current_buffer))
							(funcall scan_current_buffer))
						(nreverse candidates))))
			helm-info-cache)))

(defun helm-info-update () "Clear cache of current source."
	(remhash helm-current-source helm-info-cache))

(defconst helm-source-info-actions
	(list
		(cons
			"Goto node"
			; Jump to node-line.
			(lambda (node-line)
				(let ((alive (buffer-live-p (get-buffer "*info*"))))
					(Info-goto-node (car node-line))
					(when alive (revert-buffer nil t))
					(helm-goto-line (cdr node-line))
					(helm-highlight-current-line))))))

(defun helm-info-display-to-real (line)
"Transform LINE to an acceptable argument for `info'.
If line have a node use the node, otherwise use directly first name found."
	(let (
		(info-file (helm-source-info-file helm-current-source))
		nodename linum
	)
		; Regex that should match file name, node name and line number in
		; a line like this:
		; \* bind:                                  Bash Builtins.       (line  21).
		(when
			(string-match
				"^\\* +\\(.+\\):[[:space:]]+\\(.*\\)\\(?:[[:space:]]*\\)(line +\\([0-9]+\\))"
				line)
			(setq
				nodename (match-string 2 line)
				linum (match-string 3 line)))
		(if nodename
			(cons
				(concat
					"("
					info-file
					")"
					(replace-regexp-in-string ":\\'" "" nodename))
				(string-to-number linum))
			(cons
				(concat
					"("
					info-file
					")"
					(replace-regexp-in-string
						"::?.*\\'"
						""
						(replace-regexp-in-string "^\\* " "" line)))
				1))))

(cl-defstruct
	(helm-source-info
		(:copier nil)
		(:constructor helm-source-info--make)
		(:include helm-source-sync
			(candidates #'helm-info-candidates)
			(display-to-real #'helm-info-display-to-real)
			(action helm-source-info-actions)))
	; Mandatory.
	(file nil :type string :documentation "Info file."))

(helm-source-define-constructor "helm-source-info" "helm-source-sync")

; Search Info files

; `helm-info' is the main entry point here. It prompts the user for an Info file,
; then a term in the file's index to jump to.

(defvar helm-info-searched (make-ring 32)
"Ring of previously searched Info files.")

(defconst helm-default-info-index-list
	; List of Info files to use for `helm-info'.
	; Elements of the list are strings of Info file names without
	; extensions (e.g., "emacs" for file "emacs.info.gz").
	; Info files are found by searching directories in `Info-directory-list'.
	(progn
		(info-initialize) ; Build Info-directory-list from INFOPATH (Bug#2118)
		(sort
			(cl-labels (
				; Same as `file-name-sans-extension' but remove all extensions.
				(filepath_sans_extension_remove_extension (filepath)
					(when-let ((ext (file-name-sans-extension filepath)))
						; Start searching at index 1 for files beginning with a dot.
						(if (string-search "." (helm-basename ext) 1)
							(filepath_sans_extension_remove_extension ext)
							ext)))
			)
				(helm-fast-remove-dups
					(map_modify_list
						#'filepath_sans_extension_remove_extension
						(mapcan
							(lambda (d)
								(when (file-directory-p d)
									(directory-files d nil "\\.info")))
							(or Info-directory-list Info-default-directory-list)))))
			#'string<))
"Info files to search in with `helm-info'.
List of strings.")

(defun helm-make-commands-from-default-info-index-list ()
"Define Helm info sources for all entries in helm-default-info-index-list.
Sources will be named named helm-info-source-<NAME> where NAME is an element of
helm-default-info-index-list.
Also build commands named `helm-info-<NAME>'."
	(dolist (str helm-default-info-index-list)
		(let ((sym (intern (concat "helm-info-source-" str))))
			(set sym
				(helm-source-info-make nil
					:name (concat "Info for " str)
					:candidate-number-limit 999
					:file str))
			(fset (intern (concat "helm-info-" str))
				`(lambda () ,(format "helm info for %s" str) (interactive)
					(helm
						:sources ',(list (symbol-value sym))
						:default (thing-at-point 'symbol)
						:use-default-as-input t))))))

(helm-make-commands-from-default-info-index-list)

(defconst helm-info-source
	(helm-source-sync-make nil
		:name "Info"
		:candidates (lambda () helm-default-info-index-list)
		:volatile t
		:candidate-number-limit 999
		:nomark t
		:action
			; Search the index of CANDIDATE's Info file using
			; the function helm-info-<CANDIDATE>.
			(lambda (candidate)
				(let (
					(helm-info-function
						(intern-soft (concat "helm-info-" candidate)))
				)
					(when (fboundp helm-info-function)
						(funcall helm-info-function)
						(ring-insert helm-info-searched candidate))))))

(defun helm-info (&optional refresh)
"helm for searching Info files' indices.

With a prefix argument, set REFRESH to non-nil.

Optional parameter REFRESH, when non-nil, re-evaluates
`helm-default-info-index-list'.  If the variable has been
customized, set it to its saved value.  If not, set it to its
standard value. See `custom-reevaluate-setting' for more.

REFRESH is useful when new Info files are installed.
If `helm-default-info-index-list' has not been customized, the new
Info files are made available."
	(interactive "P")
	(let (
		(default
			(unless (ring-empty-p helm-info-searched)
				(ring-ref helm-info-searched 0)))
	)
		(when refresh (helm-make-commands-from-default-info-index-list))
		(helm
			:sources (list helm-info-source)
			:preselect (and default (concat "\\_<" (regexp-quote default) "\\_>"))
			:default (thing-at-point 'symbol)
			:use-default-as-input t)))

; Info at point

; `helm-info-at-point' is the main entry point here. It searches for the
; symbol at point through the Info sources defined in
; `helm-info-default-sources' and jumps to it.

(defvar helm-info-pages-cache nil "Cache for all Info pages on the system.")

(defconst helm-info-source-pages
	(helm-source-sync-make nil
		:name "Info pages"
		:init
			(lambda ()
				(unless helm-info-pages-cache
					; Collect candidates for initial Info node Top.
					(save-window-excursion
						(with-temp-buffer
							(info "dir" (current-buffer))
							(Info-find-node "dir" "top")
							(goto-char (point-min))
							; Info topic regex.
							(while
								(re-search-forward
									"\\* +\\([^:]+: ([^)]+)[^.]*\\)\\." nil t)
								(push
									(match-string-no-properties 1)
									helm-info-pages-cache))))
					(setq helm-info-pages-cache (nreverse helm-info-pages-cache))))
		:update
			(lambda ()
				(setq helm-info-pages-cache nil)
				(funcall (helm-source-init helm-current-source)))
		:candidates (lambda () helm-info-pages-cache)
		:volatile t
		:action
			(list
				(cons
					"Show with Info"
					(lambda (node-str)
						(info (replace-regexp-in-string "^[^:]+: " "" node-str)))))
		:requires-pattern 2))

(defconst helm-info-default-sources
	(list
		helm-info-source-elisp
		helm-info-source-cl
		helm-info-source-eieio
		helm-info-source-pages)
"Default sources to use for looking up symbols at point in Info
files with `helm-info-at-point'.")

(defun helm-info-at-point () (interactive)
	(helm
		:sources helm-info-default-sources
		:default (thing-at-point 'symbol)
		:use-default-as-input t))

(provide 'helm-info)
