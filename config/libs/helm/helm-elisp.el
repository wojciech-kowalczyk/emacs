; -*- lexical-binding:nil -*-

(defun helm-symbolify (string_or_symbol)
	(cond
		((symbolp string_or_symbol) string_or_symbol)
		((not (string= string_or_symbol "")) (intern string_or_symbol))))

; Advice Emacs fn
; Make Classes's docstrings more readable by removing the attempts to align
; unuseful stuff and add newline for separating slot documentation, also
; slots are in bold characters.

(defun helm-source--cl--print-table (&rest args)
"Advice for `cl--print-table' to make readable class slots docstrings."
	(dolist (row (nth 1 args))
		(setcar row (propertize (car row) 'face 'bold))
		(setcdr row (nthcdr 2 row))
		(insert "\n* " (apply #'format "%s\n\n  Initform=%s\n\n%s" row) "\n")))

(defun helm-describe-class (class)
"Display documentation of Eieio CLASS, a symbol or a string."
	(let ((advicep (advice-member-p #'helm-source--cl--print-table 'cl--print-table)))
		(unless advicep
			(advice-add
				'cl--print-table
				:override
				#'helm-source--cl--print-table
				'((depth . 100))))
		(unwind-protect
			(cl-describe-type (helm-symbolify class))
			(unless advicep
				(advice-remove 'cl--print-table #'helm-source--cl--print-table)))))

(defun helm-describe-function (func)
"Display documentation of FUNC, a symbol or string."
	(cl-letf (((symbol-function 'message) #'ignore))
		(describe-function (helm-symbolify func))))

(defun helm-describe-variable (var)
"Display documentation of VAR, a symbol or a string."
	(cl-letf (((symbol-function 'message) #'ignore))
		(describe-variable (helm-symbolify var))))

(defun helm-describe-face (face)
"Display documentation of FACE, a symbol or a string."
	(let ((faces (helm-marked-candidates)))
		(cl-letf (((symbol-function 'message) #'ignore))
			(describe-face
				(if (cdr faces)
					(mapcar #'helm-symbolify faces)
					(helm-symbolify face))))))


(defconst helm-find-function-default-project nil
"Default directories to search symbols definitions from `helm-apropos'.
A list of directories or nil.
Helm will allow you selecting one of those directories with `M-n' when
using a prefix arg with the `find-function' action in `helm-apropos'.
This is a good idea to add the directory names of the projects you are
working on to quickly jump to the definitions in the project source
files instead of jumping to the loaded files located in `load-path'.")

(defun helm-find-function-noselect (fn &optional type)
"Find function definition without selecting buffer.
'fn' must be a symbol.
Instead of looking in LOAD-PATH to find library, this function
TYPE when nil specify function, for other values see
`find-function-regexp-alist'."
	(let (
		(find-function-source-path
			(let (
				(dir
					(helm-read-file-name
						"Project directory: "
						:test (lambda (file) (string-suffix-p "/" file))
						:default helm-find-function-default-project
						:must-match t
						:type 'dir))
			)
				(cons dir (helm-walk-directory dir :directories 'only :path 'full))))
	)
		(if-let (
			(symbol-lib
				(if (memq type '(defvar defface))
					(or (symbol-file fn type) (help-C-file-name fn 'var))
					; Sometimes e.g. with prefix key symbols
					; `find-function-library' returns a list of only one
					; element, the symbol itself i.e. no library.
					(cdr (find-function-library fn))))
			(library (find-library-name (helm-basename symbol-lib t)))
		)
			(find-function-search-for-symbol fn type library)
			(error "Don't know where `%s' is defined" fn))))

(defun helm-find-function (fn)
"Try to jump to function definition.
'fn' must be a symbol.
With a prefix arg ask for the project directory to search in
instead of using LOAD-PATH."
	(if helm-current-prefix-arg
		(let ((place (helm-find-function-noselect fn)))
			(if (cdr place)
				(progn
					(switch-to-buffer (car place))
					(goto-char (cdr place)))
				(if-let ((buffer (car place)))
					(message "Couldn't find function `%s' in `%s'"
						func (buffer-name buffer))
					(message "Couldn't find function `%s'" fn))))
		(find-function fn)))

(defun helm-find--base (thing find_fn helm_find_function_noselect_arg)
	(if helm-current-prefix-arg
		(when-let (
			(place (helm-find-function-noselect thing helm_find_function_noselect_arg))
		)
			(switch-to-buffer (car place))
			(goto-char (cdr place)))
		(funcall find_fn thing)))

(defun helm-find-variable (var)
"Try to jump to VAR definition. 'var' must be a symbol.
With a prefix arg ask for the project directory to search in
instead of using LOAD-PATH."
	(helm-find--base var #'find-variable 'defvar))

(defun helm-find-face-definition (face)
"Try to jump to FACE definition. 'face' must be a symbol.
With a prefix arg ask for the project directory to search in
instead of using LOAD-PATH."
	(helm-find--base face #'find-face-definition 'defface))

(defun helm-group-p (symbol) "Return non nil when SYMBOL is a group."
	(or
		(and (get symbol 'custom-loads) (not (get symbol 'custom-autoload)))
		(get symbol 'custom-group)))


(defvar helm-elisp--persistent-help-symbol nil)
(defun helm-elisp--persistent-help (candidate fn)
"Persistent action to show help for CANDIDATE using FN.
CANDIDATE can be a symbol or string."
	; If help for CANDIDATE is currently displayed.
	(if
		(and
			; CANDIDATE is the same as the last one.
			helm-elisp--persistent-help-symbol
			(string= helm-elisp--persistent-help-symbol candidate)
			; Persistent action's window displays help buffer.
			(eq
				(get-buffer (help-buffer))
				(window-buffer helm-persistent-action-display-window)))
		; If persistent action is explicitly invoked, kill help buffer and do
		; something with its window - usually show helm-current-buffer back in
		; help buffer's window, but if helm-current-buffer isn't in this window's
		; prev buffers, just delete this window, because it was split by
		; helm-persistent-action-display-window (the function).
		; Reminder that the reason why help buffer is shown in
		; helm-persistent-action-display-window is let-binding in
		; helm-execute-persistent-action -
		; (display-buffer-alist '((".*" (display-buffer-same-window)))).
		;
		; Else do nothing (automatic follow action shouldn't kill help buffer).
		(when (eq this-command 'helm-execute-persistent-action)
			(let ((help_buffer (window-buffer helm-persistent-action-display-window)))
				; If this help buffer is actually also helm-current-buffer, do nothing.
				; I guess it's a problem if window was split, because now we could be left
				; with 2 windows displaying helm-current-buffer, but nothing uses splitting
				; persistent action window anyway and there is no easy solution
				; so leave it like this for now.
				(unless (eq help_buffer helm-current-buffer)
					(setq helm-elisp--persistent-help-symbol nil)
					(if-let (
						(buffer_start_point
							(assq
								helm-current-buffer
								(window-prev-buffers helm-persistent-action-display-window)))
					)
						; Simpler (and more specific) switch-to-prev-buffer.
						(set-window-buffer-start-and-point
							helm-persistent-action-display-window
							(pop buffer_start_point)
							(car buffer_start_point)
							(nth 1 buffer_start_point))
						(window--delete helm-persistent-action-display-window nil t))
					(kill-buffer help_buffer))))
		; Save candidate and display help for it.
		(setq helm-elisp--persistent-help-symbol candidate)
		(funcall fn candidate)))


(defconst helm-apropos-defaut-info-lookup-sources
	(list helm-info-source-elisp helm-info-source-cl helm-info-source-eieio)
"A list of sources to look into when searching info page of a symbol.")

(defun helm-get-first-line-documentation (sym)
"Return first line documentation of symbol SYM truncated at 72 column.
If SYM is not documented, return \"Not documented\"."
	(let (
		(doc
			(condition-case nil
				(cond
					((class-p sym) (cl--class-docstring (cl--find-class sym)))
					((custom-theme-p sym)
						(documentation-property sym 'theme-documentation t))
					((helm-group-p sym)
						(documentation-property sym 'group-documentation t))
					((fboundp sym) (documentation sym t))
					((boundp sym) (documentation-property sym 'variable-documentation t))
					((facep sym) (face-documentation sym)))
				(void-function "Void function")))
	)
		(cond
			((and
					doc
					(not (string= doc ""))
					; `documentation' return "\n\n(args...)"
					; for CL-style functions.
					(not (string-prefix-p "\n\n" doc)))
				; Some commands specify key bindings in their first line.
				; Turns out that substitute-command-keys can append
				; "\nUses <map> that is not already defined or loaded.\n"
				; or something like this, so split-string one more time.
				(car
					(split-string
						(substitute-command-keys (car (split-string doc "\n"))) "\n")))
			((or (symbol-function sym) (boundp sym) (facep sym) (helm-group-p sym))
				"Not documented")
			(t
				; Symbol exist but has no definition yet e.g.
				; (advice-add 'foo-test :override (lambda () (message "invalid
				; function"))) and foo-test is not already defined.
				"Not already defined or loaded"))))

; Apropos

(defvar helm-apropos-history nil)

(cl-defstruct
	(helm-source-apropos
		(:copier nil)
		(:constructor helm-source-apropos--make)
		(:include helm-source-sync (nomark t)))
	(default-symbol nil
		:type symbol
		:documentation
"Optional symbol that will come first as a candidate.
It will happen only if 'test' argument to helm-apropos-candidates will
will return non-nil after calling it with default-symbol."))

(helm-source-define-constructor "helm-source-apropos" "helm-source-sync")

(defun helm-apropos-candidates (test &optional fn)
"Collect candidates for `helm-apropos' sources.
If FN is not provided symbols are fetched against obarray with predicate TEST."
	(let (
		(default-symbol (helm-source-apropos-default-symbol helm-current-source))
		(symbols (if fn (funcall fn) (all-completions "" obarray test)))
	)
		(if (and default-symbol (funcall test default-symbol))
			(cons default-symbol symbols)
			symbols)))

(defun helm-apropos-short-doc-transformer (candidates)
	(let ((max_length (helm-get-max-length candidates)))
		(map_modify_list
			(lambda (cand)
				(let ((symbol (intern-soft cand)))
					(cons
						(propertize_no_copy
							(concat
								cand
								(get_space_string (1+ (- max_length (length cand))))
								(propertize
									(helm-get-first-line-documentation symbol)
									'face 'helm-command-short-doc))
							'match-part (list (cons 0 (length cand))))
						symbol)))
			candidates)))

(defun helm-apropos-clean-history-variable (candidate)
	(with-current-buffer helm-current-buffer ; var may be local
		(let* (
			(sym (intern-soft candidate))
			(cands (symbol-value sym))
		)
			(when (consp cands)
				(dolist (
					elm
					(helm-comp-read
						"Delete entries: " cands :must-match t :marked-candidates t)
				)
					(setq cands (delq elm cands)))
				(set sym cands)))))

(defun helm-apropos-clean-ring (candidate)
	(with-current-buffer helm-current-buffer ; var may be local
		(let* (
			(sym (intern-soft candidate))
			(val (symbol-value sym))
		)
			(when-let (
				((ring-p val))
				(cands (ring-elements val))
			)
				(dolist (
					elm
					(helm-comp-read
						"Delete entries: " cands :must-match t :marked-candidates t)
				)
					(ring-remove val (index_in_list_equal (ring-elements val) elm))
					(set sym val))))))

(defun helm-info-lookup-symbol (candidate)
	; ???:Running an idle-timer allows not catching RET when exiting
	; with the fallback source.
	; (run-with-idle-timer 0.01 nil #'helm-info-lookup-symbol-1 candidate)
	(helm
		:sources
			(append
				helm-apropos-defaut-info-lookup-sources
				(list
					(let ((sym (helm-symbolify candidate)) src-name fn)
						(cond
							((class-p sym)
								(setq
									fn #'helm-describe-function
									src-name "Describe class"))
							((cl-generic-p sym)
								(setq
									fn #'helm-describe-function
									src-name "Describe generic function"))
							((fboundp sym)
								(setq
									fn #'helm-describe-function
									src-name "Describe function"))
							((facep sym)
								(setq
									fn #'helm-describe-face
									src-name "Describe face"))
							(t
								(setq
									fn #'helm-describe-variable
									src-name "Describe variable")))
						(helm-source-sync-make nil
							:name src-name
							:candidates `(lambda () '(,candidate))
							:persistent-action
								`(lambda (candidate)
									(helm-elisp--persistent-help candidate #',fn))
							:nomark t
							:action fn))))
		:resume 'noresume
		:input (helm-stringify candidate)
		:execute-action-at-once-if-one 'current-source))

(defconst helm-apropos-sources
	(let (
		(function_actions
			(list
				(cons "Describe function" #'helm-describe-function)
				(cons "Find function (C-u for source)" #'helm-find-function)
				(cons "Info lookup" #'helm-info-lookup-symbol)
				(cons "Debug on entry" #'debug-on-entry)
				(cons "Cancel debug on entry" #'cancel-debug-on-entry)
				(cons "Trace function" #'trace-function)
				(cons "Trace function (background)" #'trace-function-background)
				(cons "Untrace function" #'untrace-function)))
	)
		(list
			(helm-source-apropos-make nil
				:name "Commands"
				:candidates (lambda () (helm-apropos-candidates #'commandp))
				; There can be thousands of candidates, so use candidate-transformer here.
				:candidate-transformer
					(lambda (candidates) (helm-command-transformer candidates t))
				:persistent-action
					(lambda (candidate)
						(helm-elisp--persistent-help
							candidate #'helm-describe-function))
				:action function_actions)
			(helm-source-apropos-make nil
				:name "Functions"
				:candidates
					(lambda ()
						(helm-apropos-candidates
							(lambda (x)
								(and
									(fboundp x)
									(not (commandp x))
									(not (cl-generic-p x))
									(not (class-p x))))))
				:candidate-transformer #'helm-apropos-short-doc-transformer
				:persistent-action
					(lambda (candidate)
						(helm-elisp--persistent-help
							candidate #'helm-describe-function))
				:action function_actions)
			(helm-source-apropos-make nil
				:name "Classes"
				:candidates (lambda () (helm-apropos-candidates #'class-p))
				:candidate-transformer #'helm-apropos-short-doc-transformer
				:persistent-action
					(lambda (candidate)
						(helm-elisp--persistent-help
							candidate #'helm-describe-class))
				:action
					(list
						(cons "Describe Class" #'helm-describe-class)
						(cons "Find Class (prefix - source)" #'helm-find-function)
						(cons "Info lookup" #'helm-info-lookup-symbol)))
			(helm-source-apropos-make nil
				:name "Generic functions"
				:candidates (lambda () (helm-apropos-candidates #'cl-generic-p))
				:candidate-transformer #'helm-apropos-short-doc-transformer
				:persistent-action
					(lambda (candidate)
						(helm-elisp--persistent-help
							candidate #'helm-describe-function))
				:action
					(list
						(cons "Describe function" #'helm-describe-function)
						(cons "Find function (prefix - source)" #'helm-find-function)
						(cons "Info lookup" #'helm-info-lookup-symbol)))
			(helm-source-apropos-make nil
				:name "Variables"
				:candidates
					(lambda ()
						(helm-apropos-candidates
							(lambda (x)
								(and (boundp x) (not (keywordp x)) (not (class-p x))))))
				:candidate-transformer #'helm-apropos-short-doc-transformer
				:persistent-action
					(lambda (candidate)
						(helm-elisp--persistent-help
							candidate #'helm-describe-variable))
				:action
					(list
						(cons "Describe variable" #'helm-describe-variable)
						(cons "Find variable" #'helm-find-variable)
						(cons "Info lookup" #'helm-info-lookup-symbol)
						(cons
							"Set variable"
							(lambda (sym)
								(let* (
									(val (default-value sym))
									(strv (prin1-to-string val))
								)
									(if (> (length strv) 25)
										(let* (
											(val (symbol-value sym))
											(pp (pp-to-string val))
											start
										)
											(with-current-buffer (get-buffer-create helm-pretty-print-buffer-name)
												(erase-buffer)
												(helm-edit-variable-mode)
												(setq start (point))
												; Any number of lines starting with ";" + one empty line.
												(insert
													(format "; Edit variable `%s' and hit C-c C-c when done\n" sym)
													"; Abort with C-c C-k\n\n")
												(add-text-properties start (1- (point)) '(read-only t))
												(add-text-properties (1- (point)) (point) '(read-only t rear-nonsticky t))
												(setq-local helm-pretty-print-current-symbol sym)
												(add-hook 'kill-buffer-hook
													(lambda ()
														(when (file-exists-p helm-pretty-print-temp-file-name)
															(delete-file helm-pretty-print-temp-file-name)))
													nil t)
												(save-excursion (insert pp))
												(write-region (point-min) (point-max) helm-pretty-print-temp-file-name nil 1)
												(setq buffer-file-name helm-pretty-print-temp-file-name))
											(display-buffer helm-pretty-print-buffer-name))
										(set-default sym
											(eval-minibuffer
												(format "Set `%s': " sym)
												(if
													(or
														(arrayp val)
														(memq val '(nil t))
														(numberp val))
													strv
													(format "'%s" strv)))))))))
				:action-transformer
					(lambda (actions)
						(let* (
							(sym (helm-get-selection))
							(val (buffer-local-value sym helm-current-buffer))
						)
							(cond
								((custom-variable-p sym)
									(append
										actions
										(let ((standard-value (eval (car (get sym 'standard-value)) t)))
											(unless (equal standard-value (symbol-value sym))
												(list
													(cons
														"Reset Variable to default value"
														`(lambda (candidate)
															(set candidate ',standard-value))))))
										(list (cons "Customize variable" #'customize-option))))
								((and val (ring-p val))
									(append
										actions
										(list (cons "Clean ring" #'helm-apropos-clean-ring))))
								((and (string-search "history" (symbol-name sym)) (listp val))
									(append
										actions
										(list
											(cons
												"Clean variable"
												#'helm-apropos-clean-history-variable))))
								(t actions)))))
			(helm-source-apropos-make nil
				:name "Faces"
				:candidates
					; face-list isn't that long, so prepare candidates here.
					(lambda ()
						(helm-apropos-short-doc-transformer
							(sort
								(mapcar
									(lambda (cand) (propertize (symbol-name cand) 'face cand))
									(helm-apropos-candidates #'facep #'face-list))
								#'helm-sort-alpha)))
				:sort #'helm-sort-length
				:match-part t
				:nomark nil ; Override default.
				:persistent-action
					(lambda (candidate)
						(helm-elisp--persistent-help candidate #'helm-describe-face))
				:action
					(list
						(cons "Describe face" #'helm-describe-face)
						(cons "Find face" #'helm-find-face-definition)
						(cons "Customize face" #'customize-face))))))

(defun helm-apropos (default)
"Preconfigured Helm to describe commands, functions, variables and faces.
In non interactives calls DEFAULT argument should be provided as
a string, i.e. the `symbol-name' of any existing symbol."
	(interactive
		(list
			(symbol-name
				(intern-soft
					(with-syntax-table emacs-lisp-mode-syntax-table
						(thing-at-point 'symbol))))))
	; Update default-symbol slot.
	(when-let ((default_symbol (intern-soft default)))
		(dolist (source helm-apropos-sources)
			(setf (helm-source-apropos-default-symbol source) default_symbol)))
	(helm
		:sources helm-apropos-sources
		:history 'helm-apropos-history
		:preselect (when default (concat "^\\_<" (regexp-quote default) "\\_>"))
		:helm-truncate-lines t))

; Advices

(defvar ad-advised-functions)
(defvar ad-advice-classes)
(declare-function ad-make-single-advice-docstring "advice")
(declare-function ad-get-advice-info-field "advice")
(declare-function ad-advice-set-enabled "advice")
(declare-function ad-advice-enabled "advice")

(defun helm-advice-toggle (func-class-advice)
	(let ((advice (nth 2 func-class-advice)) val msg)
		(if (ad-advice-enabled advice)
			(setq msg "Disabled")
			(setq val t msg "Enabled"))
		(ad-advice-set-enabled advice val)
		(message msg))
	(ad-activate (car func-class-advice))
	(when helm-in-persistent-action
		; Update current display string.
		(with-helm-window
			(let (
				(pos (point))
				(real (get-text-property (point) 'helm-real))
			)
				(when-let (
					(newword
						(cond
							((looking-at "Disabled") "Enabled")
							((looking-at "Enabled") "Disabled")))
				)
					(forward-word 1)
					(delete-region pos (point))
					(insert newword))
				(goto-char pos)
				(when real
					(save-excursion
						(helm-goto-candidate-end)
						(put-text-property pos (point) 'helm-real real)))))))

(defconst helm-source-advice
	(helm-source-sync-make nil
		:name "Function advice"
		:candidates
			(lambda ()
				(mapcan
					(lambda (fname)
						(let ((fn (intern fname)))
							(mapcan
								(lambda (class)
									(mapcar
										(lambda (advice)
											(list
												(concat
													(if (ad-advice-enabled advice) "Enabled" "Disabled")
													" "
													(propertize fname 'face 'font-lock-function-name-face)
													" "
													(ad-make-single-advice-docstring advice class nil))
												fn class advice))
										(ad-get-advice-info-field fn class)))
								ad-advice-classes)))
					ad-advised-functions))
		:action (list (cons "Toggle advice" #'helm-advice-toggle))
		:persistent-action
			(lambda (func-class-advice)
				(if current-prefix-arg
					(helm-advice-toggle func-class-advice)
					(describe-function (car func-class-advice))))
		:nomark t
		:multiline t))

(defun helm-manage-advice () (interactive) (helm :sources (list helm-source-advice)))

; Modify variables from Helm
; Idk I don't use this, probably should delete.

(defconst helm-edit-variable-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map (kbd "C-c C-c") #'helm-set-variable-from-pp-buffer)
		(define-key map (kbd "C-c C-k") #'helm-edit-variable-quit)
		(define-key map (kbd "C-c =")   #'helm-edit-variable-toggle-diff)
		map))

(define-derived-mode helm-edit-variable-mode emacs-lisp-mode "helm-edit-variable"
"A mode to edit variables values.

Special commands:
\\{helm-edit-variable-mode-map}")

(defvar helm-pretty-print-temp-file-name
	(expand-file-name "helm-edit-variable.el" temporary-file-directory))
(defvar helm-pretty-print-buffer-name "*pretty print output*")
(defvar helm-pretty-print-current-symbol nil)

(defun helm-edit-variable-toggle-diff () "Show diff in edit variable buffer."
	(interactive)
	(if (get-buffer-window "*Diff*" 'visible)
		(kill-buffer "*Diff*")
		(diff-buffer-with-file)))

(defun helm-set-variable-from-pp-buffer ()
"Set variable associated with buffer to buffer contents.

The associated variable is the local variable `helm-pretty-print-current-symbol'."
	(interactive)
	(with-current-buffer helm-pretty-print-buffer-name
		(goto-char (point-min))
		(when (re-search-forward "^$" nil t) (forward-line 1))
		(let ((val (symbol-value helm-pretty-print-current-symbol)))
			(set-default helm-pretty-print-current-symbol
				(save-excursion (read (current-buffer))))
			; ? but this is local vs local when default is modified? idk
			(if (equal val (symbol-value helm-pretty-print-current-symbol))
				(message "No changes done")
				(message "`%s' value modified" helm-pretty-print-current-symbol))
			(helm-edit-variable-quit))))

(defun helm-edit-variable-quit () "Quit edit variable buffer." (interactive)
	(set-buffer-modified-p nil)
	(quit-window t)
	(when-let ((window (get-buffer-window "*Diff*" 'visible)))
		(quit-window t window)))

; Timers

(defun helm-source-timers-describe-timer (timer)
	(describe-function (timer--function timer)))

(defconst helm-source-timers-actions
	(list
		(cons
			"Cancel timer"
			(lambda (_timer) (mapc #'cancel-timer (helm-marked-candidates))))
		(cons "Describe function" #'helm-source-timers-describe-timer)
		(cons
			"Find function"
			(lambda (timer)
				(when-let ((fn (timer--function timer)))
					(if (or (byte-code-function-p fn) (subr-native-elisp-p fn))
						(message "Can't find anonymous function `%s'." fn)
						(find-function fn)))))))

(defun helm-source-timers-candidate-transformer (candidates)
	(mapcar
		(lambda (timer)
			(cons
				(format "%s repeat=%s %s(%s)"
					(let ((time (timer--time timer)))
						(if (timer--idle-delay timer)
							(format "idle-for=[%s]"
								(format-seconds "%dd %hh %mmin %z%,3ss"
									(time-to-seconds time)))
							(format-time-string "%m/%d %T" time)))
					(or (timer--repeat-delay timer) "nil")
					(mapconcat
						#'identity
						(split-string
							(prin1-to-string (timer--function timer))
							"\n")
						" ")
					(mapconcat #'prin1-to-string (timer--args timer) " "))
				timer))
		candidates))

(defun helm-source-timers-make (name candidates)
	(helm-source-sync-make nil
		:name name
		:candidates `(lambda () (helm-source-timers-candidate-transformer ,candidates))
		:action helm-source-timers-actions
		:persistent-action #'helm-source-timers-describe-timer))

(defconst helm-absolute-time-timers-source
	(helm-source-timers-make "Absolute time timers" 'timer-list))

(defconst helm-idle-time-timers-source
	(helm-source-timers-make "Idle time timers" 'timer-idle-list))

(defun helm-timers () (interactive)
	(helm :sources (list helm-absolute-time-timers-source helm-idle-time-timers-source)))

; Complex command history

(defun helm-complex-command-history--called-interactively-skip (i _frame1 frame2)
	(and
		(eq 'eval (car (cdr frame2)))
		(eq
			'helm-sexp-eval-1
			(car (cdr (backtrace-frame (+ i 2) #'called-interactively-p))))
		1))

(defvar helm-sexp--last-sexp nil)
; This wont work compiled.
(defun helm-sexp-eval-1 () (interactive)
	; Trick called-interactively-p into thinking that `cand' is
	; an interactive call, See `repeat-complex-command'.
	(add-hook 'called-interactively-p-functions
		#'helm-complex-command-history--called-interactively-skip)
	(unwind-protect
		(eval (read helm-sexp--last-sexp) t)
		(remove-hook 'called-interactively-p-functions
			#'helm-complex-command-history--called-interactively-skip)))

(defun helm-sexp-eval (&optional _candidate) (call-interactively #'helm-sexp-eval-1))

(defconst helm-complex-command-history-source
	(helm-source-sync-make nil
		:name "Complex command history"
		:candidates
			(lambda ()
				; Use cdr to avoid adding `helm-complex-command-history' here.
				(cl-loop
					for i in command-history
					unless (equal i '(helm-complex-command-history))
						collect (prin1-to-string i)))
		:action
			(list
				(cons
					"Eval"
					(lambda (candidate)
						(when (boundp 'helm-sexp--last-sexp)
							(setq helm-sexp--last-sexp candidate))
						(let ((command (read candidate)))
							(unless (equal command (car command-history))
								(setq command-history (cons command command-history))))
						(run-with-timer 0.1 nil #'helm-sexp-eval)))
				(cons
					"Edit and eval"
					(lambda (candidate)
						(edit-and-eval-command "Eval: " (read candidate)))))
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-map)
				(helm-add-delete-binds-force map
					(lambda (cand)
						(setq command-history
							(cl-delete-if
								(lambda (i) (string= (prin1-to-string i) cand))
								command-history))))
				map)
		:persistent-action #'helm-sexp-eval
		:multiline t))

(defun helm-complex-command-history () (interactive)
	(helm :sources (list helm-complex-command-history-source)))

(provide 'helm-elisp)
