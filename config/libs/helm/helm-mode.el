; -*- lexical-binding:nil -*-

(require 'find-func)

(defvar crm-separator)
(defvar ido-everywhere)
(defvar password-cache)
(defvar package--builtins)
(defvar ffap-machine-p-unknown)
(defvar ffap-machine-p-local)
(defvar ffap-machine-p-known)
(defvar helm-mode)

; No warnings in Emacs built --without-x
(declare-function x-file-dialog "xfns.c")

(declare-function ido-mode "ido.el")
(declare-function help--symbol-class "help-fns.el")
(declare-function package-desc-summary "package")
(declare-function package-built-in-p "package")
(declare-function package-desc-status "package")
(declare-function package-get-descriptor "package")
(declare-function print-coding-system-briefly "mul-diag.el")
(declare-function helm-get-first-line-documentation "helm-elisp")

(defconst helm-completing-read-handlers-alist
	'(
		(tmm-menubar)
		(find-file)
		(execute-extended-command)
		(dired-do-rename . helm-read-file-name-handler-1)
		(dired-do-copy . helm-read-file-name-handler-1)
		(dired-do-symlink . helm-read-file-name-handler-1)
		(dired-do-relsymlink . helm-read-file-name-handler-1)
		(dired-do-hardlink . helm-read-file-name-handler-1)
		(basic-save-buffer . helm-read-file-name-handler-1)
		(write-file default helm-read-file-name-handler-1)
		(write-region default helm-read-file-name-handler-1)
		(all-the-icons-insert . helm-mode-all-the-icons-handler)
	)
"Completing read functions for specific Emacs commands.

By default `helm-mode' use `helm-completing-read-default-handler' to
provide helm completion in each `completing-read' or `read-file-name' found,
but other functions can be specified here for specific commands.
This also allows disabling helm completion for some commands when needed.

Each entry is a cons cell like (EMACS_COMMAND . COMPLETING-READ_HANDLER)
where key and value are symbols.
However if a command is using in its definition both a `completing-read' AND
a `read-file-name' we may want to specify a handler for both of them,
this can be done by specifying value as a list of two symbols instead of
a single symbol where the 1st element of the list specify the handler for the
`completing-read' and the second the handler for the `read-file-name'.
Special symbol \\='default' means use the default helm handler for either
`completing-read' or `read-file-name'.
e.g. (write-region . (default helm-read-file-name-handler-1))
means helm will use `helm-completing-read-default-handler' when
`write-region' calls `completing-read' and
`helm-read-file-name-handler-1' when it calls `read-file-name'.

Each key is an Emacs command that use originaly `completing-read'
or/and `read-file-name'.

Each value may be a helm function that takes same arguments as
`completing-read' plus NAME and BUFFER, where NAME is the name of the new
helm source and BUFFER the name of the buffer we will use, but it can
be also a function not using helm, in this case the function should
take the same args as `completing-read' and not be prefixed by \"helm-\".

`helm' will use the name of the command calling `completing-read' as
NAME will be computed as well with NAME but prefixed with
\"*helm-mode-\".

This function prefix name must start by \"helm-\" when it uses helm,
otherwise `helm' assumes the function is not a helm function and
expects the same args as `completing-read', this allows you to define a
handler not using helm completion.

Example:

	(defun foo/test ()
	  (interactive)
	  (message \"%S\" (completing-read \"test: \" \\='(a b c d e))))

	(defun helm-foo/test-completing-read-handler (prompt collection
												  predicate require-match
												  input hist def
												  inherit-input-method
												  name buffer)
	  (helm-comp-read prompt collection :marked-candidates t
										:name name))

	(add-to-list \\='helm-completing-read-handlers-alist
				 \\='(foo/test . helm-foo/test-completing-read-handler))


We want here to make the regular `completing-read' in `foo/test'
return a list of candidate(s) instead of a single candidate.

Note that this function will be reused for ALL the `completing-read'
of this command, so it should handle all cases. E.g.,
if first `completing-read' completes against symbols and
second `completing-read' should handle only buffer,
your specialized function should handle both.

If the value of an entry is nil completion will fall back to
Emacs vanilla behaviour.
Example:

If you want to disable helm completion for `describe-function', use:

	(describe-function . nil)

Ido is also supported, you can use `ido-completing-read' and
`ido-read-file-name' as value of an entry or just \\='ido.
Example:
Enable ido completion for `find-file':

	(find-file . ido)

same as

	(find-file . ido-read-file-name)

Note that you don't need to enable `ido-mode' for this to work, see
`helm-mode' documentation.")

(defconst helm-comp-read-case-fold-search helm-case-fold-search
"Default local setting of `helm-case-fold-search' for `helm-comp-read'.
See `helm-case-fold-search' for more info.
Symbol.")

(defconst helm-mode-handle-completion-in-region t
"Whether to replace or not `completion-in-region-function'.
This enables support for `completing-read-multiple' and `completion-at-point'
when non-nil.")

(defconst helm-mode-no-completion-in-region-in-modes nil
"A list of modes that do not want helm for `completion-in-region'.")

(defconst helm-mode-reverse-history t
"Display history source after current source when non-nil.

Apply only in `helm-mode' handled commands.")

(defconst helm-mode-ignore-diacritics nil
"Ignore diacritics in completing-read.")

(defvar helm-mode-minibuffer-setup-hook-black-list '(minibuffer-completion-help)
"Incompatible `minibuffer-setup-hook' functions go here.
A list of symbols. `helm-mode' is rejecting all lambda's, byte-code fns
and all functions belonging in this list from `minibuffer-setup-hook'.
This is mainly needed to prevent \"*Completions*\" buffers to popup.")

(defvar helm-comp-read-require-match-overrides
	'(
		(describe-function . t)
		(describe-command . t)
		(describe-minor-mode . t)
		(describe-theme . t)
		(load-theme . t)
	)
"Allow overriding REQUIRE-MATCH completing-read arg for a specific function.")

(define_face 'helm-prefix '((t :background "yellow" :foreground "black"))
"Face used to prefix new file or url paths in `helm-ff'.")

(define_face 'helm-mode-prefix '((t :extend t :background "red" :foreground "black"))
"Face used for prefix completion.")

(define_face 'helm-completion-invalid '((t :inherit font-lock-property-name-face))
"Face used to highlight invalid functions.")

(define_face 'helm-completions-detailed '((t :inherit font-lock-type-face)))

(defconst helm-completion-style 'helm
"Style of completion to use in `completion-in-region'.

This affects only `completion-at-point' and friends, and
the `completing-read' using the default handler
i.e. `helm-completing-read-default-handler'.

NB: This has nothing to do with `completion-styles', it is independent from
helm, but when using \\='emacs as helm-completion-style helm
will use the `completion-styles' for its completions.
Up to the user to configure `completion-styles'.

There are three possible values to use:

- helm, use multi match regular helm completion.

- emacs, use regular Emacs completion according to
  `completion-styles'.  Note that even in this style, helm allows using
  multi match.  Emacs-27 provides a style called `flex' that can be used
  aside `helm' style (see `completion-styles-alist').

For a better experience with emacs style, if you don't know what to use, set
`completion-styles' to \\='(flex) and keep \\='emacs as default
value for `helm-completion-style'.  Advanced users can also have a
look to `completion-category-overrides' to set styles according to category.
You can as well use `helm-completion-styles-alist' to override
`helm-completion-style' in specific modes.

Of course when using `helm' as `helm-completion-style'
emacs `completion-styles' have no effect.")

(defconst helm-completion-styles-alist
	'(
		(gud-mode . helm)
		; See https://github.com/djcb/mu/issues/2181.
		(mu4e-compose-mode . emacs)
	)
"Allow configuring `helm-completion-style' per mode or command.

NOTE: Use a mode for a completion that will be used in a buffer
i.e. completion-in-region, whereas you have to specify instead a
command to affect the completing-read trigerred by this
command. Commands specified in `helm-completing-read-handlers-alist' take
precedence on commands you put here.

Each entry is a cons cell like (mode . style) where style must be
a suitable value for `helm-completion-style'. When specifying
emacs as style for a mode or a command, `completion-styles' can
be specified by using a cons cell specifying completion-styles to
use with helm emacs style, e.g. (foo-mode . (emacs helm))
will set `completion-styles' to \\='(helm) for foo-mode.")

; Old, now unused.
(defun helm-multiline-to-singleline-transformer (candidates)
	(mapcar
		(lambda (c)
			(if (and (stringp c) (string-search "\n" c))
				(cons (string-replace "\n" "->" c) c)
				c))
		candidates))

; helm-comp-read

(defun helm-mode--quit ()
	; Use this instead of `keyboard-quit'
	; to avoid deactivating mark in current-buffer.
	(let (debug-on-quit) (signal 'quit nil)))

(defun helm-comp-read-get-candidates (collection &optional test)
"Convert COLLECTION to list removing elements that don't match TEST.
See `helm-comp-read' about supported COLLECTION arguments.

See docstring of `all-completions' for more info.

If COLLECTION is an `obarray', a TEST should be needed. See `obarray'."
	; Ensure COLLECTION is computed from `helm-current-buffer'
	; because some functions used as COLLECTION work
	; only in the context of current-buffer (Bug#1030).
	(with-current-buffer helm-current-buffer
		(cond
			((or (hash-table-p collection) (vectorp collection) (obarrayp collection))
				(all-completions "" collection test))
			; When collection is a symbol, most of the time
			; it should be a symbol used as a minibuffer-history.
			; The value of this symbol in this case return a list
			; of string which maybe are converted later as symbol
			; in special cases.
			; we treat here commandp as a special case as it return t
			; also with a string unless its last arg is provided.
			; Also, the history collections generally collect their
			; elements as string, so intern them to call predicate.
			((and (symbolp collection) (boundp collection))
				(all-completions "" (symbol-value collection)
					(when test
						(lambda (elm)
							(condition-case nil
								(if (eq test 'commandp)
									(funcall test (intern elm))
									(funcall test elm))
								(wrong-type-argument
									(funcall test (intern elm))))))))
			((functionp collection) (funcall collection "" test t))
			(t
				(when test
					(setq collection
						(cl-remove-if
							(lambda (cand) (not (funcall test (helm-get-display cand))))
							collection)))
				; A quick fix to handle symbols.
				; Idk how all-completions handles that. For now just assume if the
				; first cand is a symbol then every one is too.
				; all-completions returns strings when given for example obarray.
				(when (symbolp (car collection))
					(setq collection (mapcar #'symbol-name collection)))
				collection))))

(cl-defun helm-comp-read
	(prompt collection
		&key
		test
		input
		default
		preselect
		(must-match 'confirm)
		(requires-pattern 0)
		history
		(reverse-history helm-mode-reverse-history)
		(case-fold helm-comp-read-case-fold-search)
		persistent-action
		keymap
		(name "Completions")
		(diacritics helm-mode-ignore-diacritics)
		match-part
		execute-action-at-once-if-one
		quit-if-no-candidate
		volatile
		(sort #'helm-sort-length-alpha)
		candidate-transformer
		adaptive
		hist-candidate-transformer
		marked-candidates
		(candidate-number-limit helm-candidate-number-limit)
		allow-nest)
"Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- COLLECTION can be a list, alist, vector, obarray or hash-table.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg - candidate. Only for normal source.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- MUST-MATCH:
	Now this is completely different from require-match in completing-read.

	t - strict matching - can exit only with a normal candidate.

	nil - add candidate-transformer adding typed candidate
	(helm-maybe-add-new-candidate), never ask for confirmation.

	\\='confirm - add candidate-transformer like with nil, but ask for confirmation
	when exiting with this new candidate (see helm-source-confirm for more info).

	function with 3 args - add candidate-transformer like with nil; function for
	helm-source-confirm for both history and normal source, the third arg is t
	for history source, nil for normal source.

- REVERSE-HISTORY: When non-nil display history source after current source completion.

- HISTORY: A symbol where each result will be saved.
  When specified, all elements of HISTORY are displayed in
  a special source before or after COLLECTION according to REVERSE-HISTORY.

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (the keymap will be shared with history source)

- MARKED-CANDIDATES: If non-nil return marked candidates instead of selection.

- CASE-FOLD: Local `helm-case-fold-search'.

- NAME:
	A string for `helm-source-name' of the main source, and the base of name
	of history source.
- REQUIRES-PATTERN: `helm-source-requires-pattern' of the main source.
- PERSISTENT-ACTION: `helm-source-persistent-action' of source(s).
- SORT: `helm-source-sort' of source(s).
- MATCH-PART: `helm-source-match-part' of source(s).
- VOLATILE: `helm-source-sync-volatile' of the main source.
- ADAPTIVE: `helm-source-sync-adaptive' of source(s).
- CANDIDATE-NUMBER-LIMIT: `helm-source-candidate-number-limit' of source(s).
- CANDIDATE-TRANSFORMER: `helm-source-candidate-transformer' of the main source.
- HIST-CANDIDATE-TRANSFORMER: `helm-source-candidate-transformer' of the history source.

Args passed to helm:
PROMPT, INPUT, PRESELECT, EXECUTE-ACTION-AT-ONCE-IF-ONE, QUIT-IF-NO-CANDIDATE,
ALLOW-NEST.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That means you can pass prefix args before or after calling a command
that use `helm-comp-read'. See `helm-command' for example."
	(let (
		(action-fn
			(if marked-candidates
				(lambda (_candidate) (helm-marked-candidates))
				#'identity))
		(minibuffer-completion-predicate test)
		(minibuffer-completion-table (or minibuffer-completion-table collection))
		history_confirm
		confirm
		result
	)
		; Handle must-match.
		(unless (eq must-match t) ; Helm's default behaviour is the strict matching one.
			(setq candidate-transformer
				(if candidate-transformer
					`(lambda (candidates)
						(helm-maybe-add-new-candidate (,candidate-transformer candidates)))
					#'helm-maybe-add-new-candidate))
			(cond
				((not must-match))
				((eq must-match 'confirm) (setq confirm t))
				(t ; Function.
					(setq confirm `(lambda (cand action) (,must-match cand action nil)))
					(when history
						(setq history_confirm
							`(lambda (cand action) (,must-match cand action t)))))))
		(let (
			(sources
				(delq nil
					(list
						(when history
							(helm-source-sync-make nil
								:name (format "%s history" name)
								:candidates `(lambda () ,history)
								:sort sort
								:match-part match-part
								:candidate-transformer hist-candidate-transformer
								:adaptive adaptive
								:candidate-number-limit candidate-number-limit
								:persistent-action persistent-action
								:keymap
									(let ((map (make-sparse-keymap)))
										(set-keymap-parent map keymap)
										(helm-add-delete-binds-force
											map
											`(lambda (cand)
												(setq ,history (delete cand ,history))))
										map)
								:confirm history_confirm
								:action action-fn
								:volatile t))
						(helm-source-sync-make nil
							:name name
							:candidates
								`(lambda ()
									(let ((candidates (helm-comp-read-get-candidates ',collection #',test)))
										(delq
											nil
											(cond
												((and (consp ',default) (string= helm-pattern ""))
													(nconc
														(cl-loop
															for d in ',default
															; Don't convert nil to "nil".
															for str = (unless d (helm-stringify d))
															when (member str candidates)
																do (setq candidates (delete d candidates))
															when str collect str)
														candidates))
												; Some functions like debug-on-entry use (symbol-name sym)
												; without checking if sym is non nil, so the return value become "nil".
												((and (not (member ',default '("" "nil"))) (string= helm-pattern ""))
													(cons ',default (delete (helm-stringify ',default) candidates)))
												(t candidates)))))
							:match-part match-part
							:nomark (not marked-candidates)
							:sort sort
							:candidate-transformer candidate-transformer
							:adaptive adaptive
							:candidate-number-limit candidate-number-limit
							:requires-pattern requires-pattern
							:persistent-action persistent-action
							:diacritics diacritics
							:keymap keymap
							:confirm confirm
							:action action-fn
							:volatile volatile))))
		)
			(when reverse-history (setq sources (nreverse sources)))
			(setq result
				(helm
					:sources sources
					:input input
					:default default
					:preselect preselect
					:history history
					:prompt prompt
					:resume 'noresume
					:allow-nest allow-nest
					:execute-action-at-once-if-one execute-action-at-once-if-one
					:quit-if-no-candidate quit-if-no-candidate
					:helm-case-fold-search case-fold)))
		(or result (helm-mode--quit)))) ; Propagate quit signal.

; Generic completing-read
;
; Support also function as collection.
; e.g M-x man is supported.
; Support hash-table and vectors as collection.
; NOTE:
; Some crap emacs functions may not be supported
; like ffap-alternate-file (bad use of completing-read)
; and maybe others.
; Provide a mode `helm-mode' which turn on
; helm in all `completing-read' and `read-file-name' in Emacs.

; Extra metadata for completions-detailed.

(defconst helm-completing-read-extra-metadata
	(list
		(cons 'symbol-help #'helm-symbol-completion-table-affixation)
		(cons 'package #'helm-completion-package-affixation)
		(cons 'theme #'helm-completion-theme-affixation)
		(cons 'coding-system #'helm-completion-coding-system-affixation)
		(cons 'color #'helm-completion-color-affixation))
"Extra metadata for completing-read.

Alist composed of (category_symbol . annotation_fn).
category_symbol is extracted from original metadata.

It is used to add `affixation-function' if original
metadata doesn't have some and `completions-detailed' is non-nil.
When using emacs as `helm-completion-style', this has no effect, keeping same
behavior as emacs vanilla.")

(defconst helm-completing-read-command-categories
	'(
		("customize-variable" . symbol-help)
		("customize-set-variable" . symbol-help)
		("customize-set-value" . symbol-help)
		("customize-save-variable" . symbol-help)
		("describe-function" . symbol-help) ; For Emacs-27.
		("describe-variable" . symbol-help) ; For Emacs-27.
		("describe-symbol" . symbol-help) ; For Emacs-27.
		("describe-command" . symbol-help) ; For Emacs-27.
		("set-variable" . symbol-help)
		("customize-group" . symbol-help)
		("find-function" . symbol-help)
		("find-variable" . symbol-help)
		("trace-function" . symbol-help)
		("trace-function-foreground" . symbol-help)
		("trace-function-background" . symbol-help)
		("describe-minor-mode" . symbol-help)
		("where-is" . symbol-help)
		("execute-extended-command" . symbol-help)
		("package-install" . package)
		("package-vc-install" . package)
		("package-vc-checkout" . package)
		("describe-package" . package)
		("load-theme" . theme)
		("describe-theme" . theme)
		("describe-coding-system" . coding-system)
		("read-color" . color)
	)
"An alist to specify metadata category by command.

Some commands provide a completion-table with no category
specified in metadata, we allow here specifying the category of
the completion provided by a specific command. The command
should be specified as a string and the category as a symbol.")

(defun helm-symbol-completion-table-affixation (candidates)
"Normally affixation functions use CANDIDATES as arg, and return a list of
modified CANDIDATES. Now we allow affixations functions to return a
function instead, just like annotation functions. The function should return a
list of three elements like (comp prefix suffix). This increase significantly
the speed avoiding one useless loop on complete list of candidates.

Return a function.

It affects actually describe-variable/function/command/symbol functions.
It uses `helm-get-first-line-documentation' which allow providing documentation
for `describe-variable' symbols and align properly documentation when helm
style is used."
	(let ((max_length (helm-get-max-length candidates)))
		`(lambda (cand)
			(let* (
				(sym (intern cand))
				(doc (helm-get-first-line-documentation sym))
				(symbol-class (help--symbol-class sym))
				(group (helm-group-p sym))
			)
				(list
					; Symbol (comp).
					(if (or (symbol-function sym) (boundp sym) (facep sym) group)
						cand
						; Not already defined function. To test add an advice on a non
						; existing function.
						(propertize cand 'face 'helm-completion-invalid))
					; Prefix.
					(let (
						(class_str
							(cond
								((and symbol-class group) (concat "g" symbol-class))
								((not (string= symbol-class "")) symbol-class)
								(group "g")
								(t "i")))
					)
						(concat
							(propertize_no_copy class_str
								'face 'helm-completions-detailed)
							(get_space_string (- 5 (length class_str)))))
					; Suffix.
					(concat
						(get_space_string (1+ (- ,max_length (length cand))))
						(propertize_no_copy doc 'face 'helm-completions-detailed)
						; Key description of sym.
						(with-current-buffer helm-current-buffer
							(when-let (
								((commandp sym))
								(key (where-is-internal sym nil 'first-only))
								(binding (key-description key))
							)
								(propertize_no_copy (concat " (" binding ")")
									'face 'italic)))))))))

(defun helm-completion-package-affixation (candidates)
	(let ((max_length (helm-get-max-length candidates)))
		`(lambda (cand)
			(let* (
				(sym (intern-soft cand))
				(id (package-get-descriptor sym))
				(status (and id (package-desc-status id)))
				(desc
					(if (package-built-in-p sym)
						(aref (assoc-default sym package--builtins) 2)
						(and id (package-desc-summary id))))
			)
				(list
					cand
					(propertize_no_copy
						(if status
							(concat (substring status 0 1) " ")
							"b ")
						'face 'helm-completions-detailed)
					(when desc
						(concat
							(get_space_string (1+ (- ,max_length (length cand))))
							(propertize desc 'face 'helm-completions-detailed))))))))

(defun helm-completion-theme-affixation (candidates)
	(let ((max_length (helm-get-max-length candidates)))
		`(lambda (cand)
			(list
				cand
				nil
				(concat
					(get_space_string (1+ (- ,max_length (length cand))))
					(propertize
						(let ((sym (intern-soft cand)))
							(if (custom-theme-p sym)
								(helm-get-first-line-documentation sym)
								; Get theme doc.
								(when-let (
									(fn
										(locate-file
											(concat (symbol-name sym) "-theme.el")
											(custom-theme--load-path)
											'("" "c")))
								)
									; Avoid loading theme as much as possible.
									(with-temp-buffer
										(insert-file-contents fn)
										(let (doc)
											(let (result)
												(while
													(and
														(setq result
															(let (read-circle)
																(ignore-error end-of-file
																	(read (current-buffer)))))
														(or
															(not (eq (car-safe result) 'deftheme))
															(progn
																(setq doc
																	(car
																		(split-string
																			(nth 2 result)
																			"\n")))
																nil)))))
											(unless doc
												; Extract doc in first line of theme file.
												(goto-char (point-min))
												(setq doc
													(let (beg end)
														(when (re-search-forward "--- " (pos-eol) t)
															(setq beg (point)))
														(setq end
															(if (re-search-forward " -\\*-" (pos-eol) t)
																(match-beginning 0)
																(pos-eol)))
														(and beg end (buffer-substring beg end)))))
											doc)))))
						'face 'helm-completions-detailed))))))

(defun helm-completion-coding-system-affixation (candidates)
	(let ((max_length (helm-get-max-length candidates)))
		`(lambda (cand)
			(list
				cand
				nil
				(concat
					(get_space_string (1+ (- ,max_length (length cand))))
					(propertize_no_copy
						(string-replace "\n" ""
							(replace-regexp-in-string "^ *" ""
								(with-output-to-string
									(with-current-buffer standard-output
										(print-coding-system-briefly
											(intern comp) 'tightly)))))
						'face 'helm-completions-detailed))))))

(defun helm-completion-color-affixation (candidates)
	(let ((max_length (helm-get-max-length candidates)))
		`(lambda (cand)
			(list
				cand
				nil
				(concat
					(get_space_string (1+ (- ,max_length (length cand))))
					(let (
						(rgb
							(condition-case nil
								(cond
									((string= comp "foreground at point")
										(with-current-buffer helm-current-buffer
											(foreground-color-at-point)))
									((string= comp "background at point")
										(with-current-buffer helm-current-buffer
											(background-color-at-point)))
									(t
										(apply #'color-rgb-to-hex
											(color-name-to-rgb comp))))
								(error "SAMPLE")))
					)
						(propertize rgb
							'face
							(list :background rgb :distant-foreground "black"))))))))

; Generic completing read

(defun helm-completing-read-default-1
	(prompt collection test require-match init hist default _inherit-input-method name)
"Helm `completing-read' handler not rebuilding its candidates dynamically.

It is used usually with helm `helm-completion-style'.
Call `helm-comp-read' with same args as `completing-read'.

This handler should be used when candidate list doesn't need to be rebuilt
dynamically otherwise use `helm-completing-read-default-2'."
	(let* (
		(history (or (car-safe hist) hist))
		(input (if (stringp init) init (car init)))
		(minibuffer-completion-table collection)
		(metadata
			(or
				(completion-metadata (or input "") collection test)
				'(metadata)))
		(afun
			(or
				(plist-get completion-extra-properties :annotation-function)
				(completion-metadata-get metadata 'annotation-function)))
		(afix
			(or
				(plist-get completion-extra-properties :affixation-function)
				(completion-metadata-get metadata 'affixation-function)))
		(category (completion-metadata-get metadata 'category))
		(sort-fn (completion-metadata-get metadata 'display-sort-function))
	)
		(unless category
			(setq category (assoc-default name helm-completing-read-command-categories)))
		(and
			completions-detailed
			(setq metadata (assq category helm-completing-read-extra-metadata))
			(setq afix (cdr metadata)))
		(unwind-protect
			(helm-comp-read prompt collection
				:test test
				:history history
				:must-match require-match
				:name name
				:requires-pattern
					(if
						(and
							(equal default "")
							(memq require-match '(confirm confirm-after-completion)))
						1 0)
				:candidate-transformer
					(when (or afix afun (eq category 'file) sort-fn)
						`(lambda (candidates)
							(helm-completion--initial-filter
								,(if sort-fn
									(list sort-fn 'candidates)
									'candidates)
								#',afun #',afix ',category)))
				:quit-if-no-candidate (eq require-match t)
				:default default
				; Fail with special characters (e.g in gnus "nnimap+gmail:") if
				; regexp-quote is not used. When init is added to history,
				; it will be unquoted by helm-comp-read.
				:input input))))

; This is very weird, I never used it.
(defun helm-mode-all-the-icons-handler
	(prompt collection test require-match init hist default inherit-input-method name)
"Helm `completing-read' handler for `all-the-icons-insert'."
	(let* (
		sname
		(max-len 0)
		(candidates
			(cl-loop
				for (desc . str) in collection
				; When the FAMILY argument is passed to
				; `all-the-icons-insert' DESC is the name of icon only
				; otherwise it is "name  [family]" with unpredictable
				; spaces or tab numbers between name and [family].
				for descnp = (substring-no-properties desc)
				for sdesc =
					(if (string-match "\\(.*\\)[[:blank:]]+\\(\\[.*\\]\\)" descnp)
						; This is all-the-icons-insert function.
						(match-string 1 descnp)
						; This is one of
						; all-the-icons-insert-<family>
						; functions, extract the family name.
						(unless sname
							(setq sname
								(plist-get
									(get-text-property 0 'font-lock-face
										(get-text-property 0 'display desc))
									:family)))
						descnp)
				for sdesc2 = (match-string 2 descnp)
				do (setq max-len (max max-len (string-width sdesc)))
				collect (cons (concat sdesc " " str " " sdesc2) desc)))
		(helm-after-update-hook
			(append
				helm-after-update-hook
				(list
					(lambda ()
						(save-excursion
							(goto-char (point-min))
							(forward-line 1)
							(while (re-search-forward "^[[:alnum:]_-]+" nil t)
								(insert
									(get_space_string
										(- max-len (current-column))))))))))
	)
		(helm-completing-read-default-1
			prompt candidates test require-match init hist
			default inherit-input-method (or sname name))))

(defun helm-completing-read-default-handler
	(prompt collection test require-match init hist default inherit-input-method name)
"Default helm `completing-read' handler.

Use either `helm-completing-read-default-1' or `helm-completing-read-default-2'
according to `helm-completion-style'."
	(funcall
		(if (eq helm-completion-style 'helm)
			#'helm-completing-read-default-1
			; Old helm-completing-read-default-2.
			; "Helm `completing-read' handler with dynamic matching.
			; Call `helm-comp-read' with same args as `completing-read'.
			; For the meaning of optional args see `helm-completing-read-default-1'.
			; This handler uses dynamic matching which allows honouring
			; `completion-styles'.
			(lambda
				(
					prompt collection predicate require-match init
					hist default _inherit-input-method name
				)
				(let* (
					(history (or (car-safe hist) hist))
					(input (if (stringp init) init (car init)))
					(completion-flex-nospace t)
					(minibuffer-completion-table collection)
					(metadata
						(or
							(completion-metadata (or input "") collection predicate)
							'(metadata)))
					(afun
						(or
							(plist-get completion-extra-properties :annotation-function)
							(completion-metadata-get metadata 'annotation-function)))
					(afix
						(or
							(plist-get completion-extra-properties :affixation-function)
							(completion-metadata-get metadata 'affixation-function)))
					(category (completion-metadata-get metadata 'category))
				)
					(helm-comp-read
						; Completion-at-point and friends have no prompt :(((.
						prompt
						; Not sure but probably this will work purely on dynamic binding.
						(lambda (str _predicate _action)
							(let* (
								(completion-ignore-case (helm-set-case-fold-search))
								(comps
									(completion-all-completions
										str ; This is helm-pattern.
										collection
										predicate
										(length str)
										metadata))
								(last-data (last comps))
								(sort-fn
									(when (eq helm-completion-style 'emacs)
										(completion-metadata-get
											metadata 'display-sort-function)))
								all
							)
								(when (cdr last-data)
									; Remove the last element comps.
									(setcdr last-data nil))
								(setq all (copy-sequence comps))
								; With emacs style default is passed with the :default
								; arg of helm-comp-read and computed in its
								; get-candidates function.
								(helm-completion--initial-filter
									(let (
										(lst
											(if (and sort-fn (> (length str) 0))
												(funcall sort-fn all)
												all))
									)
										(if (and default afix)
											(prog1 (cons default (delete default lst))
												(setq default nil))
											lst))
									afun afix category)))
						:name name
						:input input
						:history history
						; In helm h-c-styles default is passed directly in candidates.
						:default
							(and
								(eq helm-completion-style 'emacs)
								(null afix)
								default)
						:quit-if-no-candidate (eq require-match t)
						:must-match require-match
						:volatile t))))
		prompt collection test require-match
		init hist default inherit-input-method name))

(defun helm-mode--read-buffer-to-switch (prompt)
"[INTERNAL] This is used to advice `read-buffer-to-switch'.
Don't use it directly or you might go to jail."
	; `read-buffer-to-switch' is passing `minibuffer-completion-table'
	; to `read-buffer' through `minibuffer-setup-hook' which is too
	; late to be known by `read-buffer-function', in our case
	; `helm--generic-read-buffer'. It should let bind it to allow us using it.
	(let ((minibuffer-completion-table (internal-complete-buffer-except)))
		(read-buffer
			prompt
			(other-buffer (current-buffer))
			(confirm-nonexistent-file-or-buffer))))

(defun helm--generic-read-buffer (prompt &optional default require-match predicate)
"The `read-buffer-function' for `helm-mode'.
Affects `switch-to-buffer' and related."
	(helm--completing-read-default
		prompt
		(or minibuffer-completion-table (internal-complete-buffer "" nil t))
		; `read-buffer' is using internally `Vbuffer_alist' which is an
		; alist with elements like (BUF-NAME . BUF-OBJ), therefore some
		; predicates in Emacs are working only on such cons cells.
		; However, helm is transforming COLLECTION in a list of strings and
		; such predicates are failing because they expect cons cells (see
		; bug#2506 with `project-switch-to-buffer'), even if they should
		; handle strings as well according to `read-buffer' documentation.
		(when predicate
			(lambda (buffer)
				(let ((buf (cons buffer (get-buffer buffer))))
					(condition-case nil
						(funcall predicate buffer)
						(wrong-type-argument (funcall predicate buf))))))
		require-match nil nil default))

(defun helm-mode--get-default-handler-for (comp-or-file entry)
	; Use 'comp for completing-read and 'file for 'read-file-name as
	; COMP-OR-FILE value.
	(let (
		(val (cdr entry))
		(reading-file (eq comp-or-file 'file))
	)
		(if (not (consp val))
			val
			(setq val (if reading-file (nth 1 val) (car val)))
			(if (eq val 'default)
				(if reading-file
					#'helm-read-file-name
					#'helm-completing-read-default-handler)
				val))))

(defvar helm-blacklist-completion-styles '(emacs21 emacs22))
(defun helm--prepare-completion-styles (&optional com-or-mode)
"Return a suitable list of styles for `completion-styles'.

When `helm-completion-style' is not `emacs' the Emacs vanilla
default `completion-styles' is used except for
`helm-dynamic-completion' which uses inconditionally `emacs' as
value for `helm-completion-style'.

If styles are specified in `helm-completion-styles-alist' for a
particular mode, use these styles for the corresponding mode.
If COM-OR-MODE (a mode or a command) is specified it is used to find the
corresponding styles in `helm-completion-styles-alist'."
	(let ((from (or com-or-mode major-mode)))
		(if (eq helm-completion-style 'helm)
			; Keep default settings, but probably nil is fine as well.
			'(basic partial-completion emacs22)
			(or
				(let ((style (cdr (assq from helm-completion-styles-alist))))
					(when (consp style) (cdr style)))
				; We need to have flex always behind helm, otherwise
				; when matching against e.g. '(foo foobar foao frogo bar baz)
				; with pattern "foo" helm style if before flex will
				; return foo and foobar only defeating flex that would
				; return foo foobar foao and frogo.
				(if (memq 'flex completion-styles)
					'(flex helm)
					(cons
						'helm
						(cl-loop
							for style in completion-styles
							unless (memq style helm-blacklist-completion-styles)
								collect style)))))))

(cl-defun helm--completing-read-default
	(prompt collection
		&optional predicate require-match input hist def inherit-input-method)
"An helm replacement of `completing-read'.
This function should be used only as a `completing-read-function'.

Don't use it directly, use instead `helm-comp-read' in your programs.

See documentation of `completing-read' and `all-completions' for details."
	(let* (
		(current-command this-command)
		(str-command
			(if current-command
				(helm-symbol-name current-command)
				"completing-read"))
		(entry (assq current-command helm-completing-read-handlers-alist))
		(def-com (helm-mode--get-default-handler-for 'comp entry))
		(str-defcom (and def-com (helm-symbol-name def-com)))
		(def-args
			(list
				prompt collection predicate
				(if-let (
					(cell (assq current-command helm-comp-read-require-match-overrides))
				)
					(cdr cell)
					require-match)
				input hist def inherit-input-method))
		; Append the one extra arg needed to set the source name
		; in helm specialized functions.
		(others-args (append def-args (list str-command)))
		; Be sure this pesty *completion* buffer doesn't popup.
		; Note: `minibuffer-with-setup-hook' may setup a lambda
		; calling `minibuffer-completion-help' or other minibuffer
		; functions we DONT WANT here, in these cases removing the hook
		; (a symbol) have no effect. Bug#448.
		; Because `minibuffer-completion-table' and
		; `minibuffer-completion-predicate' are not bound
		; anymore here, these functions should have no effect now,
		; except in some rare cases like in `woman-file-name',
		; so remove all incompatible functions
		; from `minibuffer-setup-hook' (Bug#1205, Bug#1240).
		; otherwise helm have not the time to close its initial session.
		(minibuffer-setup-hook
			(cl-loop
				for h in minibuffer-setup-hook
				unless
					(or
						(consp h) ; a lambda.
						(byte-code-function-p h)
						(subr-native-elisp-p h)
						(memq h helm-mode-minibuffer-setup-hook-black-list))
					collect h))
		; Let-bounding here helm-completion-style according to
		; helm-completion-styles-alist allow using helm style per commands.
		(helm-completion-style
			(if-let ((cell (cdr (assq current-command helm-completion-styles-alist))))
				(if (cdr-safe cell) (car cell) cell)
				(default-value 'helm-completion-style)))
		(completion-styles (helm--prepare-completion-styles current-command))
	)
		(when (eq def-com 'ido) (setq def-com 'ido-completing-read))
		(unless (or (not entry) def-com)
			; An entry in *read-handlers-alist exists but have
			; a nil value, so we exit from here, disable `helm-mode'
			; and run the command again with it original behavior.
			; `helm-mode' will be restored on exit.
			(helm-mode -1)
			(cl-return-from helm--completing-read-default
				(unwind-protect
					(apply completing-read-function def-args)
					(helm-mode))))
		; If we use now `completing-read' we MUST turn off `helm-mode'
		; to avoid infinite recursion and CRASH. It will be reenabled on exit.
		(when
			(or
				(eq def-com 'completing-read)
				; All specialized functions are prefixed by "helm"
				(and
					(stringp str-defcom)
					(not (string-prefix-p "helm" str-defcom))))
			(helm-mode -1))
		(unwind-protect
			(cond
				((and def-com helm-mode)
					; An helm specialized function exists, run it.

					; Disable `minibuffer-complete' for handlers using helm (bug #2533).
					; Some functions are calling `minibuffer-complete'
					; within `minibuffer-setup-hook' when calling their
					; `completing-read', like `woman-file-name' (bug #2527).
					; This defeat helm which is already completing minibuffer,
					; so deactivate minibuffer-complete one time for all [1].
					(cl-letf (((symbol-function 'minibuffer-complete) #'ignore))
						(apply def-com others-args)))
				; Try to handle `ido-completing-read' everywhere.
				((and def-com (eq def-com 'ido-completing-read))
					(setcar (memq collection def-args)
						(all-completions "" collection predicate))
					(apply def-com def-args))
				; A non helm function specified in
				; `helm-completing-read-handlers-alist' use it with
				; exactly the same args as in `completing-read'.
				; If we are here `helm-mode' is now disabled.
				(def-com (apply def-com def-args))
				(t
					; Same like in [1].
					(cl-letf (((symbol-function 'minibuffer-complete) #'ignore))
						; If nothing is found in
						; helm-completing-read-handlers-alist use default
						; handler which will itself use `helm-completion-style'.
						(apply #'helm-completing-read-default-handler others-args))))
			(helm-mode)
			; When exiting minibuffer, `this-command' is set to
			; `helm-exit-minibuffer', which is unwanted when starting
			; on another `completing-read', so restore `this-command' to
			; initial value when exiting.
			(setq this-command current-command))))

; Generic read-file-name

; Keys will be defined in helm-files.el.
(defconst helm-ff-default-keymap (make-sparse-keymap)
"Default keymap of `helm-ff' and `helm-read-file-name'.")

(defconst helm-read-file-name-default-keymap
	(let ((keymap (make-sparse-keymap)))
		(set-keymap-parent keymap helm-ff-default-keymap)
		; Will be updated at the start of every helm-read-file-name.
		(define-key keymap [C-return] nil)
		keymap))

(defconst helm-read-file-name-keymap
	(let ((keymap (make-sparse-keymap)))
		(set-keymap-parent keymap helm-read-file-name-default-keymap)
		(define-key keymap [return] #'helm-ff-return)
		keymap))

(defun helm-read-file-name-exit-with-default-dir () (interactive)
	(if helm-ff-default-directory
		(helm-make-action-command-phantom-base
			#'identity
			helm-ff-default-directory
			(buffer-local-value 'helm-default-source (get-buffer helm-buffer)))
		(message "No default directory.")))

; This should somehow allow to choose only a file/dir from
; some specified dir.
(cl-defun helm-read-file-name
	(prompt
		&key
		(name "Read file name")
		input
		test
		(case-fold helm-file-name-case-fold-search)
		preselect
		; Not sure if this default is a good idea.
		(history 'file-name-history)
		(must-match 'confirm)
		default
		marked-candidates
		type)
"Read a file name with helm completion.

It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read file name\".

- INPUT:
	Where to start reading file name, default is `default-directory' or $HOME$.

- TEST:
	A predicate called with one arg - candidate.
	Dirs always end with a slash so this should never be file-directory-p.

- CASE-FOLD: Same as `helm-case-fold-search'.

- PRESELECT: helm preselection (should use `helm-ff-regex-for-preselection').

- HISTORY: :history arg of helm - minibuffer history variable.

- MUST-MATCH:
	t - strict matching - no new files.

	nil - can exit with a new (non existing) file, never ask for confirmation.

	\\='confirm - can exit with a new (non existing) file,
	ask for confirmation when this happens (see helm-source-confirm for more info).

	Function for helm-source-confirm, can exit with a new (non existing) file.
	Rememeber that when :type is nil or \\='dir, and :marked-candidates is nil,
	user can exit with `helm-ff-default-directory', so this function shouldn't
	assume that candidate passed to it will be in (display . real) format.
	Use `helm-get-real' to safely obtain real value of candidate.

- MARKED-CANDIDATES:
	When non-nil return a list of marked candidates.
	When nil, don't allow marking candidates.

- TYPE:
	One of: \\='dir, \\='file or nil (default).
	Type of file that user is choosing (nil means any file - dir or file).
	Do some things based on that, namely, add a message to mode-line
	and maybe skip some useless sorting.
	Also, when it is nil or \\='dir, and :marked-candidates is nil, add keybinding
	[C-return] allowing user to exit with `helm-ff-default-directory' as selection."
	(unless input
		(setq input
			(or
				default-directory
				(file-name-as-directory (expand-file-name (getenv "HOME"))))))

	(define-key helm-read-file-name-default-keymap [C-return]
		(and
			(not marked-candidates)
			(memq type '(nil dir))
			#'helm-read-file-name-exit-with-default-dir))

	(let* (
		(minibuffer-completion-predicate test)
		(minibuffer-completing-file-name t)
		; Ensure not being prompted for password each time we
		; navigate to a directory.
		(password-cache t)
		(result
			(helm
				:sources
					(list
						(helm-source-ff-make nil
							:name
								`(lambda ()
									(or
										(helm-source-ff-mode-line-base helm-current-source)
										,(if (stringp name) name (list name))))
							:candidates
								(if test
									`(lambda ()
										(cl-loop
											with hn = (helm-ff--tramp-hostnames)
											for cand in
												(helm-ff-get-candidates ,(eq must-match t))
											when
												(or
													(member (cdr cand) hn) ; A tramp host
													(,test (cdr cand))) ; Test
												collect cand))
									`(lambda ()
										(helm-ff-get-candidates ,(eq must-match t))))
							:nomark (not marked-candidates)
							:keymap helm-read-file-name-keymap
							:action
								(if marked-candidates
									(lambda (_candidate) (helm-marked-candidates))
									#'identity)
							:mode-line
								; Add a mode-line message.
								; Kind of crap, it probably should be in the prompt,
								; but it's hard to ensure this with external commands.
								`(lambda ()
									(concat
										,(concat
											"Read "
											(when (eq must-match t) "existing ")
											(cl-case type
												(dir
													(concat
														"dir"
														(when marked-candidates "s")))
												(file
													(concat
														"file"
														(when marked-candidates "s")))
												(t
													(if marked-candidates
														"dirs or files"
														"dir or file")))
											"  ")
										(helm-ff-mode-line)))
							:confirm
								(cond
									((memq must-match '(nil t)) nil)
									((eq must-match 'confirm) t)
									(t must-match)) ; Function.
							:dirs-first
								(if (eq type 'dir)
									; Don't waste time on useless in this case sorting.
									nil
									helm-ff-default-sort-dirs-first)))
				:input input
				:prompt prompt
				:resume 'noresume
				:default default
				:preselect preselect
				:history history
				:helm-default-keymap helm-read-file-name-default-keymap
				:helm-case-fold-search case-fold
				:helm-full-frame nil))
	)
		(helm-ff-cleanup)
		(or result (helm-mode--quit)))) ; Propagate quit signal.

(defun helm-mode-root-dir (dir)
	(if (file-remote-p dir)
		(format "/%s:%s@%s:/"
			(file-remote-p dir 'method)
			(file-remote-p dir 'user)
			(file-remote-p dir 'host))
		"/"))

(cl-defun helm--generic-read-file-name
	(prompt &optional dir default-filename mustmatch initial predicate)
"Generic helm replacement of `read-file-name'.
Don't use it directly, use instead `helm-read-file-name' in your programs."
	(let* (
		(init (or initial dir default-directory))
		(current-command this-command)
		(str-command
			(if current-command
				(helm-symbol-name current-command)
				"read-file-name"))
		(entry (assq current-command helm-completing-read-handlers-alist))
		(def-com (helm-mode--get-default-handler-for 'file entry))
		(str-defcom (and def-com (helm-symbol-name def-com)))
		; Don't modify the original args list for emacs generic functions.
		(def-args (list prompt dir default-filename mustmatch initial predicate))
		; Append the extra arg needed to set the source name in helm specialized
		; functions.
		(others-args (append def-args (list str-command)))
		(reading-directory (eq predicate 'file-directory-p))
		(use-dialog
			(and
				(next-read-file-uses-dialog-p)
				; Graphical file dialogs can't handle remote files.
				(not (file-remote-p init))
				use-file-dialog))
		add-to-history
		fname
	)
		; Build `default-filename' with `dir'+`initial' when
		; `default-filename' is not specified.
		; See `read-file-name' docstring for more infos.
		(setq default-filename
			(let ((dir (or dir default-directory)))
				(unless (file-name-absolute-p dir) (setq dir (expand-file-name dir)))
				(unless default-filename
					(setq default-filename
						(expand-file-name (or initial buffer-file-name dir) dir)))
				(cond
					((consp default-filename)
						(mapcar
							(lambda (f)
								(if (file-name-absolute-p default-filename)
									(expand-file-name f (helm-mode-root-dir dir))
									(expand-file-name default-filename dir)))
							default-filename))
					((not (file-name-absolute-p default-filename))
						(expand-file-name default-filename dir))
					((file-remote-p default-filename) default-filename)
					(t
						(substitute-in-file-name
							(concat (helm-mode-root-dir dir) default-filename))))))
		; Some functions that normally call `completing-read' can switch
		; brutally to `read-file-name' (e.g find-tag), in this case
		; the helm specialized function will fail because it is build
		; for `completing-read', so set it to 'incompatible to be sure
		; we switch to `helm-read-file-name' and don't try to call it
		; with wrong number of args.
		(when (eq def-com 'ido) (setq def-com 'ido-read-file-name))
		(and
			def-com
			(length> (help-function-arglist def-com) 8)
			(setq def-com 'incompatible))
		(unless (or (not entry) def-com)
			(helm-mode -1)
			(cl-return-from helm--generic-read-file-name
				(unwind-protect
					(apply read-file-name-function def-args)
					(helm-mode))))
		; If we use now `read-file-name' or dialog we MUST turn off `helm-mode'
		; to avoid infinite recursion. It will be reenabled on exit.
		(when
			(or
				(memq def-com '(read-file-name ido-read-file-name))
				use-dialog
				(and (stringp str-defcom) (not (string-prefix-p "helm" str-defcom))))
			(helm-mode -1))
		(unwind-protect
			(setq fname
				(cond
					(use-dialog
						(let (
							(dialog-mustmatch
								(not
									(memq mustmatch '(nil confirm confirm-after-completion))))
						)
							; Dialogs don't support a list of default fnames.
							(when (consp default-filename)
								(setq default-filename
									(expand-file-name (car default-filename) init)))
							(setq add-to-history t)
							(x-file-dialog
								prompt
								init
								default-filename
								dialog-mustmatch
								reading-directory)))
					; A specialized function exists, run it with the two extra args
					; specific to helm. Note that the helm handler should ensure
					; :input is non-nil i.e. Use init which fallback
					; to default-directory instead of INITIAL.
					((and
							def-com
							helm-mode
							; The entry in `helm-completing-read-handlers-alist' is
							; a cons cell specifying a completing-read and
							; a read-file-name handler default
							; e.g. (foo (default default)).
							(not
								(memq
									def-com
									'(
										ido-read-file-name
										incompatible
										helm-read-file-name
									))))
						(apply def-com others-args))
					; Def-com value is `ido-read-file-name', run it with default args.
					((eq def-com 'ido-read-file-name)
						(ido-mode)
						(apply def-com def-args))
					; Def-com value is `read-file-name', run it with default args.
					((eq def-com 'read-file-name) (apply def-com def-args))
					(t ; Fall back to classic `helm-read-file-name'.
						; This doesn't handle default emacs arg now!
						; Meaning it's impossible to exit with empty pattern
						; and return default arg.
						(helm-read-file-name prompt
							:name str-command
							:default default-filename
							; Helm handlers should always have a non-nil input.
							:input
								(cond
									((string-match helm-ff-url-regexp init) init)
									((not (file-name-absolute-p init))
										(expand-file-name init dir))
									((file-remote-p init) init)
									(t
										(substitute-in-file-name
											(concat
												(helm-mode-root-dir (or dir init))
												init))))
							:must-match mustmatch
							:test predicate
							:type (when reading-directory 'dir)))))
			(when ido-mode (ido-mode -1))
			(helm-mode)
			; Same comment as in `helm--completing-read-default'.
			(setq this-command current-command))
		(when add-to-history
			(add-to-history 'file-name-history (minibuffer-maybe-quote-filename fname)))
		(if
			(and
				; Using `read-directory-name'.
				reading-directory
				; `file-name-as-directory' return "./" when FNAME is empty string.
				(not (string= fname "")))
			(file-name-as-directory fname)
			fname)))

; Read file name handler with history.
(defun helm-read-file-name-handler-1 (prompt dir default mustmatch initial predicate name)
"A `read-file-name' handler with history.
Can be added to `helm-completing-read-handlers-alist' for functions
that need a `read-file-name' function with directory history.
`helm-ff-history' is used here."
	(helm-read-file-name prompt
		:name name
		:history 'helm-ff-history
		:default default
		; Helm handlers should always have a non-nil initial arg.
		:input (expand-file-name (or initial dir default-directory) dir)
		:must-match mustmatch
		:test predicate
		:type (when (eq predicate 'file-directory-p) 'dir)))

; Completion in region and Helm style

(defun helm-completion--initial-filter (comps afun afix category)
"Compute COMPS with function AFIX or AFUN.

When CATEGORY is file, remove dot files from COMPS.

If both AFUN and AFIX are provided, AFIX takes precedence.

When AFUN, AFIX are nil and CATEGORY is not file return COMPS unmodified."
	; Normally COMPS should be a list of
	; string but in some cases it is given as a list of strings containing a list
	; of string e.g. ("a" "b" "c" ("d" "e" "f")) ; This happen in rgrep
	; (bug#2607) and highlight-* fns (bug #2610), so ensure the list is flattened to
	; avoid e.g. wrong-type argument: stringp '("d" "e" "f")
	; FIXME: If this create a new bug with completion-in-region, flatten COMPS
	; directly in the caller i.e. helm-completing-read-default-1.
	(when (or afix afun (eq category 'file))
		(setq comps (helm-fast-remove-dups (flatten-tree comps))))
	; Filter out dot files in file completion.
	; We were previously exiting directly without handling afix and afun, but
	; maybe some file completion tables have an afix or afun in their metadata so
	; let them a chance to run these functions if some.
	(when (eq category 'file)
		(setq comps (cl-delete-if (lambda (f) (member f '("./" "../"))) comps)))
	(cond
		(afix
			(let ((affixations (funcall afix comps)))
				(if (functionp affixations)
					(mapcar
						(lambda (comp)
							(cons
								(let ((cand (funcall affixations comp)))
									(propertize_no_copy
										(concat
											(nth 1 cand) ; prefix
											(car cand) ; comp
											(nth 2 cand)) ; suffix
										'match-part
										(list
											(let ((len (length (nth 1 cand))))
												(cons len (+ len (length (car cand))))))))
								comp))
						comps)
					(mapcar
						(lambda (comp)
							(cons
								(propertize_no_copy
									; 'comp' has format like above.
									(concat (nth 1 comp) (car comp) (nth 2 comp))
									'match-part
									(list
										(let ((len (length (nth 1 comp))))
											(cons len (+ len (length (car comp)))))))
								(car comp)))
						affixations))))
		(afun
			; Add annotation at end of candidate if needed, e.g. foo<f>,
			; this happen when completing against a quoted symbol.
			(mapcar
				(lambda (s)
					(if-let ((ann (funcall afun s)))
						(cons
							(concat
								s (propertize ann 'face 'helm-completions-detailed))
							s)
						s))
				comps))
		(t comps)))

; Helm multi matching style

(defun helm-completion-try-completion (string table pred point)
"The try completion function for `completing-styles-alist'.
Literally does nothing, who cares, I just make empty functions for a living."
	; AFAIU the try-completions style functions
	; are here to check if what is at point is suitable for TABLE but
	; there is no way to pass a multiple pattern from what is at point
	; apart sending STRING in a minibuffer like helm does. Perhaps
	; minibuffer-complete should benefit of this but for now just do
	; nothing as this is used nowhere. It is anyway not clear what the
	; try-completions functions do in emacs so just do nothing for now.
	nil)

(defun helm-completion-all-completions (string table pred point)
"The all completions function for `completing-styles-alist'."
	(let* (
		(beforepoint (substring string 0 point))
		(afterpoint (substring string point))
		(bounds (completion-boundaries beforepoint table pred afterpoint))
		(prefix (substring beforepoint 0 (car bounds)))
		; (suffix (substring afterpoint (cdr bounds)))
		(all
			; Using `regexp-quote' on STRING fixes bug#2355 but
			; breaks regexp matching in multi match, actually with
			; Helm-3.7.1 and emacs-27+ it seems using plain STRING
			; works for both so use it.

			; Doing an initial call of all-completions on the first element of
			; STRING speedup completion and fix file completion when CAPF
			; returns relative paths to initial pattern (eshell and shell).
			(let* (
				(split (split-string string))
				(all
					(let ((fpat (or (car split) "")))
						(and
							(or
								minibuffer-completing-file-name
								(eq
									(completion-metadata-get
										(completion-metadata string table pred) 'category)
									'file))
							(or
								(cdr split)
								(and
									(not (cdr split))
									; Kickin when STRING is a simple string.
									; Handle as well "foo " (space at end).
									(not (string= fpat "")))
								(string= string ""))
							(not (string-prefix-p "!" fpat))
							; all-completions should return nil if FPAT is a
							; regexp, it is what we expect.
							(all-completions fpat table
								(lambda (x &optional _y)
									(funcall (or pred #'identity)
										(if (listp x) (car x) x)))))))
				(pattern
					(when-let* (all (match (string-search " " string)))
						; Returns the part of STRING after space
						; e.g. "foo bar baz" => "bar baz".
						(substring string (1+ match))))
			)
				(if
					(or
						(and all (not (cdr split)))
						(equal pattern "")) ; e.g. STRING == "foo ".
					all
					(all-completions "" (or all table)
						(lambda (x &optional _y)
							; Second arg _y is needed when
							; table is a hash-table (Bug#2231)
							; (C-x 8 RET).
							; Elements of table may be
							; lists or alists, in this case consider the
							; car of element (Bug#2219 org-refile).
							(let ((elm (if (listp x) (car x) x)))
								; PRED have been already called in
								; initial all-completions, no need to call
								; it a second time, thus ALL is now a list
								; of strings maybe not supported by
								; PRED (e.g. symbols vs strings).
								(if (and pred (null all))
									(when (funcall pred elm)
										; ALL is nil so use whole STRING
										; against table.
										; TODO teraz to nie działa.
										(helm-mm-match (helm-stringify elm) string))
									; TODO to też.
									(helm-mm-match
										(helm-stringify elm)
										(or (and all pattern) string)))))))))
	)
		(when all (nconc all (length prefix)))))

; Completion-in-region-function

(defvar helm--completing-region nil
"[INTERNAL] flag let-bound to nil when completing in region.")

(defvar helm-crm-default-separator ","
"Default separator for `completing-read-multiple'.

`crm-separator' will take precedence on this when it is a string composed
of a single character.
If used globally, it is a string composed of a single character,
if let-bound, it can be also nil or a symbol which mean no separator.
Don't set this to a string composed of more than one character.
Be sure to know what you are doing when modifying this.")

(defun helm--completion-in-region (origfun start end collection &optional predicate)
"Helm replacement of `completion--in-region'.

Can be used for `completion-in-region-function' by advicing it with an
:around advice to allow passing the old
`completion-in-region-function' value in ORIGFUN."
	(cl-declare (special require-match prompt))
	(if (memq major-mode helm-mode-no-completion-in-region-in-modes)
		(funcall origfun start end collection predicate)
		(let (
			(old--helm-completion-style helm-completion-style)
			(exit-fun (plist-get completion-extra-properties :exit-function))
			; Always start with prefix to allow completing without
			; the need of inserting a space after cursor or
			; relaying on crap old completion-styles emacs22 which
			; add suffix after prefix. e.g. def|else.
			(input (buffer-substring-no-properties start (point)))
			(advice_ignore_errors
				(lambda (original_fn &rest args)
					(ignore-errors (apply original_fn args))))
			string
		)
			(advice-add 'lisp--local-variables :around advice_ignore_errors)
			(when-let ((style (cdr (assq major-mode helm-completion-styles-alist))))
				(setq helm-completion-style (if (cdr-safe style) (car style) style)))
			(unwind-protect
				(let* (
					(completion-flex-nospace t)
					(helm--completing-region t)
					(completion-styles (helm--prepare-completion-styles))
					(input (buffer-substring-no-properties start end))
					(prefix (and (eq helm-completion-style 'emacs) input))
					(point (point))
					(current-command
						(or
							this-command
							; Some backends are async and use a callback, in those cases,
							; we can't retrieve from frames the last interactive command,
							; so fallback to `last-command' which may be the one
							; that called the callback.
							last-command))
					(crm (eq current-command 'crm-complete))
					(str-command (helm-symbol-name current-command))
					(require-match
						(or
							(and (boundp 'require-match) require-match)
							minibuffer-completion-confirm
							; If prompt have not been propagated here, that's
							; probably mean we have no prompt and we are in
							; completion-at-point or friend, so use a non-nil
							; value for require-match.
							(not (boundp 'prompt))))
					(metadata (completion-metadata input collection predicate))
					; `completion-extra-properties' is let-bound in `completion-at-point'.
					; `afun' is a closure to call against each string in `data'.
					; it provide the annotation info for each string.
					; e.g "foo" => "foo <f>" where foo is a function.
					; See Bug#407.
					(afun
						(or
							(plist-get completion-extra-properties :annotation-function)
							(completion-metadata-get metadata 'annotation-function)))
					; Not sure if affixations are provided in
					; completion-in-region, try anyway never know.
					(afix
						(or
							(plist-get completion-extra-properties :affixation-function)
							(completion-metadata-get metadata 'affixation-function)))
					(init-space-suffix
						(unless
							(or
								(eq helm-completion-style 'emacs)
								(string-suffix-p " " input)
								(string= input ""))
							" "))
					(category (completion-metadata-get metadata 'category))
					(file-comp-p
						(or
							(eq category 'file)
							; Guess filename at point.
							(with-current-buffer (or helm-current-buffer (current-buffer))
								; Ensure to disable the evil `ffap-machine-at-point' which
								; may run here as `file-name-at-point-functions' contains
								; by default `ffap-guess-file-name-at-point' See bug#2574.
								; Use same value as in Emacs-29 for next 3 vars to ensure
								; `ffap-machine-p' never ping.
								(let (
									(ffap-machine-p-known 'accept)
									(ffap-machine-p-local 'reject)
									(ffap-machine-p-unknown 'reject)
								)
									(run-hook-with-args-until-success
										'file-name-at-point-functions)))))
					; `completion-all-completions' store the base-size in the last `cdr',
					; so data looks like this: '(a b c d . 0) and (last data) == (d . 0).
					base-size
					(compfn
						(lambda (str _predicate _action)
							(let* (
								(completion-ignore-case
									(let (
										(helm-buffer
											(if helm-alive-p
												helm-buffer
												(current-buffer)))
									)
										(helm-set-case-fold-search str)))
								(comps
									(completion-all-completions
										str ; This is helm-pattern
										collection
										predicate
										; Use prefix length at first call to
										; allow styles matching
										; "prefix*suffix" to kick in.
										(length (or prefix str))
										metadata))
								(last-data (last comps))
								(bs
									(if-let ((val (cdr last-data)))
										(prog1 val
											; Remove the last element of
											; comps by side-effect.
											(setcdr last-data nil))
										0))
								(sort-fn
									(and
										(eq helm-completion-style 'emacs)
										(completion-metadata-get
											metadata 'display-sort-function)))
								all
							)
								; Reset prefix to allow using length of
								; helm-pattern on next calls (this avoid
								; args-out-of-range error).
								(setq prefix nil)
								; base-size needs to be set only once at
								; first call.
								(unless base-size (setq base-size bs))
								(setq all (copy-sequence comps))
								(helm-completion--initial-filter
									(if (and sort-fn (> (length str) 0))
										(funcall sort-fn all)
										all)
									afun afix category))))
					(data
						(if (eq helm-completion-style 'helm)
							(funcall compfn input nil nil)
							compfn))
					(result
						(if (stringp data)
							data
							(helm-comp-read
								; Completion-at-point and friends have no prompt.
								(or (and (boundp 'prompt) prompt) "Pattern: ")
								data
								:name str-command
								:marked-candidates crm
								:input
									(let (
										(propertize_initial_input_for_helm
											; Only to use when (eq helm-completion-style 'helm).
											; Highlight prefix in helm `helm-completion-styles'.
											(lambda (str)
												(propertize str
													'face 'helm-mode-prefix
													'read-only t
													'rear-nonsticky t)))
									)
										(cond
											((string-suffix-p "/" input)
												(when (eq helm-completion-style 'emacs) input))
											(file-comp-p
												(concat
													(if (eq helm-completion-style 'helm)
														(funcall propertize_initial_input_for_helm
															(helm-basename input))
														input)
													init-space-suffix))
											((or (null require-match) (stringp require-match))
												(if (eq helm-completion-style 'helm)
													(funcall propertize_initial_input_for_helm
														input)
													input))
											(t
												(concat
													(if (eq helm-completion-style 'helm)
														(funcall propertize_initial_input_for_helm
															input)
														input)
													init-space-suffix))))
								:execute-action-at-once-if-one t
								:quit-if-no-candidate (lambda () (message "No matches."))
								:must-match require-match
								:volatile t)))
				)
					; Something here is stripping out properties on RESULT
					; by side-effect (perhaps `choose-completion-string'?)
					; so make a copy of STRING to not loose props.
					(setq string (copy-sequence result))
					(cond
						((stringp result)
							; When RESULT have annotation, annotation is displayed
							; in it with a display property attached to a space
							; added at end of string, take care of removing this
							; space (Bug#2360). However keep RESULT intact to
							; pass it to `:exit-function' i.e. Don't store the
							; modified string in STRING.
							(choose-completion-string
								(replace-regexp-in-string " \\'" "" result)
								(current-buffer)
								(list (+ start base-size) point)
								completion-list-insert-choice-function))
						((consp result) ; crm.
							(let (
								(beg (+ start base-size))
								(sep
									; If `crm-separator' is a string of length 1
									; assume it can be used as separator (Bug#2298),
									; otherwise it is a regexp and use the value
									; it matches or default to "," if no match.
									(if (= (length crm-separator) 1)
										crm-separator
										helm-crm-default-separator))
							)
								; Try to find a default separator. If `crm-separator' is a
								; regexp use the string the regexp is matching.
								; If SEP is not a string, it have been probably bound to a
								; symbol or nil through `helm-crm-default-separator' that serve
								; as a flag to say "Please no separator" (Bug#2353 with
								; `magit-completing-read-multiple').
								(if (stringp sep)
									(save-excursion
										(goto-char beg)
										(when (looking-back crm-separator (1- (point)))
											(setq sep (match-string 0))))
									(setq sep nil))
								(funcall completion-list-insert-choice-function
									beg end
									(mapconcat #'identity (append result '("")) sep))))))
				; Allow running extra property `:exit-function' (Bug#2265, Bug#2356).
				; Function is called with 'exact if for a unique
				; match which is exact, the return value of `try-completion'
				; is t or a string ending with "/" i.e. possibly a directory (Bug#2274),
				; otherwise it is called with 'finished.
				(and
					(stringp string)
					exit-fun
					(let ((tcomp (try-completion input collection)))
						(funcall exit-fun string
							(if
								(or
									(eq tcomp t) ; Unique.
									(and
										(stringp tcomp)
										(string-suffix-p "/" tcomp))) ; A directory.
								'exact 'finished))))
				(setq helm-completion-style old--helm-completion-style)
				(advice-remove 'lisp--local-variables advice_ignore_errors)))))

(defun helm-mode--disable-ido-maybe (&optional from-hook)
	(when (and (boundp 'ido-everywhere) ido-everywhere)
		(remove-function read-file-name-function #'ido-read-file-name)
		(remove-function read-buffer-function #'ido-read-buffer)
		(setq ido-everywhere nil)
		(user-error
			(if from-hook
				"Unable to turn on Ido-everywhere while Helm-mode is enabled"
				"Helm-mode enabled (Ido-everywhere is incompatible with helm-mode, disabling it)"))))

(defun helm-mode--ido-everywhere-hook ()
	; Called only when user calls directly ido-everywhere and helm-mode is enabled.
	(when helm-mode (helm-mode--disable-ido-maybe t)))

; Minibuffer history

(defconst helm-minibuffer-history-key [?\C-h]
"The key `helm-minibuffer-history' is bound to in minibuffer local maps.
Nil to don't bind anything.
Key will be arg to 'define-key' so it should be a vector or maybe string.")

(defconst helm-minibuffer-history-old-key
	(cl-loop
		for map in
			'(
				minibuffer-local-completion-map
				minibuffer-local-filename-completion-map
				minibuffer-local-isearch-map
				minibuffer-local-map
				minibuffer-local-must-match-map
				minibuffer-local-ns-map
			)
		when (and (boundp map) (symbol-value map))
			collect
				(cons map (lookup-key (symbol-value map) helm-minibuffer-history-key))))

(define-minor-mode helm-minibuffer-history-mode
"Bind `helm-minibuffer-history-key' in all minibuffer maps.
This mode is enabled by `helm-mode', so there is no need to enable it directly."
	:global t
	(when helm-minibuffer-history-key
		(mapc
			`(lambda (map)
				(when-let (((boundp map)) (vmap (symbol-value map)))
					(define-key vmap helm-minibuffer-history-key
						,(if helm-minibuffer-history-mode
							'#'helm-minibuffer-history
							'(car (assq map helm-minibuffer-history-old-key))))))
			'(
				minibuffer-local-completion-map
				minibuffer-local-filename-completion-map
				minibuffer-local-isearch-map
				minibuffer-local-map
				minibuffer-local-must-match-map
				minibuffer-local-ns-map
			))))

(defvar helm-minibuffer-history-cache nil)
(defvar helm-minibuffer-history-variable nil)

; name will be set before every helm call.
(defconst helm-minibuffer-history-source
	(helm-source-sync-make nil
		:candidates (lambda () helm-minibuffer-history-cache)
		:volatile t
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-map)
				(helm-add-delete-binds
					map
					(lambda (cand)
						(setq helm-minibuffer-history-cache
							(delete cand helm-minibuffer-history-cache))
						(set helm-minibuffer-history-variable
							(delete
								cand (symbol-value helm-minibuffer-history-variable)))))
				map)))

(defvar helm-minibuffer-history--in nil)
(defun helm-minibuffer-history ()
"helm for `minibuffer-history'.
Only to use in minibuffer active window."
	(interactive)
	; Protect from recursive use.
	(unless helm-minibuffer-history--in
		(let (is_multiline)
			(setq helm-minibuffer-history-cache
				(cl-loop
					for i in (symbol-value minibuffer-history-variable)
					unless (string= i "")
						collect
							(progn
								(and
									(not is_multiline)
									(string-search "\n" i)
									(setq is_multiline t))
								i)))
			(if (not helm-minibuffer-history-cache)
				(message "No saved history.")
				; Update :multiline slot.
				(setf (helm-source-multiline helm-minibuffer-history-source) is_multiline)
				(setq helm-minibuffer-history-variable minibuffer-history-variable)
				(let (
					(query-replace-p
						(memq last-command '(query-replace query-replace-regexp)))
					(minibuffer-completion-table
						(or minibuffer-completion-table helm-minibuffer-history-cache))
					minibuffer-completion-predicate
					(helm-minibuffer-history--in t)
					result
				)
					(setf (helm-source-name helm-minibuffer-history-source)
						(symbol-name minibuffer-history-variable))
					(setq result
						(helm
							:sources (list helm-minibuffer-history-source)
							:resume 'noresume
							:allow-nest t
							:helm-case-fold-search helm-comp-read-case-fold-search))
					(when result
						; Handle query-replace candidates with separators.
						(when-let* (
							query-replace-p
							(pos (string-search "\0" elm))
						)
							(add-text-properties pos (1+ pos)
								`(display ,query-replace-from-to-separator separator t)
								result))
						(delete-minibuffer-contents)
						(insert result)))))))

(define-minor-mode helm-mode
"Toggle generic helm completion.

All functions in Emacs that use `completing-read',
`read-file-name', `completion-in-region' and friends will use helm
interface when this mode is turned on.

However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

About `ido-mode':
DO NOT enable `ido-everywhere' when using `helm-mode'. Instead of
using `ido-mode', add the commands where you want to use ido to
`helm-completing-read-handlers-alist' with `ido' as value."
	:global t
	(if helm-mode
		(progn
			(add-function :override completing-read-function
				#'helm--completing-read-default)
			(add-function :override read-file-name-function
				#'helm--generic-read-file-name)
			(add-function :override read-buffer-function
				#'helm--generic-read-buffer)
			(when helm-mode-handle-completion-in-region
				(add-function :around completion-in-region-function
					#'helm--completion-in-region))
			; If user have enabled ido-everywhere BEFORE enabling
			; helm-mode disable it and warn user about its
			; incompatibility with helm-mode (Bug#2085).
			(helm-mode--disable-ido-maybe)
			; If ido-everywhere is not enabled yet anticipate and
			; disable it if user attempt to enable it while helm-mode
			; is running (Bug#2085).
			(add-hook 'ido-everywhere-hook #'helm-mode--ido-everywhere-hook)
			(advice-add 'read-buffer-to-switch :override #'helm-mode--read-buffer-to-switch)
			(helm-minibuffer-history-mode))
		(remove-function completing-read-function #'helm--completing-read-default)
		(remove-function read-file-name-function #'helm--generic-read-file-name)
		(remove-function read-buffer-function #'helm--generic-read-buffer)
		(remove-function completion-in-region-function #'helm--completion-in-region)
		(remove-hook 'ido-everywhere-hook #'helm-mode--ido-everywhere-hook)
		(advice-remove 'read-buffer-to-switch #'helm-mode--read-buffer-to-switch)
		(helm-minibuffer-history-mode -1)))

(helm-mode)

(provide 'helm-mode)
