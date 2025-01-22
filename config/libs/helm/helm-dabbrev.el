; -*- lexical-binding:nil -*-

(defconst helm-dabbrev-always-search-all t
"Always search in all buffers when non-nil.
Note that even if nil, a search in all buffers will occur if the
length of candidates is <= than 'helm-dabbrev-max-length-result'.")

(defconst helm-dabbrev-candidates-number-limit 1000
"Maximum number of candidates to collect.

The higher this number is, the slower the computation of candidates will be.
Note that this have nothing to do with
`helm-candidate-number-limit', this means that computation of
candidates stop when this value is reached but only
`helm-candidate-number-limit' candidates are displayed in the Helm buffer.")

(defconst helm-dabbrev-ignored-buffers-regexps
	'("\\*helm" "\\*Messages" "\\*Echo Area" "\\*Buffer List")
"List of regexps matching names of buffers that `helm-dabbrev' should not check.")

(defvar helm-dabbrev-related-buffer-fn
	; Decide if current-buffer is related to START-BUFFER.
	(lambda (start-buffer)
		; START-BUFFER is the current-buffer where we start searching.
		; Determine the major-mode of START-BUFFER as `cur-maj-mode'.
		; Each time the loop go in another buffer we try from this buffer
		; to determine if its `major-mode' is:
		; - same as the `cur-maj-mode'
		; - derived from `cur-maj-mode' and from
		;   START-BUFFER if its mode is derived from the one in START-BUFFER.
		; - have an assoc entry (major-mode . cur-maj-mode)
		; - have an rassoc entry (cur-maj-mode . major-mode)
		; - check if one of these entries inherit from another one in `alist'.
		(let* (
			(cur-maj-mode (buffer-local-value 'major-mode start-buffer))
			(maj-mode major-mode)
			(c-assoc-mode (assq cur-maj-mode helm-dabbrev-major-mode-assoc))
			(c-rassoc-mode (rassq cur-maj-mode helm-dabbrev-major-mode-assoc))
			(o-assoc-mode (assq major-mode helm-dabbrev-major-mode-assoc))
			(o-rassoc-mode (rassq major-mode helm-dabbrev-major-mode-assoc))
			(cdr-c-assoc-mode (cdr c-assoc-mode))
			(cdr-o-assoc-mode (cdr o-assoc-mode))
		)
			(or
				(eq major-mode cur-maj-mode)
				(derived-mode-p cur-maj-mode)
				(with-current-buffer start-buffer (derived-mode-p maj-mode))
				(eq cdr-c-assoc-mode major-mode)
				(eq (car c-rassoc-mode) major-mode)
				(eq
					(cdr (assq cdr-c-assoc-mode helm-dabbrev-major-mode-assoc))
					major-mode)
				(eq
					(car (rassq cdr-c-assoc-mode helm-dabbrev-major-mode-assoc))
					major-mode)
				(eq cdr-o-assoc-mode cur-maj-mode)
				(eq (car o-rassoc-mode) cur-maj-mode)
				(eq
					(cdr (assq cdr-o-assoc-mode helm-dabbrev-major-mode-assoc))
					cur-maj-mode)
				(eq
					(car (rassq cdr-o-assoc-mode helm-dabbrev-major-mode-assoc))
					cur-maj-mode))))
"A function that decide if a buffer to search in its related to `current-buffer'.

This is actually determined by comparing `major-mode' of the
buffer to search and the `current-buffer'.

The function take one arg, the buffer which is current.

When nil all buffers are considered related to `current-buffer'.")

(defconst helm-dabbrev-major-mode-assoc nil
"Major mode association alist, elements are (symbol . symbol).

This allow helm-dabbrev searching in buffers with the associated `major-mode'.
E.g. (emacs-lisp-mode . lisp-interaction-mode)

will allow searching in the lisp-interaction-mode buffer when
`current-buffer' is an `emacs-lisp-mode' buffer and vice versa
i.e. no need to provide (lisp-interaction-mode .
emacs-lisp-mode) association.

When nil check is the searched buffer has same `major-mode' than
the `current-buffer'.

This has no effect when `helm-dabbrev-related-buffer-fn' is nil
or of course bound to a function that doesn't handle this var.")

(defconst helm-dabbrev-lineno-around 30
"Search first in this number of lines before and after point.")


; Check for beginning of line should happen last (^\n\\|^).
(defvar helm-dabbrev-separator-regex
	"\\s-\\|\t\\|[(\\[\\{\"'`=<>$;,@.#+]\\|\\s\\\\|^\n\\|^"
"Regex matching the start of a dabbrev candidate.")

(defvar helm-dabbrev-cache nil)

(defun helm-thing-before-point (&optional bounds) "Return symbol before point."
	(let (
		(end (point))
		(beg
			(save-excursion
				(let ((boundary (field-beginning nil nil (pos-bol))))
					(if (re-search-backward helm-dabbrev-separator-regex boundary t)
						(match-end 0)
						boundary))))
	)
		(when (/= beg end)
			(if bounds
				(cons beg end)
				(buffer-substring-no-properties beg end)))))

(defconst helm-dabbrev-source
	(helm-source-sync-make nil
		:name "Dabbrev"
		:candidates (lambda () helm-dabbrev-cache)
		:volatile t
		:action
			(lambda (candidate)
				; When there is no space after point
				; we are completing inside a symbol or
				; after a partial symbol with the next arg aside
				; without space, in this case mark the region.
				; deleting it would remove the
				; next arg which is unwanted.
				(delete-region (car (helm-thing-before-point t)) (point))
				(insert candidate)
				(when-let (
					(pos
						(cdr
							(or
								(bounds-of-thing-at-point 'symbol)
								(bounds-of-thing-at-point 'filename))))
					((< (point) pos))
				)
					(set-mark pos)))))

(defun helm-dabbrev () (interactive)
	(let ((dabbrev (helm-thing-before-point)))
		(if (or (not (stringp dabbrev)) (string= dabbrev ""))
			(message "helm-dabbrev: Nothing found before point.")
			(message "Waiting for helm-dabbrev candidates...")
			(setq helm-dabbrev-cache
				(let (
					(limit helm-dabbrev-candidates-number-limit)
					(case-fold-search (helm-set-case-fold-search dabbrev))
					(buffer1 (current-buffer)) ; start buffer.
					(minibuf (minibufferp))
					dabbrev_regex separator_regex results pos-before pos-after
				)
					(let (
						(base_regex
							(concat
								"\\("
								helm-dabbrev-separator-regex
								"\\)\\"))
					)
						(setq separator_regex (concat base_regex "'"))
						(setq dabbrev_regex
							(concat
								base_regex
								"(?99:\\("
								(regexp-quote dabbrev)
								"\\(\\sw\\|\\s_\\)+\\)\\)")))
					(catch 'break
						(dolist (
							buf
							(if helm-dabbrev-always-search-all
								(cl-loop
									for buf in (buffer-list)
									unless
										(cl-loop
											for r in helm-dabbrev-ignored-buffers-regexps
											thereis (string-match r (buffer-name buf)))
										collect buf)
								(list buffer1))
						)
							(with-current-buffer buf
								(when
									(or
										minibuf ; Check against all buffers when in minibuffer.
										(not helm-dabbrev-related-buffer-fn)
										(funcall helm-dabbrev-related-buffer-fn buffer1))
									(let (
										(search_and_store
											; Search words or symbols matching dabbrev in DIRECTION up to limit.
											; Argument DIRECTION can be:
											; - (1): Search forward from point.
											; - (-1): Search backward from point.
											; - (2): Search forward from the
											; `helm-dabbrev-lineno-around'
											; lines after point.
											; - (-2): Search backward from the `helm-dabbrev-lineno-around'
											; lines before point.
											; This function modifies 'result'.
											(lambda (direction)
												(let (after before)
													(while
														(and
															(not (length> results limit))
															(cl-case direction
																(1 (search-forward dabbrev nil t))
																(-1 (search-backward dabbrev nil t))
																(2
																	(setq after
																		(pos-bol (1+ helm-dabbrev-lineno-around)))
																	(search-forward dabbrev after t))
																(-2
																	(setq before
																		(pos-bol (- 1 helm-dabbrev-lineno-around)))
																	(search-backward dabbrev before t))))
														(when-let (
															(match-word
																; Search word or symbol at point matching dabbrev.
																; The search starts at (1- BEG) with a regexp starting
																; with `helm-dabbrev-separator-regex' followed by
																; dabbrev followed by a regexp matching syntactically
																; any word or symbol. The possible false positives
																; matching separator regex at end are finally removed.
																(let ((eol (pos-eol)))
																	(save-excursion
																		(goto-char (1- (match-beginning 0)))
																		(when (re-search-forward dabbrev_regex eol t)
																			(replace-regexp-in-string
																				separator_regex ""
																				(match-string-no-properties 99))))))
															((not (member match-word results)))
														)
															(push match-word results)))
													(cons after before))))
									)
										(save-excursion
											; Start searching before thing before point.
											(goto-char (- (point) (length dabbrev)))
											; Search the last 30 lines BEFORE point and set POS-BEFORE.
											(setq pos-before (cdr (funcall search_and_store -2))))
										(save-excursion
											; Search the next 30 lines AFTER point and set POS-AFTER.
											(setq pos-after (car (funcall search_and_store 2))))
										(save-excursion
											; Search all BEFORE point maybe starting from
											; POS-BEFORE to not search again what previously found.
											; If limit is reached in previous call of
											; `search_and_store' POS-BEFORE is nil and
											; goto-char will fail, so check it.
											(when pos-before (goto-char pos-before))
											(funcall search_and_store -1))
										(save-excursion
											; Search all AFTER point maybe starting from POS-AFTER.
											; Same comment as above for POS-AFTER.
											(when pos-after (goto-char pos-after))
											(funcall search_and_store 1)))))
							(unless (length< results limit) (throw 'break nil))))
					(nreverse results)))
			(helm
				:sources (list helm-dabbrev-source)
				:input (concat "^" dabbrev " ")
				:allow-nest t
				:execute-action-at-once-if-one t
				:quit-if-no-candidate (lambda () (message "No expansion found.")))
			(setq helm-dabbrev-cache nil))))

(provide 'helm-dabbrev)
