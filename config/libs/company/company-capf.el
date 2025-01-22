; -*- lexical-binding:nil -*-

; company-mode completion-at-point-functions backend

; The CAPF backend provides a bridge to the standard
; completion-at-point-functions facility, and thus can support any major mode
; that defines a proper completion function, including emacs-lisp-mode,
; css-mode and nxml-mode.

(declare-function python-shell-get-process "python")

(defvar-local company::capf::current_completion_data nil
"Value last returned by `company-capf' when called with `candidates'.
For most properties/actions, this is just what we need: the exact values
that accompanied the completion table that's currently is use.

`company-capf', however, could be called at some different positions during
a completion session (most importantly, by `company::sort_by_occurrence'),
so we can't just use the preceding variable instead.")

(defvar-local company::capf::sorted nil)

; Prevents several calls to capf from the same position.
(defvar company::capf::cache nil)

(defun company-capf (command &optional arg)
"`company-mode' backend using `completion-at-point-functions'."
	(interactive '(interactive))
	(let (
		(get_data
			(lambda () ; No dynamic binding.
				(let ((cache company::capf::cache))
					(if
						(and
							(eq (point) (pop cache))
							(eq (current-buffer) (pop cache))
							(eq (buffer-chars-modified-tick) (pop cache)))
						(car cache) ; Return cached data.
						(let (
							(data
								(cl-letf (
									((default-value 'completion-at-point-functions)
										; Ignore tags-completion-at-point-function because
										; it subverts company-etags in the default value
										; of company-backends, where the latter comes
										; later.
										(remq 'tags-completion-at-point-function
											(default-value
												'completion-at-point-functions)))
								)
									(let* (
										(completion-at-point-functions
											(if
												(or
													(not
														(memq
															'python-completion-complete-at-point
															completion-at-point-functions))
													(python-shell-get-process))
												completion-at-point-functions
												(remq 'python-completion-complete-at-point
													completion-at-point-functions)))
										(data
											(run-hook-wrapped 'completion-at-point-functions
												; Ignore misbehaving functions.
												(lambda (fn)
													(let (
														(buffer-read-only t)
														inhibit-read-only
														(completion-in-region-function
															`(lambda (beg end coll pred)
																(throw 'company-illegal-completion-in-region
																	(list #',fn
																		beg end coll :predicate pred))))
													)
														(catch 'company-illegal-completion-in-region
															(ignore-error buffer-read-only
																(completion--capf-wrapper
																	fn 'optimist)))))))
									)
										(and
											(consp (cdr data))
											(integer-or-marker-p (nth 1 data))
											data))))
						)
							(setq company::capf::cache
								(list
									(point)
									(current-buffer)
									(buffer-chars-modified-tick)
									data))
							data)))))
		(maybe_funcall_from_current_completion_data
			(lambda (keyword) ; Dynamic binding: arg.
				(when-let (
					(fn
						(plist-get
							(nthcdr 4 company::capf::current_completion_data) keyword))
				)
					(funcall fn arg))))
	)
		(cl-case command
			(interactive (company-begin-backend 'company-capf))
			(prefix
				(when-let ((res (funcall get_data)))
					(let (
						(prefix_length (plist-get (nthcdr 4 res) :company-prefix-length))
						(prefix (buffer-substring-no-properties (nth 1 res) (point)))
					)
						(cond
							(prefix_length (cons prefix prefix_length))
							(t prefix)))))
			(candidates
				(let ((res (funcall get_data)))
					(setq company::capf::current_completion_data res)
					(add-hook 'company-after-completion-hook
						; Clear current data.
						(lambdaSymbol (_)
							(setq company::capf::current_completion_data nil))
						nil
						t)
					(when res
						(let* (
							(table (nth 3 res))
							(pred (plist-get (nthcdr 4 res) :predicate))
							(meta
								(completion-metadata
									(buffer-substring (nth 1 res) (nth 2 res))
									table
									pred))
							(candidates
								(completion-all-completions
									arg table pred (length arg) meta))
							(sortfun (cdr (assq 'display-sort-function meta)))
							(last (last candidates))
							; See `last' and `safe-length'.
							(base-size (if (numberp (cdr last)) (cdr last)))
						)
							(when base-size (setcdr last nil))
							(setq company::capf::sorted (functionp sortfun))
							(when sortfun (setq candidates (funcall sortfun candidates)))
							(if (or (not base-size) (= 0 base-size))
								candidates
								(let ((before (substring arg 0 base-size)))
									(mapcar
										(lambda (candidate) (concat before candidate))
										candidates)))))))
			(annotation
				(let (
					(annotation
						(when-let (
							(fn
								(or
									(plist-get
										(nthcdr 4 company::capf::current_completion_data)
										:annotation-function)
									; FIXME: Add a test.
									(cdr
										(assq 'annotation-function
											(completion-metadata
												(buffer-substring
													(nth 1 company::capf::current_completion_data)
													(nth 2 company::capf::current_completion_data))
												(nth 3 company::capf::current_completion_data)
												(plist-get
													(nthcdr 4 company::capf::current_completion_data)
													:predicate))))))
						)
							(funcall fn arg)))
				)
					; TODO completely unacceptable - just change elisp-completion-at-point a nie takie ciulostwo
					(unless
						(and
							(equal annotation " <f>") ; elisp-completion-at-point, pre-icons
							(plist-get
								(nthcdr 4 company::capf::current_completion_data)
								:company-kind))
						annotation)))
			(match
				; Ask the for the `:company-match' function. If that doesn't help,
				; fallback to sniffing for face changes to get a suitable value.
				(if-let (
					(fn
						(plist-get
							(nthcdr 4 company::capf::current_completion_data)
							:company-match))
				)
					(funcall fn arg)
					; Compute match result from a CAPF's completion fontification.
					(let (
						(limit (length arg))
						(pos -1)
						prop-value
						match-start
						chunks
					)
						(while (< pos limit)
							(setq
								pos (if (< pos 0) 0 (next-property-change pos arg limit))
								prop-value
									(or
										(get-text-property pos 'face arg)
										(get-text-property pos 'font-lock-face arg)))
							; If prop-value has face.
							(if
								(if (listp prop-value)
									(memq 'completions-common-part prop-value)
									(eq 'completions-common-part prop-value))
								(unless match-start (setq match-start pos))
								(when match-start
									(push (cons match-start pos) chunks)
									(setq match-start nil))))
						(nreverse chunks))))
			(kind (funcall maybe_funcall_from_current_completion_data :company-kind))
			(deprecated (funcall maybe_funcall_from_current_completion_data :company-deprecated))
			(sorted company::capf::sorted)
			(duplicates t)
			(no-cache t) ; Not much can be done here, as long as we handle non-prefix matches.
			(meta (funcall maybe_funcall_from_current_completion_data :company-docsig))
			(doc-buffer (funcall maybe_funcall_from_current_completion_data :company-doc-buffer))
			(location (funcall maybe_funcall_from_current_completion_data :company-location))
			(require-match (plist-get (nthcdr 4 (funcall get_data)) :company-require-match))
			(post-completion
				(let ((res company::capf::current_completion_data))
					(if-let ((exit-function (plist-get (nthcdr 4 res) :exit-function)))
						; We can more or less know when the user is done with completion,
						; so we do something different than `completion--done'.
						(funcall exit-function arg
							; FIXME: Should probably use an additional heuristic:
							; completion-at-point doesn't know when the user picked a
							; particular candidate explicitly (it only checks whether
							; further completions exist). Whereas company user can press
							; RET (or use implicit completion with company-tng).
							(if (= (length arg) (car (completion-boundaries arg (nth 3 res) nil "")))
								'sole
								'finished))))))))

(provide 'company-capf)
