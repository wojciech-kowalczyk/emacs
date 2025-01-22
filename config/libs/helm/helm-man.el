; -*- lexical-binding:nil -*-

(declare-function woman-file-name "woman.el" (topic &optional re-cache))
(declare-function woman-file-name-all-completions "woman.el" (topic))
(declare-function woman-expand-directory-path "woman.el" (path-dirs path-regexps))
(declare-function woman-topic-all-completions "woman.el" (path))
(declare-function Man-getpage-in-background "man.el" (topic))

(defvar woman-topic-all-completions)
(defvar woman-manpath)
(defvar woman-path)
(defvar woman-expanded-directory-path)

(defconst helm-man-or-woman-function #'Man-getpage-in-background
"Default command to display a man page.
Either 'Man-getpage-in-background' or 'woman'.")

(defconst helm-man-format-options
	(if (memq system-type '(darwin macos)) "%s" "-l %s")
"Arguments to pass to the `manual-entry' function.
Arguments are passed to `manual-entry' with `format.'")

(defvar helm-man-cache nil "All man pages on system.")

(defun helm-man-candidates ()
	(setq woman-expanded-directory-path
		(woman-expand-directory-path woman-manpath woman-path))
	(setq woman-topic-all-completions
		(woman-topic-all-completions woman-expanded-directory-path))
	(setq helm-man-cache
		(sort (mapcar #'car woman-topic-all-completions) #'helm-sort-alpha)))

(defconst helm-man-source
	(helm-source-sync-make nil
		:name "Manual pages"
		:candidates (lambda () helm-man-cache)
		:volatile t
		:sort #'helm-sort-length
		:update #'helm-man-candidates
		:action
			(lambda (candidate)
				(let ((wfiles (mapcar #'car (woman-file-name-all-completions candidate))))
					(condition-case nil
						(let (
							(file
								(if (cdr wfiles)
									(helm-comp-read "Man file: " wfiles :must-match t)
									(car wfiles)))
						)
							(if
								(eq
									helm-man-or-woman-function
									#'Man-getpage-in-background)
								(manual-entry (format helm-man-format-options file))
								(condition-case nil
									(woman-find-file file)
									; If woman is unable to format correctly
									; try Man instead.
									(error
										(kill-buffer)
										(manual-entry
											(format helm-man-format-options file))))))
						; If even Man failed with file as argument,
						; try again with Man but using Topic candidate
						; instead of the file calculated by woman.
						(error
							(kill-buffer)
							(Man-getpage-in-background candidate)))))
		:nomark t))

(defun helm-man-woman (arg)
"helm for Man and Woman pages.
With a prefix arg reinitialize the cache."
	(interactive "P")
	(require 'woman)
	(when arg (setq helm-man-cache nil))
	(unless helm-man-cache (helm-man-candidates))
	(helm
		:sources (list helm-man-source)
		:default (thing-at-point 'symbol)
		:use-default-as-input t))

(provide 'helm-man)
