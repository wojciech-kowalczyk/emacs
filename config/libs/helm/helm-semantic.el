; -*- lexical-binding:nil -*-

(require 'semantic)

(declare-function pulse-momentary-highlight-one-line "pulse.el" (point &optional face))

(defconst helm-semantic-display-style
	'(
		(c-mode . semantic-format-tag-concise-prototype-c-mode)
		(emacs-lisp-mode . semantic-format-tag-abbreviate-emacs-lisp-mode)
		(python-mode . semantic-format-tag-summarize)
	)
"Function to present a semantic tag according to `major-mode'.

Alist (major-mode . semantic-format-tag-function).

If no function is found for current `major-mode', fall back to
`semantic-format-tag-summarize' default function.

You can have more or less informations depending of the `semantic-format-tag-*'
function you choose.

All the supported functions are prefixed with \"semantic-format-tag-\",
you have completion on these functions with `C-M i' in the customize interface.")

; Internals vars
(defvar helm-semantic--tags-cache nil)

(defun helm-semantic-candidates (tags depth &optional class)
	(let (
		(stylefn
			(or
				(cdr
					(assq
						(buffer-local-value 'major-mode helm-current-buffer)
						helm-semantic-display-style))
				#'semantic-format-tag-summarize))
		cur-type
		candidates ; I haven't tested if candidates added have newlines or not.
	)
		(dolist (tag tags)
			(when (listp tag)
				(cl-case (setq cur-type (semantic-tag-class tag))
					((function variable type)
						(let (
							(spaces (get_space_string (* depth 2)))
							(type-p (eq cur-type 'type))
						)
							(when (or (<= depth 0) type-p)
								(setq class nil))
							(push
								(concat
									(if (and class (not type-p))
										(concat
											spaces
											(when (>= depth 2) "├►")
											"("
											class
											") ")
										spaces)
									; Save the tag for later
									(propertize (funcall stylefn tag nil t) 'semantic-tag tag))
								candidates)
							(when type-p (setq class (car tag)))
							; Recurse to children.
							(unless (eq cur-type 'function)
								(setq candidates
									(nconc
										(helm-semantic-candidates
											(semantic-tag-components tag) (1+ depth) class)
										candidates)))))

					; Don't do anything with packages or includes for now
					((package include)
						(push
							(propertize (funcall stylefn tag nil t) 'semantic-tag tag)
							candidates)))))
		(nreverse candidates)))

(defun helm-semantic-action ()
	(with-current-buffer helm-buffer
		(when (looking-at " ")
			(goto-char
				(next-single-property-change
					(pos-bol) 'semantic-tag nil (pos-eol))))
		(let ((tag (get-text-property (point) 'semantic-tag)))
			(semantic-go-to-tag tag)))
	nil)

(defconst helm-source-semantic
	(helm-source-sync-make nil
		:name "Semantic Tags"
		:candidates
			(lambda ()
				(when (semantic-parse-tree-needs-update-p)
					(semantic-parse-tree-set-needs-update))
				(setq helm-semantic--tags-cache (semantic-fetch-tags))
				(let ((major-mode (buffer-local-value 'major-mode helm-current-buffer)))
					(helm-semantic-candidates helm-semantic--tags-cache 0)))
		:persistent-action #'helm-semantic-action
		:action
			(lambda (_)
				(helm-semantic-action)
				(pulse-momentary-highlight-one-line))
		:candidate-number-limit 9999
		:nomark t))

(defun helm-semantic (arg)
"If ARG is supplied, pre-select symbol at point instead of current."
	(interactive "P")
	(let (
		(tag
			(if-let ((parent (car (semantic-current-tag-parent))))
				(let ((curtag (car (semantic-current-tag))))
					(if (string= parent curtag)
						(format "\\_<%s\\_>" curtag)
						(cons
							(format "\\_<%s\\_>" parent)
							(format "\\_<%s\\_>" curtag))))
				(format "\\_<%s\\_>" (car (semantic-current-tag)))))
	)
		(helm
			:sources (list helm-source-semantic)
			:preselect (if arg (thing-at-point 'symbol) tag))))

;(setq semantic-default-submodes
;	'(
;		global-semanticdb-minor-mode
;		global-semantic-idle-scheduler-mode
;		global-semantic-idle-summary-mode
;		global-semantic-idle-completions-mode
;		global-semantic-decoration-mode
;		global-semantic-highlight-func-mode
;		global-semantic-stickyfunc-mode
;		global-semantic-idle-local-symbol-highlight-mode
;	))

(provide 'helm-semantic)
