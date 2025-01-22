; -*- lexical-binding:nil -*-

; company-mode completion backend using Semantic

(require 'company-template)

(defvar semantic-idle-summary-function)
(declare-function semantic-documentation-for-tag "semantic/doc" )
(declare-function semantic-analyze-current-context "semantic/analyze")
(declare-function semantic-analyze-possible-completions "semantic/complete")
(declare-function semantic-analyze-find-tags-by-prefix "semantic/analyze/fcn")
(declare-function semantic-tag-class "semantic/tag")
(declare-function semantic-tag-name "semantic/tag")
(declare-function semantic-tag-start "semantic/tag")
(declare-function semantic-tag-buffer "semantic/tag")
(declare-function semantic-active-p "semantic")
(declare-function semantic-format-tag-prototype "semantic/format")

(defconst company::semantic::metadata_fn #'company::semantic::summary_and_doc
"The function turning a semantic tag into doc information.
Options:
company::semantic::doc_or_summary,
company::semantic::summary_and_doc - the default.")

(defconst company::semantic::begin_after_member_access t
"When non-nil, automatic completion will start whenever the current
symbol is preceded by \".\", \"->\" or \"::\", ignoring `company::min_prefix_length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and \":\".")

(defconst company::semantic::insert_arguments t
"When non-nil, insert function arguments as a template after completion.")

(defconst company::semantic::mode_list '(c-mode c++-mode jde-mode java-mode))

(defvar-local company::semantic::current_tags nil "Tags for the current context.")

(defun company::semantic::documentation_for_tag (tag)
	(if (semantic-tag-buffer tag)
		; When TAG's buffer is unknown, the function below raises an error.
		(semantic-documentation-for-tag tag)))

(defun company::semantic::doc_or_summary (tag)
	(or
		(company::semantic::documentation_for_tag tag)
		(and
			(require 'semantic-idle nil t)
			(require 'semantic/idle nil t)
			(funcall semantic-idle-summary-function tag nil t))))

(defun company::semantic::summary_and_doc (tag)
	(let ((doc (company::semantic::documentation_for_tag tag))
		  (summary (funcall semantic-idle-summary-function tag nil t)))
		(if (and (stringp doc) (string-match "\n*\\(.*\\)$" doc))
			(setq doc (match-string 1 doc)))
		(concat
			summary
			(if doc
				(if (< (+ (length doc) (length summary) 4) (window-body-width))
					" -- "
					"\n"))
			doc)))

(defun company-semantic (command &optional arg)
"`company-mode' completion backend using CEDET Semantic."
(interactive '(interactive))
	(let ((get_annotation
			(lambda () ; Dynamic binding: arg.
				(if-let ((tag (assq arg company::semantic::current_tags)) ((eq 'function (elt tag 1))))
					(let* ((prototype (semantic-format-tag-prototype tag nil nil))
						   (par-pos (string-match "(" prototype)))
						(if par-pos
							(substring prototype par-pos)))))))
		(cl-case command
			(interactive (company-begin-backend 'company-semantic))
			(prefix
				(and
					(featurep 'semantic)
					(semantic-active-p)
					(memq major-mode company::semantic::mode_list)
					(inside_code)
					(if company::semantic::begin_after_member_access
						(company::get_prefix_after_member_access)
						(company-grab-symbol))))
			(candidates
				(if (and (equal arg "") (not (looking-back "->\\|\\.\\|::" (- (point) 2))))
					(progn
						(setq company::semantic::current_tags nil)
						(dolist (tag (semantic-analyze-find-tags-by-prefix arg))
							(unless (eq (semantic-tag-class tag) 'include)
								(push tag company::semantic::current_tags)))
						(delete "" (mapcar 'semantic-tag-name company::semantic::current_tags)))
					(ignore-errors
						(let ((completion-ignore-case nil)
							  (context (semantic-analyze-current-context)))
							(setq company::semantic::current_tags
								(semantic-analyze-possible-completions context 'no-unique))
							(all-completions arg company::semantic::current_tags)))))
			(meta (funcall company::semantic::metadata_fn (assoc arg company::semantic::current_tags)))
			(annotation (funcall get_annotation))
			(doc-buffer
				(let* ((tag (assoc arg company::semantic::current_tags))
					   (doc (company::semantic::documentation_for_tag tag)))
					(if doc
						(company-doc-buffer
							(concat
								(funcall semantic-idle-summary-function tag nil t)
								"\n"
								doc)))))
			; Because "" is an empty context and doesn't return local variables.
			(no-cache (equal arg ""))
			(duplicates t)
			(location
				(let ((tag (assoc arg company::semantic::current_tags)))
					(when (buffer-live-p (semantic-tag-buffer tag))
						(cons (semantic-tag-buffer tag) (semantic-tag-start tag)))))
			(post-completion
				(when-let* (company::semantic::insert_arguments (annotation (funcall get_annotation)))
					(insert annotation)
					(company::template::c_like_templatify (concat arg annotation)))))))

(provide 'company-semantic)
