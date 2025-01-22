; -*- lexical-binding:nil -*-

(require 'bookmark)

(defconst helm-bookmark-default-sort-method 'adaptive
"Sort method for `helm-filtered-bookmarks'.

Value can be either \\='native' or \\='adaptive'.

Once you use \\='native' the bookmark variable `bookmark-sort-flag'
will be honored.")

(define_face 'helm-bookmark-info '((t :foreground "green"))
"Face used for W3m Emacs bookmarks (not w3m bookmarks).")

(define_face 'helm-bookmark-w3m '((t :foreground "yellow"))
"Face used for W3m Emacs bookmarks (not w3m bookmarks).")

(define_face 'helm-bookmark-gnus '((t :foreground "magenta"))
"Face used for Gnus bookmarks.")

(define_face 'helm-bookmark-man '((t :foreground "Orange4"))
"Face used for Woman/man bookmarks.")

(define_face 'helm-bookmark-file '((t :foreground "Deepskyblue2"))
"Face used for file bookmarks.")

(define_face 'helm-bookmark-file-not-found '((t foreground "Slategray4"))
"Face used for file bookmarks.")

(define_face 'helm-bookmark-directory '((t :inherit helm-ff-directory))
"Face used for file bookmarks.")

(define_face 'helm-bookmark-addressbook '((t :foreground "tomato"))
"Face used for addressbook bookmarks.")


(defun helm-bookmark-jump (candidate &optional fn)
	(let ((current-prefix-arg helm-current-prefix-arg))
		(bookmark-jump candidate fn)))

(defun helm-bookmark-jump-other-window (candidate)
	(helm-bookmark-jump candidate #'switch-to-buffer-other-window))

(defun helm-bookmark-jump-other-frame (candidate)
	(helm-bookmark-jump candidate #'switch-to-buffer-other-frame))


(defun helm-bookmark-get-bookmark-from-name (bmk)
"Return bookmark name even if it is a bookmark with annotation.
E.g. prepended with *."
	(let ((bookmark (replace-regexp-in-string "\\`\\*" "" bmk)))
		(if (assoc bookmark bookmark-alist) bookmark bmk)))

(defun helm-bookmark-delete (bookmark)
	(bookmark-delete (helm-bookmark-get-bookmark-from-name bookmark) t))


(defconst helm-bookmark-map
	(let ((map (make-sparse-keymap)))
		(set-keymap-parent map helm-map)
		(define-key map [S-return]
			(helm-make-action-command #'helm-bookmark-jump-other-window))
		(define-key map [C-return]
			(helm-make-action-command #'helm-bookmark-jump-other-frame))
		(define-key map [?\C-e]
			(helm-make-action-command #'helm-bookmark-edit-bookmark))
		; This could be helm-add-delete-binds-force to update first column width,
		; but I guess it's not worth it (performance loss).
		(helm-add-delete-binds
			map
			(lambda (cand cache)
				(helm-bookmark-delete cand)
				(rassq-delete-all cand cache))
			t)
		map)
"Keymap for Emacs bookmark sources.")

(defconst helm-bookmark-actions
	(list
		(cons "Jump to bookmark" #'helm-bookmark-jump)
		(cons "Jump to bookmark other window" #'helm-bookmark-jump-other-window)
		(cons "Jump to bookmark other frame" #'helm-bookmark-jump-other-frame)
		(cons
			"Delete bookmark(s)"
			(lambda (_candidate)
				(mapc #'helm-bookmark-delete (helm-marked-candidates))))
		(cons "Edit bookmark" #'helm-bookmark-edit-bookmark)
		(cons "Rename bookmark" #'helm-bookmark-rename)
		(cons "Relocate bookmark" #'bookmark-relocate)
		(cons "Edit annotation" #'bookmark-edit-annotation)
		(cons "Show annotation" #'bookmark-show-annotation)))

(defun helm-source-bookmark-action-transformer (actions)
	(if (bookmark-get-annotation (helm-get-selection))
		actions
		(cl-remove-if
			(lambda (cell) (eq (cdr cell) #'bookmark-show-annotation))
			actions)))

(defun helm-source-bookmark-make (candidates action_transformer &rest args)
	(apply #'helm-source-sync-make nil
		:candidates `(lambda () (helm-bookmark-highlight (,candidates)))
		:action-transformer
			(or action_transformer #'helm-source-bookmark-action-transformer)
		:match-part t
		:keymap helm-bookmark-map
		:action helm-bookmark-actions
		args))

(defconst helm-bookmark-source
	(helm-source-bookmark-make
		(lambda () (mapcar #'bookmark-name-from-full-record (bookmark-maybe-sort-alist)))
		(lambda (actions)
			; If selected candidate is a new bookmark.
			(if (helm-with-window-or-buffer (get-text-property (point) 'helm-new))
				#'bookmark-set
				(helm-source-bookmark-action-transformer actions)))
		:name "Bookmarks"
		:candidate-transformer #'helm-maybe-add-new-candidate
		:confirm t))

; Predicates

(defun helm-bookmark--bookmark-p-base (bookmark handlers)
	(memq (bookmark-get-handler bookmark) handlers))

(defun helm-bookmark-gnus-bookmark-p (bookmark)
"Return non-nil if BOOKMARK is a Gnus bookmark.
BOOKMARK is a bookmark name or a bookmark record."
	(helm-bookmark--bookmark-p-base
		bookmark
		'(bmkext-jump-gnus gnus-summary-bookmark-jump bookmarkp-jump-gnus)))

(defun helm-bookmark-mu4e-bookmark-p (bookmark)
"Return non nil if BOOKMARK is a mu4e bookmark.
BOOKMARK is a bookmark name or a bookmark record."
	(helm-bookmark--bookmark-p-base
		bookmark
		'(mu4e-bookmark-jump mu4e--jump-to-bookmark)))

(defun helm-bookmark-w3m-bookmark-p (bookmark)
"Return non-nil if BOOKMARK is a W3m bookmark.
BOOKMARK is a bookmark name or a bookmark record."
	(helm-bookmark--bookmark-p-base
		bookmark
		'(bmkext-jump-w3m bookmark-w3m-bookmark-jump bookmarkp-jump-w3m)))

(defun helm-bookmark-woman-man-bookmark-p (bookmark)
"Return non-nil if BOOKMARK is a Man or Woman bookmark.
BOOKMARK is a bookmark name or a bookmark record."
	(helm-bookmark--bookmark-p-base
		bookmark
		'(
			bmkext-jump-man bmkext-jump-woman
			Man-bookmark-jump woman-bookmark-jump
			bookmarkp-jump-man bookmarkp-jump-woman
		)))

(defun helm-bookmark-info-bookmark-p (bookmark)
"Return non-nil if BOOKMARK is an Info bookmark.
BOOKMARK is a bookmark name or a bookmark record."
	(helm-bookmark--bookmark-p-base bookmark '(Info-bookmark-jump)))

(defun helm-bookmark-helm-ff-p (bookmark)
"Return non-nil if BOOKMARK bookmarks a `helm-ff' session.
BOOKMARK is a bookmark name or a bookmark record."
	(helm-bookmark--bookmark-p-base bookmark '(helm-ff-bookmark-jump)))

(defun helm-bookmark-image-bookmark-p (bookmark)
"Return non-nil if BOOKMARK bookmarks an image file."
	(assq 'image-type
		(if (stringp bookmark)
			(assq bookmark bookmark-alist)
			bookmark)))

(defun helm-bookmark-file-p (bookmark)
"Return non-nil if BOOKMARK bookmarks a file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
	(let ((filename (bookmark-get-filename bookmark)))
		(and filename (not (bookmark-get-handler bookmark)))))

(defun helm-bookmark-org-file-p (bookmark)
	(let ((filename (bookmark-get-filename bookmark)))
		(or
			(string-suffix-p ".org" filename t)
			(string-suffix-p ".org_archive" filename t))))

(defun helm-bookmark-addressbook-p (bookmark)
"Return non-nil if BOOKMARK is a contact recorded with addressbook-bookmark.
BOOKMARK is a bookmark name or a bookmark record."
	(string= (bookmark-prop-get bookmark 'type) "addressbook"))

(defun helm-bookmark-uncategorized-bookmark-p (bookmark)
"Return non-nil if BOOKMARK match no known category."
	(cl-loop
		for pred in
			'(
				helm-bookmark-org-file-p
				helm-bookmark-addressbook-p
				helm-bookmark-gnus-bookmark-p
				helm-bookmark-mu4e-bookmark-p
				helm-bookmark-w3m-bookmark-p
				helm-bookmark-woman-man-bookmark-p
				helm-bookmark-info-bookmark-p
				helm-bookmark-image-bookmark-p
				helm-bookmark-file-p
				helm-bookmark-helm-ff-p
				helm-bookmark-addressbook-p
			)
		never (funcall pred bookmark)))

; Bookmark handlers

(defvar w3m-async-exec)
(defun helm-bookmark-jump-w3m (bookmark)
"Jump to W3m bookmark BOOKMARK, setting a new tab.
If `browse-url-browser-function' is set to something else than
`w3m-browse-url' use it."
	(let* (
		(file
			(or
				(bookmark-prop-get bookmark 'filename)
				(bookmark-prop-get bookmark 'url)))
		(buf (generate-new-buffer-name "*w3m*"))
		w3m-async-exec
		; If user don't have anymore w3m installed let it browse its
		; bookmarks with default browser otherwise assume bookmark
		; have been bookmarked from w3m and use w3m.
		(browse-url-browser-function
			(or
				(and (fboundp 'w3m-browse-url) (executable-find "w3m") #'w3m-browse-url)
				browse-url-browser-function))
		(really-use-w3m (equal browse-url-browser-function 'w3m-browse-url))
	)
		(helm-browse-url file really-use-w3m)
		(when really-use-w3m
			(bookmark-default-handler
				`("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark))))))

; All bookmarks recorded with the handler provided with w3m
; (`bookmark-w3m-bookmark-jump') will use our handler which open
; the bookmark in a new tab or in an external browser depending
; on `browse-url-browser-function'.
(defalias 'bookmark-w3m-bookmark-jump #'helm-bookmark-jump-w3m)

; Provide compatibility with old handlers provided in external
; packages bookmark-extensions.el and bookmark+.
(defalias 'bmkext-jump-woman #'woman-bookmark-jump)
(defalias 'bmkext-jump-man #'Man-bookmark-jump)
(defalias 'bmkext-jump-w3m #'helm-bookmark-jump-w3m)
(defalias 'bmkext-jump-gnus #'gnus-summary-bookmark-jump)
(defalias 'bookmarkp-jump-gnus #'gnus-summary-bookmark-jump)
(defalias 'bookmarkp-jump-w3m #'helm-bookmark-jump-w3m)
(defalias 'bookmarkp-jump-woman #'woman-bookmark-jump)
(defalias 'bookmarkp-jump-man #'Man-bookmark-jump)

; Edit/rename/save bookmarks.

(defun helm-bookmark-edit-bookmark (bookmark-name)
"Edit bookmark's name and file name, and maybe save them.
BOOKMARK-NAME is the current (old) name of the bookmark to be renamed."
	(let (
		(bmk (helm-bookmark-get-bookmark-from-name bookmark-name))
		(handler (bookmark-prop-get bookmark-name 'handler))
	)
		(if (eq handler 'addressbook-bookmark-jump)
			(addressbook-bookmark-edit (assoc bmk bookmark-alist))
			(let* (
				(bookmark-fname (bookmark-get-filename bookmark-name))
				(new-name (read-from-minibuffer "Name: " bookmark-name))
				(new-loc
					(let ((bookmark-loc (bookmark-prop-get bookmark-name 'location)))
						(and
							(or bookmark-fname bookmark-loc)
							(read-from-minibuffer "Filename or location: "
								(or
									bookmark-fname
									(if (consp bookmark-loc)
										(car bookmark-loc)
										bookmark-loc))))))
				(new-message-id
					(and
						(memq handler '(mu4e--jump-to-bookmark mu4e-bookmark-jump))
						(read-string "Message-id: "
							(bookmark-prop-get bookmark-name 'message-id))))
			)
				(unless
					(or
						(equal new-name "")
						(and (equal new-loc "") (equal new-message-id ""))
						(not (y-or-n-p "Save changes?")))
					(if bookmark-fname
						(progn
							(helm-bookmark-rename bookmark-name new-name 'batch)
							(bookmark-set-filename new-name new-loc))
						(bookmark-prop-set
							(bookmark-get-bookmark bookmark-name)
							(cond (new-loc 'location) (new-message-id 'message-id))
							(or new-loc new-message-id))
						(helm-bookmark-rename bookmark-name new-name 'batch))
					(helm-bookmark-maybe-save-bookmark)
					(list new-name new-loc))))))

(defun helm-bookmark-maybe-save-bookmark ()
"Increment save counter and maybe save `bookmark-alist'."
	(++ bookmark-alist-modification-count)
	(when (bookmark-time-to-save-p) (bookmark-save)))

(defun helm-bookmark-rename (old &optional new batch)
"Change bookmark's name from OLD to NEW.
Interactively prompt for OLD.
If NEW is nil, then prompt for its string value.

If BATCH is non-nil, then do not rebuild the menu list.

While the user enters the new name, repeated `C-w' inserts
consecutive words from the buffer into the new bookmark name."
	(interactive (list (bookmark-completing-read "Old bookmark name")))
	(bookmark-maybe-historicize-string old)
	(bookmark-maybe-load-default-file)
	(save-excursion (skip-chars-forward " ") (setq bookmark-yank-point (point)))
	(setq bookmark-current-buffer (current-buffer))
	(let (
		(newname
			(or
				new
				(read-from-minibuffer "New name: " nil
					(let ((map (make-sparse-keymap)))
						(set-keymap-parent map minibuffer-local-map)
						(define-key map [?\C-w] #'bookmark-yank-word)
						map)
					nil
					'bookmark-history)))
	)
		(bookmark-set-name old newname)
		(setq bookmark-current-bookmark newname)
		(unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
		(helm-bookmark-maybe-save-bookmark)
		newname))


(defun helm-bookmark-highlight (candidates)
	(let ((max_length (helm-get-max-length candidates)))
		(mapcar
			(lambda (cand)
				(cons
					(let (
						(location (bookmark-location cand))
						(file (bookmark-get-filename cand))
						(annotation (bookmark-get-annotation cand))
						(is_helm_ff (helm-bookmark-helm-ff-p cand))
						(is_gnus (helm-bookmark-gnus-bookmark-p cand))
						(is_man_or_woman (helm-bookmark-woman-man-bookmark-p cand))
						(is_info (helm-bookmark-info-bookmark-p cand))
						(match_part_start 0)
						(match_part_end 0)
					)
						(cond
							((listp location) (setq location (car location)))
							; If no location specified.
							((string= location "-- Unknown location --")
								(setq location nil)))
						(let (
							(display
								(concat
									(when-let (
										(icon
											(cond
												(is_helm_ff
													(helm-files-icon-for-dir
														(file-name-nondirectory
															(substring-no-properties
																(bookmark-prop-get cand 'default-dir)
																0 -1))))
												((and file is_info) (all-the-icons-octicon "info"))
												(file (all-the-icons-icon-for-file (helm-basename file)))
												(is_man_or_woman (all-the-icons-fileicon "man-page"))
												; Gnus or mu4e
												((or is_gnus (helm-bookmark-mu4e-bookmark-p cand))
													(all-the-icons-octicon "mail-read"))))
									)
										(setq match_part_start 3)
										(all-the-icons-align icon))
									(let ((str (copy-sequence cand)))
										(propertize_no_copy str
											'face
											(cond
												(is_info 'helm-bookmark-info)
												(is_gnus 'helm-bookmark-gnus)
												(is_man_or_woman 'helm-bookmark-man)
												(is_helm_ff
													(if (file-exists-p (bookmark-prop-get cand 'default-dir))
														'helm-bookmark-directory
														'helm-bookmark-file-not-found))
												((helm-bookmark-w3m-bookmark-p cand) 'helm-bookmark-w3m)
												((helm-bookmark-addressbook-p cand) 'helm-bookmark-addressbook)
												; Uncategorized.
												((not file) 'helm-completions-detailed)
												((not (file-exists-p file)) 'helm-bookmark-file-not-found)
												; Directories (dired)
												((file-directory-p file) 'helm-bookmark-directory)
												(t 'helm-bookmark-file)))
										(when-let ((help_echo (or annotation file)))
											(propertize_no_copy str 'helm-echo help_echo))
										str)
									; Idk how bookmarks are usually used,
									; so for now just try to show location.
									(cond
										(location
											(concat
												"  "
												(propertize location 'face 'helm-completions-detailed)))
										(file
											(concat
												"  "
												(propertize file 'face 'helm-completions-detailed))))
									(when annotation
										(setq match_part_end 3)
										; Idk why, but if bookmark has annotation,
										; append "*" (it was like this originally).
										(concat
											"  "
											(propertize "*" 'face 'helm-completions-detailed)))))
						)
							(propertize_no_copy display
								'match-part
								(list (cons match_part_start (- (length display) match_part_end))))))
					cand))
			candidates)))

(defconst helm-bookmark-filtered-sources
	(let (
		(candidates_fn
			`(lambda (pred_fn)
				(cl-loop
					for bookmark in
						; Make it one time check, because candidate_transformer
						; is like that too.
						,(if (eq helm-bookmark-default-sort-method 'native)
							'(bookmark-maybe-sort-alist)
							'bookmark-alist)
					when (funcall pred_fn bookmark)
						collect (bookmark-name-from-full-record bookmark))))
		candidate_transformer
		adaptive
	)
		(when (eq helm-bookmark-default-sort-method 'adaptive)
			(setq candidate_transformer #'helm-adaptive-sort)
			(setq adaptive t))
		(mapcar
			(lambda (name_and_pred_fn)
				(helm-source-bookmark-make
					`(lambda () (,candidates_fn #',(cdr name_and_pred_fn)))
					nil
					:name (car name_and_pred_fn)
					:candidate-transformer candidate_transformer
					:adaptive adaptive))
			(vector
				(cons "Bookmark helm-ff sessions" #'helm-bookmark-helm-ff-p)
				(cons "Bookmark files" #'helm-bookmark-file-p)
				(cons "Bookmark Org files" #'helm-bookmark-org-file-p)
				(cons "Bookmark Info" #'helm-bookmark-info-bookmark-p)
				(cons "Bookmark Gnus" #'helm-bookmark-gnus-bookmark-p)
				(cons "Bookmark Mu4e" #'helm-bookmark-mu4e-bookmark-p)
				(cons "Bookmark Woman&Man" #'helm-bookmark-woman-man-bookmark-p)
				(cons "Bookmark Images" #'helm-bookmark-image-bookmark-p)
				(cons "Bookmark W3m" #'helm-bookmark-w3m-bookmark-p)
				(cons "Bookmark uncategorized" #'helm-bookmark-uncategorized-bookmark-p))))
"List of sources to use in `helm-filtered-bookmarks'.")

(defun helm-bookmarks () (interactive)
	(bookmark-maybe-load-default-file)
	(helm
		:sources (list helm-bookmark-source)
		:default (delq nil (list (thing-at-point 'symbol) (buffer-name)))))

(defun helm-filtered-bookmarks () "helm for bookmarks (filtered by category)."
	(interactive)
	(bookmark-maybe-load-default-file)
	(helm
		:sources helm-bookmark-filtered-sources
		:default (delq nil (list (thing-at-point 'symbol) (buffer-name)))))

(provide 'helm-bookmark)
