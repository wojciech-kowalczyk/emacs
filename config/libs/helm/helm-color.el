; -*- lexical-binding:nil -*-

(require 'facemenu)

(defvar helm-color-cache (make-hash-table :test 'eq :size 2))

(defun helm-color-update () (remhash helm-current-source helm-color-cache))

(defun helm-color-candidates (buffer_name list_x_display point_min)
	(or
		(gethash helm-current-source helm-color-cache)
		(puthash
			helm-current-source
			(progn
				(save-selected-window
					(funcall list_x_display)
					(message nil)
					(when-let ((window (get-buffer-window buffer_name t)))
						(delete-window window)))
				(prog1
					; This is sorted with string< by default,
					; so sort it with helm-sort-alpha.
					(sort
						(split-string
							(with-current-buffer buffer_name
								(goto-char (point-min))
								; (1- (point-max)) because last line is empty.
								(buffer-substring point_min (1- (point-max))))
							"\n")
						#'helm-sort-alpha)
					(kill-buffer buffer_name)))
			helm-color-cache)))

; Customize face

(defconst helm-customize-face-source
	(helm-source-sync-make nil
		:name "Customize face"
		:candidates
			(lambda ()
				(helm-color-candidates
					"*Faces*"
					#'list-faces-display
					; (pos-bol 4) because 3 first lines are some info,
					; not actual lines describing faces.
					(pos-bol 4)))
		:nomark t
		; Just leave original sorting for now (string<), there is no point
		; to sort this based on length, as these lines are all the same length
		; I think (at least from list-colors-display are).
		:sort nil
		:persistent-action
			(lambda (candidate)
				(helm-elisp--persistent-help
					(intern (car (split-string candidate)))
					#'helm-describe-face))
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-map)
				(define-key map [?\C-\S-c] #'helm-copy-candidate)
				(define-key map [?\C-c]
					(lambda () (interactive)
						(clipboard-add
							(car (split-string (helm-get-selection) " " t))
							helm-buffer)))
				map)
		:action
			(list
				(cons
					"Customize"
					(lambda (line)
						(customize-face (intern (car (split-string line)))))))
		:update #'helm-color-update))

; Colors browser

(defun helm-colors-get-name (candidate) "Get color name."
	(with-temp-buffer
		(insert candidate)
		(goto-char 1)
		(re-search-forward "\\s-\\{2,\\}")
		(buffer-substring (point-min) (match-beginning 0))))

(defun helm-colors-get-rgb (candidate) "Get color RGB."
	(with-temp-buffer
		(insert candidate)
		(goto-char (point-max))
		(re-search-backward "\\s-\\{2,\\}")
		(buffer-substring (match-end 0) (point-max))))

(defconst helm-colors-source
	(helm-source-sync-make nil
		:name "Colors"
		:candidates
			(lambda ()
				(helm-color-candidates
					"*Colors*"
					#'list-colors-display
					(point-min)))
		:nomark t
		; Just leave original sorting for now (string<), there is no point
		; to sort this based on length, as these lines are all the same length
		; I think (at least from list-colors-display are).
		:sort nil
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-map)
				(define-key map [?\C-\S-c] #'helm-copy-candidate)
				(define-key map [?\C-c ?1]
					(lambda () (interactive)
						(clipboard-add
							(helm-colors-get-name (helm-get-selection))
							helm-buffer)))
				(define-key map [?\C-c ?2]
					(lambda () (interactive)
						(clipboard-add
							(helm-colors-get-rgb (helm-get-selection))
							helm-buffer)))
				map)
		:action
			(list
				(cons
					"Insert name"
					(lambda (candidate) (insert (helm-colors-get-name candidate))))
				(cons
					"Insert RGB"
					(lambda (candidate) (insert (helm-colors-get-rgb candidate)))))
		:update #'helm-color-update))

(defun helm-colors () (interactive)
	(helm :sources (list helm-colors-source helm-customize-face-source)))

(provide 'helm-color)
