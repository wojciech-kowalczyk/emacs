; -*- lexical-binding:nil -*-

; Utility library for template expansion.

(defface company::template::field_face '((t (:background "yellow" :foreground "black")))
"Face used for editable text in template fields.")

(defvar company::template::navigation_map
	(let ((map (make-sparse-keymap)))
		(define-key map [tab] #'company::template::forward_field)
		map))

(defvar company::template::field_map
	(let ((map (make-sparse-keymap)))
		(set-keymap-parent map company::template::navigation_map)
		(define-key map [?\C-d] #'company::template::clear_field)
		map))

(defvar-local company::template::buffer_template_list nil)

(defun company::template::forward_field () (interactive)
	(unless (run-hook-with-args-until-success 'yas-keymap-disable-hook)
		(let ((start (point))
			  (next_field_start (company::template::find_next_field)))
			(set_mark (point))
			(unless mark-active
				(activate_mark))
			(goto-char next_field_start)
			(company::template::remove_field (company::template::field_at start)))))

(defun company::template::move_to_first (template) (interactive)
	(goto-char (overlay-start template))
	(company::template::forward_field))

(defun company::template::clear_field () "Clear the field at point." (interactive)
	(when-let (((not (run-hook-with-args-until-success 'yas-keymap-disable-hook)))
			   (overlay (company::template::field_at (point))))
		(company::template::remove_field overlay t)
		(if-let ((after_clear_fn (overlay-get overlay 'company::template::afterClearFn)))
			(funcall after_clear_fn))))

(defun company::template::find_next_field ()
	(let* ((start (point))
		   (templates ; Templates at start.
			(let (overlay_list)
				(dolist (overlay (overlays-at start))
					; FIXME: Always return the whole list of templates?
					; We remove templates not at point after every command.
					(if (memq overlay company::template::buffer_template_list)
						(push overlay overlay_list)))
				overlay_list))
		   (minimum (apply #'max (mapcar #'overlay-end templates)))
		   (fields
			(cl-loop
				for template in templates
				append (overlay-get template 'company::template::fields))))
		(dolist (pos (mapcar #'overlay-start fields))
			(if (and pos (> pos start) (< pos minimum))
				(setq minimum pos)))
		minimum))

(defun company::template::field_at (position)
	(cl-loop
		for overlay in (overlays-at position (point))
		when (overlay-get overlay 'company::template::parent)
			return overlay))


(defun company::template::declare_template (start end)
	(let ((overlay (make-overlay start end)))
		; (overlay-put overlay 'face 'highlight)
		(overlay-put overlay 'keymap company::template::navigation_map)
		(overlay-put overlay 'priority 101)
		(overlay-put overlay 'evaporate t)
		(push overlay company::template::buffer_template_list)
		(add-hook 'post-command-hook 'company::template::post_command nil t)
		overlay))

(defun company::template::remove_template (template)
	(mapc #'company::template::remove_field
		(overlay-get template 'company::template::fields))
	(setq company::template::buffer_template_list
		(delq template company::template::buffer_template_list))
	(delete-overlay template)
	nil)

(defun company::template::add_field (template start end &optional display after_clear_fn)
"Add new field to template spanning from start to end.
When display is non-nil, set the respective property on the overlay.
Leave point at the end of the field.
after_clear_fn is a function that can be used to apply custom behavior
after deleting a field in `company::template::remove_field'."
	(if (> end (overlay-end template))
		(move-overlay template (overlay-start template) end))
	(let ((overlay (make-overlay start end))
		  (siblings (overlay-get template 'company::template::fields)))
		; (overlay-put overlay 'evaporate t)
		(overlay-put overlay 'intangible t)
		(overlay-put overlay 'face 'company::template::field_face)
		(if display
			(overlay-put overlay 'display display))
		(overlay-put overlay 'company::template::parent template)
		(overlay-put overlay 'insert-in-front-hooks '(company::template::insert_hook))
		(if after_clear_fn
			(overlay-put overlay 'company::template::afterClearFn after_clear_fn))
		(overlay-put overlay 'keymap company::template::field_map)
		(overlay-put overlay 'priority 101)
		(push overlay siblings)
		(overlay-put template 'company::template::fields siblings))
	nil)

(defun company::template::remove_field (overlay &optional clear)
	(when (overlayp overlay)
		(when (overlay-buffer overlay)
			(if clear
				(delete-region (overlay-start overlay) (overlay-end overlay)))
			(delete-overlay overlay))
		(let* ((template (overlay-get overlay 'company::template::parent))
			   (siblings (overlay-get template 'company::template::fields)))
			(setq siblings (delq overlay siblings))
			(overlay-put template 'company::template::fields siblings)))
	nil)

; ==================================== Hooks ====================================

(defun company::template::insert_hook (overlay after &rest _)
"Called when a snippet input prompt is modified."
	(unless after
		(company::template::remove_field overlay t)))

(defun company::template::post_command ()
	; Clean up all templetes that don't contain caret.
	(let ((local_overlay_list (overlays-at (point))))
		(dolist (template company::template::buffer_template_list)
			(unless (memq template local_overlay_list)
				(company::template::remove_template template))))
	(unless company::template::buffer_template_list
		(remove-hook 'post-command-hook 'company::template::post_command t)))

; =============================== Nice things ===============================

(defun company::template::c_like_templatify (call)
	(let* ((end (point-marker))
		   (start (- (point) (length call)))
		   (template (company::template::declare_template start end))
		   paren-open
		   paren-close)
		(with-syntax-table (make-syntax-table (syntax-table))
			(modify-syntax-entry ?< "(")
			(modify-syntax-entry ?> ")")
			(when (search-backward ")" start t)
				(setq paren-close (point-marker))
				(forward-char 1)
				(delete-region (point) end)
				(backward-sexp)
				(forward-char 1)
				(setq paren-open (point-marker)))
			(when (search-backward ">" start t)
				(let ((angle-close (point-marker)))
					(forward-char 1)
					(backward-sexp)
					(forward-char)
					(company::template::c_like_args template angle-close)))
			(when (looking-back "\\((\\*)\\)(" (line-beginning-position))
				(delete-region (match-beginning 1) (match-end 1)))
			(when paren-open
				(goto-char paren-open)
				(company::template::c_like_args template paren-close)))
		(if (overlay-get template 'company::template::fields)
			(company::template::move_to_first template)
			(company::template::remove_template template)
			(goto-char end))))

(defun company::template::c_like_args (template end)
	(let ((last_position (point)))
		(while (re-search-forward "\\([^,]+\\),?" end 'move)
			(when (= 0 (car (parse-partial-sexp last_position (point))))
				(company::template::add_field
					template
					last_position
					(match-end 1)
					nil
					; Function that can be called after deleting a field of a c-like template.
					; For c-like templates it is set as `afterPostFn' property on fields in
					; `company::template::add_field'. If there is a next field, delete everything
					; from point to it. If there is no field after point, remove preceding comma if present.
					(lambda ()
						(let* ((position (point))
							   (next_field_start (company::template::find_next_field))
							   (is_last_field (not (company::template::field_at next_field_start))))
							(cond
								((and
										(not is_last_field)
										(< position next_field_start)
										(string-match "^[ ]*,+[ ]*$"
											(buffer-substring-no-properties position next_field_start)))
									(delete-region position next_field_start))
								((and is_last_field (looking-back ",+[ ]*" (line-beginning-position)))
									(delete-region (match-beginning 0) position))))))
				(skip-chars-forward " ")
				(setq last_position (point))))))

(defun company::template::objc_templatify (selector)
	(let* ((end (point-marker))
		   (start (- (point) (length selector) 1))
		   (template (company::template::declare_template start end))
		   (i 0))
		(save-excursion
			(goto-char start)
			(cl-loop
				while (search-forward ":" end t)
				if (looking-at "\\(([^)]*)\\) ?") do
					(company::template::add_field template (point) (match-end 1))
				else do
					(progn
						; Not sure which conditions this case manifests under, but
						; apparently it did before, when I wrote the first test for this function.
						; FIXME: Revisit it.
						(company::template::add_field
							template
							(point)
							(progn (insert "arg" (number-to-string i)) (point)))
						(if (< (point) end)
							(insert " "))
						(++ i))
				while (< (point) end)))
		(company::template::move_to_first template)))

(provide 'company-template)
