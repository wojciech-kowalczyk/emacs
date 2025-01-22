; -*- lexical-binding:nil -*-

; This must be > 0, else disable jump history by binding functions
; from here to #'ignore.
(defconst jumpHistory::LIMIT 80)
(defvar-window-local jump-history-marker-list-and-index)

(defun jumpHistory::add ()
"Add current position and buffer as marker to selected window's jump history,
unless it's a duplicate of the newest entry."
	(if-let ((marker_list_and_index (jump-history-marker-list-and-index)))
		(progn
			(ntake jumpHistory::LIMIT (car marker_list_and_index))
			; When any command will add new entry while jumping
			; in history takes place, discard further saved jumps.
			(when (/= (cdr marker_list_and_index) 0)
				(setcar marker_list_and_index
					(nthcdr (cdr marker_list_and_index) (car marker_list_and_index)))
				(setcdr marker_list_and_index 0))
			; Omit duplicates.
			(unless
				(and
					(eq (point) (marker-position (car (car marker_list_and_index))))
					(eq
						(current-buffer)
						(marker-buffer (car (car marker_list_and_index)))))
				(setcar marker_list_and_index
					(cons (copy-marker (point)) (car marker_list_and_index)))))
		(setf (jump-history-marker-list-and-index) (cons (list (copy-marker (point))) 0)))
	nil)

(defun jumpHistory::is_jump_in_place (marker)
	(and
		(= (point) (marker-position marker))
		(eq (current-buffer) (marker-buffer marker))))

(defun jumpHistory::is_jump_in_view (marker)
	(and
		(eq (current-buffer) (marker-buffer marker))
		(in_range_inclusive (marker-position marker) (window-start) (window-end nil t))))

(defun jumpHistory::jump_base (arg jump_fn)
	(if-let ((marker_list_and_index (jump-history-marker-list-and-index)))
		(let* (
			(marker_list (car marker_list_and_index))
			(index (cdr marker_list_and_index))
			(marker (nth index marker_list))
		)
			; Don't call deactivate-mark here for rectangle::mode,
			; because it wants to not deactivate mark.
			(setq deactivate-mark t)
			; Delete dead markers. index still needs to point to something
			; similar, so if points to the dead marker, make this marker point
			; at the caret so it will no longer be dead and jumping will happen as
			; usual. I think it's an ok solution.
			(unless (marker-buffer marker) (set-marker marker (point)))
			(setq marker_list (cl-delete-if-not #'marker-buffer marker_list))
			(setq index (index_in_list_eq marker_list marker))
			(setq marker
				(catch 'return
					(funcall jump_fn
						marker_list
						index
						marker
						(if arg
							#'jumpHistory::is_jump_in_view
							#'jumpHistory::is_jump_in_place))))
			; Set new index.
			(setcdr marker_list_and_index (car marker))
			(when (setq marker (cdr marker))
				; Don't call this uselessly in switch-to-buffer.
				(let ((after_move_hook_fn #'ignore))
					; switch-to-buffer checks if buffer is current.
					(switch-to-buffer (marker-buffer marker) nil t))
				(goto-char (marker-position marker))
				(funcall after_move_hook_fn)))
		(message "History is empty.")))

(defun jumpHistory::backward (arg)
"Jump to the past. With prefix arg jump to the out of view entry."
	(interactive "P")
	(jumpHistory::jump_base
		arg
		(lambda (marker_list index marker is_invalid_jump_fn)
			(setq marker_list (nthcdr index marker_list))
			(while t
				(if (funcall is_invalid_jump_fn marker)
					(if (setq marker_list (cdr marker_list))
						(progn
							(setq marker (car marker_list))
							(++ index))
						(throw 'return (list index)))
					; Save jump to history if it's the first jump in a row,
					; so it's possible to later go back to it by jumping forward.
					(when (= index 0)
						(jumpHistory::add)
						(++ index))
					(throw 'return (cons index marker))))))) ; Jump to marker.

(defun jumpHistory::forward (arg)
"Jump to the future. With prefix arg jump to the out of view entry."
	(interactive "P")
	(jumpHistory::jump_base
		arg
		(lambda (marker_list index marker is_invalid_jump_fn)
			(while t
				(let ((invalid (funcall is_invalid_jump_fn marker)))
					(when (= index 0)
						(throw 'return (cons index (unless invalid marker))))
					(unless invalid (throw 'return (cons index marker))))
				(setq marker (nth (-- index) marker_list))))))


(define-key global-map [C-up] #'jumpHistory::forward)
(define-key global-map [C-down] #'jumpHistory::backward)
(mouse::bind_in_fringes global-map [down-mouse-5] #'jumpHistory::forward)
(mouse::bind_in_fringes global-map [S-down-mouse-5] #'jumpHistory::forward)
(mouse::bind_in_fringes global-map [down-mouse-4] #'jumpHistory::backward)
(mouse::bind_in_fringes global-map [S-down-mouse-4] #'jumpHistory::backward)

(provide 'myJumpHistory)
