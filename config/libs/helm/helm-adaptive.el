; -*- lexical-binding:nil -*-

(defconst helm-adaptive-history-length 50
"Maximum number of candidates stored for a source.")

(defconst helm-adaptive-sort-by-frequent-recent-usage t
"Try to sort on an average of frequent and recent usage when non-nil.

When nil sort on frequency usage only.

nil - only frequency:
When candidate have low frequency, you have to hit on it many
times to make it going up on top.

t - frequency + recent:
Even with a low frequency, candidate go up on top. If a candidate
have a high frequency but it is not used since some time, it goes
down slowly, but as soon you reuse it it go up on top quickly.")

; Internal
(defvar helm-adaptive-done nil
"Nil if history information is not yet stored for the current selection.")

(defvar helm-adaptive-history nil
"Contains the stored history information.
Format: ((SOURCE-NAME (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

(defconst helm-adaptive-freq-coefficient
	; This '5' is for customization.
	(if helm-adaptive-sort-by-frequent-recent-usage 5 1))
(defconst helm-adaptive-recent-coefficient 2)

(defun helm-adaptive-store-selection ()
"Store history information for the selected candidate."
	(unless helm-adaptive-done
		(setq helm-adaptive-done t)
		(when-let (
			(source (helm-get-current-source))
			((helm-source-sync-p source))
			((helm-source-sync-adaptive source))
		)
			(let* (
				(source-name (helm-get-name source))
				(source-info
					(or
						(assoc source-name helm-adaptive-history)
						(push (list source-name) helm-adaptive-history)))
				(selection (helm-get-selection t))
				(selection-info
					(car
						(setcdr source-info
							(cons
								(if-let ((found (assoc selection (cdr source-info))))
									(progn
										; move entry to the beginning of the
										; list, so that it doesn't get
										; trimmed when the history is truncated
										(setcdr source-info
											(delq found (cdr source-info)))
										found)
									; new entry
									(list selection))
								(cdr source-info)))))
				(pattern-info
					(car
						(setcdr selection-info
							(cons
								(if-let ((found (assoc helm-pattern (cdr selection-info))))
									(progn
										; move entry to the beginning of the
										; list, so if two patterns used the
										; same number of times then the one
										; used last appears first in the list
										(setcdr selection-info
											(delq found (cdr selection-info)))
										found)
									; new entry
									(cons helm-pattern 0))
								(cdr selection-info)))))
				(timestamp-info
					(or
						(assq 'timestamp (cdr selection-info))
						(car
							(setcdr selection-info
								(cons (cons 'timestamp 0) (cdr selection-info))))))
			)
				; Increase usage count.
				(setcdr pattern-info (1+ (cdr pattern-info)))
				; Update timestamp.
				(setcdr timestamp-info (float-time))
				; Truncate history if needed.
				(ntake helm-adaptive-history-length (cdr selection-info))))))

(defun helm-adaptive-done-reset () (setq helm-adaptive-done nil))

(define-minor-mode helm-adaptive-mode "Toggle adaptive sorting in all sources."
	:global t
	(if helm-adaptive-mode
		(progn
			; Should run at beginning of `helm-initial-setup'.
			(add-hook 'helm-before-initialize-hook #'helm-adaptive-done-reset)
			; Should run at beginning of `helm-exit-minibuffer'.
			(add-hook 'helm-before-action-hook #'helm-adaptive-store-selection)
			; Should run at beginning of `helm-select-action'.
			(add-hook 'helm-select-action-hook #'helm-adaptive-store-selection)
			; Save adaptive history in desktop.
			(cl-pushnew 'helm-adaptive-history desktop::globals_to_save :test #'eq))
		(setq helm-adaptive-history nil)
		(remove-hook 'helm-before-initialize-hook #'helm-adaptive-done-reset)
		(remove-hook 'helm-before-action-hook #'helm-adaptive-store-selection)
		(remove-hook 'helm-select-action-hook #'helm-adaptive-store-selection)
		(setq desktop::globals_to_save
			(delq 'helm-adaptive-history desktop::globals_to_save))))

(helm-adaptive-mode)

(defun helm-adaptive-sort (candidates)
"Sort the CANDIDATES for SOURCE by usage frequency.
This is a filtered candidate transformer function."
	(if-let (
		(source-info
			(assoc (helm-source-name helm-current-source) helm-adaptive-history))
	)
		(nconc
			; Put those candidates first which have the highest usage count.
			(cl-loop
				with multiline = (helm-source-multiline helm-current-source)
				for cand_and_freq in
					; Loop in the SOURCE entry of `helm-adaptive-history'
					; and assemble a list containing the (CANDIDATE
					; . USAGE-COUNT) pairs.

					; Sort the list in descending order, so
					; candidates with highest priority come first.
					(sort
						(mapcar
							(lambda (info)
								(let* (
									(count-freq 0)
									(timestamp (assq 'timestamp (cdr info)))
									(count-rec
										(if
											(and
												helm-adaptive-sort-by-frequent-recent-usage
												timestamp)
											(*
												helm-adaptive-recent-coefficient
												(+ (float-time) (cdr timestamp)))
											0))
								)
									(cl-loop
										for (pattern . score) in (remq timestamp (cdr info))
										; If current pattern is equal to
										; the previously used one then
										; this candidate has priority
										; (that's why its count-freq is
										; boosted by 10000) and it only
										; has to compete with other
										; candidates which were also
										; selected with the same pattern.
										when (equal pattern helm-pattern)
											return (setq count-freq (+ 10000 score))
										do (+= count-freq score))
									(cons
										(car info)
										(+
											(* count-freq helm-adaptive-freq-coefficient)
											count-rec))))
							(cdr source-info))
						(lambda (first second) (> (cdr first) (cdr second))))
				for cand =
					(find_in_list candidates
						; Compare display parts if some of candidates X and Y.
						; Arguments X and Y are cons cell in (DISPLAY . REAL)
						; format or atoms.
						; This is obviously shit when display part changes for example based
						; on helm-window width or something, then nothing will match.
						; It isn't even mentioned anywhere.
						`(lambda (x)
							(string=
								,(if (listp (car cand_and_freq))
									(car (car cand_and_freq))
									(car cand_and_freq))
								(if (listp x) (car x) x))))
				when cand
					collect
						(progn
							(setq candidates (remq cand candidates))
							cand))
			candidates)
		; If there is no information stored for this source then do nothing.
		candidates))

(provide 'helm-adaptive)
