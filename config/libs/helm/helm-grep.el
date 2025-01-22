; -*- lexical-binding:nil -*-

; TODO Add support for grep and rg for skipping boring files by expanding dirs
; and then discarding boring files.

; On Windows max size of a string containing cmd arguments is 32,767, so that's
; a big problem, because selecting all files in a large directory, easily exceeds
; this limit.
; When I'll properly add support for skipping boring files by expanding dirs,
; this will be an even more apparent problem.
; Idk, the best solution I can think of right now is to split grep calls, but
; that would require a lot of changes to helm. That is w/o modifying emacs C code,
; but otherwise, it's definitely better to use grep C library that avoids this
; cross process communication. That in turn would require not using emacs processes
; probably, so it also doesn't fit with helm async stuff now.
;
; Also a random thing I found:
;
; (make-process
;	:name "grep-test"
;	:buffer "grep-test-output"
;	:command (append (list "grep" "--help") (make-list 2974 (make-string 10 ?a))))
;
; doesn't signal lisp error and outputs:
;
;	Wyjątek nieobsłużony: System.ComponentModel.Win32Exception: Nazwa pliku lub jej rozszerzenie są za długie
;	w System.Diagnostics.Process.StartWithCreateProcess(ProcessStartInfo startInfo)
;	w shim.CommandExecutor.execute(String process, String arguments, String working_directory, Boolean is_gui, Boolean wait_for_exit, Boolean requires_elevation)
;	w shim.ShimProgram.Main(String[] args)
;
;	Process grep-test exited abnormally with code 82
;
; Idk, seems like a "bug" in CallProcess<A/W> or something called by it, because it
; should return 0 if it "fails", which is arguably what happens here, but instead
; it "succeeds" and proceeds which later results in an error. Maybe it checks for this
; limit but later something called by CallProcess adds a couple of chars and that fails.

(declare-function helm-buffers-list "helm-buffers")
(declare-function doc-view-goto-page "doc-view" (page))
(declare-function pdf-view-goto-page "pdf-view" (page &optional window))

(defvar helm-tramp-verbose)
(defvar tramp-verbose)

(defconst helm-grep-split-line-regex
	"^\\(\\(?:[[:alpha:]]:\\)?.*\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)$"
"For raw grep line - file:line_number:byte_offset:line.")
(defconst helm-grep-split-display-line-regex "^ +\\([0-9]+\\) \\(.*\\)$"
"For \"display\" part of candidate (line_number:line).")

(defvar helm-grep-history nil
"helm-grep history variable (contains helm-patterns).")
(desktop::add_to_globals 'helm-grep-history)

(defvar helm-grep-targets nil)
(defvar helm-grep-backend nil "nil - grep, \\='zgrep or \\='git.")
(defvar helm-grep-time nil)

(defvar helm-grep-trim nil
"Non-nil to trim starting blanks from grep results.")

; Keymaps/actions

(defconst helm-grep-default-keymap
	(let ((keymap (make-sparse-keymap)))
		(set-keymap-parent keymap helm-map)
		(define-key keymap [?\C-b]
			(lambda () (interactive)
				(setq helm-grep-trim (not helm-grep-trim))
				(helm-force-update)))
		keymap))

(defconst helm-grep-keymap
	(let ((keymap (make-sparse-keymap)))
		(set-keymap-parent keymap helm-grep-default-keymap)
		(let (
			(get_file
				(lambda () (helm-grep-cand-file (get-text-property (point) 'helm-real))))
		)
			; Goto next/previous file.
			(helm-add-goto-bindings keymap get_file)
			; Mark matches from the same file.
			(define-key keymap [?\C-\S-\s]
				`(lambda () (interactive)
					(with-helm-window
						(let ((current_file (,get_file)))
							(helm-mark-some-toggle
								(lambda () (string= (,get_file) current_file))))))))
		(define-key keymap [?\C-\S-s] (helm-make-action-command #'helm-grep-save-results))
		(define-key keymap [?\C-r] (helm-make-action-command #'helm-grep-replace))
		keymap))

(defconst helm-grep-actions
	(list
		(cons "Find file" #'helm-grep-default-action)
		(cons "Find file other window" #'helm-grep-jump-other-window)
		(cons "Find file other frame" #'helm-grep-jump-other-frame)
		(cons "Save results" #'helm-grep-save-results)
		(cons "Replace marked (prefix - fixed case)" #'helm-grep-replace)))

(defconst helm-grep-mode-map
	(let ((keymap (make-sparse-keymap)))
		; Jump in selected window.
		(define-key keymap [C-down-mouse-3]
			(lambda (event) (interactive "e")
				(mouse::down_mouse_command event)
				(helm-grep-mode-jump)))
		; Jump in other window w/o selecting it.
		(define-key keymap [down-mouse-3]
			(lambda (event) (interactive "e")
				(mouse::down_mouse_command event)
				(save-selected-window (helm-grep-mode-jump 'other-window))))
		; Jump in selected window.
		(define-key keymap [return] #'helm-grep-mode-jump)
		; Jump in other window and select it.
		(define-key keymap [S-return]
			(lambda () (interactive) (helm-grep-mode-jump 'other-window)))
		; Jump in other window w/o selecting it.
		(define-key keymap [C-up] #'helm-grep-mode-jump-other-window-backward)
		(define-key keymap [C-down] #'helm-grep-mode-jump-other-window-forward)

		; Navigate in helm-grep-buffer.
		(define-key keymap [C-S-down] #'helm-grep-mode-goto-next-file)
		(define-key keymap [C-S-up] #'helm-grep-mode-goto-previous-file)

		; Resume helm session.
		(define-key keymap [?\C-g]
			(lambda () (interactive)
				; default-directory is the one from helm session saved
				; in helm-grep-mode buffer.
				(helm-grep
					(nth 1 helm-grep-mode-resume-info)
					(nth 3 helm-grep-mode-resume-info)
					:input (car helm-grep-mode-resume-info)
					:helm-grep-time (nth 2 helm-grep-mode-resume-info))))
		keymap))

; General options

(defconst helm-grep-save-buffer-name-no-confirm nil
"When *helm-grep* already exists, auto append suffix.")

(defconst helm-grep-input-idle-delay helm-input-idle-delay
"Idle time before updating, specified in seconds.
A lower value (default) means Helm will display the results faster.
Increasing it to a higher value (e.g. 0.6) prevents the
buffer from flickering when updating.")

(defconst helm-grep-truncate-lines helm-truncate-lines
"When nil the grep line that appears will not be truncated.")

(defconst helm-grep-filename-type t
"Nil - use absolute filenames as prefix of displayed grep result lines,
\\='basename, or some other non-nil value to use relative filenames.")

(defconst helm-grep-line-number-min-width 4
"Min number of places for digits in line-number margin.
E.g. 4 for 1111. 0 - no minimum (there is always at least 1 digit).
Useful to better align results from multiple files.
Results from a single file are always aligned, e.g.
if this var = 0:
file1
 5  line1
 22 line2
file2
 1111 line1
if this var = 4:
 5    line1
 22   line2
file2
 1111 line1")

; Programs and their settings

(defconst helm-grep-exe (executable-find "grep"))

(defconst helm-grep-options
	'(
		"--color=never"
		"--no-messages"
		"--byte-offset"
		"--line-number"
		"--with-filename"
		; Skip binary files.
		; I tried with =text but for some reason there was no file:line-number:byte-offset
		; prefix, idk why, maybe line was too long, so the start of it was discarded?
		; I'm still not sure, but lines in binary files can be thousands chars long,
		; so it lags emacs terribly, so better not use it anyway.
		"--binary-files=without-match"
		; After some troubles with multibyte chars like "―" (byte-offset was wrong),
		; I switched to using insert-file-contents-literally, for byte-offset to be
		; correct. Turns out that only seems to affect byte-offset option - lines
		; still always end with \r\n (on Windows), so emacs should decode them to
		; "\n", so helm-source-async-separator can still just be "\n".
		; So helm-grep-replace uses insert-file-content-literally, and it is the
		; only function that uses byte-offsets, other functions, like jumping
		; to lines in buffers, use lines because buffers shown to users have
		; multibyte enabled, so byte-offset is incorrect in them.
		"--binary"
		"--directories=recurse"
	))

; I don't use zgrep, pdfgrep and git-grep so there may be errors.

(defconst helm-grep-git-exe (executable-find "git"))

(defconst helm-grep-git-options
	'(
		"--no-pager"
		"grep"
		"--color=never"
		"--byte-offset"
		"--line-number"
		"--with-filename"
		"--full-name"
		"-e"
	)
"The \"--exclude-standard\" and \"--no-index\" options allow
skipping unwanted files specified in ~/.gitignore_global and
searching files not already staged (not enabled by default).

You have also to enable this in global \".gitconfig\" with
\"git config --global core.excludesfile ~/.gitignore_global\".")

(defconst helm-zgrep-exe (executable-find "zgrep"))

(defconst helm-zgrep-options helm-grep-options)

(defconst helm-zgrep-file-extension-regexp "\\.\\(gz\\|bz\\|xz\\|lzma\\)$"
"Default file extensions zgrep will search in.")


(defconst helm-pdfgrep-exe (executable-find "pdfgrep"))

(defconst helm-pdfgrep-options nil)

(defconst helm-pdfgrep-read-fn nil
"Function with 2 args - file and page number.

Should show pdf file.

If nil, either `doc-view-mode' or `pdf-view-mode' will be
used instead of an external command.")

(define_face 'helm-grep-line-number '((t :foreground "Darkorange1"))
"Face used to highlight grep number lines.")


(cl-defstruct (helm-grep-cand (:copier nil) (:constructor helm-grep-cand-make))
"Every field is a string - results of splitting grep result line with
`helm-grep-split-line-regex'."
	file line-number byte-offset line)


(defun helm-grep-split-line (line) "Split a grep output line."
	(string-match helm-grep-split-line-regex line)
	(cl-loop for n from 1 to 4 collect (match-string n line)))

(defun helm-grep-split-display-line (line) "Split a display line."
	(string-match helm-grep-split-display-line-regex line)
	(list (match-string 1 line) (match-string 2 line)))

; Actions

(defun helm-grep-default-action (_cand)
"Default action for `helm-grep'.

Like in `helm-ff-action':
If there are multiple files selected -
	Open them with find-file-noselect, with 2 prefix args don't show them,
	with any other prefix arg value pass them to `helm-window-show-buffers',
	where prefix arg may be used to alter window configuration.

	If there are multiple lines selected in a single file,
	sort them descendingly, goto the first one in a base buffer and make
	indirect buffers to the rest (with a \"reasonable\" limit of 20 indirect buffers).
	UPDATE: I checked and indirect buffers are broken with my emacs config in many ways,
	so only the first line-number is used now, the rest is ignored.

If there is one candidate -
	With prefix arg is non-nil, don't select it.
	Else find-file, helm-goto-line, helm-highlight-current-line;
	so just like persistent action."
	(let (
		(marked (helm-marked-candidates))
		(get_line_number
			(lambda (cand) (string-to-number (helm-grep-cand-line-number cand))))
	)
		(if (cdr marked)
			(let (alist)
				; Create alist (file . line_number_list).
				(dolist (cand marked)
					(let (
						(file (helm-grep-cand-file cand))
						(line_number (funcall get_line_number cand))
					)
						(if-let ((cell (assoc file alist)))
							(setcdr cell (cons line_number (cdr cell)))
							(push (list file line_number) alist))))
				(let (buffer_list)
					(dolist (cell alist)
						; Limit line_number_lists to be max 21 buffers - 1 base,
						; 20 indirect.
						; Sort them descendingly.
						(setcdr cell (sort (ntake 21 (cdr cell)) #'<))
						(let ((buffer (find-file-noselect (car cell))))
							(push buffer buffer_list)
							(unrecord-window-buffer nil buffer)
							; Old version using indirect buffers.
;							(let ((line_number_list (cdr cell)))
;								(with-current-buffer buffer
;									(helm-goto-line (car line_number_list))
;									; Create indirect buffers for the rest of buffers.
;									(dolist (line_number (cdr line_number_list))
;										(let (
;											(indirect_buffer
;												(clone-indirect-buffer nil nil))
;										)
;											(push indirect_buffer buffer_list)
;											(with-current-buffer indirect_buffer
;												(helm-goto-line line_number))))))))
							(with-current-buffer buffer
								(helm-goto-line (nth 1 cell)))))
					(unless (equal helm-current-prefix-arg '(16))
						; Order of buffers should be correct, because
						; alist was created in reverse and buffer_list too.
						(helm-window-show-buffers buffer_list)
						; On visible windows also goto first match
						; and highlight matches around.
						(dolist (buffer buffer_list)
							(when-let ((window (get-buffer-window buffer)))
								(with-selected-window window
									(helm-highlight-current-line t t)))))))
			(let* (
				(cand (car marked))
				(file (helm-grep-cand-file cand))
				(line_number (funcall get_line_number cand))
			)
				(if helm-current-prefix-arg
					(let ((buffer (find-file-noselect file)))
						(unrecord-window-buffer nil buffer)
						(with-current-buffer buffer (helm-goto-line line_number)))
					(find-file file)
					(helm-goto-line line_number)
					(helm-highlight-current-line t t))))))

(defun helm-grep-jump (cand &optional where)
	(let ((file (helm-grep-cand-file cand)))
		(cl-case where
			(other-window (helm-window-show-buffers (list (find-file-noselect file)) t))
			(other-frame (find-file-other-frame file))
			(t (find-file file))))
	; Move to line and mark the nearest matching regex from bol.
	(helm-goto-line (string-to-number (helm-grep-cand-line-number cand)))
	(helm-highlight-current-line t t))

(defun helm-grep-jump-other-window (cand) (helm-grep-jump cand 'other-window))
(defun helm-grep-jump-other-frame (cand) (helm-grep-jump cand 'other-frame))

(defvar helm-grep-replace-history nil
"History variable for strings replacing grep matches.")

(defun helm-grep-replace (_candidate)
"Action to replace marked results with text prompted from user.
`case-fold-search' matters.
With prefix arg let-bind `case-replace' to nil - replacing text will have fixed case,
else let-bind it to t.

This only takes visible marked candidates (those that are currently in helm-buffer),
because this is unclear what should be matched in other ones (those that was matched
with different helm-pattern). That wouldn't fit in helm anyway, because it's not
possible to mark the same candidate twice in helm (helm-pattern doesn't influence equality
of candidates)."
	(let* (
		(case-replace (not helm-current-prefix-arg))
		(candidates
			; Copied from helm-marked-candidates.
			(or
				(nreverse
					(cl-loop
						for ov in
							(buffer-local-value
								'helm-marked-candidates (get-buffer helm-buffer))
						when
							(and
								(overlay-buffer ov)
								(eq helm-current-source (overlay-get ov 'source)))
							collect
								(cons (overlay-get ov 'withprop) (overlay-get ov 'real))))
				(list
					(let ((cand (helm-get-selection 'withprop)))
						(cons cand (get-text-property 0 'helm-real cand))))))
		(line_list (mapcar (lambda (cand) (helm-grep-cand-line (cdr cand))) candidates))
		(match_data_2D_list (helm-match line_list nil t helm-current-source))
		(match_count 0)
		(skip_count 0)
		alist
		new_text
	)
		; Remove invalid marked candidates by discarding those that don't match
		; anything (don't have highlighted part) - used only negation
		; so there is nothing to match and replace.

		; First handle problematic first cell.
		(while (and match_data_2D_list (not (car match_data_2D_list)))
			(setq candidates (cdr candidates))
			(setq line_list (cdr line_list))
			(setq match_data_2D_list (cdr match_data_2D_list)))
		; Now first cell is a proper match.
		(let (
			(candidates_ candidates)
			(line_list_ line_list)
			(match_data_2D_list_ match_data_2D_list)
		)
			(while (cdr candidates_)
				(unless (nth 1 match_data_2D_list_)
					; Next match is invalid.
					(setcdr candidates_ (nthcdr 2 candidates_))
					(setcdr line_list_ (nthcdr 2 line_list_))
					(setcdr match_data_2D_list_ (nthcdr 2 match_data_2D_list_)))
				(setq candidates_ (cdr candidates_))
				(setq line_list_ (cdr line_list_))
				(setq match_data_2D_list_ (cdr match_data_2D_list_))))

		; Count matches (just to display to user, this isn't very useful).
		(dolist (match_data_list match_data_2D_list)
			(+= match_count (length match_data_list)))

		; Create alist (file . (byte_offset line . match_data_list)_list).
		(dolist (cand candidates)
			(setq cand (cdr cand))
			(let (
				(file (helm-grep-cand-file cand))
				(element
					(cons
						(string-to-number (helm-grep-cand-byte-offset cand))
						(cons
							(pop line_list)
							(pop match_data_2D_list))))
			)
				(if-let ((cell (assoc file alist)))
					(setcdr cell (cons element (cdr cell)))
					(push (list file element) alist))))

		; We now have all vaguely important info about matches to show to user,
		; so show it and ask for the replacing text.
		(setq new_text
			(helm-with-display-candidates
				(map_modify_list #'car candidates)
				(read-from-minibuffer
					(format
"Replace %d match%s in %d line%s in %d file%s of \"%s\" %s-case with (\\&, \\N and \\ are special):\n"
						match_count
						(if (= match_count 1) "" "es")
						(length candidates)
						(if (length= candidates 1) "" "s")
						(length alist)
						(if (length= alist 1) "" "s")
						helm-pattern
						(if case-replace "match" "fixed"))
					nil
					nil
					nil
					'helm-grep-replace-history
					(thing-at-point 'symbol t))))

		; [1] Error if new_text is in wrong format for `replace-match' -
		; backslash before anything else than \, &, N, or ?.
		(set-match-data (list 0 0))
		(replace-match new_text (not case-replace) nil "Queen Ahri is the best.")

		(let (
			(holistic_sort
				(lambda (cell)
					(setcdr cell
						(sort (cdr cell) (lambda (c1 c2) (> (car c1) (car c2)))))))
		)
			; Sort byte_offsets descendingly, to just use byte_offsets
			; as start of lines and not have to add any offset.
			(mapc holistic_sort alist)
			; Sort match_data_lists correspondingly.
			(dolist (cell alist)
				(dolist (element (cdr cell))
					; Make element be (line . match_data_list).
					(setq element (cdr element))
					; This is based on the assumption that there are no overlapping
					; matches (there can be like ((1 1) (1 3) (3 4)),
					; but no ((1 1) (0 2))).
					; This is the case now and I think it makes the most sense,
					; but it could theoretically change in the future.
					(funcall holistic_sort element))))

		; Replace in lines in alist and transform elements of alist from
		; (file . (byte_offset line . match_data_list)_list) to
		; (file . (byte_offset line . new_line)_list),
		(dolist (cell alist)
			(dolist (element (cdr cell))
				(setq element (cdr element))
				(setcdr element
					(let (
						(line (car element))
						(match_data_list (cdr element))
					)
						(while
							(progn
								(set-match-data (pop match_data_list))
								; This should never error - new_text was validated in [1].
								(setq line
									(replace-match new_text (not case-replace) nil line))
								match_data_list))
						line))))

		; Finally replace in files.
		(dolist (cell alist)
			(with-temp-buffer
				(erase-buffer)
				(let ((file (car cell)))
					(if (not (withDemotedErrors (insert-file-contents-literally file) t))
						(++ skip_count)
						; Transform cdr of cell from (byte_offset line . new_line)_list
						; to (byte_offset line_size . new_line)_list,
						; where line_size is a size of LINE in bytes.
						; If this file has \r\n as eols, change every \n to \r\n,
						; that means increment line_size for every \n in it,
						; also replace \n to \r\n in new_line.
						(if (eq (char-before (pos-eol)) ?\r)
							(dolist (element (cdr cell))
								(setq element (cdr element))
								(setcar element
									(+
										(string-bytes (car element))
										(cl-count ?\n (car element))))
								(setcdr element
									(string-replace "\n" "\r\n" (cdr element))))
							(dolist (element (cdr cell))
								(setq element (cdr element))
								(setcar element (string-bytes (car element)))))
						(dolist (element (cdr cell))
							; Goto (byte-offset + 1) - start of line.
							; +1 because byte-offset is 0-indexed and buffer
							; positions are 1-indexed.
							(goto-char (1+ (pop element)))
							(delete-region (point) (+ (point) (car element)))
							(insert (cdr element)))
						 (let ((coding-system-for-write 'no-conversion))
							(write-region nil nil file))))))

		; Message if errors occurred.
		; Details will be available in *Messages* because withDemotedErrors was used.
		(when (> skip_count 0)
			(message "Skipped %d file%s." skip_count (if (= skip_count 1) "" "s")))))

; Mode

(defun helm-grep-mode-goto-next-file (n) (interactive "p")
	(when (/= n 0)
		(let* (
			(get_file
				(lambda ()
					(when-let ((real (get-text-property (pos-bol) 'helm-real)))
						(helm-grep-cand-file real))))
			(file (funcall get_file))
			(end_fn (if (> n 0) #'eobp #'bobp))
		)
			(when file
				(while
					(and
						(not (funcall end_fn))
						(progn
							(forward-line n)
							(let ((this_file (funcall get_file)))
								; Until found different file.
								(or
									(not this_file)
									(string= file this_file))))))
				; Go back to something if we ended up at start/end of buffer.
				(cond
					((and (> n 0) (eobp))
						(re-search-backward ".")
						(goto-char (pos-bol)))
					((and
							(< n 0)
							(bobp)
							; Give some reasonable limit.
							(re-search-forward helm-grep-split-display-line-regex 1000 t))
						(goto-char (pos-bol))))))))

(fset 'helm-grep-mode-goto-previous-file
	(get_reverse_command #'helm-grep-mode-goto-next-file))

(define-derived-mode helm-grep-mode fundamental-mode "helm-grep-mode"
"Major mode to provide actions in helm grep saved buffer.")

(defvar helm-grep-mode-source nil)

(defun helm-grep-mode-jump-base (real arg)
	(let (
		(helm-pattern (car helm-grep-mode-resume-info))
		(helm-current-source helm-grep-mode-source)
		(helm-buffer "*helm-grep*")
	)
		(helm-grep-jump real arg)))

(defun helm-grep-mode-jump (&optional arg) (interactive)
	(when-let ((real (get-text-property (pos-bol) 'helm-real)))
		(helm-grep-mode-jump-base real arg)))

(defun helm-grep-mode-jump-other-window-forward (n) (interactive "p")
	(let ((start (point)))
		(when
			(memq last-command
				'(
					helm-grep-mode-jump-other-window-forward
					helm-grep-mode-jump-other-window-backward
				))
			(forward-line n))
		(if-let ((real (get-text-property (pos-bol) 'helm-real)))
			(save-selected-window (helm-grep-mode-jump-base real 'other-window))
			(goto-char start))))

(fset 'helm-grep-mode-jump-other-window-backward
	(get_reverse_command #'helm-grep-mode-jump-other-window-forward))

(defvar helm-grep-mode-resume-info nil)

(defun helm-grep-save-results (_cand)
	; Save grep result in a `helm-grep-mode' buffer.
	(let ((buffer "*helm-grep-result*"))
		(when (get-buffer buffer)
			(setq buffer
				(if helm-grep-save-buffer-name-no-confirm
					(concat
						"*helm-grep-|"
						helm-pattern
						"|-"
						(format-time-string "%H:%M:%S")
						"*")
					(let (helm-pattern)
						(helm-read-buffer
							:prompt "Helm grep buffer: "
							:input buffer
							:must-match 'confirm-existing)))))
		(with-current-buffer (get-buffer-create buffer)
			(helm-grep-mode)
			; Save some vars locally for resuming this helm session.
			(setq-local
				helm-grep-mode-resume-info
					(list
						helm-pattern
						helm-grep-targets
						(buffer-local-value 'helm-grep-time (get-buffer helm-buffer))
						helm-grep-backend)
				helm-grep-mode-source helm-grep-source)
			(setq default-directory (helm-default-directory))
			(setq buffer-read-only t)
			(let ((inhibit-read-only t))
				(erase-buffer)
				(insert
					"Results from helm-grep for pattern \""
					helm-pattern
					"\":\n")
				(save-excursion
					(insert (with-current-buffer "*helm-grep*" (buffer-string))))
				; Remove useless or problematic props.
				(remove-text-properties (point) (point-max)
					'(
						keymap nil
						helm-source nil
						helm-header nil
						helm-index nil
						helm-noncandidate nil
					))
				; Move to the first candidate line.
				(forward-line 1)))
		(pop-to-buffer buffer)))

; Source

(defun helm-source-grep-make (&optional source &rest args)
	(apply #'helm-source-async-make source
		(append
			; Put args first to allow them to override default settings.
			args
			(list
				:candidate-transformer #'helm-grep-candidate-transformer
				:sort nil ; Use line number based sorting.
				:action helm-grep-actions
				:persistent-action #'helm-grep-jump
				:keymap helm-grep-keymap
				; Save all file visiting buffers, to keep grep results in sync
				; with emacs state. I prefer it this way, but someone else might not.
				:resume
					(lambda ()
						(save-file-buffers)
						; If it was a relatively fast search, update.
						(and
							(not helm-suspend-update-flag)
							(floatp helm-grep-time)
							(< helm-grep-time 3.0)
							(helm-force-update)))
				:candidate-number-limit 9999
				:requires-pattern 1
				:match-part t
				:match-strict t))))

(defvar helm-grep-last-file-candidates nil)

(defvar helm-grep-file-list nil
"List of files of current matches, in order.")

(defun helm-grep-file-absolute-to-display (file)
	(cond
		((eq helm-grep-filename-type 'basename) (file-name-nondirectory file))
		(helm-grep-filename-type (file-relative-name file))
		(t file)))

(defun helm-grep-transform-to-display (candidates line_number_max_length)
"For every candidate:
	trim blanks from the start if `helm-grep-trim' is non-nil,
	turn tabs to spaces,
	add line number column to every candidate.
Return nil."
	(let (
		(max_length (max line_number_max_length helm-grep-line-number-min-width))
		(trim helm-grep-trim)
	)
		(with-temp-buffer
			(dolist (cand candidates)
				(insert (car cand))
				(goto-char 1)
				(when trim (skip-chars-forward " \t") (delete-region 1 (point)))
				(line_tabs_to_spaces)
				(setcar cand
					(let ((line_number (helm-grep-cand-line-number (cdr cand))))
						(concat
							(get_space_string (1+ (- max_length (length line_number))))
							(propertize_no_copy line_number 'face 'helm-grep-line-number)
							" "
							(buffer-string))))
				(erase-buffer)))))

(defun helm-grep-add-separator-line (candidates)
	(cons
		(let ((file (helm-grep-cand-file (cdr (car candidates)))))
			(propertize_no_copy (concat (helm-grep-file-absolute-to-display file) "\n")
				'face 'helm-source-header
				'help-echo (abbreviate-file-name file)
				'helm-noncandidate t))
		candidates))

; name will be set before every helm call.
(defconst helm-grep-source
	(helm-source-grep-make nil
		:candidates
			(lambda ()
				(setq-local helm-grep-time nil)
				(let (
					pattern_args
					match_args
					(maybe_fold_pattern
						(lambda (pattern)
							(cond
								((not helm-match-fold-diacritics) pattern)
								((eq helm-match-regex-type 'pcre)
									(rxt-elisp-to-pcre
										(char-fold-to-regexp
											(rxt-pcre-to-elisp pattern))))
								(t (char-fold-to-regexp pattern)))))
					(patterns_to_multi
						(lambda (patterns)
							(ignore-error invalid-regexp
								(if helm-match-mixed
									(funcall maybe_fold_pattern (car patterns))
									(mapconcat maybe_fold_pattern patterns ".*")))))
				)
					; Do this first to quickly recognize if helm-pattern is an invalid regex.
					; If it is and only regex matching is used, don't even call grep.
					(if (eq (helm-source-match helm-current-source) (cdr helm-match-substring-cell))
						; Substring.
						(setq
							match_args (list "--fixed-strings")
							pattern_args (list helm-pattern))
						(when (eq helm-match-regex-type 'pcre)
							(setq match_args (list "--perl-regexp")))
						(cond
							; Regex.
							((eq (helm-source-match helm-current-source) (cdr helm-match-regex-cell))
								(when-let (
									(pattern
										(ignore-error invalid-regexp
											(funcall maybe_fold_pattern helm-pattern)))
								)
									(setq pattern_args (list pattern))))
							; Multi single.
							((eq (helm-source-match helm-current-source) (cdr helm-match-multi-single-cell))
								(setq pattern_args
									(let ((patterns (helm-match-get-patterns helm-pattern)))
										(cond
											((cdr patterns)
												(setq patterns
													(funcall patterns_to_multi (cdr patterns)))
												(when patterns (list patterns)))
											((car patterns) (list ""))))))
							(t ; Multi match.
								(let (
									(grep_regex
										(ignore-error invalid-regexp
											(funcall maybe_fold_pattern helm-pattern)))
									; AFAIK there is no way to use substring
									; and regex matching in the same call to
									; grep, so quote helm-pattern.
									(fixed_str_pattern
										; This doesn't signal.
										(if (eq helm-match-regex-type 'pcre)
											(rxt-quote-pcre helm-pattern)
											(regexp-quote helm-pattern)))
									(multi_pattern
										; If helm-pattern contains parts to match.
										(when-let ((patterns (cdr (helm-match-get-patterns helm-pattern))))
											(funcall patterns_to_multi patterns)))
								)
									(setq pattern_args
										(list
											(let (
												(or_regex_operator
													(if (eq helm-match-regex-type 'pcre) "|" "\\|"))
											)
												(concat
													fixed_str_pattern
													(when grep_regex (concat or_regex_operator grep_regex))
													(when multi_pattern (concat or_regex_operator multi_pattern))))))))))
					(when pattern_args
						(setq helm-grep-time (current-time))
						(setq helm-grep-last-file-candidates nil)
						(setq helm-grep-file-list nil)
						(let ((tramp-verbose helm-tramp-verbose) process-connection-type)
							; I don't add :stderr here because for some reason delete-process
							; makes grep send on its stderr error like this:
							; grep.exe:write error: Invalid argument
							; Idk, maybe it's not delete-process but something else.
							(make-process
								:name "helm-grep"
								:command
									(append
										(cl-case helm-grep-backend
											(zgrep (cons helm-zgrep-exe helm-zgrep-options))
											(git (cons helm-grep-git-exe helm-grep-git-options))
											(t (cons helm-grep-exe helm-grep-options)))
										(list
											(concat
												"--max-count="
												(number-to-string
													(helm-candidate-number-limit helm-current-source))))
										(when (helm-set-case-fold-search) (list "--ignore-case"))
										match_args
										(list "--")
										pattern_args
										helm-grep-targets)
								; There was:
								; (when (<= (process-exit-status process) 0)
								; (helm-process-deferred-sentinel-hook
								; process event (helm-default-directory)))
								; but I don't use tramp now and I think it's only for tramp.
								:sentinel
									(lambda (process event)
										(when-let (
											((or
												(string= event "finished\n")
												(= (process-exit-status process) 1)))
											(process_info (helm-get-process-info process))
										)
											(with-current-buffer helm-buffer
												(setq helm-grep-time (float-time (time-subtract nil helm-grep-time))))
											(when helm-grep-last-file-candidates
												; Handle results from last file.
												; Copied from helm-async-process-filter and
												; this source's candidate-transformer.
												(with-helm-buffer
													(let (
														(helm-pattern (helm-async-process-pattern process_info))
														(helm-match-fold-diacritics
															(helm-source-diacritics helm-grep-source))
														(case-fold-search (helm-async-process-case-fold process_info))
														(helm-current-source helm-grep-source)
														(line_number_max_length
															(length
																(helm-grep-cand-line-number
																	(cdr (car helm-grep-last-file-candidates)))))
														(candidates helm-grep-last-file-candidates)
													)
														(setq candidates (nreverse candidates))
														(setq candidates (helm-async-candidate-transformer candidates))
														(when candidates
															(helm-grep-transform-to-display
																candidates line_number_max_length)
															; Add file to the helm-grep-file-list.
															(setq helm-grep-file-list
																(nconc
																	helm-grep-file-list
																	(list
																		(helm-grep-cand-file
																			(cdr (car candidates))))))
															(setq candidates
																(helm-grep-add-separator-line candidates))
															; Insert candidates into helm-buffer.
															(save-excursion
																(goto-char (point-max))
																(helm-insert-items
																	candidates
																	helm-grep-source
																	(helm-async-process-item-count process_info))))))
												(helm-async-after-update))
											(helm-async-set-finished helm-grep-source))
										; Reset just to not hold useless reference.
										; The important reset is in :candidates - that
										; ensures helm-grep-last-file-candidates won't
										; be mistakenly reused in :candidate-transformer.
										(setq helm-grep-last-file-candidates nil)))))))
		:candidate-transformer
			(lambda (candidates)
				(when candidates
					; Transform candidates to (line . real) and group them by file.
					(let (
						(file
							(if helm-grep-last-file-candidates
								(helm-grep-cand-file
									(cdr (car helm-grep-last-file-candidates)))
								(let ((cand (car candidates)))
									(string-match helm-grep-split-line-regex cand)
									(match-string-no-properties 1 cand))))
						(group helm-grep-last-file-candidates)
						group_list ; List of lists with candidates.
					)
						(while candidates
							(let ((cand (pop candidates)))
								(cl-destructuring-bind
									(this_file line_number byte_offset line)
									(helm-grep-split-line cand)
									(unless (string= file this_file)
										; We are now at the cell with different file than
										; file.
										(push group group_list)
										(setq group nil)
										(setq file this_file))
									(push
										(cons
											line
											(helm-grep-cand-make
												:file this_file
												:line-number line_number
												:byte-offset byte_offset
												:line line))
										group))))
						; Save last file for next output or for sentinel.
						(setq helm-grep-last-file-candidates group)
						; If we have results from at least 2 files
						; (so group_list has at least 1 element).
						(when group_list
							(let (
								; Candidates are in reverse, so the first cand is the last
								; line. Use this to get max length of line number prefix.
								(line_number_max_length_list
									(mapcar
										(lambda (group)
											(length
												(helm-grep-cand-line-number
													(cdr (car group)))))
										group_list))
								(group_list_1 group_list)
							)
								; Reverse candidates in groups before matching,
								; to have proper grouping by match method.
								(map_modify_list #'nreverse group_list)
								; Perform matching on every file.
								(map_modify_list
									#'helm-async-candidate-transformer group_list)
								; Add line-number column to display part of candidates.
								(while group_list_1
									(when (car group_list_1)
										(helm-grep-transform-to-display
											(car group_list_1)
											(car line_number_max_length_list)))
									(setq line_number_max_length_list
										(cdr line_number_max_length_list))
									(setq group_list_1 (cdr group_list_1))))
							; Remove empty groups.
							(when (setq group_list (delq nil group_list))
								; Reverse files to the correct order.
								(setq group_list (nreverse group_list))
								; Add files to the helm-grep-file-list.
								(setq helm-grep-file-list
									(nconc
										helm-grep-file-list
										(mapcar
											(lambda (group)
												(helm-grep-cand-file (cdr (car group))))
											group_list)))
								; Add separator line.
								(map_modify_list
									#'helm-grep-add-separator-line
									group_list)
								; Merge everything into one list.
								(apply #'nconc group_list))))))
		:mode-line
			(lambda ()
				(concat
					; Display current file and it's index.
					(when-let ((real (get-text-property (point) 'helm-real)))
						(let ((file (helm-grep-cand-file real)))
							(concat
								(number-to-string
									(1+ (index_in_list_string= helm-grep-file-list file)))
								"/"
								(number-to-string (length helm-grep-file-list))
								" "
								(helm-grep-file-absolute-to-display file)
								"  ")))
					(when helm-grep-trim "trim  ")
					(helm-async-mode-line)
					(when (floatp helm-grep-time)
						(format "%.2f  " helm-grep-time))))
		:match-part nil
		:noncandidate 'header))

(defun helm-grep (&optional targets backend &rest args)
"helm for grepping `default-directory'.

Launch helm using backend BACKEND on a list of TARGETS files.

BACKEND can be nil to use grep, \\='zgrep or \\='git.

`default-directory' is very important here and must be non-nil
(and exist but that isn't checked here).

TARGETS list can be modified.
If it is nil, search default directory, like grep does by default (with -r)."
	(interactive)
	(if (not default-directory)
		(message "No default directory.")
		(setf (helm-source-name helm-grep-source)
			(cl-case backend
				(zgrep "Zgrep")
				(git "Git-grep")
				(t "Grep")))
		(setq helm-grep-backend backend)
		(setq helm-grep-targets targets)
		(save-file-buffers)
		(apply #'helm
			:sources (list helm-grep-source)
			:default (thing-at-point 'symbol t)
			:history 'helm-grep-history
			:resume "*helm-grep*"
			:helm-default-keymap helm-grep-default-keymap
			:helm-delay helm-grep-input-idle-delay
			:helm-truncate-lines helm-grep-truncate-lines
			args)))

; This is wrong now, nothing should use this (so helm-ff-pdf-grep or something is wrong).
(defun helm-grep-one-by-one (candidate)
"Return (display . CANDIDATE) cons where DISPLAY has format filename:line_number:line
CANDIDATE is a grep result line with no colouring, in format
absolute_filename:line_number:byte_offset:line."
	(let* (
		(split (helm-grep-split-line candidate))
		(absolute_filename (pop split))
		(line_number (pop split))
		(byte_offset (car split))
		(line (nth 1 split))
		(filename
			(cond
				((eq helm-grep-filename-type 'basename)
					(file-name-nondirectory absolute_filename))
				(helm-grep-filename-type (file-relative-name absolute_filename))
				(t absolute_filename)))
	)
		(cons
			(propertize_no_copy
				(concat
					(propertize_no_copy filename
						'help-echo (abbreviate-file-name absolute_filename))
					":"
					(propertize_no_copy line_number 'face 'helm-grep-line-number)
					":"
					line)
				'match-part
				(list
					(let ((start (+ (length filename) (length line_number) 2)))
						(cons start (+ start (length line))))))
			(helm-grep-cand-make
				:file absolute_filename
				:line-number line_number
				:byte-offset byte_offset
				:line line))))

(defun helm-grep-candidate-transformer (candidates)
	(helm-async-candidate-transformer
		(map_modify_list #'helm-grep-one-by-one candidates)))

; RG - github.com/BurntSushi/ripgrep

(defconst helm-grep-rg-exe (executable-find "rg"))

(defconst helm-grep-rg-options
	'(
		"--color=never"
		"--no-messages"
		"--line-number"
		"--byte-offset"
		"--heading"
		"--crlf" ; Treat \r\n as eol (not sure why this isn't the default).
		; Check if pattern uses lookarounds or backreferences and decide to use
		; pcre2 or default engine.
		; Helm will translate user input with rxt-pcre-to-elisp or rxt-elisp-to-pcre
		; as usual.
		"--engine=auto"
		; Use this for easier searching for end of filepath - simple search for ?\1
		; instead of regex "\\(\\(?:[[:alpha:]]:\\)?.*\\):".
		; \1 because \0 doesn't work for some reason.
		"--field-match-separator=\1"
		"--path-separator=/"
		"--block-buffered"
		; This one is unfortunately necessary, but only with --multiline, because
		; there is no easy way to know how many lines constitute a match,
		; e.g. "foo\n.*\n|bar" can result in matches with 2 matching lines
		; or 1 matching line, so we either would need to check colouring
		; or get this info through --json option.
		"--json"
	))

; Example of rg usage with --json option:
; pattern = "sd\nq"
; file = "C:/test", has \n as newlines
; file contents as a string = "asd\nqwe\n\nzxc\n"
; command line = rg "--json" "--multiline" "sd\nq" "C:/test" "C:/file_without_matches"
; output =
; {"type":"begin","data":{"path":{"text":"C:/test"}}}
; {"type":"match","data":{"path":{"text":"C:/test"},"lines":{"text":"asd\nqwe\n"},"line_number":1,"absolute_offset":0,"submatches":[{"match":{"text":"sd\nq"},"start":1,"end":5}]}}
; {"type":"end","data":{"path":{"text":"C:/test"},"binary_offset":null,"stats":{"elapsed":{"secs":0,"nanos":57100,"human":"0.000057s"},"searches":1,"searches_with_match":1,"bytes_searched":12,"bytes_printed":320,"matched_lines":2,"matches":1}}}
; {"data":{"elapsed_total":{"human":"0.017855s","nanos":17855400,"secs":0},"stats":{"bytes_printed":320,"bytes_searched":12,"elapsed":{"human":"0.000057s","nanos":57100,"secs":0},"matched_lines":2,"matches":1,"searches":1,"searches_with_match":1}},"type":"summary"}
;
; "end" line is used only as info that there will be no more matches for file;
; "data" line is unused.
;
; [1] Also, this --multiline matching is pretty weird - "foo\n" will output 1 line,
; "foo\n\n" - 2 lines, "foo\n$" - 1 line.
; So for matching to work correctly, if rg match ends with \n, a temporary newline will
; be added.
;
; [2] I found wrong behaviour - with "^" or similar patterns, with --multiline on,
; rg sends entire file in one "match" line for no reason.
; With "" it splits every match into one line like it should,
; so it doesn't make any sense and idk how to deal with this.
; So multi mix matching method will be broken sometimes, because it can contain
; "^" and similar patterns, e.g. pattern "^\nasd" will be split into "^" and "asd",
; so if multi matching uses permutation then only the first pattern will be used
; in rg call, so it will cause this stupid behaviour.
; I added some code to try to deal with such cases by changing "^" and "$" to "",
; but some more complicated stuff can still go through.
;
; [3] Another stupid shit with --multiline - if there are multiple matching lines
; in a row, this crap program merges them into 1 json output for no reason, e.g.
; pattern = "as", file = "helm-tags.el":
; - no --multiline:
; {"type":"match","data":{"path":{"text":"d:/ProgramFiles/Emacs/config/libs/helm/helm-tags.el"},"lines":{"text":"\"Store the last modification time of etags files here.\")\n"},"line_number":41,"absolute_offset":1356,"submatches":[{"match":{"text":"as"},"start":12,"end":14}]}}
; {"type":"match","data":{"path":{"text":"d:/ProgramFiles/Emacs/config/libs/helm/helm-tags.el"},"lines":{"text":"(defvar helm-etags-cache (make-hash-table :test 'equal)\n"},"line_number":42,"absolute_offset":1413,"submatches":[{"match":{"text":"as"},"start":32,"end":34}]}}
; {"type":"match","data":{"path":{"text":"d:/ProgramFiles/Emacs/config/libs/helm/helm-tags.el"},"lines":{"text":"\"Cache content of etags files used here for faster access.\")\n"},"line_number":43,"absolute_offset":1469,"submatches":[{"match":{"text":"as"},"start":45,"end":47}]}}
; - --multiline:
; {"type":"match","data":{"path":{"text":"d:/ProgramFiles/Emacs/config/libs/helm/helm-tags.el"},"lines":{"text":"\"Store the last modification time of etags files here.\")\n(defvar helm-etags-cache (make-hash-table :test 'equal)\n\"Cache content of etags files used here for faster access.\")\n"},"line_number":41,"absolute_offset":1356,"submatches":[{"match":{"text":"as"},"start":12,"end":14},{"match":{"text":"as"},"start":89,"end":91},{"match":{"text":"as"},"start":158,"end":160}]}}
;
; So now if there is no newline in first rg "match", I'll split lines by \n.
; This is a quick fix that won't work in some situations - if first match ends with
; \n, then other special behaviour is triggerred (the [1] one) and that doesn't
; run the fix I added, so that could be changed - add loop going through every match
; and every line, if there are no line-crossing matches, split line,
; if the last match in a line ends with \n, leave it a newline in the first line.
; Something like that, but it's not very important, maybe there are some more weird
; behaviours that will make that will break that fix, and at this point I'd rather
; change C code of rg than do this fixing here.

; Create some constants and functions for quick extraction of relevant data.

(defconst helm-grep-rg--type-index (length "{\"type\":\""))

(defconst helm-grep-rg--file-index
	(length "{\"type\":\"begin\",\"data\":{\"path\":{\"text\":\""))

(defconst helm-grep-rg--line-index
	(+ helm-grep-rg--file-index (length "\"},\"lines\":{\"text\":\"")))

(defconst helm-grep-rg--line-number-index
	(+ helm-grep-rg--line-index (length "\"},\"line_number\":")))

(defconst helm-grep-rg--byte-offset-index
	(+ helm-grep-rg--line-number-index (length ",\"absolute_offset\":")))

(defconst helm-grep-rg--match-index
	(+
		helm-grep-rg--byte-offset-index
		(length ",\"submatches\":[{\"match\":{\"text\":\"")))

(defun helm-grep-rg--get-file (output_line)
	(substring-no-properties
		output_line
		helm-grep-rg--file-index
		(string-search "\"" output_line helm-grep-rg--file-index)))

(defun helm-grep-rg--get-line (output_line file)
	(let ((start (+ helm-grep-rg--line-index (length file))))
		(substring-no-properties
			output_line
			start
			(string_search_not_backslashed "\"" output_line (+ start 2)))))

(defun helm-grep-rg--get-line-number (output_line file line)
	(let ((start (+ helm-grep-rg--line-number-index (length file) (length line))))
		(substring-no-properties
			output_line
			start
			(string-search "," output_line (1+ start)))))

(defun helm-grep-rg--get-byte-offset (output_line file line line_number)
	(let (
		(start
			(+
				helm-grep-rg--byte-offset-index
				(length file)
				(length line)
				(length line_number)))
	)
		(substring-no-properties
			output_line
			start
			(string-search "," output_line (1+ start)))))

(defun helm-grep-rg--get-match (output_line file line line_number byte_offset)
	(let (
		(start
			(+
				helm-grep-rg--match-index
				(length file)
				(length line)
				(length line_number)
				(length byte_offset)))
	)
		(substring-no-properties
			output_line
			start
			(string_search_not_backslashed "\"" output_line start))))

(defconst helm-grep-rg-default-types
	(split-string
		; Remove spaces, idk why are they here, can't they be part of globs?
		(string-replace ": " ":"
			(string-replace ", " ","
				(with-temp-buffer
					(call-process helm-grep-rg-exe nil t nil "--type-list")
					(buffer-substring-no-properties (point-min) (1- (point-max))))))
		"\n")
"List of default rg types - e.g. \"yaml:*.yaml,*.yml\"")

; helm-grep-rg-types should be set to helm-grep-rg-default-types once, after
; that every user modification should be recorded here across emacs sessions.
(defvar helm-grep-rg-types nil)
(desktop::add_to_globals 'helm-grep-rg-types)
(defvar helm-grep-rg-included-types nil)
(defvar helm-grep-rg-excluded-types nil)
(defvar helm-grep-rg-types-options nil
"List with strings - --type-clear= and --type-add= rg options.")
(defvar helm-grep-rg-sort nil)
(defvar helm-grep-rg-sort-reverse nil)
(defvar helm-grep-rg-max-depth nil)
(defvar helm-grep-rg-use-multiline nil)

; Mode

(defun helm-grep-rg-save-results (_cand)
	; Save grep result in a `helm-grep-rg-mode' buffer.
	(let ((buffer "*helm-grep-rg-result*"))
		(when (get-buffer buffer)
			(setq buffer
				(if helm-grep-save-buffer-name-no-confirm
					(concat
						"*helm-grep-rg-|"
						helm-pattern
						"|-"
						(format-time-string "%H:%M:%S")
						"*")
					(let (helm-pattern)
						(helm-read-buffer
							:prompt "Helm grep rg buffer: "
							:input buffer
							:must-match 'confirm-existing)))))
		(with-current-buffer (get-buffer-create buffer)
			(helm-grep-rg-mode)
			; Save some vars locally for resuming this helm session.
			(setq-local
				helm-grep-mode-resume-info
					(list
						helm-pattern
						helm-grep-targets
						(buffer-local-value 'helm-grep-time (get-buffer helm-buffer))
						helm-grep-rg-included-types
						helm-grep-rg-excluded-types
						helm-grep-rg-sort
						helm-grep-rg-sort-reverse
						helm-grep-rg-max-depth)
				helm-grep-mode-source helm-grep-rg-source)
			(setq default-directory (helm-default-directory))
			(setq buffer-read-only t)
			(let ((inhibit-read-only t))
				(erase-buffer)
				(insert
					"Results from helm-grep-rg for pattern \""
					helm-pattern
					"\":\n")
				(save-excursion
					(insert (with-current-buffer "*helm-grep*" (buffer-string))))
				; Remove useless or problematic props.
				(remove-text-properties (point) (point-max)
					'(
						keymap nil
						helm-source nil
						helm-header nil
						helm-index nil
						helm-noncandidate nil
					))
				; Move to the first candidate line.
				(forward-line 1)))
		(pop-to-buffer buffer)))

(defconst helm-grep-rg-mode-map
	(let ((keymap (make-sparse-keymap)))
		(set-keymap-parent keymap helm-grep-mode-map)
		(define-key keymap [?\A-f] #'helm-grep-rg-browse-types)
		; Resume helm session.
		(define-key keymap [?\C-g]
			(lambda () (interactive)
				(setq
					helm-grep-rg-included-types (nth 3 helm-grep-mode-resume-info)
					helm-grep-rg-excluded-types (nth 4 helm-grep-mode-resume-info)
					helm-grep-rg-sort (nth 5 helm-grep-mode-resume-info)
					helm-grep-rg-sort-reverse (nth 6 helm-grep-mode-resume-info)
					helm-grep-rg-max-depth (nth 7 helm-grep-mode-resume-info))
				; default-directory is the one from helm session saved
				; in helm-grep-mode buffer.
				(helm-grep-rg
					(nth 1 helm-grep-mode-resume-info)
					:input (car helm-grep-mode-resume-info)
					:helm-grep-time (nth 2 helm-grep-mode-resume-info))))
		keymap))

(define-derived-mode helm-grep-rg-mode fundamental-mode "helm-grep-rg-mode"
"The only mode you will ever need.")

; Types

(defun helm-grep-rg-type-to-name (type)
	(substring-no-properties type 0 (string-search ":" type 1)))
(defun helm-grep-rg-type-to-definition (type)
	(substring-no-properties type (1+ (string-search ":" type 1))))
(defun helm-grep-rg-type-to-name-and-definition (type)
	(let ((name (helm-grep-rg-type-to-name type)))
		(cons name (substring-no-properties type (1+ (length name))))))

(defun helm-grep-rg-type-find-by-name (name types)
	(find_in_list types (lambda (type) (string= name (helm-grep-rg-type-to-name type)))))

; name and candidates will be set before every helm call.
(defconst helm-grep-rg-types-source
	(helm-source-sync-make
		nil
		:volatile t
		:action (lambda (_cand) (helm-marked-candidates))
		:candidate-number-limit 9999))

(defun helm-grep-rg-select-types-base (name types_symbol opposite_types_symbol)
	(setf (helm-source-name helm-grep-rg-types-source) name)
	(let (
		(types (symbol-value types_symbol))
		(opposite_types (symbol-value opposite_types_symbol))
	)
		(setf (helm-source-candidates helm-grep-rg-types-source)
			`(lambda ()
				',(mapcar
					(lambda (type)
						(let (
							(name_and_def (helm-grep-rg-type-to-name-and-definition type))
						)
							(concat
								(propertize (car name_and_def)
									'face
									(if
										(helm-grep-rg-type-find-by-name
											(car name_and_def) opposite_types)
										'(:strike-through t :weight 'bold)
										'bold))
								(propertize ":" 'face 'font-lock-punctuation-face)
								(string-replace
									","
									(propertize "," 'face 'font-lock-punctuation-face)
									(cdr name_and_def)))))
					helm-grep-rg-types)))
		; Mark candidates after first update.
		(helm::call_after_next_update
			`(lambda ()
				(helm-map-candidates-in-source
					(lambda ()
						(when
							(member
								(buffer-substring-no-properties (point) (pos-eol))
								',types)
							(helm-make-visible-mark))))))
		(setq types
			(helm
				:sources (list helm-grep-rg-types-source)
				:allow-nest t
				:resume 'noresume))
		; Remove the same types from OPPOSITE_TYPES.
		(dolist (type types) (setq opposite_types (delete type opposite_types)))
		(set types_symbol types)
		(set opposite_types_symbol opposite_types))
	(helm-force-update))

(defun helm-grep-rg-include-types () (interactive)
	(helm-grep-rg-select-types-base
		"Include types" 'helm-grep-rg-included-types 'helm-grep-rg-excluded-types))

(defun helm-grep-rg-exclude-types () (interactive)
	(helm-grep-rg-select-types-base
		"Exclude types" 'helm-grep-rg-excluded-types 'helm-grep-rg-included-types))

(defun helm-grep-rg-types-options-set ()
"Set `helm-grep-rg-types-options' based on `helm-grep-rg-types'."
	(setq helm-grep-rg-types-options
		(mapcan
			(lambda (type)
				(let (
					(name_and_def (helm-grep-rg-type-to-name-and-definition type))
					(add_type
						(lambda () ; Dynamic binding: name_and_def.
							(map_modify_list
								(lambda (glob)
									(concat "--type-add=" (car name_and_def) ":" glob))
								(split-string (cdr name_and_def) ","))))
				)
					(if-let (
						(default
							(helm-grep-rg-type-find-by-name
								(car name_and_def) helm-grep-rg-default-types))
					)
						; Simple default type - don't add any option.
						(unless
							(string=
								(cdr name_and_def)
								(helm-grep-rg-type-to-definition default))
							; Modified default type - update its definition.
							(cons
								(concat "--type-clear=" (car name_and_def))
								(funcall add_type)))
						; New type - add it and its definition.
						(funcall add_type))))
			helm-grep-rg-types)))

(defconst helm-grep-rg-browse-types-source
	(helm-source-sync-make nil
		:name "Browse types"
		:candidates
			(lambda ()
				(mapcar
					(lambda (type)
						(let (
							(name_and_def (helm-grep-rg-type-to-name-and-definition type))
						)
							(concat
								(propertize (car name_and_def) 'face 'bold)
								(propertize ":" 'face 'font-lock-punctuation-face)
								(string-replace
									","
									(propertize "," 'face 'font-lock-punctuation-face)
									(cdr name_and_def)))))
					helm-grep-rg-types))
		:candidate-transformer #'helm-maybe-add-new-candidate
		:keymap
			(let ((keymap (make-sparse-keymap)))
				(set-keymap-parent keymap helm-map)
				(helm-add-delete-binds-force
					keymap
					(lambda (cand)
						(setq helm-grep-rg-types (delete cand helm-grep-rg-types))
						(setq helm-grep-rg-included-types
							(delete cand helm-grep-rg-included-types))
						(setq helm-grep-rg-excluded-types
							(delete cand helm-grep-rg-excluded-types))))
				keymap)
		:action ; Add/update candidate.
			(lambda (cand)
				; If this is a new candidate.
				(when
					(with-current-buffer helm-buffer
						(get-text-property (point) 'helm-new))
					(setq helm-grep-rg-types
						(cl-delete-if
							`(lambda (type)
								(string=
									,(helm-grep-rg-type-to-name cand)
									(helm-grep-rg-type-to-name type)))
							helm-grep-rg-types))
					(push cand helm-grep-rg-types)))
		:candidate-number-limit 9999))

(defun helm-grep-rg-browse-types () (interactive)
	(helm
		:sources (list helm-grep-rg-browse-types-source)
		:allow-nest t
		:helm-truncate-lines nil)
	; If this was a nested call, update rg options and call rg.
	(when (and helm-alive-p (string= helm-buffer "*helm-grep*"))
		(helm-grep-rg-types-options-set)
		(helm-force-update)))

; Commands for changing rg options while in helm.

(defun helm-grep-rg-max-depth-set (arg) (interactive "P")
	(setq helm-grep-rg-max-depth
		(let (
			(number
				(if arg
					(max 0 (prefix-numeric-value arg))
					(string-to-number
						(let (
							(default
								(when helm-grep-rg-max-depth
									(number-to-string helm-grep-rg-max-depth)))
							str
						)
							(while
								(progn
									(setq str
										(string-trim
											(read-from-minibuffer
												"Max depth: " nil nil nil nil default)))
									(not (string-match-p "\\`[0-9]*\\'" str)))
								(message "Enter a positive number or nothing.")
								(sit-for 1))
							str))))
		)
			(and (/= number 0) number)))
	(helm-force-update))

(defun helm-grep-rg-sort-reverse-toggle () (interactive)
	(setq helm-grep-rg-sort-reverse (not helm-grep-rg-sort-reverse))
	(unless helm-grep-rg-sort
		; If nothing in mode-line will be shown, send message.
		(message "Sort reverse %s." (if helm-grep-rg-sort-reverse "on" "off")))
	(helm-force-update))

(defun helm-grep-rg-sort-none () (interactive)
	(setq helm-grep-rg-sort nil)
	(helm-force-update))
(defun helm-grep-rg-sort-path () (interactive)
	(setq helm-grep-rg-sort "path")
	(helm-force-update))
(defun helm-grep-rg-sort-modified () (interactive)
	(setq helm-grep-rg-sort "modified")
	(helm-force-update))
(defun helm-grep-rg-sort-accessed () (interactive)
	(setq helm-grep-rg-sort "accessed")
	(helm-force-update))
(defun helm-grep-rg-sort-created () (interactive)
	(setq helm-grep-rg-sort "created")
	(helm-force-update))

(defconst helm-grep-rg-default-keymap
	(let ((keymap (make-sparse-keymap)))
		(set-keymap-parent keymap helm-grep-default-keymap)
		(define-key keymap [?\C-f] #'helm-grep-rg-include-types)
		(define-key keymap [?\C-\S-f] #'helm-grep-rg-exclude-types)
		(define-key keymap [?\A-f] #'helm-grep-rg-browse-types)
		(define-key keymap [?\C-.] #'helm-grep-rg-max-depth-set)
		(define-key keymap [?\C-s ?r] #'helm-grep-rg-sort-reverse-toggle)
		(define-key keymap [?\C-s ?n] #'helm-grep-rg-sort-none)
		(define-key keymap [?\C-s ?p] #'helm-grep-rg-sort-path)
		(define-key keymap [?\C-s ?m] #'helm-grep-rg-sort-modified)
		(define-key keymap [?\C-s ?a] #'helm-grep-rg-sort-accessed)
		(define-key keymap [?\C-s ?c] #'helm-grep-rg-sort-created)
		keymap))

(cl-defstruct
	(helm-grep-rg-cand
		(:copier nil) (:constructor helm-grep-rg-cand-make) (:include helm-grep-cand))
	; Add one internal flag.
	temp-newline)

(defconst helm-grep-rg-source
	(helm-source-grep-make nil
		:name "RG"
		:candidates
			(lambda ()
				(setq-local helm-grep-time nil)
				(setq helm-grep-rg-use-multiline
					(not (not (string-search "\n" helm-pattern))))
				(let* (
					match_args
					(translate_newlines
						(lambda (pattern)
							(string-replace "\n" "(?:\r?\n)" pattern)))
					(maybe_translate_newlines
						(lambda (pattern)
							(if helm-grep-rg-use-multiline
								(funcall translate_newlines pattern)
								pattern)))
					(pattern_args
						(cond
							; Substring.
							((eq (helm-source-match helm-current-source) (cdr helm-match-substring-cell))
								; Another pretty weird behaviour - even with --crlf flag,
								; rg still matches \n to literally \n, not \r?\n.
								; I would practically never use actual \n matching, also
								; when inserting file into buffer, emacs by default
								; does \r\n to \n conversion, so matching in buffer will
								; almost never allow for literal \n matching and I think
								; in 99% of cases this is desired, so to have the same
								; behaviour in rg, translate every \n to (?:\r?\n) in
								; helm-pattern.
								(list
									(if helm-grep-rg-use-multiline
										(funcall translate_newlines (rxt-quote-pcre helm-pattern))
										(setq match_args (list "--fixed-strings"))
										helm-pattern)))
							; Multi single.
							((eq (helm-source-match helm-current-source) (cdr helm-match-multi-single-cell))
								(let ((patterns (helm-match-get-patterns helm-pattern)))
									(cond
										((cdr patterns)
											(setq patterns
												(helm-match-patterns-to-multi (cdr patterns)))
											(when patterns
												(list
													; Try to prevent behaviour described in [2].
													(if (member patterns '("^" "$"))
														""
														(funcall maybe_translate_newlines patterns)))))
										((car patterns)
											; When we have only negations, don't support
											; multiline, as there is no way to know what
											; limits candidates should have (1 line, 10,
											; or maybe entire file) so give them the
											; normal limit - 1 line.
											(setq helm-grep-rg-use-multiline nil)
											(list "")))))
							(t
								(let (
									(pcre_regex
										(ignore-error invalid-regexp
											(helm-match-pattern-to-pcre helm-pattern)))
								)
									; Regex.
									(if (eq (helm-source-match helm-current-source) (cdr helm-match-regex-cell))
										(when pcre_regex
											(list (funcall maybe_translate_newlines pcre_regex)))
										; Multi mix.
										(list
											(concat
												(funcall maybe_translate_newlines
													(rxt-quote-pcre helm-pattern))
												(when pcre_regex
													(concat
														"|" (funcall maybe_translate_newlines pcre_regex)))
												(when-let (
													; If helm-pattern contains parts to match.
													(patterns
														(cdr (helm-match-get-patterns helm-pattern)))
													((setq patterns
														(helm-match-patterns-to-multi patterns)))
												)
													(concat
														"|"
														(funcall maybe_translate_newlines
															patterns))))))))))
					(tramp-verbose helm-tramp-verbose)
					process-connection-type
				)
					(when pattern_args
						(setq helm-grep-time (current-time))
						(setf (helm-source-multiline helm-current-source)
							helm-grep-rg-use-multiline)
						(setq helm-grep-last-file-candidates nil)
						(setq helm-grep-file-list nil)
						(make-process
							:name "helm-grep-rg"
							:command
								(append
									(cons helm-grep-rg-exe helm-grep-rg-options)
									(list
										(concat
											"--max-count="
											(number-to-string
												(helm-candidate-number-limit helm-current-source))))
									(when (helm-set-case-fold-search) (list "--ignore-case"))
									(when helm-grep-rg-use-multiline (list "--multiline"))
									(when helm-grep-rg-max-depth
										(list
											(concat
												"--max-depth="
												(number-to-string helm-grep-rg-max-depth))))
									(when helm-grep-rg-sort
										(list
											(concat
												"--sort"
												(when helm-grep-rg-sort-reverse "r")
												"="
												helm-grep-rg-sort)))
									; Add/delete/update types.
									helm-grep-rg-types-options
									; Include/exclude types.
									(mapcar
										(lambda (type)
											(concat
												"--type="
												(helm-grep-rg-type-to-name type)))
										helm-grep-rg-included-types)
									(mapcar
										(lambda (type)
											(concat
												"--type-not="
												(helm-grep-rg-type-to-name type)))
										helm-grep-rg-excluded-types)
									match_args
									(list "--")
									pattern_args
									; Apply include and exclude rules to targets.
									(if (or helm-grep-rg-included-types helm-grep-rg-excluded-types)
										(cl-loop
											with types_to_regex =
												(lambda (types)
													(mapconcat
														#'dired-glob-regexp
														(mapcan
															(lambda (type)
																(split-string
																	(helm-grep-rg-type-to-definition
																		type)
																	","))
															types)
														"\\|"))
											with must_match_regex =
												(when helm-grep-rg-included-types
													(funcall types_to_regex helm-grep-rg-included-types))
											with cant_match_regex =
												(when helm-grep-rg-excluded-types
													(funcall types_to_regex helm-grep-rg-excluded-types))
											for target in helm-grep-targets
											for target_basename = (helm-basename target)
											when
												(and
													(or
														(not must_match_regex)
														(string-match-p
															must_match_regex target_basename))
													(not
														(and
															cant_match_regex
															(string-match-p
																cant_match_regex target_basename))))
												collect target)
										helm-grep-targets))
							:sentinel
								(lambda (process event)
									(when
										(and
											(or
												(string= event "finished\n")
												(= (process-exit-status process) 1))
											(helm-get-process-info process))
										(with-current-buffer helm-buffer
											(setq helm-grep-time
												(float-time (time-subtract nil helm-grep-time))))
										(helm-async-set-finished helm-grep-rg-source)))
							:stderr (helm-async-get-stderr "helm-grep-rg")))))
		:candidate-transformer
			(lambda (output_line_list)
				(when output_line_list
					(let (group_list) ; List of lists with candidates.
						(let (file group)
							(when helm-grep-last-file-candidates
								(setq file
									(if (stringp helm-grep-last-file-candidates)
										helm-grep-last-file-candidates
										(setq group helm-grep-last-file-candidates)
										(helm-grep-cand-file
											(cdr (car helm-grep-last-file-candidates))))))
							; Transform candidates to (line . real) and group them by file.
							(dolist (output_line output_line_list)
								(cl-case (aref output_line helm-grep-rg--type-index)
									; Begin.
									(?b (setq file (helm-grep-rg--get-file output_line)))
									; End.
									(?e (push group group_list) (setq group nil))
									; Match.
									(?m
										(let* (
											(line (helm-grep-rg--get-line output_line file))
											(line_number
												(helm-grep-rg--get-line-number output_line file line))
											(byte_offset
												(helm-grep-rg--get-byte-offset
													output_line file line line_number))
										)
											(if helm-grep-rg-use-multiline
												(let (
													(match
														(helm-grep-rg--get-match
															output_line file line line_number byte_offset))
												)
													; Use elisp reader to parse json string.
													; Idk if there are some cases where this won't
													; work correctly, I can't think of any.
													(setq line (read (concat "\"" line "\"")))
													; If rg matched final newline, don't remove
													; last newline now, leave it temporarily for matching.
													; Save info that after matching this last newline
													; will need to be removed (only from display part)
													; in helm-grep-rg-cand-temp-newline.
													(if
														(and
															(string-suffix-p "\\n" match)
															(not
																(is_backslashed_str
																	(- (length match) 2) match)))
														(push
															(cons
																; rg doesn't do this conversion so do it here.
																(setq line (string-replace "\r\n" "\n" line))
																(helm-grep-rg-cand-make
																	:file file
																	:line-number line_number
																	:byte-offset byte_offset
																	:line line
																	:temp-newline t))
															group)
														(let (
															(newline_index
																(string_search_not_backslashed "\\n" match))
															newline_str
															newline_byte_size
														)
															(if (string-suffix-p "\r\n" line)
																(setq
																	newline_str "\r\n"
																	newline_byte_size 2)
																(setq
																	newline_str "\n"
																	newline_byte_size 1))
															(setq line
																(substring-no-properties
																	line 0 (- newline_byte_size)))
															(if newline_index
																(push
																	(cons
																		(setq line
																			(string-replace "\r\n" "\n" line))
																		(helm-grep-rg-cand-make
																			:file file
																			:line-number line_number
																			:byte-offset byte_offset
																			:line line
																			:temp-newline nil))
																	group)
																(let (
																	(line_number_str line_number)
																	(line_number (string-to-number line_number))
																	(byte_offset_str byte_offset)
																	(byte_offset (string-to-number byte_offset))
																)
																	(dolist (line (split-string line newline_str))
																		(push
																			(cons
																				line
																				(helm-grep-rg-cand-make
																					:file file
																					:line-number line_number_str
																					:byte-offset byte_offset_str
																					:line line
																					:temp-newline nil))
																			group)
																		(+= byte_offset
																			(string-bytes line) newline_byte_size)
																		(setq byte_offset_str
																			(number-to-string byte_offset))
																		(++ line_number)
																		(setq line_number_str
																			(number-to-string line_number))))))))
												; Simpler non-multiline version.
												(setq line (read (concat "\"" line "\"")))
												(setq line
													(substring-no-properties
														line
														0
														(if (string-suffix-p "\r\n" line)
															-2 -1)))
												(push
													(cons
														line
														(helm-grep-rg-cand-make
															:file file
															:line-number line_number
															:byte-offset byte_offset
															:line line
															:temp-newline nil))
													group))))))
							; Save last file or candidates for next output.
							(setq helm-grep-last-file-candidates (or group file)))
						; If we have results.
						(when group_list
							; Reverse candidates in groups before matching,
							; to have proper grouping by match method.
							(map_modify_list #'nreverse group_list)
							; Perform matching on every file.
							(map_modify_list #'helm-async-candidate-transformer group_list)
							; Remove empty groups.
							(when (setq group_list (delq nil group_list))
								; Remove temporary newline added for matching
								; and save last cand from every group.
								(let (last_cand_list)
									(dolist (group group_list)
										(let (last_cand)
											(dolist (cand group)
												(setq last_cand cand)
												(when (helm-grep-rg-cand-temp-newline (cdr cand))
													(setcar cand (substring (car cand) 0 -1))))
											(push last_cand last_cand_list)))
									; Reverse files to the correct order.
									(setq group_list (nreverse group_list))
									; Transform display parts - add line-number column and trim.
									(if helm-grep-rg-use-multiline
										; Copied from helm-grep-transform-to-display.
										(with-temp-buffer
											(dolist (group group_list)
												(let (
													(max_length
														(max
															helm-grep-line-number-min-width
															(length
																(number-to-string
																	(let ((last_cand (pop last_cand_list)))
																		(+
																			(string-to-number
																				(helm-grep-cand-line-number
																					(cdr last_cand)))
																			(cl-count
																				?\n (car last_cand))))))))
													(trim helm-grep-trim)
												)
													(dolist (cand group)
														(insert (car cand))
														(goto-char 1)
														(let* (
															(line_number_str
																(helm-grep-cand-line-number (cdr cand)))
															(line_number (string-to-number line_number_str))
														)
															(while
																(let ((bol (point)))
																	(when trim
																		(skip-chars-forward " \t")
																		(delete-region 1 (point)))
																	(line_tabs_to_spaces)
																	(let ((eol_offset (- (point) bol)))
																		(goto-char bol)
																		(insert
																			(get_space_string
																				(1+
																					(-
																						max_length
																						(length
																							line_number_str))))
																			(propertize_no_copy line_number_str
																				'face 'helm-grep-line-number)
																			" ")
																		(forward-char eol_offset))
																	(= (forward-line 1) 0))
																(++ line_number)
																(setq line_number_str
																	(number-to-string line_number))))
														(setcar cand (buffer-string))
														(erase-buffer)))))
										(dolist (group group_list)
											(helm-grep-transform-to-display
												group
												(length
													(helm-grep-cand-line-number
														(cdr (pop last_cand_list))))))))
								; Add files to the helm-grep-file-list.
								(setq helm-grep-file-list
									(nconc
										helm-grep-file-list
										(mapcar
											(lambda (group)
												(helm-grep-cand-file (cdr (car group))))
											group_list)))
								; Add separator line.
								(map_modify_list
									#'helm-grep-add-separator-line
									group_list)
								; Merge everything into one list.
								(apply #'nconc group_list))))))
		:mode-line
			(lambda ()
				(concat
					; Display current file and it's index.
					(when-let ((real (get-text-property (point) 'helm-real)))
						(let ((file (helm-grep-cand-file real)))
							(concat
								(number-to-string
									(1+ (index_in_list_string= helm-grep-file-list file)))
								"/"
								(number-to-string (length helm-grep-file-list))
								" "
								(helm-grep-file-absolute-to-display file)
								"  ")))
					(when helm-grep-trim "trim  ")
					(when helm-grep-rg-max-depth
						(concat
							"Max depth: " (number-to-string helm-grep-rg-max-depth) "  "))
					(when helm-grep-rg-sort
						(concat
							"Sort"
							(when helm-grep-rg-sort-reverse " reverse")
							": "
							helm-grep-rg-sort
							"  "))
					(when helm-grep-rg-included-types
						(concat
							"In: "
							(mapconcat
								#'helm-grep-rg-type-to-name
								(take 5 helm-grep-rg-included-types)
								", ")
							"  "))
					(when helm-grep-rg-excluded-types
						(concat
							"Ex: "
							(mapconcat
								#'helm-grep-rg-type-to-name
								(take 5 helm-grep-rg-excluded-types)
								", ")
							"  "))
					(helm-async-mode-line)
					(when (floatp helm-grep-time)
						(format "%.2f  " helm-grep-time))))
		:keymap
			(let ((keymap (make-sparse-keymap)))
				(set-keymap-parent keymap helm-grep-keymap)
				(define-key keymap [?\C-\S-s]
					(helm-make-action-command #'helm-grep-rg-save-results))
				(define-key keymap [?\C-f] #'helm-grep-rg-include-types)
				(define-key keymap [?\C-\S-f] #'helm-grep-rg-exclude-types)
				(define-key keymap [?\A-f] #'helm-grep-rg-browse-types)
				(define-key keymap [?\C-.] #'helm-grep-rg-max-depth-set)
				(define-key keymap [?\C-s ?r] #'helm-grep-rg-sort-reverse-toggle)
				(define-key keymap [?\C-s ?n] #'helm-grep-rg-sort-none)
				(define-key keymap [?\C-s ?p] #'helm-grep-rg-sort-path)
				(define-key keymap [?\C-s ?m] #'helm-grep-rg-sort-modified)
				(define-key keymap [?\C-s ?a] #'helm-grep-rg-sort-accessed)
				(define-key keymap [?\C-s ?c] #'helm-grep-rg-sort-created)
				keymap)
		:action
			(list
				(cons "Find file" #'helm-grep-default-action)
				(cons "Find file other window" #'helm-grep-jump-other-window)
				(cons "Find file other frame" #'helm-grep-jump-other-frame)
				(cons "Save results" #'helm-grep-rg-save-results)
				(cons "Replace marked (prefix - fixed case)" #'helm-grep-replace))
		:match-part nil
		:noncandidate 'header))

(defun helm-grep-rg (&optional targets &rest args)
"helm for rg-grepping `default-directory'.

Like `helm-grep' but this one has some extra features:
	Multiline matching - automatically turned on, when pattern has a newline in
	it. When on, candidates will be separated with `helm-candidate-separator'.

	Selecting types of files to search.
	While in helm, use `helm-grep-rg-include-types' or `helm-grep-rg-exclude-types'
	to launch nested helm and select rg file types to use. This selection is
	persistent across `helm-grep-rg' sessions. Use `helm-grep-rg-browse-types'
	to add/modify/delete rg file types, which are saved in `helm-grep-rg-types'
	and should be saved across emacs sessions.

	Max depth of directory traversal.
	Use `helm-grep-rg-max-depth-set' to set this while in helm.

	Sorting - sort files by some metric like mod time, size, etc.
	If some sorting is used, rg is slower, uses only 1 thread."
	(interactive)
	(if (not default-directory)
		(message "No default directory.")
		(setq helm-grep-targets targets)
		(save-file-buffers)
		(helm-grep-rg-types-options-set)
		(apply #'helm
			:sources (list helm-grep-rg-source)
			:default (thing-at-point 'symbol t)
			:history 'helm-grep-history
			:resume "*helm-grep*"
			:helm-default-keymap helm-grep-rg-default-keymap
			:helm-delay helm-grep-input-idle-delay
			:helm-truncate-lines helm-grep-truncate-lines
			args)))

; Git grep

(defun helm-grep-git-1 (directory &optional all)
"Run git-grep on DIRECTORY.
If DIRECTORY is not inside or part of a git repo exit with error.
If optional arg ALL is non-nil grep the whole repo otherwise
start at DIRECTORY."
	(require 'vc)
	(if-let ((default-directory (vc-find-root directory ".git")))
		(helm-grep (list (if all "" directory)) 'git)
		(message "Not inside a Git repository.")))

(defun helm-grep-git (arg)
"helm for git-grepping `default-directory'.
With a prefix arg git-grep the whole repository."
	(interactive "P")
	(helm-grep-git-1 default-directory arg))

(provide 'helm-grep)


; When helm-grep-replace exists, this below is pretty useless and hacky, so I don't use it.
; This version is untested, but probably works.
;(defun helm-grep-query-replace-regex (_candidate)
;"Query replace regex on candidates."
;	(let* (
;		(marked (map_modify_list #'car (helm-marked-candidates nil t)))
;		(regex (mapconcat #'identity (split-string helm-pattern) "\\|"))
;		(new_text (query-replace-read-to regex "Query replace regex" t))
;		current_cell
;		(isearch-filter-predicate
;			`(lambda (start end)
;				(let (sublist)
;					(and
;						(,isearch-filter-predicate start end)
;						; Return nil if match was marked (skip it).
;						(or
;							(not
;								(save-excursion
;									(goto-char start) (is_caret_on_the_same_line end)))
;							(not
;								(setq sublist
;									; Well, this won't work when new_text has a newline...
;									; I guess then I need to go to every line and give it
;									; a special marker to know which line was which.
;									(memq (line-number-at-pos start t) (cdr current_cell))))
;							(progn
;								(setcdr current_cell (cdr sublist))
;								nil))))))
;		alist
;	)
;		; Trasform to alist (file . list_of_line_numbers).
;		(while marked
;			(let* (
;				(line (pop marked))
;				(file (get-text-property 0 'helm-grep-fname line))
;				(line_number (string-to-number (nth 1 (helm-grep-split-line line))))
;			)
;				(if-let ((cell (assoc file alist)))
;					(setcdr cell (cons line_number (cdr cell)))
;					(push (list file line_number) alist))))
;		; Sort list_of_line_numbers.
;		(dolist (cell alist) (setcdr cell (sort (cdr cell) #'<)))
;		; From query-replace-read-from.
;		(add-to-history query-replace-from-history-variable regex nil t)
;		(dolist (buffer (mapcar (lambda (cell) (find-file-noselect (car cell) t)) alist))
;			(setq current_cell (pop alist))
;			(save-window-excursion
;				(switch-to-buffer buffer)
;				(save-excursion
;					(goto-char (point-min))
;					(perform-replace
;						regex
;						new_text
;						t
;						t
;						nil
;						nil
;						multi-query-replace-map))))))
