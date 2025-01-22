; -*- lexical-binding:nil -*-

; Save the Desktop, i.e.
;	- some global variables
; 	- the list of buffers with associated files. For each buffer also
;		- the point
;		- the mark & mark-active
;		- buffer-read-only
;		- some local variables
;	- frame and window configuration

; When the desktop module is loaded, the function
; `desktop::try_to_save_desktop_on_emacs_kill' is
; added to the `kill-emacs-query-functions'.
; In addition, `desktop::read' is added to the `after-init-hook'.
; It will load the desktop file.

; Frames that have 'desktop-dont-save parameter non-nil won't be restored by desktop.

(require 'frameset)

(defconst desktop::filepath (concat INIT_DIR "desktop_save.el")
"Filepath for desktop file.")

(defconst desktop::filepath_lock
;	(concat
;		(file-name-directory desktop::filepath)
;		".#"
;		(file-name-nondirectory desktop::filepath))
	(concat desktop::filepath ".lock")
"Filepath for locked desktop file.
Should probably follow emacs' naming convention of lock files,
that is basename should be prepended with \".#\".
Ok actually it turns out that such names for arbitrary files break emacs. Epic.")

(defconst desktop::save 'ask-if-new
"Specifies whether the desktop should be saved when it is killed.
A desktop is killed when the user changes desktop or quits Emacs.
Possible values are:
   t             -- always save.
   ask           -- always ask.
   ask-if-new    -- ask if no desktop file exists, otherwise just save.
   ask-if-exists -- ask if desktop file exists, otherwise don't save.
   if-exists     -- save if desktop file exists, otherwise don't save.
   nil           -- never save.
`desktop::filepath' determine where the desktop is saved.")

(defconst desktop::load_locked_desktop 'check-pid
"Specifies whether the desktop should be loaded if locked.
Possible values are:
   t          -- load anyway.
   nil        -- don't load.
   ask        -- ask the user.
   check-pid  -- load if locking Emacs process is missing locally.

If the value is `check-pid', load the desktop if the Emacs
process that has locked it is not running on the local machine.
This should not be used in circumstances where the locking Emacs
might still be running on another machine.")


(defvar desktop::globals_to_save
	'(
		tags-file-name
		tags-table-list
		search-ring
		regexp-search-ring
		register-alist
		file-name-history
	)
"List of global variables saved by `desktop::save'.
An element may be variable name (a symbol) or a cons cell of the form
(VAR . MAX-SIZE), which means to truncate VAR's value to at most
MAX-SIZE elements (if the value is a list) before saving the value.")

(defvar desktop::locals_to_save '(buffer-display-time)
"List of local variables to save for each buffer.
The variables are saved only when they really are local.")

(defvar desktop::serialization_var_list nil
"List with lists like (var_symbol serializer_fn deserializer_fn).
This is for (probably buffer-local) variables that need more complicated saving and reading.
Each buffer is saved with result of calling serializer_fn with (symbol-value var_symbol).
Reading sets var_symbol to result of calling deserializer_fn with buffer's saved value.
Example of entry:
(list
	'mark-ring
	(lambda (buffer_local_mark_ring) (mapcar #'marker-position buffer_local_mark_ring))
	(lambda (position_list) (mapcar #'copy-marker position_list))).")


(defvar desktop::exclude::buffer_regex "\\` "
"Nil or regex identifying buffers that are to be excluded from saving.
This runs only for buffers that don't visit files.")

; Skip tramp and ange-ftp files.
(defvar desktop::exclude::file_regex "\\(\\`/[^/:]*:\\|(ftp)\\'\\)"
"Nil or regex identifying files whose buffers are to be excluded from saving.
The default value excludes buffers visiting remote files.
This runs only for buffers that visit files.")

(defvar desktop::exclude::fn nil
"Nil or function that should return nil to save buffer state
in the desktop file.
It's called with: (filename bufname mode &rest rest)
FILENAME is the visited file name, BUFNAME is the buffer name,
and MODE is the major mode.
This runs for both file-visiting and not file-visiting buffers.")

; We skip TAGS files to save time (tags-file-name is saved instead).
(defvar desktop::exclude::mode_list '(tags-table-mode)
"List of major modes and its derivatives whose buffers should not be saved.
This runs for both file-visiting and not file-visiting buffers.")

(defvar desktop::save_buffer nil
"If a buffer doesn't visit file and it hasn't been discarded
by desktop::exclude::buffer_regex, then if this is non-nil, save buffer.
In other words this must be non-nil to save a buffer that doesn't visit file.

If the value is a function, it is called by `desktop::save' without args
to obtain auxiliary information to save in the desktop
file along with the state of the buffer for which it was called.

Later, when `desktop::read' evaluates the desktop file, auxiliary information
is passed as the argument DESKTOP-BUFFER-MISC to desktop::restore_buffer_fn.")

(defvar desktop::restore_buffer_fn nil
"Function to restore buffer.
It should return buffer, or nil to not restore buffer.
Called with buffer name, buffer filename (may be nil) and with data that
desktop::save_buffer returned for that buffer when saving it (may be nil).
Point, mark, variables from desktop::locals_to_save and
desktop::serialization_var_list, buffer read only status and buffer-display-time
are restored later, though if this function wants to change point or mark,
it can set buffer-point and buffer-mark to nil, or to some other value.

If this is nil, buffers that visit files are recreated by calling find-file-noselect.")


(defconst desktop::restore_frames t
"When non-nil, save and restore the frame and window configuration.")

(defconst desktop::restore_in_current_display t
"Controls how restoring of frames treats displays.
If t, restores frames into the current display.
If nil, restores frames into their original displays (if possible).
If `delete', deletes frames on other displays instead of restoring them.")

(defconst desktop::restore_reuses_frames t
"If t, restoring frames reuses existing frames.
If nil, deletes existing frames.
If `keep', keeps existing frames and does not reuse them.
For example, if desktop saved one frame, on startup:
if t, use initial frame;
if nil, delete initial frame and create a new one (this will still show
initial frame till desktop::read execution, so it won't look great);
if 'keep, leave initial frame untouched, create a new frame,
so we end up with 2 frames.")

(defconst desktop::restore_forces_onscreen t
"If t, restores frames that are fully offscreen onscreen instead.
If `all', also restores frames that are partially offscreen onscreen.
If nil, do not force frames onscreen.

Note that checking of frame boundaries is only approximate.
It can fail to reliably detect frames whose onscreen/offscreen state
depends on a few pixels, especially near the right / bottom borders
of the screen.
Text-mode frames are always considered onscreen, so this option has
no effect on restoring frames in a non-GUI session.")

(defconst desktop::restore_smart t
"Non-nil to use different kind of restoration:

If saved frameset has only one frame, put it's saved window state
on current frame without asking.
This is the usual emacs startup - looks the best, because it doesn't mess
with initial frame, outside of restoring windows.
Also emacs is usually closed with one frame alive, contrary to desktop
auto saving, where there well could be a couple frames.
So this try to be something like browser's auto restore feature after crashing.

If saved frameset has more frames, ask to restore them all,
and use variables above to determine how to do that.")


(defconst desktop::restore_eager t
"Number of buffers to restore immediately.
Remaining buffers are restored when Emacs is idle.
If value is t, all buffers are restored immediately.")

(defconst desktop::lazy_verbose t
"Non-nil - verbose reporting of lazily created buffers.")

(defconst desktop::lazy_idle_delay 5
"Idle delay before starting to create buffers.
See `desktop::restore_eager'.")

(defconst desktop::auto_save_timeout 20
"Number of seconds of idle time before auto-saving the desktop.
The desktop will be auto-saved when this amount of idle time have
passed after some change in the window configuration
(that includes current-buffer changes).
Nil means disable auto-saving due to idleness.
Don't set this to 0.")

(cl-assert (or (null desktop::auto_save_timeout) (> desktop::auto_save_timeout 0)))


(defconst desktop::header_string ""
"Header to place in Desktop file. Must be a string.")

(defvar desktop::read_hook nil
"Normal hook run after a successful `desktop::read'.")

(defvar desktop::save_hook nil
"Normal hook run before the desktop is saved in a desktop file.
Run with the desktop buffer current with only the header present.
May be used to add to the desktop code or to truncate history lists.
Also may be used for updating variables in desktop::globals_to_save,
because they are not yet encoded in desktop file.")


(defconst desktop::globals_to_clear
	'(
		search-ring
		search-ring-yank-pointer
		regexp-search-ring
		regexp-search-ring-yank-pointer
	)
"List of global variables that `desktop::clear' will clear.
An element may be variable name (a symbol) or a cons cell of the form
(VAR . FORM). Symbols are set to nil and for cons cells VAR is set
to the value obtained by evaluating FORM.")

(defconst desktop::clear_preserve_buffers
	'("\\*scratch\\*" "\\*Messages\\*" "\\*server\\*" "\\*tramp/.+\\*" "\\*Warnings\\*")
"List of buffers that `desktop::clear' should not delete.
Each element is a regular expression.")

; End of customization section.

(defun desktop::add_to_globals (var &optional max_length)
	(setq desktop::globals_to_save (delq var desktop::globals_to_save))
	(setq desktop::globals_to_save (assq-delete-all var desktop::globals_to_save))
	(push (if max_length (cons var max_length) var) desktop::globals_to_save)
	nil)

(defvar desktop::setup_markers_hook nil
"Hooks run after all buffers are loaded, to setup markers.")

(defvar desktop::file_modtime nil
"When the desktop file was last modified to the knowledge of this Emacs.
Used to detect desktop file conflicts.")

(defun desktop::set_file_modtime () "In list form."
	(setq desktop::file_modtime
		(time-convert
			(file-attribute-modification-time (file-attributes desktop::filepath))
			'list)))

; Ownership and lock file.

(defun desktop::owner ()
"Return the PID of the Emacs process that owns the desktop file.
Return nil if no desktop file found or no Emacs process is using it."
	(if (file-exists-p desktop::filepath_lock)
		(ignore-errors
			(with-temp-buffer
				(insert-file-contents-literally desktop::filepath_lock)
				(goto-char (point-min))
				(let ((owner (read (current-buffer))))
					(when (integerp owner) owner))))))

(defun desktop::claim_lock ()
"Record this Emacs process as the owner of the desktop file."
	(write-region (number-to-string (emacs-pid)) nil desktop::filepath_lock))

(defun desktop::release_lock () "Remove the desktop lock file."
	(when (file-exists-p desktop::filepath_lock)
		(delete-file desktop::filepath_lock)))


(defvar desktop::buffer_args_list nil "List of args for `desktop::create_buffer'.")

(defvar desktop::lazy_timer nil)

(defun desktop::lazy_abort () "Abort lazy loading of the desktop."
	(interactive)
	(when desktop::lazy_timer
		(cancel-timer desktop::lazy_timer)
		(setq desktop::lazy_timer nil))
	(when desktop::buffer_args_list
		(setq desktop::buffer_args_list nil)
		(if (called-interactively-p 'interactive)
			(message "Lazy desktop load aborted."))))

(defun desktop::clear ()
"Empty the Desktop.
This kills all buffers except for internal ones and those with names matched by
a regular expression in the list `desktop::clear_preserve_buffers'.
Furthermore, it clears the variables listed in `desktop::globals_to_clear'.
When called interactively and `desktop::restore_frames' is non-nil, it also
deletes all frames except the selected one (and its minibuffer frame, if different)."
	(interactive)
	(desktop::lazy_abort)
	(dolist (var desktop::globals_to_clear)
		(if (symbolp var)
			(set-default var nil)
			(set-default var (eval (cdr var)))))
	(let ((preserve-regexp
			(concat
				"\\`\\("
				(mapconcat
					(lambda (regexp) (concat "\\(" regexp "\\)"))
					desktop::clear_preserve_buffers
					"\\|")
				"\\)\\'")))
		(dolist (buffer (buffer-list))
			(if-let ((bufname (buffer-name buffer))
					 ((/= ?\s (aref bufname 0))) ; Don't kill internal buffers.
					 ((not (string-match-p preserve-regexp bufname))))
				(kill-buffer buffer))))
	(delete-other-windows)
	; Non-interactive calls to desktop::clear happen before desktop::read
	; which already takes care of frame restoration and deletion.
	(if (and desktop::restore_frames (called-interactively-p 'any))
		(let* ((this (selected-frame))
			   (mini (window-frame (minibuffer-window this)))) ; In case they differ.
			(dolist (frame (sort (frame-list) #'frameset-minibufferless-first-p))
				(unless (or
						(eq frame this)
						(eq frame mini)
						(frame-parameter frame 'desktop-dont-clear))
					(condition-case err
						(delete-frame frame)
						(error (delay-warning 'desktop (error-message-string err)))))))))


(defun desktop::value_to_string_quote_and_sexp (value)
"Convert VALUE to a pair (QUOTE . SEXP); (eval SEXP) gives VALUE.
SEXP is an sexp that when evaluated yields VALUE.
QUOTE may be `may' (value may be quoted),
`must' (value must be quoted), or nil (value must not be quoted).
Internal of value_to_string in desktop::save."
	(cond
		((or (numberp value) (null value) (eq t value) (keywordp value))
			(cons 'may value))
		((stringp value)
			; Get rid of unreadable text properties.
			(if (ignore-errors (read (format "%S" value)))
				(cons 'may value)
				(let ((copy (copy-sequence value)))
					(set-text-properties 0 (length copy) nil copy)
					(cons 'may copy))))
		((symbolp value) (cons 'must value))
		((vectorp value)
			(let* ((pass1 (mapcar #'desktop::value_to_string_quote_and_sexp value))
				   (special (assq nil pass1)))
				(if special
					(cons
						nil
						(vconcat
							(mapcar
								(lambda (el)
									(if (eq (car el) 'must)
										(list #'quote (cdr el))
										(cdr el)))
								pass1)))
					(cons
						'may
						(vconcat (mapcar #'cdr pass1))))))
		((and (recordp value) (symbolp (aref value 0)))
			(let* ((pass1
					(let (res)
						(dotimes (i (length value))
							(push (desktop::value_to_string_quote_and_sexp (aref value i)) res))
						(nreverse res)))
				   (special (assq nil pass1)))
				(if special
					(cons
						nil
						(apply #'record
							(mapcar
								(lambda (el)
									(if (eq (car el) 'must)
										(list #'quote (cdr el))
										(cdr el)))
								pass1)))
					(cons 'may (apply #'record (mapcar #'cdr pass1))))))
		((consp value)
			(let ((p value) newlist use-list*)
				(while (consp p)
					(push (desktop::value_to_string_quote_and_sexp (car p)) newlist)
					(setq p (cdr p)))
				(when p
					(setq use-list* t)
					(push (desktop::value_to_string_quote_and_sexp p) newlist))
				(if (assq nil newlist)
					(cons
						nil
						(cons
							(if use-list*
								; Safe, because mapcar below will return non-nil,
								; so cl-list* will be called with at least one arg.
								#'cl-list*
								#'list)
							(mapcar
								(lambda (el)
									(if (eq (car el) 'must)
										(list #'quote (cdr el))
										(cdr el)))
								(nreverse newlist))))
					(cons
						'must
						(nconc
							(mapcar #'cdr (nreverse (if use-list* (cdr newlist) newlist)))
							(if use-list* (cdr (car newlist))))))))
		((subrp value)
			(cons
				nil
				`(symbol-function ',(intern-soft (substring (prin1-to-string value) 7 -1)))))
		((markerp value)
			(cons
				nil
				`(let ((marker (make-marker)))
					(add-hook 'desktop::setup_markers_hook
						(list #'lambda '()
							(list #'set-marker
								marker
								,(marker-position value)
								'(get-buffer ,(buffer-name (marker-buffer value))))))
					marker)))
		(t ; Save as text.
			(cons 'may "Unprintable entity"))))

(defun desktop::save (&optional release)
"RELEASE non-nil says we're done with this desktop, in which case this function
releases the lock of the desktop file.

Let-bind desktop::filepath and maybe desktop::filepath_lock
to change where desktop file will be saved.

To restore the desktop, use `desktop::read'."
	(save-excursion
		(let ((eager desktop::restore_eager)
			  (new-modtime
				(file-attribute-modification-time (file-attributes desktop::filepath))))
			(when (or
					(not new-modtime) ; nothing to overwrite
					(time-equal-p desktop::file_modtime new-modtime)
					(if desktop::file_modtime
						(or
							; If file is more recent than we saved it, so probably manual modification
							; taken place; overwrite it.
							(time-less-p desktop::file_modtime new-modtime)
							(y-or-n-p
								(format "Desktop file \"%s\" isn't the one loaded. Overwrite it? "
									desktop::filepath)))
						(y-or-n-p
							(format
								"Current desktop was not loaded from a file. Overwrite file \"%s\"? "
								desktop::filepath)))
					(unless release (error "Desktop file conflict")))
				; If we're done with it, release the lock.
				; Otherwise, claim it if it's unclaimed or if we created it.
				(if release
					(desktop::release_lock)
					(unless (and new-modtime (desktop::owner)) (desktop::claim_lock)))
				(let ((value_to_string
						; Convert value to a string that when read evaluates to the same value.
						; Not all types of values are supported.
						(lambda (value)
							(let* ((print-escape-newlines t)
								   (print-length nil)
								   (print-level nil)
								   (float-output-format nil)
								   (quote_and_sexp (desktop::value_to_string_quote_and_sexp value))
								   (print-quoted t)
								   (txt (prin1-to-string (cdr quote_and_sexp))))
								(if (eq (car quote_and_sexp) 'must)
									(concat "'" txt)
									txt)))))
					(withTempBuffer
						(insert
							"; -*- coding: utf-8-emacs; -*-\n"
							desktop::header_string
							"; Created " (current-time-string) "\n"
							"; Emacs version " emacs-version "\n\n")
						(run-hooks 'desktop::save_hook)
						(goto-char (point-max))
						; Save the state of existing frames.
						(if desktop::restore_frames
							(insert
								"(setq desktop::saved_frameset "
								(funcall value_to_string
									(frameset-save nil
										:predicate
											(lambda (frame)
												(not (frame-parameter frame 'desktop-dont-save)))))
								")\n\n"))
						(insert "; Global section:\n")
						(dolist (varspec desktop::globals_to_save)
							; Output a setq statement for variable to the desktop file.
							; varspec may be the variable name (a symbol),
							; or a cons cell of the form (VAR . MAX-SIZE),
							; which means to truncate VAR's value to at most MAX-SIZE elements
							; (if the value is a list) before saving the value.
							(let (var size)
								(if (consp varspec)
									(setq var (car varspec)
										  size (cdr varspec))
									(setq var varspec))
								(when (boundp var)
									; Truncate var list to at most size elements.
									(if (and size (listp (symbol-value var)))
										(ntake size (symbol-value var)))
									(insert
										"(setq "
										(symbol-name var)
										" "
										(funcall value_to_string (symbol-value var))
										")\n"))))

						(insert "\n; Buffer section - buffers listed in same order as in buffer list:\n")
						(dolist (buffer (buffer-list))
							(set-buffer buffer)
							(let ((buffer_info
									; Information describing buffer.
									; List of all the necessary information to recreate the
									; buffer, which is (in order):
									; `buffer-file-name'
									; `buffer-name'
									; `point'
									; `mark'
									; `buffer-read-only'
									; return value of calling `desktop::save_buffer'
									; buffer-local value of `desktop::restore_buffer_fn'
									; local variables
									; serialization of vars in `desktop::serialization_var_list'.
									(list
										; basic information
										(buffer-file-name)
										(buffer-name)
										(point)
										(list (mark) mark-active)
										buffer-read-only
										(if (functionp desktop::save_buffer)
											(funcall desktop::save_buffer))
										desktop::restore_buffer_fn
										(let ((buffer_local_variables (buffer-local-variables))
											  locals_to_save)
											(dolist (local desktop::locals_to_save)
												(let ((symbol_and_value (assq local buffer_local_variables)))
													(cond
														(symbol_and_value (push symbol_and_value locals_to_save))
														; Locally unbound vars. See buffer-local-variables.
														((member local buffer_local_variables) (push local locals_to_save)))))
											locals_to_save)
										(mapcar
											(lambda (record)
												(let ((var (car record)))
													(cons var (funcall (nth 1 record) (symbol-value var)))))
											desktop::serialization_var_list))))

								; If buffer should be saved in the desktop file.
								(when
									(let ((buffer_file_name (car buffer_info))
										  (buffer_name (nth 1 buffer_info))
										  case-fold-search)
										(and
											(or ; Discard by buffer regex if buffer_file_name is nil.
												buffer_file_name
												(null desktop::exclude::buffer_regex)
												(not (string-match-p desktop::exclude::buffer_regex buffer_name)))
											; Discard by major mode.
											(not (apply #'derived-mode-p desktop::exclude::mode_list))
											(or
												(and ; Discard by file regex if buffer_file_name is non-nil.
													buffer_file_name
													(not
														(and
															desktop::exclude::file_regex
															(string-match-p desktop::exclude::file_regex buffer_file_name))))
												(let (exclude_dired_buffer)
													(or
														; Discard by file regex if buffer is a dired buffer.
														; Use default-directory instead of buffer_file_name.
														(and
															(apply #'derived-mode-p '(dired-mode vc-dir-mode))
															(not
																(and
																	desktop::exclude::file_regex
																	(setq exclude_dired_buffer
																		(string-match-p
																			desktop::exclude::file_regex
																			default-directory)))))
														; Discard by desktop::save_buffer buffer-local value
														; if buffer_file_name is nil
														; (and not discarded by dired buffer's regex check).
														(and
															(not buffer_file_name)
															(not exclude_dired_buffer)
															desktop::save_buffer))))
											(not ; Discard by function.
												(and
													desktop::exclude::fn
													(funcall desktop::exclude::fn
														buffer_file_name buffer_name major-mode (nthcdr 2 buffer_info))))))
									; If there's a non-empty base name, we save it instead of the buffer name.
									(when-let ((buffer_name (uniquify-buffer-base-name))
											   ((not (string= buffer_name ""))))
										(setcar (cdr buffer_info) buffer_name))
									(set-buffer temp_buffer) ; Go back to modifying temp_buffer.
									(insert
										"(desktop::"
										(if (or
												(not (integerp eager))
												(if (= 0 eager)
													nil
													(-- eager)))
											"create_buffer"
											"append_buffer_args"))
									(dolist (arg buffer_info)
										(insert "\n\t" (funcall value_to_string arg)))
									(insert ")\n\n"))))
						(set-buffer temp_buffer)
						(setq default-directory (file-name-directory desktop::filepath))
						(let ((coding-system-for-write 'utf-8-emacs))
							(write-region nil nil desktop::filepath nil 'nomessage))
						; We remember when it was modified (which is just now).
						(desktop::set_file_modtime))))))
	nil)


(defvar desktop::auto_save_timer nil)

(defun desktop::auto_save_cancel_timer ()
	(when desktop::auto_save_timer
		(cancel-timer desktop::auto_save_timer)
		(setq desktop::auto_save_timer nil)))

(defun desktop::auto_save_set_timer ()
"Set the desktop auto-save timer.
Cancel any previous timer.
If `desktop::auto_save_timeout' is non-nil, start a new idle timer
to call `desktop::save' after that many seconds of idle time.
This function is called from `window-configuration-change-hook'."
	(desktop::auto_save_cancel_timer)
	(if desktop::auto_save_timeout
		(setq desktop::auto_save_timer
			(run-with-idle-timer desktop::auto_save_timeout nil
				(lambda ()
					(if (and
							desktop::auto_save_timeout
							; Avoid desktop saving during lazy loading.
							(not desktop::lazy_timer)
							; Save only to own desktop file.
							(eq (emacs-pid) (desktop::owner)))
						(desktop::save)))))))

(if desktop::auto_save_timeout
	(add-hook 'window-configuration-change-hook #'desktop::auto_save_set_timer))


; Just to silence the byte compiler.
; Dynamically bound in `desktop::read'.
(defvar desktop::first_buffer)
(defvar desktop::buffer_ok_count)
(defvar desktop::buffer_fail_count)

(defun desktop::read ()
"Read and process the desktop file.
If a desktop file is found, it is processed and `desktop::read_hook' is run.
If no desktop file is found, clear the desktop.
Return t if a desktop file was loaded."
	(let ((owner (desktop::owner)) (emacs_pid (emacs-pid)))
		(if (eq owner emacs_pid)
			(message "Not reloading the desktop - already loaded.")
			(if (file-exists-p desktop::filepath)
				; Desktop file found, but is it already in use?
				(let ((desktop::buffer_ok_count 0)
					  (desktop::buffer_fail_count 0)
					  desktop::first_buffer
					  ; Avoid desktop saving during evaluation of desktop buffer.
					  desktop::save
					  desktop-autosave-was-enabled)
					; If a locked desktop file shouldn't be loaded.
					(if (and
							owner
							(not
								(cl-case desktop::load_locked_desktop
									(ask
										(y-or-n-p
											(format
"Warning: desktop file appears to be in use by PID %d.
Using it may cause conflicts. Use it anyway?"
												owner)))
									(check-pid
										(or
											(= owner emacs_pid)
											; t if emacs process whose id is owner isn't running.
											(not
												(if-let ((attr (process-attributes owner)))
													(let ((proc-cmd (alist-get 'comm attr))
														  (my-cmd (file-name-nondirectory (car command-line-args)))
														  (case-fold-search t))
														(or
															(equal proc-cmd my-cmd)
															(and
																(eq system-type 'windows-nt)
																(eq
																	t
																	(compare-strings
																		proc-cmd
																		nil
																		(if (string-suffix-p ".exe" proc-cmd t)
																			-4)
																		my-cmd
																		nil
																		(if (string-suffix-p ".exe" my-cmd t)
																			-4))))
															; We should err on the safe side here: if any of the
															; executables is something like "emacs-nox" or "emacs-42.1"
															; or "gemacs", let's recognize them as well.
															(and
																(string-match-p "emacs" proc-cmd)
																(string-match-p "emacs" my-cmd))))))))
									(t desktop::load_locked_desktop))))
						(message "Desktop file in use - loading aborted.")
						(desktop::lazy_abort)
						; Temporarily disable the autosave that will leave it
						; disabled when loading the desktop fails with errors,
						; thus not overwriting the desktop with broken contents.
						(setq desktop-autosave-was-enabled
							(memq #'desktop::auto_save_set_timer
								; Use the global value of the hook, in case some
								; feature makes window-configuration-change-hook
								; buffer-local, and puts there stuff which
								; doesn't include our timer.
								(default-value 'window-configuration-change-hook)))
						(remove-hook 'window-configuration-change-hook #'desktop::auto_save_set_timer)
						(desktop::auto_save_cancel_timer)

						; Evaluate desktop buffer and remember when it was modified.
						(desktop::set_file_modtime)
						(let (desktop::saved_frameset) ; Local variable maybe set in desktop file.
							(load desktop::filepath t t t)
							; If it wasn't already, mark it as in-use, to bother other desktop instances.
							(unless (eq owner emacs_pid)
								(condition-case nil
									(desktop::claim_lock)
									(file-error
										(message "Couldn't record use of desktop file.")
										(sit-for 1))))

							(let ((frameset_will_be_restored (and desktop::restore_frames desktop::saved_frameset)))
								(unless frameset_will_be_restored
									; `desktop::create_buffer' puts buffers at end of the buffer list.
									; We want buffers existing prior to evaluating the desktop (and not reused)
									; to be placed at the end of the buffer list, so we move them here.
									(mapc #'bury-buffer
										(nreverse (cdr (memq desktop::first_buffer (nreverse (buffer-list))))))
									(switch-to-buffer (car (buffer-list))))
								(run-hooks 'desktop::setup_markers_hook)
								(setq desktop::setup_markers_hook nil)
								; Restore frameset.
								(if frameset_will_be_restored
									(let ((restore_frameset
											(lambda ()
												(frameset-restore desktop::saved_frameset
													:reuse-frames (eq desktop::restore_reuses_frames t)
													:cleanup-frames (not (eq desktop::restore_reuses_frames 'keep))
													:force-display desktop::restore_in_current_display
													:force-onscreen desktop::restore_forces_onscreen)
												; When at least one restored frame contains a tab bar,
												; enable `tab-bar-mode' that takes care about recalculating
												; the correct values of the frame parameter `tab-bar-lines'
												; (that depends on `tab-bar-show'), and also loads graphical buttons.
												(if (find_in_list (frame-list)
														(lambda (frame)
															(menu-bar-positive-p (frame-parameter frame 'tab-bar-lines))))
													(tab-bar-mode))
												nil)))
										(if desktop::restore_smart
											(let ((frameset_states (frameset-states desktop::saved_frameset)))
												(if (cdr frameset_states) ; If there is more than one frame saved.
													(if (y-or-n-p
															(concat
																"Dekstop: There are "
																(number-to-string (length frameset_states))
																" frames saved. Restore them? "))
														(funcall restore_frameset))
													(window-state-put (cdr (car frameset_states)) (frame-root-window) 'safe)))
											(funcall restore_frameset))))
								(run-hooks 'desktop::read_hook)
								(let ((inhibit-message t))
									(message "Desktop: %s%d buffer%s restored%s%s."
										(if desktop::saved_frameset
											(let ((frame_count (length (frameset-states desktop::saved_frameset))))
												(concat
													(number-to-string frame_count)
													" frame"
													(if (/= 1 frame_count) "s")
													", "))
											"")
										desktop::buffer_ok_count
										(if (= desktop::buffer_ok_count 1) "" "s")
										(if (> desktop::buffer_fail_count 0)
											(format ", %d failed to restore" desktop::buffer_fail_count)
											"")
										(if desktop::buffer_args_list
											(format ", %d to restore lazily" (length desktop::buffer_args_list))
											"")))
								(unless frameset_will_be_restored
									; Bury the *Messages* buffer to not reshow it when burying
									; the buffer we switched to above.
									(bury-buffer "*Messages*")
									; Clear all windows' previous and next buffers, these have
									; been corrupted by the `switch-to-buffer' calls in default handler
									; in `desktop::create_buffer'. This is a
									; brute force fix and should be replaced by a more subtle
									; strategy eventually.
									(walk-window-tree
										(lambda (window)
											(set-window-prev-buffers window nil)
											(set-window-next-buffers window nil))))))
						(if (and desktop-autosave-was-enabled desktop::auto_save_timeout)
							(add-hook 'window-configuration-change-hook #'desktop::auto_save_set_timer))
						t))
				(message "Desktop file not found.")
				nil))))


(defun desktop::create_buffer
	(buffer-filename buffer-name buffer-point buffer-mark buffer-readonly
	 buffer-misc restore_fn buffer-locals serialized_vars)
"Create a buffer, load its file, set its mode.
Called from desktop file."
	; To make desktop files with relative file names possible, we cannot
	; allow `default-directory' to change. Therefore we save current buffer.
	(save-current-buffer
		(let ((buffer_list (buffer-list))
			  (result
				(condition-case-unless-debug err
					(if restore_fn
						(funcall restore_fn buffer-name buffer-filename buffer-misc)
						; Default buffer restoration.
						; Works only for buffers with file, as otherwise it would be empty buffer anyway.
						; So every buffer that don't have file should use non-default restoring handler.
						(cond
							((not buffer-filename)
								(message
									"Desktop: Default restoration of buffers without files is unsupported. (buffer \"%s\")"
									desktop-buffer-name)
								nil)
							((file-exists-p buffer-filename)
								(let* ((coding-system-for-read
										(or
											coding-system-for-read
											(cdr (assq 'buffer-file-coding-system buffer-locals))))
									   (buffer (find-file-noselect buffer-filename t)))
									(condition-case nil ; Not sure why it's here.
										(switch-to-buffer buffer)
										(error (message "myDesktop.el line around 1000 switch-to-buffer errored!!!!!!!!!!!!!!!!!!!") (pop-to-buffer buffer)))
									buffer))
							(t
								(message "Desktop: File \"%s\" no longer exists." buffer-filename)
								nil)))
					(error
						(message "Desktop: Can't load buffer %s: %s"
							buffer-filename (error-message-string err))
						nil))))
			; Restore buffer list order with new buffer at end.
			(dolist (buffer buffer_list)
				(if (buffer-live-p buffer)
					(bury-buffer buffer)))
			(if (bufferp result)
				(progn
					(++ desktop::buffer_ok_count)
					(bury-buffer result)
					(set-buffer result)
					(unless desktop::first_buffer
						(setq desktop::first_buffer result))
					(unless (equal (buffer-name) buffer-name)
						(rename-buffer buffer-name t))
					; Even though point and mark are non-nil when written by `desktop::save',
					; they may be modified by handlers wanting to set point or mark themselves.
					(when buffer-point
						(goto-char
							(condition-case err
								; Evaluate point. Thus point can be something like
								; '(search-forward ...
								(eval buffer-point)
								(error (message "%s" (error-message-string err)) 1))))
					(when buffer-mark
						(if (not (consp buffer-mark))
							(set-marker (mark-marker) buffer-mark)
							(set-marker (mark-marker) (car buffer-mark))
							(when (car (cdr buffer-mark)) (activate-mark))))
					; Never override file system if the file really is read-only marked.
					(if buffer-readonly
						(setq buffer-read-only buffer-readonly))
					(dolist (local buffer-locals)
						(if (consp local)
							; An entry of this form `(symbol . value)'.
							(set (make-local-variable (car local)) (cdr local))
							; An entry of the form `symbol'.
							(makunbound (make-local-variable local))))
					(dolist (record serialized_vars)
						(let* ((var (car record))
							   (deserialize_fn (nth 2 (assq var desktop::serialization_var_list))))
							(if deserialize_fn
								(set var (funcall deserialize_fn (cdr record))))))
					; Adjust `buffer-display-time' for the downtime. e.g.,
					; * if `buffer-display-time' was 8:00
					; * and emacs stopped at `desktop::file_modtime' == 11:00
					; * and we are loading the desktop file at (current-time) 12:30,
					; -> then we restore `buffer-display-time' as 9:30,
					; for the sake of `clean-buffer-list': preserving the invariant
					; "how much time the user spent in Emacs without looking at this buffer".
					(setq buffer-display-time
						(time-since
							(if buffer-display-time
								(time-subtract desktop::file_modtime buffer-display-time)
								0)))
					result)
				(++ desktop::buffer_fail_count)
				nil))))


(defun desktop::lazy_create_buffer ()
"Pop args from `desktop::buffer_args_list', create buffer and bury it."
	(if desktop::buffer_args_list
		(let* ((remaining (length desktop::buffer_args_list))
			   (args (pop desktop::buffer_args_list))
			   (buffer-name (nth 2 args))
			   (msg
				(format "Desktop lazily opening %s (%s remaining)..."
					buffer-name remaining)))
			(if desktop::lazy_verbose
				(message "%s" msg))
			(let ((desktop::buffer_ok_count 0)
				  (desktop::buffer_fail_count 0)
				  desktop::first_buffer)
				(apply #'desktop::create_buffer args)
				(run-hooks 'desktop::setup_markers_hook)
				(setq desktop::setup_markers_hook nil)
				(bury-buffer (get-buffer buffer-name))
				(if desktop::lazy_verbose
					(message "%s%s" msg (if (> desktop::buffer_ok_count 0) "done" "failed")))))))

(defun desktop::append_buffer_args (&rest args)
"Append ARGS at end of `desktop::buffer_args_list'.
ARGS must be an argument list for `desktop::create_buffer'.
Called from desktop file."
	(setq desktop::buffer_args_list (nconc desktop::buffer_args_list (list args)))
	(unless desktop::lazy_timer
		(setq desktop::lazy_timer
			(run-with-idle-timer desktop::lazy_idle_delay t
				; Create buffers until the user does something, then stop.
				; If there are no buffers left to create, kill the timer.
				(lambda ()
					(let ((repeat 1))
						(while (and repeat desktop::buffer_args_list)
							(save-window-excursion (desktop::lazy_create_buffer))
							(setq repeat (sit-for 0.2))
							(unless desktop::buffer_args_list
								(cancel-timer desktop::lazy_timer)
								(setq desktop::lazy_timer nil)
								(message "Lazy desktop load complete.")
								(sit-for 3)
								(message nil)))))))))

(defun desktop::lazy_complete () "Run the desktop load to completion." (interactive)
	(let ((desktop::lazy_verbose t))
		(while desktop::buffer_args_list
			(save-window-excursion (desktop::lazy_create_buffer)))
		(message "Lazy desktop load complete.")))

(defun desktop::revert () "Revert to the last loaded desktop." (interactive)
	(unless (file-exists-p desktop::filename)
		(error "Desktop file not found"))
	(desktop::clear)
	(desktop::read))


(defun desktop::try_to_release_lock ()
	; If we own it, we don't anymore.
	(if (eq (emacs-pid) (desktop::owner))
		; Allow exiting Emacs even if we can't delete the desktop file.
		(ignore-error 'file-error
			(desktop::release_lock))))

(add-hook 'kill-emacs-query-functions
	(fn_symbol "desktop::try_to_save_desktop_on_emacs_kill"
		(lambda ()
			(if (or
					(eq desktop::save t)
					(let ((desktop_file_exists (file-exists-p desktop::filepath)))
						(or
							(and desktop_file_exists (memq desktop::save '(if-exists ask-if-new)))
							(and
								(or
									(memq desktop::save '(ask ask-if-new))
									(and desktop_file_exists (eq desktop::save 'ask-if-exists)))
								(y-or-n-p "Save desktop?")))))
				(condition-case err
					(desktop::save t)
					(file-error
						(unless (y-or-n-p "Error while saving the desktop. Ignore?")
							(signal (car err) (cdr err))))))
			(desktop::try_to_release_lock)
			t)))

; Certain things should be done even if `kill-emacs-query-functions' are not called.
(add-hook 'kill-emacs-hook #'desktop::try_to_release_lock)

(add-hook 'after-init-hook #'desktop::read)

(provide 'myDesktop)
