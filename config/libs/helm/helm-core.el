; -*- lexical-binding:nil -*-

(unless (eq system-type 'windows-nt) (require 'tramp-archive))

; Add helm completion style.
(cl-pushnew
	'(
		helm
		helm-completion-try-completion
		helm-completion-all-completions
		"helm multi completion style"
	)
	completion-styles-alist
	:test #'equal)

(defvar helm-pattern ""
"The pattern used to update helm buffer.
Don't have any text properties (normally).")
(defvar helm-buffer "*helm*"
"Name of the buffer showing completions.
Let-bound in helm-internal.

It has different values on these 3 different occasions:
	When :resume arg of helm call is \\='noresume, then value is \"*helm-noresume*\".

	In nested sessions, value is
	(concat \"*helm-nested-level-\" (number-to-string (1+ helm-nested-level)) \"*\").

	Otherwise it's this variable's default value.")
(defvar helm-current-buffer nil
"Current buffer when `helm' is invoked.")
(defvar helm-suspend-update-flag nil)
(defvar helm-action-buffer "*helm-action*" "Buffer showing actions.")
(defconst helm-marked-buffer-name "*helm-marked*")
(defvar helm-current-prefix-arg nil
"Record `current-prefix-arg' when exiting minibuffer.")
(defvar helm-default-source nil
"Default source for current helm session or nil.

Local in helm buffers.

Used in some places, e.g. to display mode-line; to show marked candidates;
to toggle some source-local settings like follow mode, match method, etc.

By default, when there is only one source in helm call, it is set as the
default source.
This can be overriden by :helm-default-source arg to helm.
When there are multiple sources and you still want to set one to be the
default one, use :helm-default-source helm arg as well.

`helm-source-mode-line' and `helm-source-header-line' functions of this
source can be called when helm buffer is empty.")
(defvar helm-source-filter nil
"A list of sources to be displayed.
Other sources won't appear in the search results.
If nil, no filtering is done.
Use `helm-set-source-filter' during a helm session to modify it.
Non-nil only in helm-buffer.")
(defvar helm-action-info nil
"Internal, used to pass info about action when exiting helm.
List (source action_fn cand_to_confirm cand_to_action).

cand_to_confirm is passed to confirm function, if any,
It should be in (display . real) or display format, like it was originally
(after candidate-transformer, probably a result of
(helm-withprop-to-cand (helm-get-selection 'withprop))).

cand_to_action is passed to action function, so it should be a real
value of candidate.")
(defvar helm-action-source-and-cand nil
"Non-nil always and only when `helm-in-action-buffer'.
Result of
(cons (helm-get-current-source) (helm-get-selection 'withprop))
evaluated just before showing actions.
When exiting helm by selecting action through helm-action-buffer, this info
is used to call selected action with.
This is only actually needed (I think) for async sources, to protect against
situations when output from process arrives and it moves selection in
helm-buffer, so old selection is lost (the one that we are showing actions for).
So the \"source\" part of this variable may be not needed? I'm not sure, maybe
some error from process can make source different? Idk, it's fine as it is.")
(defvar helm-action-update nil
"Non-nil to run helm-force-update after coming back to helm-buffer
from helm-action-buffer.")
(defvar helm-action-pattern nil
"Store helm-pattern here while showing actions.")
(defvar helm--pattern nil
"Internal, store locally `helm-pattern' value for later use in `helm-resume'.")
(defvar helm-use-default-as-input-pattern nil
"Non-nil only when in this weird state that minibuffer is empty
(helm-pattern = \"\"), but helm-buffer shows results for `default'.
Until next helm-update, store helm-pattern here.

If your `helm' call uses :use-default-as-input arg and there is some
action that can run at that time described above, use
(or helm-use-default-as-input-pattern helm-pattern) instead of helm-pattern.")
(defvar helm-in-persistent-action nil
"Flag whether in persistent-action or not.")
(defvar helm-sources nil
"List of current sources in use.
Globally always nil, local in helm-buffer (or helm-action-buffer).")
(defvar helm-resume-buffers nil
"List of buffers that are available to resume.
They are listed in order of most recently used.")
(defvar helm-candidate-cache (make-hash-table :test 'eq)
"Holds the available candidates within a single helm invocation.")
(defvar helm-match-fold-diacritics nil
"Let-bound for helm matching functions to know
if source wants to fold diacritics.")
(defvar helm-current-source nil)
(defvar helm--local-variables nil)
(defvar helm-split-window-state nil)
(defvar helm--window-side-state nil)
(defvar helm-alive-p nil)
(defvar helm-default-keymap nil
"Nil or buffer-local default keymap for current helm-session.
Probably always should have helm-map as a parent.

Default keymap to use when there is no current source.
Key bindings in such keymap should expect that there will be no
source and selection in helm-buffer.
For example used by helm-ff for bind that toggles between showing and
hiding boring files, to prevent situations where there has been only
boring files and now they are hidden so there is no current source
so helm-ff's keymap is now inactive.")
(defvar helm-default-preselect nil
"Default preselection method for `helm-buffer'.

If arg PRESELECT to `helm-update' is nil, use value of this variable.

Should be used as a buffer-local variable through
:helm-default-preselect arg to `helm'.")
(defvar helm-marked-candidates nil
"Marked candidates. List of overlays with many properties.
See `helm-make-visible-mark' for all of them.")
(defvar helm-update-blacklist-regexps
	'("^" "^ *" "$" "!" " " "\\b"
	"\\<" "\\>" "\\_<" "\\_>" ".*"
	"??" "?*" "*?" "?"))
(defvar helm--force-updating-p nil
"[INTERNAL] Don't use this in your programs.")
(defvar helm--minibuffer-undo-list nil
"Saved `buffer-undo-list' for `helm-resume'.
Local in helm-buffer if non-nil.")
(defvar helm--history nil
"History variable of helm session stored locally in `helm-buffer' for `helm-resume'.
Arg :history of `helm'.")
(defvar helm-exit-status 0
"Flag to inform if helm did exit or quit.
0 means helm did exit when executing an action.
1 means helm did quit with abort-recursive-edit or similar.
Knowing this exit-status could help restore a window config when
helm aborts in some special circumstances.")
(defvar helm-last-frame-or-window-configuration nil
"Used to store window or frame configuration at helm start.")
(defvar helm-onewindow-p nil)
(defvar helm-persistent-action-display-window nil)
(defvar helm-prompt "Pattern: "
"Value of prompt for helm.
Local in helm-buffer.
Default value is the default.")
; Similar to 'ffap-url-regexp'.
(defconst helm--url-regexp
	"\\`\\(news\\(post\\)?:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://\\)")
(defvar helm--action-prompt "Select action: ")
(defvar helm--cycle-resume-iterator nil)
(defvar helm--buffer-in-new-frame-p nil)
(defvar helm-initial-frame nil
"[INTERNAL] The selected frame before starting helm.
helm use this internally to know in which frame it started, don't
modify this yourself.")
(defvar helm--nested nil)
(defconst helm--frame-default-attributes
	'(
		width height tool-bar-lines left top
		title undecorated vertical-scroll-bars
		visibility fullscreen menu-bar-lines undecorated
		alpha foreground-color background-color
	)
"Frame parameters to save in `helm--last-frame-parameters'.")
(defvar helm--last-frame-parameters nil
"Frame parameters to save for later resuming.
Local to `helm-buffer'.")

(defvar helm-selection-overlay nil
"Overlay used to highlight the currently selected candidate.")

(defvar helm-async-processes nil
"List of information about asynchronous processes managed by helm.")

(defvar helm-before-initialize-hook nil
"Runs before helm initialization.
This hook runs before init functions in `helm-sources', which is
before creation of `helm-buffer'. Set local variables for
`helm-buffer' that need a value from `current-buffer' with
`helm-set-local-variable'.")

(defvar helm-after-initialize-hook nil
"Runs after helm initialization.
This hook runs after `helm-buffer' is created but not from `helm-buffer'.
The hook needs to specify in which buffer to run.")

(defvar helm-after-update-hook nil
"Runs after updating the helm buffer with the new input pattern.")

(defvar helm-before-update-hook nil
"Runs before updating the helm buffer with the new input pattern.")

(defvar helm-cleanup-hook nil
"Runs after exiting the minibuffer and before performing an action.

This hook runs even if helm exits the minibuffer abnormally.")

(defvar helm-select-action-hook nil
"Runs when opening the action buffer.")

(defvar helm-before-action-hook nil
"Runs before executing action.
Unlike `helm-cleanup-hook', this hook runs before helm closes the
minibuffer and also before performing an action.")

(defvar helm-after-action-hook nil
"Runs after executing action.")

(defvar helm-exit-minibuffer-hook nil
"Runs just before exiting the minibuffer.

This hook runs only when helm exits the minibuffer normally
(e.g. via candidate selection).")

(defvar helm-window-configuration-hook nil
"Runs when switching to and from the action buffer.
Should run also at end of `helm-display-function'.")

(defvar helm-after-window-setup-hook nil
"Hook run with no args just after creating helm window.
It is run in window of helm-buffer.")

(defvar helm-minibuffer-set-up-hook '(helm-hide-minibuffer-maybe)
"Hook that runs at minibuffer initialization.
A hook useful for modifying minibuffer settings in helm.

Uses `helm-hide-minibuffer-maybe' by default which hide minibuffer contents with
header-line contents when `helm-echo-input-in-header-line' is non nil.")

(defvar helm-autoresize-mode) ; Undefined in `helm-default-display-buffer'.

(defvar helm-quit-hook nil
"A hook that runs when quitting helm.
In helm-current-buffer and its window.")

(defvar helm-inhibit-move-to-first-candidate nil
"Don't move to second candidate if the first one has helm-new text prop.")

; Keymap

(defun helm-copy-candidate (arg)
"Copy selected candidate (force display part with prefix)."
	(interactive "P")
	; If mark is active, copy region like normally.
	(if mark-active
		(clipboard-add (filter-buffer-substring (region-beginning) (region-end)))
		(when-let (
			(selection (helm-get-selection arg))
			((stringp selection))
		)
			(clipboard-add (substring-no-properties selection) helm-buffer))))

(define-key global-map [?\C-g] #'helm-resume)

(defvar helm-map
	(let ((map (make-sparse-keymap)))
		(set-keymap-parent map minibuffer-local-map)
		(define-key map [up] #'helm-previous-line)
		(define-key map [down] #'helm-next-line)
		(define-key map [prior] #'helm-previous-page)
		(define-key map [next] #'helm-next-page)
		(define-key map [A-C-up] #'helm-scroll-down)
		(define-key map [A-C-down] #'helm-scroll-up)
		(define-key map [A-C-S-up] #'helm-scroll-other-window-down)
		(define-key map [A-C-S-down] #'helm-scroll-other-window-up)
		(define-key map [C-home] #'helm-beginning-of-buffer)
		(define-key map [C-end] #'helm-end-of-buffer)
		(define-key map [kp-add]
			(lambda () (interactive)
				(with-helm-window
					(call-interactively #'enlarge_window_horizontal_command))))
		(define-key map [kp-subtract]
			(lambda () (interactive)
				(with-helm-window
					(call-interactively #'enlarge_window_horizontal_neg_command))))
		(define-key map [S-kp-add]
			(lambda () (interactive)
				(with-helm-window
					(call-interactively #'enlarge_window_vertical_command))))
		(define-key map [S-kp-subtract]
			(lambda () (interactive)
				(with-helm-window
					(call-interactively #'enlarge_window_vertical_neg_command))))
		(define-key map [return] #'helm-return)
		(define-key map [tab] #'helm-toggle-visible-mark)
		(define-key map [S-tab]
			(lambda () (interactive)
				(insert-and-inherit ?\t)
				(funcall after_move_hook_fn)))
		(define-key map [C-tab] #'helm-select-action)
		(define-key map [C-S-tab] #'helm-show-marked)
		(define-key map [?\C-x] #'helm-execute-persistent-action)
		(define-key map [?\C-\S-x] #'helm-follow-mode)
		(define-key map [C-S-up] #'helm-previous-source)
		(define-key map [C-S-down] #'helm-next-source)
		(define-key map [A-up] #'helm-prev-visible-mark)
		(define-key map [A-down] #'helm-next-visible-mark)
		(define-key map [?\C-\S-a] #'helm-toggle-all-marks)
		(define-key map [?\A-a] #'helm-show-all-candidates-in-source)
		(define-key map [kp-delete] #'helm-swap-windows)
		(define-key map [S-kp-delete] #'helm-toggle-resplit-window)
		(define-key map [C-S-kp-delete] #'helm-toggle-full-frame)
		(define-key map [kp-space] #'helm-recenter-top-bottom-other-window)
		; Manual refresh.
		(define-key map [f5] #'helm-force-update)
		(define-key map [S-f5] #'helm-suspend-update-toggle)
		(define-key map [?\C-i] #'helm-case-fold-search-toggle)
		(define-key map [?\C-m] #'helm-match-toggle)
		; Insert selected candidate as pattern (force display part with prefix).
		(define-key map [?\C-\s]
			(lambda (arg) (interactive "P")
				(when-let (
					(selection (helm-get-selection arg))
					((stringp selection))
				)
					(helm-set-pattern selection))))
		(define-key map [?\C-c] #'helm-copy-candidate)
		; I guess pasting should work just fine, like normal char insertion.
		; So no need to add helm-specific C-v commands.

		; Toggle `truncate-lines' value in `helm-buffer'.
		(define-key map [?\C-\\]
			(lambda () (interactive)
				(with-helm-buffer
					(setq truncate-lines (not truncate-lines))
					(helm-force-update)
					(message "Truncate lines %s." (if truncate-lines "on" "off")))))

		; Bind keys from C-1 to C-9 to 'helm-select-nth-action'.
		(dotimes (i 9)
			(define-key map (vector (+ ?\C-1 i))
				`(lambda () (interactive) (helm-select-nth-action ,i))))
		map)
"Keymap used as a minor mode's keymap in the minibuffer for helm.")

(defconst helm-local-map
	(let ((map (make-sparse-keymap)))
		; To not select helm-window and also to not move to last buffer line (empty),
		; or to source header lines.
		; Selecting candidates will happen through local 'keymap text props.
		; Idk if there is a better way to ignore mouse events.
		(mapc
			(lambda (key_modifier)
				(dolist (multi_modifier '("" "double-" "triple-"))
					; Normally used mouse-i are [1, 7].
					(dotimes (i 7)
						(mouse::bind_in_fringes
							map
							(vector
								(intern
									(concat
										key_modifier
										multi_modifier
										"down-mouse-"
										(number-to-string (1+ i)))))
							#'ignore))))
			KEY_MODIFIER_VECTOR)
		; Scrolling.
		(define-key map [wheel-up] #'helm-scroll-down)
		(define-key map [wheel-down] #'helm-scroll-up)
		(define-key map [C-wheel-up] #'helm-previous-page)
		(define-key map [C-wheel-down] #'helm-next-page)
		map)
"Local map of helm-buffer and helm-action-buffer.

Ignore mouse events in helm buffer - handling of those is done through
keymap text property where appropriate.

Also adjusted mouse scrolling commands.")

(defconst helm::password_reading_fn_list
	nil ; '(tramp-read-passwd ange-ftp-get-passwd epa-passphrase-callback-function)
"List of functions (symbols) to advice-add :around when helm is running,
to suspend helm while reading password.")

(defconst helm-scroll-amount 3
"Scroll amount when scrolling helm window or other window in a helm session.
It is used by `helm-scroll-other-window', `helm-scroll-up', `helm-scroll-down'
and `helm-scroll-other-window-down'.

If you prefer scrolling line by line, set this value to 1.")

(defconst helm-scroll-margin 2
"`scroll-margin' to use for helm window.
Set to 0 to disable.")

(defconst helm-left-margin-width 0
"`left-margin-width' value for the `helm-buffer'.")

(defvar helm-candidate-number-limit 200
"Global limit for number of candidates displayed.
When the pattern is empty, the number of candidates shown will be
as set here instead of the entire list, which may be hundreds or
thousands. Since narrowing and filtering rapidly reduces
available candidates, having a small list will keep the interface
responsive.

Set this value to nil for no limit.")

(defconst helm-input-idle-delay 0.25
"Idle time before updating, in seconds, when helm-delay is non-nil.
Must be a number > 0.")

(defconst helm-follow-input-idle-delay 'instant
"`helm-follow-mode' will execute its persistent action after this delay.
Can also be symbol 'instant, then execute it instantly.
Note that if the `follow-delay' attr is present in source, it will take
precedence over this.
Nil to use value of 'helm-input-idle-delay'.")

(defvar helm-full-frame nil
"Use current window for showing candidates.
If non-nil, then helm does not pop-up a new window.")

(defconst helm-candidate-separator
	(propertize
		(if (fontp (char-displayable-p ?\x2015))
			"――――――――――――――――――――――――――――――――――――――――――――――――――\n"
			"-------------------------------------------------------------------------------------\n")
		'face 'helm-separator)
"Candidates separator (a string) of `multiline' source. Must end with a newline.")

(defvar helm-case-fold-search 'smart
"Adds \\='smart' option to `case-fold-search'.
Smart option ignores case for searches as long as there are no
upper case characters in the pattern.

Otherwise t or nil, like `case-fold-search'.

Default is smart.

Use `helm-case-fold-search-toggle' to change it (temporarily) while in helm.")

(defconst helm-file-name-case-fold-search helm-case-fold-search
"Local setting of `helm-case-fold-search' for reading filenames.
See `helm-case-fold-search' for more info.")

(defconst helm-file-globstar t
"Same as globstar bash shopt option.
When non-nil a pattern beginning with two stars will expand recursively.
Directories expansion is not supported yet.")

(defconst helm-save-configuration-functions
	'(set-window-configuration . current-window-configuration)
"Functions used to restore or save configurations for frames and windows.
Specified as a pair of functions, where car is the restore
function and cdr is the save function.

To save and restore frame configuration, set this variable to
'(set-frame-configuration . current-frame-configuration)

NOTE: This may not work properly with own-frame minibuffer
settings. Older versions saves/restores frame configuration, but
the default has changed now to avoid flickering.")

(defvar helm-display-function #'helm-default-display-buffer
"Function used to display `helm-buffer'.

Local value in `helm-buffer' will take precedence over this default value.
If no local value is found use this default value.

To set it locally to `helm-buffer' in helm sources use
`helm-set-local-variable' in init function or use
:helm-display-function slot in `helm' call.")

(defconst helm::split_window_side 'below
"Arg for split-window.
Used when splitting sole window of a frame.
Also used to choose window for helm, when frame has more than one window.")

(defconst helm::default_window_height nil
"Initial height of `helm-buffer', specified as an integer or nil.
Used only when splitting sole window vertically.")

(defconst helm::default_window_width nil
"Initial height of `helm-buffer', specified as an integer or nil.
Used only when splitting sole window horizontally.")

(defconst helm::buffer_width_in_own_frame 72
"Frame width when displaying helm-buffer in own frame.
Must be int.")

(defconst helm::buffer_height_in_own_frame 20
"Frame height when displaying helm-buffer in own frame.
Must be int.")

(defconst helm-prevent-escaping-from-minibuffer t
"Prevent escaping from minibuffer with `other-window' during the helm session.")

(defvar helm-current-buffer-mode-line-face 'mode-line-active
"Nil or face to use for mode-line of `helm-current-buffer'.
It's trivial to add the same for tab-line or header-line.
Can be local in helm-current-buffer to customize it per buffer.")

(defconst helm-move-to-line-cycle-in-source nil
"Cycle to the beginning or end of the list after reaching the bottom or top.
This applies when using `helm-next/previous-line'.")

(defconst helm-autoresize-max-height 40
"Specify maximum height and defaults to percent of helm window's frame height.

If nil, `window-min-width' is used.
See `fit-window-to-buffer' for more infos.")

(defconst helm-autoresize-min-height 10
"Specify minimum height and defaults to percent of helm window's frame height.

If nil, `window-min-height' is used.
See `fit-window-to-buffer' for details.")

(defconst helm-echo-input-in-header-line nil
"Send current input to header-line when non-nil.")

(defconst helm-header-line-space-before-prompt 'left-fringe
"Specify the space before prompt in header-line.

This will be used when `helm-echo-input-in-header-line' is
non-nil.

Value can be one of the symbols \\='left-fringe or \\='left-margin or
an integer specifying the number of spaces before prompt.  Note
that on input longer that `window-width' the continuation string
will be shown on left side of window without taking care of this.")

(defconst helm-tramp-connection-min-time-diff 5
"Value of `tramp-connection-min-time-diff' for helm remote processes.
If set to zero helm remote processes are not delayed.

Setting this to a value less than 5 or disabling it with a zero
value is risky, however on Emacs versions starting at 24.5 it
seems it is now possible to disable it.

Anyway at any time in helm you can suspend your processes while
typing by hitting \\<helm-map> `\\[helm-suspend-update-toggle]'.

Only async sources than use a sentinel calling
`helm-process-deferred-sentinel-hook' are affected by this.")

(defconst helm-actions-inherit-frame-settings t
"Actions inherit helm frame settings of initial command when non nil.")

(defconst helm::use_undecorated_frame t
"Display helm frame undecorated when non nil.")

(defconst helm-frame-background-color nil
"Background color for helm frames, a string.
Fallback to default face background when nil.")

(defconst helm-frame-foreground-color nil
"Foreground color for helm frames, a string.
Fallback to default face foreground when nil")

(defconst helm-frame-alpha nil
"Alpha parameter for helm frames, an integer.
Fallback to 100 when nil.")

(defconst helm::use_frame_if_more_than_n_windows 4
"Display helm buffer in own frame when more than n windows.
Nil to disable, integer otherwise.")

(defconst helm::use_frame_if_started_from_dedicated_window nil
"Display helm buffer in own frame when helm is started from a dedicated window.")

(defconst helm-truncate-lines t
"The value of `truncate-lines' when helm starts.")

(defconst helm-visible-mark-prefix ""
"Prefix used in margin for marked candidates.
Set this to an empty string if you don't want prefix in margin when marking.")

(defconst helm-async-status-finished-message
	(propertize "finished" 'face '(:foreground "green"))
"Message in mode-line when external process finished.")

(defconst helm-async-status-error-message
	(propertize "error" 'face '(:foreground "red"))
"Message in mode-line when external process send something to stderr.")

; Faces

(define_face_with_default_height 'helm-source-header
	'(
		:extend t
		:family "Segoe UI Short"
		:background "#080052"
		:foreground "#cdd2d2"
		:weight bold
	))

(define_face 'helm-header '((t :inherit header-line)))

(define_face 'helm-candidate-number '((t :background "green yellow" :foreground "black"))
"For candidate number in mode-line.")

(define_face 'helm-candidate-number-suspended
	'((t :inherit helm-candidate-number :inverse-video t))
"For candidate number in mode-line when helm is suspended.")

(define_face 'helm-visible-mark '((t :extend t :background "black")))
; :underline (:color "grey70" :position 2))))

(define_face 'helm-selection
	'((t :extend t :background "#113A5C" :distant-foreground "black"))
"Face for currently selected item in the helm buffer.")

(define_face 'helm-selection-marked '((t :extend t :background "#071a29"))
"Face for currently selected marked item in the helm buffer.")

(define_face 'helm-separator '((t :extend t :foreground "red"))
"Face for multiline source separator.")

(define_face 'helm-action '((t))
"Face for action lines in the helm action buffer.")

(define_face 'helm-prefarg '((t :foreground "green"))
"Face for showing prefix arg in mode-line.")

(define_face 'helm-match '((t :foreground "gold1"))
"Face used to highlight matches.")

(define_face 'helm-header-line-left-margin '((t :foreground "black" :background "yellow"))
"Face used to highlight helm-header sign in left-margin.
This face is used only when using `helm-echo-input-in-header-line' and pattern
is wider than screen.")

(define_face 'helm-minibuffer-prompt '((t :inherit minibuffer-prompt))
"Face used for the minibuffer/headline prompt (such as \"Pattern: \") in helm.")

(define_face 'helm-eob-line '((t))
"Face for empty line at end of sources in the helm buffer.
Allow specifying the height of this line.")


(defun helm-set-pattern (pattern &optional noupdate)
"Set minibuffer contents to PATTERN.
If optional NOUPDATE is non-nil, the helm buffer is not changed."
	(with-selected-window (or (active-minibuffer-window) (minibuffer-window))
		(delete-minibuffer-contents)
		(insert pattern))
	(when noupdate (setq helm-pattern pattern)))

(defun helm-get-display (cand) (if (consp cand) (car cand) cand))
(defun helm-get-real (cand) (if (consp cand) (cdr cand) cand))

(defun helm-withprop-to-cand (withprop)
"Return candidate in format (display . real) if WITHPROP has a helm-real prop,
else WITHPROP.
WITHPROP should probably be (helm-get-selection \\='withprop)."
	(if-let ((real (get-text-property 0 'helm-real withprop)))
		(cons withprop real)
		withprop))

(defun helm-empty-buffer-p () "Check if helm-buffer has candidates."
	(is_buffer_empty (get-buffer helm-buffer)))

(defun helm-apply-fn-from-source (source fn &rest args)
"From source apply fn on args."
	(let ((helm-current-source source))
		(apply fn args)))

(defun helm-get-name (source)
	(let ((name (helm-source-name source)))
		(if (stringp name) name (helm-apply-fn-from-source source name))))

(defun helm-in-action-buffer () "Non-nil if in `helm-action-buffer'."
	; It used to be (get-buffer helm-action-buffer), but now this should be enough.
	helm-action-source-and-cand)

(defun helm-buffer-get ()
"Return `helm-action-buffer' if shown, else `helm-buffer'."
	(if (helm-in-action-buffer) helm-action-buffer helm-buffer))

(defmacro with-helm-buffer (&rest body) "Eval BODY inside `helm-buffer'."
	(declare (debug t))
	`(with-current-buffer (helm-buffer-get) ,@body))

(defun helm-window () "Window of `helm-buffer'."
	(get-buffer-window (helm-buffer-get) 0))

(defmacro with-helm-window (&rest body) "Eval BODY inside `helm-window'."
	(declare (debug t))
	`(with-selected-window (helm-window) ,@body))

(defmacro helm-with-window-or-buffer (&rest body)
"Eval BODY inside `helm-window' or `helm-buffer'."
	(declare (debug t))
	(let ((window (make-symbol "")))
		`(if-let ((,window (helm-window)))
			(with-selected-window ,window ,@body)
			(with-helm-buffer ,@body))))

(defmacro with-helm-default-directory (directory &rest body)
	(declare (debug t))
	`(let ((default-directory (or ,directory default-directory)))
		,@body))

(defun helm-default-directory ()
"Return the local value of `default-directory' in `helm-buffer'."
	(buffer-local-value 'default-directory (get-buffer helm-buffer)))

(defun helm-set-local-variable (&rest args)
"Bind each pair in ARGS locally to `helm-buffer'.

Use this to set local vars before calling helm.

When used from an init or update function
\(i.e. when `helm-force-update' is running) the variables are set
using `make-local-variable' within the `helm-buffer'.

Usage: helm-set-local-variable ([VAR VALUE]...)
Just like `setq' except that the vars are not set sequentially.
IOW Don't use VALUE of previous VAR to set the VALUE of next VAR.

\(fn VAR VALUE ...)"
	(if helm--force-updating-p
		(with-helm-buffer
			(cl-loop
				for i on args by #'cddr
				do (set (make-local-variable (car i)) (nth 1 i))))
		(setq helm--local-variables
			(nconc
				(cl-loop
					for i on args by #'cddr
					collect (cons (car i) (cadr i)))
				helm--local-variables))))

(defun helm::call_after_next_update (fn)
"Call 'fn' after next helm update using 'helm-after-update-hook'.
'fn' doesn't need to be a symbol."
	(let (
		(after_update_hook (make-symbol "helm::call_after_next_update"))
		(cleanup_hook (make-symbol "helm::call_after_next_update"))
	)
		(fset cleanup_hook
			`(lambda ()
				(remove-hook 'helm-after-update-hook ',after_update_hook)
				(remove-hook 'helm-cleanup-hook ',cleanup_hook)))
		(fset after_update_hook `(lambda () (,cleanup_hook) (,fn)))
		(add-hook 'helm-after-update-hook after_update_hook)
		(add-hook 'helm-cleanup-hook cleanup_hook))
	nil)

; Source filter

(defun helm-set-source-filter (sources)
"Set the value of `helm-source-filter' to SOURCES and update.

This function sets a filter for helm sources and it may be called
while helm is running. It can be used to toggle displaying of
sources dynamically."
	(unless (helm-in-action-buffer)
		(with-current-buffer helm-buffer (setq-local helm-source-filter sources))
		; No need to clear cache - number of candidates shouldn't affect
		; cache in any way.
		(helm-force-update)))

(defun helm-show-all-candidates-in-source (arg)
"Toggle all or only candidate-number-limit cands in current source.
With a numeric prefix arg show only the ARG number of candidates.
The prefix arg has no effect when toggling to only candidate-number-limit."
	(interactive "p")
	(unless (helm-in-action-buffer)
		(with-current-buffer helm-buffer
			(if helm-source-filter
				(progn
					(kill-local-variable 'helm-candidate-number-limit)
					(kill-local-variable 'helm-source-filter))
				(setq-local helm-candidate-number-limit (when (> arg 1) arg))
				(when-let ((source (or (helm-get-current-source) helm-default-source)))
					(setq-local helm-source-filter (list source)))))
		(helm-force-update)))


(defun helm-update-window-margins ()
	(set-window-margins nil
		(if helm-marked-candidates
			(+
				(string-width helm-visible-mark-prefix)
				helm-left-margin-width)
			helm-left-margin-width)))

(defun helm-maybe-add-new-candidate (candidates &optional prefix_string)
	(or
		(string= helm-pattern "")
		(when-let ((first_cand (car candidates)))
			(equal helm-pattern (if (consp first_cand) (cdr first_cand) first_cand)))
		(push
			(cons
				; This doesn't need 'match-part prop because it is only used after
				; matching.
				(propertize_no_copy
					(concat
						(propertize_no_copy (or prefix_string "[+]") 'face 'helm-prefix)
						" "
						(propertize helm-pattern 'face 'helm-match))
					'helm-new t)
				helm-pattern)
			candidates))
	candidates)

; Source info.

(defun helm-get-selection (&optional display)
"Return the currently selected candidate from helm-buffer or helm-action-buffer.

DISPLAY:
	nil - return real part of candidate,
	\\='withprop - use buffer-substring to get candidate from buffer,
	other non-nil - use buffer-substring-no-properties to get candidate from buffer.

Note that when DISPLAY is non-nil, it takes precedence over
helm-source-display-to-real."
	(helm-with-window-or-buffer
		(unless (is_buffer_empty)
			(if-let (
				(get_substring_fn
					(cond
						((eq display 'withprop) #'buffer-substring)
						(display #'buffer-substring-no-properties)))
			)
				(funcall get_substring_fn
					(point) (1- (save-excursion (helm-goto-candidate-end) (point))))
				(or
					(get-text-property (point) 'helm-real)
					(let (
						(cand
							(buffer-substring-no-properties
								(point)
								(1- (save-excursion (helm-goto-candidate-end) (point)))))
						(source (helm-get-source))
					)
						(if-let ((display_to_real (helm-source-display-to-real source)))
							(helm-apply-fn-from-source
								source
								display_to_real
								cand)
							cand)))))))

(defun helm-get-regex-for-preselection (&optional display)
	(concat "^" (regexp-quote (or display (helm-get-selection t))) "$"))

(defun helm-get-actions-from-source (source)
"Return the associated action for the selected candidate.
It is a function (sole action) or list of (display_string . function).
Selection must be valid."
	(let (
		(actions (helm-source-action source))
		(action-transformer (helm-source-action-transformer source))
	)
		(if action-transformer
			(helm-apply-fn-from-source source action-transformer actions)
			actions)))

(defun helm-get-source () (get-text-property (point) 'helm-source))

(defun helm-get-current-source ()
"Return the source for the current selection.
Return nil when `helm-buffer' is empty."
	(or helm-current-source (helm-with-window-or-buffer (helm-get-source))))

(defun helm-make-action-command-base (action)
	(setq helm-action-info
		(let ((cand (helm-withprop-to-cand (helm-get-selection 'withprop))))
			(list
				(helm-get-current-source)
				action
				cand
				(helm-get-real cand))))
	(helm-maybe-exit-minibuffer))

(defun helm-make-action-command-phantom-base (action phantom_cand source)
	(setq helm-action-info (list source action phantom_cand phantom_cand))
	(helm-maybe-exit-minibuffer))

(defun helm-make-action-command (action)
"Return an interactive lambda, running ACTION similar to how `helm-return'
runs actions. The difference is only if this will be bound in the default keymap -
this can be run with nil as candidate arg and helm-current-source nil."
	`(lambda () (interactive) (helm-make-action-command-base #',action)))

(defun helm-make-action-command-phantom (action get_phantom_cand_fn get_source_fn)
"Like `helm-make-action-command' but use return value of GET_PHANTOM_CAND_FN instead
of selection and GET_SOURCE_FN instead of (helm-get-current-source).

GET_PHANTOM_CAND_FN is a function with no args.
It can return anything, it isn't processed in any way, just passed to ACTION.

GET_SOURCE_FN is a function with no args, called when returned lambda is called.

This function can be used to make commands exiting helm with arbitrary values.
This was created originally for helm-read-file-name, so it can return
`helm-ff-default-directory' when user pressed [C-return].

Remember that phantom cand can be passed to `helm-source-confirm' function
of source returned by GET_SOURCE_FN, so `helm-source-confirm' should handle
such cases."
	`(lambda () (interactive)
		(helm-make-action-command-phantom-base
			#',action (,get_phantom_cand_fn) (,get_source_fn))))

(defun helm-return (&optional no_confirm)
"Exit helm with current selection and default action for it.
It in the helm-action-buffer, use selection saved when entering
helm-action-buffer and execute selected action.

Don't do any of that, if there is no current selection or helm is updating.

Ask for confirmation for new candidates if helm-source-confirm of current source
is non-nil and NO_CONFIRM is nil."
	(interactive)
	(if (helm-in-action-buffer)
		(if-let ((action (helm-get-selection)))
			(let ((cand (helm-withprop-to-cand (cdr helm-action-source-and-cand))))
				(setq helm-action-info
					(list
						(car helm-action-source-and-cand)
						action
						cand
						(helm-get-real cand)))
				(helm-maybe-exit-minibuffer no_confirm))
			(message "Nothing is selected."))
		(if-let ((source (helm-get-current-source)))
			(if-let (
				(action
					(let ((action (helm-get-actions-from-source source)))
						(if (functionp action) action (cdr (car action)))))
			)
				(let ((cand (helm-withprop-to-cand (helm-get-selection 'withprop))))
					(setq helm-action-info
						(list
							source
							action
							cand
							(helm-get-real cand)))
					(helm-maybe-exit-minibuffer no_confirm))
				(message "No actions available."))
			(message "Nothing is selected."))))

; Select action by index.
(defun helm-select-nth-action (n)
"Select the N nth action for the currently selected candidate.
First action has index 0."
	(let (
		(get_action
			(lambda (actions)
				(cond
					((and (= 0 n) (functionp actions)) actions)
					((and (not (functionp actions)) (cdr (nth n actions))))
					(t (message "No such action.") nil))))
	)
		(if (helm-in-action-buffer)
			; Select action from absolute index, even if helm-action-buffer
			; is now empty (because of helm-pattern not matching anything).
			; There are a couple ways to handle this, this is just a random
			; somewhat reasonable and useful one.
			(when-let ((action (funcall get_action helm-action-alist)))
				(let ((cand (helm-withprop-to-cand (cdr helm-action-source-and-cand))))
					(setq helm-action-info
						(list
							(car helm-action-source-and-cand)
							action
							cand
							(helm-get-real cand))))
				(helm-maybe-exit-minibuffer))
			(if-let ((source (helm-get-current-source)))
				(when-let (
					(action (funcall get_action (helm-get-actions-from-source source)))
				)
					(setq helm-action-info
						(let (
							(cand
								(helm-withprop-to-cand
									(helm-get-selection 'withprop)))
						)
							(list
								source
								action
								cand
								(helm-get-real cand))))
					(helm-maybe-exit-minibuffer))
				(message "Nothing is selected.")))))

(defun helm-exit-minibuffer () (interactive)
	(with-current-buffer helm-buffer
		; Ensure next action use same display function as initial helm-buffer
		; when helm-actions-inherit-frame-settings is non-nil.
		(when (and helm-actions-inherit-frame-settings helm--buffer-in-new-frame-p)
			(push (cons 'helm-display-function helm-display-function)
				helm--local-variables)
			(push (cons 'helm--last-frame-parameters (helm--get-frame-parameters))
				helm--local-variables)
			; The helm-buffer keeps `helm-display-function' and
			; `helm--get-frame-parameters' values during 0.5 seconds, just
			; the time to execute the possible helm action with those values.
			; If no `helm' calling action runs within 0.5 seconds, the next helm
			; session will have to resolve again those variable values.
			;
			; Ok this is pretty crappy solution but I don't use it so I'll leave
			; it like this for now.
			(run-with-idle-timer 0.5 nil
				(lambda ()
					(helm-set-local-variable
						'helm-display-function nil
						'helm--last-frame-parameters nil)))))
	(unless helm-current-prefix-arg (setq helm-current-prefix-arg current-prefix-arg))
	(setq helm-exit-status 0)
	(run-hooks 'helm-exit-minibuffer-hook)
	(exit-minibuffer))

(defun helm-maybe-exit-minibuffer (&optional no_confirm)
"Maybe exit current helm session and execute action based on info in
helm-action-info.

Never ask for confirmation if NO_CONFIRM is non-nil."
	(if
		; When using delayed input handling, helm-timer hasn't
		; yet responded to helm-pattern change.
		(string= (minibuffer-contents-no-properties) helm-pattern)
		; Ask for confirmation or exit helm.
		(let (confirm)
			(if
				(or
					no_confirm
					(not
						(when-let (
							(source (car helm-action-info))
							((setq confirm (helm-source-confirm source)))
						)
							(if (functionp confirm)
								(setq confirm
									(helm-apply-fn-from-source
										source
										confirm
										; Candidate (display . real) or display.
										(nth 2 helm-action-info)
										(nth 1 helm-action-info))) ; Action.
								(get-text-property
									0
									'helm-new
									(helm-get-display (nth 2 helm-action-info)))))))
				(helm-exit-minibuffer)
				(message "%s" (if (stringp confirm) confirm "confirm"))
				(set-transient-map
					(let ((keymap (make-sparse-keymap)))
						; Forward prefix arg.
						(setq prefix-arg current-prefix-arg)
						; Always allow confirming with [return].
						(define-key keymap [return] #'helm-exit-minibuffer)
						; Also add keys that invoked this command.
						(define-key keymap (this-command-keys-vector)
							#'helm-exit-minibuffer)
						keymap))))
		(message "Display not ready")
		(sit-for 0.5)
		(message nil)
		(helm-update)))


(defun helm--get-frame-parameters ()
	(cl-loop
		with params = (frame-parameters)
		for p in helm--frame-default-attributes
		for parameter_cell = (assq p params)
		when parameter_cell collect parameter_cell))

(defun helm-reenable-update ()
"Only to run in a timer - otherwise post-command-hook
will take care of updating."
	(when (and helm-alive-p helm-suspend-update-flag)
		(setq helm-suspend-update-flag nil)
		(helm-update-after-input)))

; Movement

(defun helm-get-index () (get-text-property (point) 'helm-index))

(defun helm-goto-candidate-start ()
"Move before the first char of candidate at point, multiline or not."
	(let ((index (helm-get-index)))
		(while
			(progn
				(goto-char (pos-eol 0))
				(eq index (helm-get-index)))))
	(forward-char 1)
	nil)

(defun helm-goto-candidate-end ()
"Move past the last newline of candidate at point, multiline or not."
	(let ((index (helm-get-index)))
		(while
			(progn
				(forward-line 1)
				(eq index (helm-get-index))))))

(defun helm-goto-candidate-base (arg)
"There must be at least 1 candidate line after/before point."
	(let (
		(index (or (helm-get-index) -1)) ; Never match if no candidate at point.
		(source (helm-get-source))
		current_index
	)
		(while
			(progn
				(forward-line arg)
				(or
					(not (setq current_index (helm-get-index)))
					; eq source because there can be 2 sources back to back with
					; only 1 candidate each, so we would skip the second one.
					(and (= index current_index) (eq source (helm-get-source))))))))

(defun helm-goto-next-candidate ()
"There must be at least 1 candidate line after point.
Leave point at the beginning of candidate."
	(helm-goto-candidate-base 1))

(defun helm-goto-prev-candidate ()
"There must be at least 1 candidate line before point.
Leave point at the beginning of last line of candidate."
	(helm-goto-candidate-base -1))

(defun helm-on-header () (get-text-property (point) 'helm-header))

(defun helm-get-next-header-pos ()
"Return the position of the next header from point or nil."
	(save-excursion
		(while (not (or (helm-on-header) (eobp))) (forward-line 1))
		(unless (eobp) (point))))

(defun helm-goto-first-candidate () "Goto first candidate in current source."
	(while (not (helm-on-header)) (goto-char (pos-eol 0)))
	(helm-goto-next-candidate)
	nil)

(defun helm-move-selection (where direction &optional no-follow)
"Move the selection marker to a new position.
Position is determined by WHERE and DIRECTION.
WHERE can be one of:
 - line
 - page
 - edge
 - source
DIRECTION can be one of:
 - previous
 - next
no-follow non-nil means to not execute persistent action after move,
even if follow mode is on, source has persistent action, etc."
	(when-let ((window (helm-window)))
		(with-selected-window window
			(unless (is_buffer_empty)
				(let ((pos_before_move (point)))
					(cl-case where
						(line
							(if (eq direction 'previous)
								(if (= (helm-get-index) 0)
									(if helm-move-to-line-cycle-in-source
										(progn
											(goto-char
												(or
													(helm-get-next-header-pos)
													(point-max)))
											(helm-goto-prev-candidate)
											(helm-goto-candidate-start))
										(while
											(progn
												(forward-line -1)
												(not (or (helm-get-index) (bobp)))))
										(if (bobp)
											(goto-char pos_before_move)
											(helm-goto-candidate-start)))
									(helm-goto-prev-candidate)
									(helm-goto-candidate-start))
								(helm-goto-candidate-end)
								(if helm-move-to-line-cycle-in-source
									(progn
										(while
											(not
												(or
													(helm-get-index)
													(helm-on-header)
													(eobp)))
											(forward-line 1))
										(unless (helm-get-index)
											; Next cand wasn't found.
											(goto-char pos_before_move)
											; Move to the first cand in source.
											(helm-goto-first-candidate)))
									(while (not (or (helm-get-index) (eobp)))
										(forward-line 1))
									(when (eobp) (goto-char pos_before_move)))))
						(page
							(if (eq direction 'next)
								(progn
									(ignore-error end-of-buffer
										(scroll-up helm-scroll-amount))
									(when (/= (point) pos_before_move)
										(goto-char (pos-bol)) ; Probably useless.
										(let ((pos_after_scroll (point)))
											(if (helm-get-index)
												(when
													(/=
														(point)
														(save-excursion
															(helm-goto-candidate-start)
															(point)))
													(helm-goto-candidate-end)
													(while (not (or (helm-get-index) (eobp)))
														(forward-line 1))
													(when (eobp)
														(goto-char pos_after_scroll)
														(helm-goto-candidate-start)))
												(while (not (or (helm-get-index) (eobp)))
													(forward-line 1))
												(when (eobp)
													(goto-char pos_after_scroll)
													(helm-goto-prev-candidate)
													; Reminder that this doesn't work
													; correctly with scroll-margin -
													; at end-of-buffer we can end up
													; inside top scroll margin.
													; This needs to be fixed in C,
													; then this should work properly.
													(helm-goto-candidate-start))))))
								(ignore-error beginning-of-buffer
									(scroll-down helm-scroll-amount))
								(when (/= (point) pos_before_move)
									(goto-char (pos-bol)) ; Probably useless.
									(let ((pos_after_scroll (point)))
										(while (not (or (helm-get-index) (bobp)))
											(forward-line -1))
										(when (bobp)
											(goto-char pos_after_scroll)
											(helm-goto-prev-candidate)))
									(helm-goto-candidate-start))))
						(edge
							(if (eq direction 'next)
								(progn
									(goto-char (point-max))
									(helm-goto-prev-candidate)
									(helm-goto-candidate-start))
								(goto-char (point-min))
								(helm-goto-next-candidate)))
						(t ; source
							(if (eq direction 'next)
								(progn
									; Cycle to the first source if this is the last one.
									(goto-char (or (helm-get-next-header-pos) (point-min)))
									(helm-goto-next-candidate))
								; Cycle to the last source if this is the first one.
								(when (= (helm-get-index) 0)
									(while
										(progn
											(forward-line -1)
											(not (or (helm-get-index) (bobp)))))
									(when (bobp) (goto-char (point-max))))
								(helm-goto-first-candidate))))
					; Don't run persistent action if moved in place.
					(when (/= (point) pos_before_move)
						(helm-select-line)
						(unless no-follow
							(helm-follow-execute-persistent-action-maybe))))))))

(defun helm-next-of-previous-line-base (direction arg)
	; Be sure to not use this in non-interactives calls.
	(let ((helm-move-to-line-cycle-in-source (and helm-move-to-line-cycle-in-source arg)))
		(if (and arg (> arg 1))
			(dotimes (
				_
				(min
					arg
					(with-helm-window
						(let ((pos (helm-get-index)))
							(if (eq direction 'next)
								(- (helm-get-candidate-number t) pos 1)
								pos))))
			)
				(helm-move-selection 'line direction))
			(helm-move-selection 'line direction))))

(defun helm-next-line (&optional arg)
"Move selection to the next ARG line(s).
When numeric prefix arg is > than the number of candidates, then move
to the last candidate of current source (i.e. don't move to next source)."
	(interactive "p")
	(helm-next-of-previous-line-base 'next arg))

(defun helm-previous-line (&optional arg)
"Move selection to the ARG previous line(s).
Same behavior as `helm-next-line' when called with a numeric prefix arg."
	(interactive "p")
	(helm-next-of-previous-line-base 'previous arg))

(defun helm-scroll-up () "Scroll up helm-buffer by `helm-scroll-amount' lines."
	(interactive)
	(helm-move-selection 'page 'next))

(defun helm-scroll-down () "Scroll down helm-buffer by `helm-scroll-amount' lines."
	(interactive)
	(helm-move-selection 'page 'previous))

(defun helm-next-page () "Move selection forward with a pageful." (interactive)
	(let (helm-scroll-amount) (helm-move-selection 'page 'next)))

(defun helm-previous-page () "Move selection back with a pageful." (interactive)
	(let (helm-scroll-amount) (helm-move-selection 'page 'previous)))

(defun helm-next-source () "Move selection to the next source." (interactive)
	(helm-move-selection 'source 'next))

(defun helm-previous-source () "Move selection to the previous source." (interactive)
	(helm-move-selection 'source 'previous))

(defun helm-beginning-of-buffer () "Move selection to the top." (interactive)
	(helm-move-selection 'edge 'previous))

(defun helm-end-of-buffer () "Move selection to the bottom." (interactive)
	(helm-move-selection 'edge 'next))

(defun helm-get-candidate-number (&optional in-current-source)
"Return candidates number in `helm-buffer'.
If IN-CURRENT-SOURCE, return the number of candidates of current source only.
This should be called from `helm-window'."
	(cond
		((is_buffer_empty) 0)
		; We should be on a candidate line.
		(in-current-source
			(save-excursion
				(goto-char (or (helm-get-next-header-pos) (point-max)))
				(helm-goto-prev-candidate)
				(1+ (helm-get-index))))
		(t
			(save-excursion
				(goto-char (point-min))
				(let ((count 0) header_pos)
					(while
						(progn
							(helm-goto-next-candidate)
							(setq header_pos (helm-get-next-header-pos)))
						(goto-char header_pos)
						(helm-goto-prev-candidate)
						(+= count (helm-get-index) 1)
						(goto-char header_pos))
					(goto-char (point-max))
					(helm-goto-prev-candidate)
					(+ count (helm-get-index) 1))))))

; Async processes

(cl-defstruct
	(helm-async-process
		(:copier nil)
		(:constructor helm-async-process--make)
		(:predicate nil))
	process source pattern case-fold item-count incomplete-line)

(defun helm-async-process-make (process source pattern case-fold)
	(push
		(helm-async-process--make
			:process process :source source :pattern pattern :case-fold case-fold)
		helm-async-processes))

(defun helm-kill-async-process (process)
"Stop output from `helm-async-process-filter' and kill associated PROCESS."
	(set-process-filter process nil)
	(delete-process process))

(defun helm-kill-async-processes ()
"Kill all asynchronous processes registered in `helm-async-processes'."
	(while helm-async-processes
		(helm-kill-async-process
			(helm-async-process-process (pop helm-async-processes)))))

(defun helm-get-process-info (process)
"Return process info (helm-async-process object) for helm managed PROCESS,
or nil if helm doesn't manage this process.

This is important for async sources to use this function in their sentinels
to check if this sentinel call is this weird and very rare one, that happens when:
process is running, user input happens that causes `helm-update' (e.g. typed a letter
in minibuffer, so emacs is responding to that - process sentinels are
not called until we exit this responding to input state) and somewhere in this input
responding state, but before calling `helm-kill-async-processes' in `helm-update'
(because `helm-kill-async-processes' calls `delete-process' which calls sentinels
even when emacs is responding to input), emacs receives info that process exited,
so this process is put in a special state where it waits for a non input-responding-state
to call its sentinel and even `delete-process' doesn't call this sentinel, so this
sentinel is called after entire `helm-update' has run, so probably a new process
has been started by helm-source-candidates, so doing anything by the original
process' sentinel is a waste, and can even cause problems like overriding mode-line
info, or much more.
This is probably not a problem for helm calls using delayed input handling,
because `helm-update' is called in idle-timer, hmm or maybe it's still a problem,
but it's rarer because there is almost no time to put
process in that state, but maybe it's still possible, idk.
It's safer to check this in every sentinel I think.
So use something like
(when (and (string= event \"finished\\n\") (helm-get-process-info process)) ...)
to don't do anything in these situations.
Setting process sentinel in `helm-kill-async-process' to nil is not a good solution,
because some special sentinels want to reset some caches in theirs sentinels,
in these exact situations."
	(find_in_list
		helm-async-processes
		(lambda (process_info) (eq process (helm-async-process-process process_info)))))

(defun helm-async-after-update ()
	(when-let ((window (get-buffer-window helm-buffer 'visible)))
		(with-selected-window window
			; Update selection if this is the first batch of candidates.
			; This should support preselection like helm-update does.
			(when (and (not (is_buffer_empty)) (bobp))
				(helm-goto-next-candidate)
				(helm-select-line)
				(helm-follow-execute-persistent-action-maybe))
			(run-hooks 'helm-after-update-hook)
			(helm-update-keymap)
			(helm-set-selection-overlay-face)
			(helm-mode-line-update))))

(defun helm-async-process-filter (process output_str)
"The `process-filter' function for helm async sources."
	(let (need_update)
		(with-local-quit
			(with-current-buffer helm-buffer
				(let* (
					(process-info (helm-get-process-info process))
					(source (helm-async-process-source process-info))
					(candidates
						(split-string output_str (helm-source-async-separator source)))
				)
					; Collect lines maybe completing the truncated first and last lines.
					; The output of process may come in chunks of any size, so the last
					; line of LINES could be truncated, this truncated line is stored
					; in INCOMPLETE-LINE to be concatenated with the first
					; incomplete line of the next arriving chunk.
					(setcar candidates
						(concat
							(helm-async-process-incomplete-line process-info)
							(car candidates)))
					; If there is only one line, save it in incomplete line and do nothing.
					(if (not (cdr candidates))
						(setf (helm-async-process-incomplete-line process-info)
							(car candidates))
						(let ((pre_last_cell (last candidates 2)))
							; Store last incomplete line (last chunk truncated) until
							; new output arrives.
							; On last output this is an empty string.
							(setf (helm-async-process-incomplete-line process-info)
								(nth 1 pre_last_cell))
							; Delete last line from candidates.
							(setcdr pre_last_cell nil))
						; Handle candidate-transformer.
						(when-let ((candidate_transformer (helm-source-candidate-transformer source)))
							; Restore some stuff from when helm-source-candidates was called and
							; call candidate-transformer.
							(let (
								(helm-pattern (helm-async-process-pattern process-info))
								(helm-match-fold-diacritics (helm-source-diacritics source))
								(case-fold-search (helm-async-process-case-fold process-info))
								(helm-current-source source)
							)
								(setq candidates (funcall candidate_transformer candidates))
								; Save possibly changed helm-pattern for the next batch
								; of candidates.
								; This emulates how sync sources work.
								(setf (helm-async-process-pattern process-info) helm-pattern)))
						(when candidates
							(setq need_update t)
							; Insert candidates into helm-buffer.
							(save-excursion
								(goto-char (point-max))
								(setf (helm-async-process-item-count process-info)
									(helm-insert-items
										candidates
										source
										(helm-async-process-item-count process-info)))))))))
		(when need_update (helm-async-after-update))))

(defun helm-process-deferred-sentinel-hook (process event file)
"Defer remote processes in sentinels.
Meant to be called at the beginning of a sentinel process function."
	(when
		(and
			(/= 0 helm-tramp-connection-min-time-diff)
			(string= event "finished\n")
			(or
				(file-remote-p file)
				; `helm-suspend-update-flag'
				; is non-`nil' here only during a
				; running process, this will never be called
				; when user set it explicitly with `C-!'.
				helm-suspend-update-flag))
		(setq helm-suspend-update-flag t)
		; Kill the process but don't delete entry in `helm-async-processes'.
		(helm-kill-async-process process)
		; When tramp opens the same connection twice in less than 5
		; seconds, it throws 'suppress, which calls the real-handler on
		; the main "Emacs". To avoid this [1] helm waits for 5 seconds
		; before updates yet allows user input during this delay. [1] In
		; recent Emacs versions, this has been fixed so tramp returns nil
		; in such conditions. Note: `tramp-connection-min-time-diff' cannot
		; have values less than 5 seconds otherwise the process dies.
		(run-at-time helm-tramp-connection-min-time-diff nil #'helm-reenable-update)))

(defun helm-async-candidate-transformer (candidates)
"Highlight and sort CANDIDATES for helm-current-source.
The default candidate-transformer function for async sources.
Reuses (modifies) list CANDIDATES.
Don't support helm-source-sync-match-on-real."
	(if (string= helm-pattern "")
		candidates
		(setq candidates
			(let (
				(target_list (mapcar #'helm-get-display candidates))
				(add_face
					(lambda (target start end)
						(add-face-text-property start end 'helm-match nil target)))
				apply_highlighting
				sort_target_list
				; Only for match part.
				match_part_2D_list
				dont_apply_highlighting
			)
				(if-let ((match_part (helm-source-match-part helm-current-source)))
					(setq
						match_part_2D_list ; List of lists of ranges.
							(mapcar
								(lambda (target)
									(or
										(get-text-property 0 'match-part target)
										(funcall match_part target)))
								target_list)
						sort_target_list ; List of strings to match.
							(let ((match_part_2D_list_i match_part_2D_list))
								(mapcar
									(lambda (target)
										(helm-substring-from-range-list
											target
											(pop match_part_2D_list_i)))
									target_list))
						; Quick fix.
						dont_apply_highlighting (lambda () (pop match_part_2D_list))
						apply_highlighting
							(lambda (target match) ; Dynamic binding: match_part_2D_list.
								; Example:
								; Target:
								; ABCDEFGH
								; 01234567
								; Match part:
								;  BCD  G
								;  123  6
								; match_part_list = ((1 . 4) (6 . 7))
								; matched string = CDG
								; match = ((1 . 4))
								; Actual parts to highlight: ((2 . 4) (6 . 7))
								(let* (
									(match_part_list (pop match_part_2D_list))
									(match_part_range (pop match_part_list))
									; number of chars not present
									; in string passed to 'match_fn'
									; before 'real_start'.
									(offset (car match_part_range))
									; index in 'target' that ends part that we are
									; in (started by real_start, ended by this var)
									(real_this_part_end (cdr match_part_range))
									(range (car match))
								)
									(while range
										(let ((real_start (+ offset (car range))))
											; Jump to next match-part parts, until
											; we find the part that real_start belongs to.
											(while (>= real_start real_this_part_end)
												; [1] Jump to next match-part part.
												(setq
													match_part_range (pop match_part_list)
													offset
														(+
															offset
															(-
																(car match_part_range)
																real_this_part_end))
													real_this_part_end (cdr match_part_range)
													real_start (+ offset (car range))))
											; Now we have valid real_start.
											(let ((real_end (+ offset (cdr range))))
												(if (> real_end real_this_part_end)
													(progn
														(funcall add_face
															target real_start real_this_part_end)
														; Keep the same end,
														; but change start to be start of
														; next match-part part
														; (this will happen because check
														; above [1] will pass).
														(setq range
															(cons
																(- real_this_part_end offset)
																(cdr range))))
													(funcall add_face target real_start real_end)
													(setq range (pop match)))))))))
					(setq
						sort_target_list target_list
						dont_apply_highlighting #'ignore
						apply_highlighting
							(lambda (target match) ; No dynamic binding.
								(dolist (range match)
									(funcall add_face target (car range) (cdr range))))))
				(let (
					; List of match results - list of
					; nil_or_(match_fn_index . t_or_list_of_ranges).
					(match_list (helm-match-internal sort_target_list t))
				)
					; Apply highlighting to target (display part of candidate)
					; and transform candidates to the list of (sort_target . cand).
					(let (
						(match_list_i match_list)
						(apply_highlighting_wrapper
							(let (
								(apply_highlighting_real
									(lambda ()
										(setq target (copy-sequence target))
										(funcall apply_highlighting target match)
										(if (consp cand) (cons target (cdr cand)) target)))
							)
								`(lambda (cand target match)
									; If helm-source-async-match-strict is non-nil,
									; discard non matched candidates (delq nil is below).
									,(if (helm-source-async-match-strict helm-current-source)
										`(cond
											((not match) (funcall dont_apply_highlighting) nil)
											((eq match t) (funcall dont_apply_highlighting) cand)
											(t (,apply_highlighting_real)))
										`(if (memq match '(nil t))
											(progn (funcall dont_apply_highlighting) cand)
											(,apply_highlighting_real))))))
					)
						(map_modify_list
							(lambda (cand)
								(cons
									(pop sort_target_list)
									(funcall apply_highlighting_wrapper
										cand (pop target_list) (cdr (pop match_list_i)))))
							candidates))
					(let* (
						(match_fn_list_length
							(length (helm-source-match helm-current-source)))
						(group_vector (make-vector (1+ match_fn_list_length) nil))
					)
						; Split candidates into groups by function that matched them.
						(dolist (cand candidates)
							(let ((index (or (car (pop match_list)) match_fn_list_length)))
								(aset
									group_vector
									index
									(cons cand (aref group_vector index)))))
						; Part above used pushing, so nreverse every list.
						(dotimes (i match_fn_list_length)
							(aset group_vector i (nreverse (aref group_vector i))))
						; Sort every group separately and then merge groups into
						; one list.
						(map_modify_list
							#'cdr
							(mapcan
								(if-let (
									(sort_fn (helm-source-sort helm-current-source))
								)
									`(lambda (group)
										(sort
											group
											; Transform sort_fn to act on sort_targets.
											(lambda (c_1 c_2)
												(,sort_fn (car c_1) (car c_2)))))
									#'identity)
								group_vector))))))
		; Discard non matched candidates if helm-source-async-match-strict is non-nil.
		(if (helm-source-async-match-strict helm-current-source)
			(delq nil candidates)
			candidates)))

(defun helm-async-stderr-filter (process output source)
	(with-current-buffer (process-buffer process)
		(let (
			(pt_marker (copy-marker (point) t))
			(inhibit-read-only t)
		)
			(goto-char (process-mark process))
			(if (process-get process 'helm-error)
				(insert output)
				(setf (helm-source-async-status source) helm-async-status-error-message)
				(helm-mode-line-update)
				(process-put process 'helm-error t)
				(unless (bobp) (insert "\n\n"))
				(insert
					"Error in process "
					(process-name process)
					".\nhelm session info:\n\thelm-pattern = \""
					helm-pattern
					"\"\n\thelm-sources: "
					(mapconcat
						(lambda (source)
							(concat
								"\""
								(helm-get-name source)
								"\""))
						(buffer-local-value
							'helm-sources (get-buffer helm-buffer))
						", ")
					".\nError:\n"
					output))
			(set-marker (process-mark process) (point))
			(goto-char pt_marker))))

(defun helm-async-get-stderr (name)
"Create and return pipe process for :stderr arg of `make-process'.
It will be associated with *helm-errors* buffer."
	(make-pipe-process
		:name (concat name "-stderr")
		:buffer
			(or
				(get-buffer "*helm-errors*")
				(with-current-buffer (get-buffer-create "*helm-errors*")
					(fundamental-mode)
					(current-buffer)))
		:filter
			`(lambda (process output)
				(helm-async-stderr-filter process output ',helm-current-source))
		:sentinel #'ignore))

; Entry point

(defun helm (&rest plist)
"Main function to execute helm sources.

Keywords:
	:sources -
		List of cl-structs inheriting from helm-source-sync or helm-source-async.

	:input - Initial input of minibuffer.

	:prompt - Minibuffer prompt. Default value is `helm-prompt'.

	:resume -
		\\='noresume - This helm call cannot be resumed.
		nil -
			This helm call can be resumed. Any other helm call with :resume nil
			(or t - resumed session) will use default value of `helm-buffer',
			so this helm call won't be resumable anymore.
		string -
			Helm calls that want to be resumable for longer, can use this to specify
			buffer name to use as `helm-buffer' for this session. Only helm calls
			with the same value of :resume will reuse the same `helm-buffer',
			making this helm call no longer resumable.
			Such buffer names are stored in `helm-resume-buffers'.
		t -
			internal value used by `helm-resume' to tell `helm-internal'
			to resume helm session saved in `helm-buffer'.

	:preselect -
		A regex, a candidate index in first source or a function with no args
		used to initially select a candidate.

		If preselection was successful, function should leave caret on a candidate
		line and return t, then helm will move caret to the start of this candidate.
		If preselection failed, it should return nil, then helm will move caret to
		the start of first candidate, so this function doesn't need to use any
		save-excursion.

		This argument always has a higher precedence than
		helm-inhibit-move-to-first-candidate.
		If PRESELECT is nil, try `helm-default-preselect'.
		This is only used in `helm-update'.
		For now, preselection in async sources is unsupported.

	:default -
		Default value inserted into the minibuffer \ with
		\\<minibuffer-local-map>\\[next-history-element].

		It can be a string or a list of strings, in this case
		\\<minibuffer-local-map>\\[next-history-element] cycles through
		the list items, starting with the first.

		This isn't used in determining the returned value in any way, like it is
		in `read-string'

		If :input, :use-default-as-input and :default are non-nil, :input is used.

	:use-default-as-input -
		If non-nil, the first display of helm-window is updated using :default value
		if that value matches, otherwise it is ignored.

	:execute-action-at-once-if-one -
		If non-nil, execute the default action and exit if only one candidate.

		If \\='current-source, count candidates only from current source,
		else from all sources.

		If this is non-nil, helm doesn't display helm-buffer until it is
		known that there is 0 or >1 candidates, so helm-after-window-setup-hook
		will not run until then. So if calculating candidates needs helm-window to
		exist, this must be nil.

	:quit-if-no-candidate -
		If non-nil, quit if there are no candidates.

		If a function, it is called with no args before exiting this way,
		its return value is ignored. Usually used to message something.

		This has similar effect as execute-action-at-once-if-one - helm-window
		is created just before entering minibuffer.

	:history - History variable used by minibuffer (`minibuffer-history-variable').

	:allow-nest -
		Allow running this helm command in a running helm session.
		Every place calling helm that has this non-nil should check if this helm
		call will result in the same source running in 2 helm sessions at the same
		time, because this won't work - some global variables will be overriden
		and helm will probably crash.

Other keywords are interpreted as local variables of this helm, e.g.
:show-trailing-whitespace t will set show-trailing-whitespace to t in helm-buffer."
	(let (fn)
		(cond
			((not helm-alive-p) (setq fn #'helm-internal))
			((plist-get plist :allow-nest) (setq fn #'helm--nest))
			(helm-alive-p
				; A helm session is normally running.
				(message "Trying to run helm within a running helm session."))
			(t
				; A helm session is already running and user jump somewhere else
				; without deactivating it.
				(with-helm-buffer
					(message "Aborting a helm session running in background.")
					(abort-recursive-edit))))
		(when fn
			; Move non standard arguments to helm--local-variables
			; and call 'fn' with standard arguments.
			(setq helm--local-variables
				(nconc
					helm--local-variables
					; Vars passed by keyword on helm call
					; take precedence on same vars
					; that may have been passed before helm call.
					(cl-loop
						for (key value) on plist by #'cddr
						unless
							(memq key
								'(
									:sources :input :prompt :resume :preselect :default
									:use-default-as-input :execute-action-at-once-if-one
									:quit-if-no-candidate :history :allow-nest
								))
							collect
								(cons
									; Can just as well intern, as later make-local-variable
									; will be called with this symbol, which will intern
									; it if it isn't already.
									(intern (substring (symbol-name key) 1))
									value))))
			(funcall fn
				(plist-get plist :sources)
				(plist-get plist :input)
				(plist-get plist :prompt)
				(plist-get plist :resume)
				(plist-get plist :preselect)
				(plist-get plist :default)
				(plist-get plist :use-default-as-input)
				(plist-get plist :execute-action-at-once-if-one)
				(plist-get plist :quit-if-no-candidate)
				(plist-get plist :history)))))

(defun helm-suspend-while-reading-password (original_fn &rest args)
"Used only as :around advice."
	(setq overriding-terminal-local-map nil)
	(setq helm-suspend-update-flag t)
	(unwind-protect (apply original_fn args)
		(setq helm-suspend-update-flag nil)))

(defun helm-minibuffer-exit-hook ()
"Save `buffer-undo-list' in `helm-buffer' in `helm--minibuffer-undo-list' from
minibuffer for `helm-resume'."
	(let ((undo_list buffer-undo-list))
		(with-current-buffer helm-buffer
			(setq-local helm--minibuffer-undo-list undo_list))))

(defun helm-update-after-input ()
"Main function called after user input (instantly or after delay)."
	; Stop updating in persistent action
	; or when `helm-suspend-update-flag' is non-nil.
	(or
		helm-in-persistent-action
		helm-suspend-update-flag
		(helm-check-minibuffer-input))
	(helm-update-keymap)
	(helm-set-selection-overlay-face)
	(helm-mode-line-update)
	; For delayed input handling:
	; There could be helm--update-header-line, but that would be almost always
	; useless, as it is already called in post-command-hook, where, to my knowledge,
	; only helm-ff changes helm-pattern in helm-update, so this update call should
	; be there (if it was to use delayed input handling, which it won't, but something
	; else might in the future so remember this).
	nil)

(defun helm-delay-post-command-hook ()
"post-command-hook for delayed input handling."
	(helm-update-keymap)
	(helm-set-selection-overlay-face)
	(helm-mode-line-update)
	(helm--update-header-line)
	nil)

(defun helm-instant-post-command-hook ()
"post-command-hook for instant input handling."
	(helm-update-after-input)
	(helm--update-header-line)
	nil)

(defvar helm-timer nil
"Idle timer used in delayed input handling.
Runs `helm-update-after-input' after `helm-input-idle-delay'
or overrided value (by `helm-delay').")

(defun helm-setup-idle-update (delay) "Must be called from active helm's minibuffer."
	(setq helm-timer
		(run-with-idle-timer delay t
			`(lambda ()
				; Protect from updating in nested minibuffers or just from some other
				; buffer than a minibuffer that helm uses. post-command-hooks responding
				; to input in minibuffer are buffer-local, so they only run in minibuffer,
				; but timers are not buffer-local, so restrict use of this one to
				; minibuffer with this 'when'.
				(when (eq ,(current-buffer) (current-buffer))
					(helm-update-after-input))))))

(defvar helm-delay nil
"Non-nil if current helm session uses delayed input handling.
Buffer-local in helm-buffer.
Can be a number specifying idle time after input
(if helm-delay is non-nil, not a number, then helm-input-idle-delay is used,
else if helm-delay is a number, it is used).")

(defvar helm--minor-mode nil
"Ala minor mode used only in minibuffer to use helm map.")
(defconst helm--minor-mode-map-cons (cons 'helm--minor-mode helm-map)
"Cons for helm--minor-mode in minor-mode-map-alist.
Used to manage current helm map.")
(push helm--minor-mode-map-cons minor-mode-map-alist)

(defun helm-internal (
	sources input prompt resume preselect default use_default_as_input
	execute-action-at-once-if-one quit-if-no-candidate history
)
	; Nullify use_default_as_input if it should be overrided by input or
	; there is no default to even use.
	(and
		use_default_as_input
		(or input (not default))
		(setq use_default_as_input nil))

	; Setup initial helm-pattern.
	; Input have precedence over default.
	(setq helm-pattern
		(or
			input
			(and use_default_as_input (if (listp default) (car default) default))
			""))

	(setq helm-alive-p t)
	(setq helm-action-update nil)
	(unless helm--nested (setq helm-initial-frame (selected-frame)))
	(let (
		reenable_highlight_parens_out_of_view_mode
		(saved_minibuffer_follows_selected_frame
			(default-toplevel-value 'minibuffer-follows-selected-frame))
		is_saved_mode_line_face_set
		saved_mode_line_face
		(source-process-p (helm-source-async-p (car sources)))
		(helm-buffer
			; Nested buffers are unique, but not nested, not resumable buffers are
			; not, so use some other buffer to leave the one for resuming untouched.
			(if (and (eq resume 'noresume) (not helm--nested))
				"*helm-noresume*"
				(let ((buffer (if (stringp resume) resume helm-buffer)))
					; Move resumable buffer on top of helm-resume-buffers.
					(setq helm-resume-buffers (delete buffer helm-resume-buffers))
					(setq helm-resume-buffers (cons buffer helm-resume-buffers))
					buffer)))
		; If some function called with let-bound helm-current-source to some source
		; calls helm, let-bind this to nil like it should be.
		helm-current-source
		; The same thing with this var.
		helm-in-persistent-action
		(display_helm_buffer
			; Choose window and display helm-buffer in it.
			(lambda ()
				; This allows giving the focus to a nested helm session which
				; uses a frame, like completion in `helm-eval-expression'.
				; When non-nil (the default) the current active
				; minibuffer is used in new frame, which is not what we
				; want in helm when starting from an active minibuffer,
				; either a helm minibuffer or something line M-:.
				(when saved_minibuffer_follows_selected_frame
					(setq minibuffer-follows-selected-frame
						(not
							(or
								helm--nested
								; Allow keeping initial minibuffer visible
								; e.g. completion-at-point from M-:.
								(minibufferp helm-current-buffer)))))

				(with-current-buffer helm-current-buffer
					; Save window/frame configuration.
					(setq helm-last-frame-or-window-configuration
						(let (
							(window-persistent-parameters
								(append
									'(
										(no-other-window . t)
										(mode-line-format . t)
										(tab-line-format . t)
										(header-line-format . t)
									)
									window-persistent-parameters))
						)
							(funcall (cdr helm-save-configuration-functions))))

					; Save and set helm-current-buffer's mode-line-face.
					(when helm-current-buffer-mode-line-face
						(setq is_saved_mode_line_face_set
							(local-variable-p 'mode-line-face))
						(when is_saved_mode_line_face_set
							(setq saved_mode_line_face mode-line-face))
						(setq mode-line-face helm-current-buffer-mode-line-face)))

				(helm-display-buffer resume)
				(select-window (get-buffer-window helm-buffer t))
				(when (eq resume t) (helm-update-window-margins))
				(run-hooks 'helm-after-window-setup-hook)))
	)
		(unless (eq resume t)
			; Initialize helm settings and set up the helm buffer.

			(run-hooks 'helm-before-initialize-hook)
			(dolist (source sources)
				(when-let ((fn (helm-source-before-init source)))
					(helm-apply-fn-from-source source fn)))

			(setq helm-suspend-update-flag nil)
			(setq helm-current-buffer (current-buffer))
			(unless (or helm--window-side-state helm-split-window-state)
				(setq helm--window-side-state helm::split_window_side)
				(setq helm-split-window-state
					(if
						(or
							(null split-width-threshold)
							(and
								(integerp split-width-threshold)
								(>= split-width-threshold (+ (frame-width) 4))))
						'vertical
						'horizontal)))
			(clrhash helm-candidate-cache)
			; Call the init function for sources.
			(dolist (source sources)
				(when-let ((init_fn (helm-source-init source)))
					(helm-apply-fn-from-source source init_fn)))
			; Create and setup 'helm-buffer'.
			(let ((inhibit-read-only t))
				(with-current-buffer (get-buffer-create helm-buffer)
					(kill-all-local-variables)
					(setq buffer-undo-list t)
					(erase-buffer)
					(setq-local
						helm-display-function nil
						helm-marked-candidates nil
						; When there is only one source, make it the default for this
						; session. This can be overriden by :helm-default-source arg
						; to helm.
						helm-default-source (unless (cdr sources) (car sources))
						helm-persistent-action-display-window nil
						helm-prompt (or prompt helm-prompt)
						helm--history history
						helm-sources sources
						default-directory
							(buffer-local-value 'default-directory helm-current-buffer)
						scroll-margin helm-scroll-margin)

					; Here to allow overriding it through helm--local-variables.
					(setq show-trailing-whitespace nil)

					; If `helm-set-local-variable' is called twice or more
					; on same variable use the last value entered which is
					; the first on stack e.g.
					; (helm-set-local-variable 'helm-foo 1)
					; (helm-set-local-variable 'helm-foo 2)
					; helm--local-variables =>
					; '((helm-foo . 2) (helm-foo. 1))
					; (helm-foo . 2) is retained and (helm-foo . 1) ignored.
					(let (set_list)
						(while helm--local-variables
							(let ((var_and_value (pop helm--local-variables)))
								(unless (memq (car var_and_value) set_list)
									(set (make-local-variable (car var_and_value))
										(cdr var_and_value))
									(push (car var_and_value) set_list)))))

					; Handle helm-delay (its non-nil, not a number value).
					(and
						helm-delay
						(not (numberp helm-delay))
						(setq-local helm-delay helm-input-idle-delay))

					; Local keymap.
					(use-local-map helm-local-map)

					; Locals.
					(setq truncate-lines helm-truncate-lines)
					(setq left-margin-width helm-left-margin-width)
					; Don't display cursor.
					(setq cursor-type nil)
					(setq cursor-in-non-selected-windows t)

					; Modes.
					; This one is unfortunately global, it shouldn't be.
					(when highlightParens::outOfView::mode
						(setq reenable_highlight_parens_out_of_view_mode t)
						(highlightParens::outOfView::mode -1))))
			(helm-init-selection-overlay helm-buffer)
			(helm-clear-visible-mark)
			; Run global hook.
			(run-hooks 'helm-after-initialize-hook)
			; Run local source hook.
			(dolist (source sources)
				(when-let ((fn (helm-source-after-init source)))
					(helm-apply-fn-from-source source fn))))
		(unwind-protect
			(condition-case nil
				(progn
					(unwind-protect
						(with-current-buffer helm-buffer
							(if (eq resume t)
								(progn
									(funcall display_helm_buffer)
									; helm-selection-overlay isn't local in helm-buffers,
									; so (re)init it.
									(helm-init-selection-overlay buffer)
									(dolist (source helm-sources)
										(when-let ((resume_fn (helm-source-resume source)))
											(helm-apply-fn-from-source source resume_fn)))
									(unless (is_buffer_empty)
										(helm-select-line)
										(helm-follow-execute-persistent-action-maybe)))
								(or
									execute-action-at-once-if-one
									quit-if-no-candidate
									(funcall display_helm_buffer))
								; Initial computation of candidates.
								(helm-update preselect))
							; Reset `helm-pattern' and update display if no result
							; found with precedent value of `helm-pattern', unless
							; `quit-if-no-candidate' is non-nil.
							(when use_default_as_input
								; Store value of actual helm-pattern temporarily here waiting for
								; the next update to allow actions like helm-moccur-action
								; matching pattern at the place it jump to.
								(setq helm-use-default-as-input-pattern helm-pattern)
								(let (
									(fn
										(lambda ()
											(setq helm-pattern "")
											(helm::call_after_next_update
												(lambda ()
													(setq helm-use-default-as-input-pattern nil)))))
								)
									(if source-process-p
										(helm::call_after_next_update fn)
										(funcall fn)))
								(when (and (not quit-if-no-candidate) (is_buffer_empty))
									; Clear cache and update with helm-pattern = "".
									(dolist (source sources)
										(remhash source helm-candidate-cache))
									(helm-update preselect)))
							; Handle quit-if-no-candidate and execute-action-at-once-if-one.
							(unless
								(if (is_buffer_empty)
									(when quit-if-no-candidate
										(when (functionp quit-if-no-candidate)
											(funcall quit-if-no-candidate))
										(signal 'quit nil))
									(and
										execute-action-at-once-if-one
										(=
											(helm-get-candidate-number
												(eq
													execute-action-at-once-if-one
													'current-source))
											1)
										(let* (
											(source (helm-get-source))
											(action (helm-get-actions-from-source source))
										)
											(unless (functionp action)
												(setq action (cdr (car action))))
											(setq helm-action-info
												(list
													source
													action
													nil
													(helm-get-selection)))
											t)))
								(when (or execute-action-at-once-if-one quit-if-no-candidate)
									(funcall display_helm_buffer)
									(unless (is_buffer_empty)
										(helm-select-line)
										(helm-follow-execute-persistent-action-maybe)))
								; Display helm-buffer and enter minibuffer.
								(let (
									(source (helm-get-source))
									; Don't automatically restore window config,
									; helm handles that differently.
									read-minibuffer-restore-windows
									minibuffer-completion-confirm
									(resize-mini-windows
										(and
											(not helm-echo-input-in-header-line)
											resize-mini-windows))
									; As we are using abort-recursive-edit for `C-g' we have
									; to prevent emacs command loop redefining `C-g' during
									; helm-session. This happen only on async source with
									; large output after a certain delay. The effect is that
									; the minibuffer is exited but the helm async process
									; continue running, and because minibuffer is lost `C-g'
									; have no more effect. By binding `inhibit-quit' here we
									; prevent this and allow `C-g' to quit immediately.
									(inhibit-quit source-process-p)
								)

									; Now we are definitely in a helm-window and with the first batch
									; of candidates presented to the user. If certain criteria (described
									; in helm-source.el -> auto-resize field) are met, shrink helm-window
									; vertically.
									(or
										(cdr helm-sources) ; There is more than 1 source.
										(not (string= helm-pattern ""))
										(let* (
											(source (car helm-sources))
											(auto_resize (helm-source-auto-resize source))
										)
											(when auto_resize
												; Not sure if this matters, but add mode-line to helm-window
												; before fit-window-to-buffer.
												(setq mode-line-format t)
												(let (
													; Because turns out that 'shrink-window-if-larger-than-buffer'
													; is broken in many ways, who would have guessed.
													; Most of code below is copied from 'fit-window-to-buffer'.
													(actually_correct_shrink_window_if_larger_than_buffer
														(lambda ()
															(let ((window (selected-window)))
																(and
																	; Is window vertically combined.
																	(window-combined-p window)
																	(not (eq window (frame-root-window window)))
																	(not (eq fit-window-to-buffer-horizontally 'only))
																	(not (window-size-fixed-p window 'preserved))
																	(let* (
																		; This is probably useless, but this section
																		; acts as if 'window-resize-pixelwise' is non-nil,
																		; so set this in case some function called here uses it.
																		(window-resize-pixelwise t)
																		(max-height (window-pixel-height window))
																		(height
																			(max
																				; Min height.
																				(*
																					(window-default-line-height window)
																					window-min-height)
																				(window-min-size window nil window t)

																				(min
																					max-height
																					(+
																						(cdr
																							(window-text-pixel-size
																								window nil t nil
																								(frame-pixel-height
																									(window-frame window))
																								t))
																						(window-scroll-bar-height window)
																						(window-bottom-divider-width window)))))
																	)
																		(unless (= height max-height)
																			(window-preserve-size window)
																			(window-resize-no-error
																				window
																				(- height max-height)
																				nil window t)
																			t))))))
												)
													(if (numberp auto_resize)
														(save-excursion
															(let ((start (point-max)))
																(goto-char start)
																; Add auto_resize number of lines.
																(while (> auto_resize 0)
																	(insert "I love Ahri\n")
																	(-- auto_resize))
																(when (funcall actually_correct_shrink_window_if_larger_than_buffer)
																	; It's already local.
																	(setq scroll-margin 0))
																; Delete auto_resize number of lines.
																(delete-region start (point))))
														(when (funcall actually_correct_shrink_window_if_larger_than_buffer)
															; It's already local.
															(setq scroll-margin 0)))))))

									(dolist (symbol helm::password_reading_fn_list)
										(advice-add symbol :around #'helm-suspend-while-reading-password))

									; Don't update mode-line here, because just after entering minibuffer
									; (before any redisplay), post-command-hook is run, which will
									; update mode-line and header-line.

									(minibuffer-with-setup-hook
										(lambda ()
											; Minibuffer has already been filled here.
											(run-hooks 'helm-minibuffer-set-up-hook)
											; Activate keymap.
											(setq-local helm--minor-mode t)
											(helm-update-keymap source)
											(helm--update-header-line)
											(unless (eq resume 'noresume)
												; Save buffer-undo-list for helm-resume.
												(add-hook 'minibuffer-exit-hook #'helm-minibuffer-exit-hook nil t)
												; If we are resuming, restore buffer-undo-list.
												(when (eq resume t)
													(setq buffer-undo-list
														(buffer-local-value
															'helm--minibuffer-undo-list
															(get-buffer helm-buffer)))))
											(let (post_command_hook)
												(if-let (
													(delay
														(buffer-local-value 'helm-delay (get-buffer helm-buffer)))
												)
													(progn
														(setq post_command_hook #'helm-delay-post-command-hook)
														(helm-setup-idle-update delay))
													(setq post_command_hook #'helm-instant-post-command-hook))
												; This local hook will be cleared when
												; exiting minibuffer, just like all minibuffer
												; local variables.
												(add-hook 'post-command-hook post_command_hook 90 t)))
										(read-from-minibuffer
											(propertize helm-prompt 'face 'helm-minibuffer-prompt)
											input
											; Don't pass keymap here, helm handles this through
											; `helm-update-keymap'.
											nil
											nil
											history
											default
											t)))))
						; Cleanup before action (if any).

						; Yes, kind of crap, should make it a local mode...
						(when reenable_highlight_parens_out_of_view_mode
							(highlightParens::outOfView::mode))

						(helm-kill-async-processes)

						(let ((in_action_buffer (helm-in-action-buffer)))
							; Cleanup if showing actions.
							(when in_action_buffer
								(setq helm-action-source-and-cand nil)
								; Set helm-pattern back to the correct value.
								(unless helm-use-default-as-input-pattern
									(setq helm-pattern helm-action-pattern))
								(setq helm-action-pattern nil))

							; For rare situations when helm is exited in this
							; use-default-as-input state.
							(when helm-use-default-as-input-pattern
								(setq helm-pattern helm-use-default-as-input-pattern)
								(setq helm-use-default-as-input-pattern nil))

							; Save helm-pattern in helm-buffer for helm-resume.
							(with-current-buffer helm-buffer
								(setq-local helm--pattern helm-pattern))

							; Nil if helm-buffer wasn't displayed above.
							(if-let (
								(helm_window
									(get-buffer-window
										(if in_action_buffer helm-action-buffer helm-buffer)
										t))
							)
								(progn
									; Cleanup after actually showing helm-buffer and entering
									; minibuffer.
									(when helm-timer
										(cancel-timer helm-timer)
										(setq helm-timer nil))
									(dolist (symbol helm::password_reading_fn_list)
										(advice-remove
											symbol #'helm-suspend-while-reading-password))
									(when saved_minibuffer_follows_selected_frame
										(set-default-toplevel-value
											'minibuffer-follows-selected-frame
											saved_minibuffer_follows_selected_frame))

									; A helm session running in a frame that runs a nested
									; session share the same frame for both sessions so
									; don't delete the common frame.
									(or
										(not helm--buffer-in-new-frame-p)
										helm--nested
										(with-selected-window helm_window
											(let ((frame (selected-frame)))
												(setq-local helm--last-frame-parameters
													(helm--get-frame-parameters))
												(bury-buffer)
												(delete-frame frame))))

									(with-current-buffer helm-buffer
										; Ensure restoring default-value of mode-line to allow user
										; using the mouse when helm is inactive (Bug#1517, Bug#2377).
										(kill-local-variable 'mode-line-format)
										; Be sure we call cleanup functions from helm-buffer.
										(dolist (source helm-sources)
											(when-let ((cleanup_fn (helm-source-cleanup source)))
												(helm-apply-fn-from-source source cleanup_fn))))
									(run-hooks 'helm-cleanup-hook)

									; Restore window/frame configuration.
									(funcall (car helm-save-configuration-functions)
										helm-last-frame-or-window-configuration)

									; Restore helm-current-buffer's mode-line-face.
									(with-current-buffer helm-current-buffer
										(when helm-current-buffer-mode-line-face
											(if is_saved_mode_line_face_set
												(setq mode-line-face saved_mode_line_face)
												(kill-local-variable 'mode-line-face))))

									; Kill helm-action-buffer only after window is deleted, to not
									; waste time on replacing helm-action-buffer in helm-window.
									(when in_action_buffer (kill-buffer helm-action-buffer))

									; Restore frame focus.
									; This is needed for minibuffer own-frame config
									; when recursive minibuffers are in use.
									; e.g M-: + helm-minibuffer-history.
									(select-frame-set-input-focus
										(if (minibufferp helm-current-buffer)
											(selected-frame)
											(last-nonminibuffer-frame)))

									; Now bury-buffer from underlying windows,
									; otherwise if this window is killed,
									; the underlying buffer will be helm-buffer.
									(replace-buffer-in-windows helm-buffer))
								(with-current-buffer helm-buffer
									; See above why.
									(kill-local-variable 'mode-line-format)
									(dolist (source helm-sources)
										(when-let ((cleanup_fn (helm-source-cleanup source)))
											(helm-apply-fn-from-source source cleanup_fn)))
									(bury-buffer))
								(run-hooks 'helm-cleanup-hook)))

						(setq helm-alive-p nil))

					; Execute action.

					; This branch is not in any unwind-protect or condition-case,
					; it's only executed after not signaling anything.
					; This is the only way for helm to return a non-nil value.

					(setq helm-exit-status 0)
					(run-hooks 'helm-before-action-hook)
					(prog1
						(helm-apply-fn-from-source
							(pop helm-action-info) ; Source.
							(pop helm-action-info) ; Action function.
							(nth 1 helm-action-info)) ; Real value of candidate.
						(run-hooks 'helm-after-action-hook)))
				(quit
					(setq helm-exit-status 1)
					(run-hooks 'helm-quit-hook)
					nil))
			; Cleanup after action (if any).
			; This is mainly to not hold some useless references.
			(setq helm-current-prefix-arg nil)
			(setq helm-action-info nil)
			(setq helm--force-updating-p nil)
			(setq helm--buffer-in-new-frame-p nil))))

(setq helm-nested-level 0)
(defun helm--nest (
	sources input prompt _resume preselect default use_default_as_input
	execute-action-at-once-if-one quit-if-no-candidate history
)
"[INTERNAL] Call `helm' within a running helm session.
Don't use this directly, use instead `helm' with the keyword :allow-nest."
	; Small issue: when selecting action, return to helm-buffer and
	; start nested helm from there.
	; We could just return nil here and message something,
	; but I guess this is the only real use for that, so it's better this way.
	(when (helm-in-action-buffer) (helm-select-action))
	(let (
		(orig-helm-current-buffer helm-current-buffer)
		(orig-helm--display-frame helm--buffer-in-new-frame-p)
		(orig-helm-onewindow-p helm-onewindow-p)
	)
		(prog1
			(let (
				; If every helm call doesn't have its own frame, reuse current
				; helm-window. I just like it, it could easily be changed.
				(helm--nested (or helm--buffer-in-new-frame-p (helm-window)))
				helm-last-frame-or-window-configuration
				helm-current-buffer
				helm-pattern
				(helm-buffer
					(concat
						"*helm-nested-level-"
						(number-to-string (1+ helm-nested-level))
						"*"))
				(helm-nested-level (1+ helm-nested-level))
			)
				(prog1
					(helm-internal
						sources input prompt 'noresume preselect default
						use_default_as_input execute-action-at-once-if-one
						quit-if-no-candidate history)
					(kill-buffer helm-buffer)))
			(setq helm-alive-p t)
			(setq helm--buffer-in-new-frame-p orig-helm--display-frame)
			(setq helm-current-buffer orig-helm-current-buffer)
			(setq helm-onewindow-p orig-helm-onewindow-p)
			(helm-init-selection-overlay helm-buffer)
			(with-helm-window
				(setq helm-full-frame nil)
				(unless (is_buffer_empty) (helm-select-line)))
			; Be sure advices, hooks, and local modes keep running.
			(dolist (symbol helm::password_reading_fn_list)
				(advice-add symbol :around #'helm-suspend-while-reading-password))
			(with-current-buffer (window-buffer (minibuffer-window))
				(if-let (
					(delay (buffer-local-value 'helm-delay (get-buffer helm-buffer)))
				)
					(progn
						(add-hook 'post-command-hook #'helm-delay-post-command-hook 90 t)
						(unless helm-timer (helm-setup-idle-update delay)))
					(add-hook
						'post-command-hook #'helm-instant-post-command-hook 90 t))))))

; helm resume

(defconst helm-resume-source
	(helm-source-sync-make nil
		:name "helm-resume"
		:candidates (lambda () helm-resume-buffers)
		:volatile t
		:nomark t))

(defun helm-resume ()
"Resume a previous helm session.
With a prefix arg ask for the buffer to resume helm session from,
else use the most recent one."
	(interactive)
	; Delete dead buffers.
	(setq helm-resume-buffers (cl-delete-if-not #'get-buffer helm-resume-buffers))
	(cond
		(helm-alive-p (message "Trying to run helm within a running helm session."))
		((not helm-resume-buffers) (message "No helm buffers to resume."))
		(t
			(when-let (
				(helm-buffer
					(if current-prefix-arg
						(helm :sources (list helm-resume-source) :resume 'noresume)
						(car helm-resume-buffers)))
			)
				(let* (
					(buffer (get-buffer helm-buffer))
					(helm-full-frame (buffer-local-value 'helm-full-frame buffer))
				)
					; Take the current buffer if the old one isn't displayed in a window.
					(unless (get-buffer-window helm-current-buffer t)
						(setq helm-current-buffer (current-buffer)))
					(helm
						:sources (buffer-local-value 'helm-sources buffer)
						:input (buffer-local-value 'helm--pattern buffer)
						:prompt (buffer-local-value 'helm-prompt buffer)
						:history (buffer-local-value 'helm--history buffer)
						:resume t))))))

; Windows and frames

(defun helm-prevent-switching-other-window (value)
	(dolist (window (window-list nil 0))
		(unless (window-dedicated-p window)
			(set-window-parameter window 'no-other-window value))))

(defun helm::split_window (window)
"Default function to split windows before displaying `helm-buffer'.

It is used as let-bound value of `split-window-preferred-function'
in `helm-display-buffer'.
When `helm-display-function' which default to `helm-default-display-buffer'
is called from `helm-display-buffer' the value of
`split-window-preferred-function' will be used by `display-buffer'."
	(let (
		(window_in_direction
			; Like window-in-direction but avoid dedicated windows.
			(lambda (direction)
				(when-let (
					(win (window-in-direction direction))
					((not (window-dedicated-p win)))
				)
					win)))
	)
		(cond
			((windowp helm--nested) helm--nested)
			; Don't try to split when starting in a minibuffer.
			((minibufferp helm-current-buffer) window)
			((one-window-p t)
				(split-window nil nil helm::split_window_side))
			; If more than one window reuse one of them.
			((funcall window_in_direction helm::split_window_side))
			((funcall window_in_direction
					(cl-case helm::split_window_side
						(right 'left)
						(left 'right)
						(above 'below)
						(t 'above))))
			((funcall window_in_direction
					(if (memq helm::split_window_side '(right left)) 'below 'right)))
			((funcall window_in_direction
					(if (memq helm::split_window_side '(right left)) 'above 'left)))
			((window::split_window_sensibly window))
			(t (selected-window)))))

(defun helm-display-buffer (&optional resume)
"Display helm-buffer.

The function used to display `helm-buffer' by calling
`helm-display-function' which splits window with `helm::split_window'.

This function must be called from helm-buffer (or helm-action-buffer)."
	(let (
		(split-window-preferred-function #'helm::split_window)
		(display_fn
			; If helm-display-function is non-nil in helm buffer, use that.
			; Probably the best option for commands that want to use frame,
			; is to pass :helm-display-function #'helm-display-buffer-in-own-frame
			; to helm call.
			; If `helm-initial-frame' has no minibuffer,
			; use `helm-display-buffer-in-own-frame' function.
			; Fallback to global value of `helm-display-function' when no local
			; value found.
			(cond
				(helm-display-function)
				((or
						(and
							helm::use_frame_if_started_from_dedicated_window
							(let ((window (get-buffer-window helm-current-buffer)))
								(or
									(window-dedicated-p window)
									(window-parameter window 'window-side))))
						(and
							helm::use_frame_if_more_than_n_windows
							(not helm--nested)
							(nthcdr helm::use_frame_if_more_than_n_windows (window-list)))
						(not (frame-parameter helm-initial-frame 'minibuffer)))
					#'helm-display-buffer-in-own-frame)
				(t (default-value 'helm-display-function))))
	)
		(prog1
			(funcall display_fn
				(or
					(eq resume t)
					(and helm-actions-inherit-frame-settings helm-action-info)))
			; This is already buffer-local.
			(setq helm-display-function display_fn)

			(setq helm-onewindow-p (one-window-p t))
			; This is probably very bad when called from helm-toggle-full-frame.
			; See fixme there.
			(let ((window (get-buffer-window helm-buffer t)))
				(set-window-parameter window 'mode-line-format nil)
				(set-window-parameter window 'tab-line-format nil)
				(set-window-parameter window 'header-line-format nil))
			; Don't allow other-window and friends switching out of minibuffer.
			(when helm-prevent-escaping-from-minibuffer
				(helm-prevent-switching-other-window t)))))

(defun helm-default-display-buffer (&optional _resume)
"Default function to display `helm-buffer'.

It is the default value of `helm-display-function'.
It uses `switch-to-buffer' or `display-buffer' depending on the
value of `helm-full-frame'."
	(let (
		pop-up-frames
		(curwin (get-buffer-window helm-current-buffer))
		(maybe_delete_other_windows
			(lambda ()
				(and
					(not (minibufferp helm-current-buffer))
					; side-windows can't be the only window in frame,
					; emacs refuse to delete other windows when
					; current is a side-window.
					(not (window-parameter curwin 'window-side))
					(delete-other-windows))))
	)
		(if (buffer-local-value 'helm-full-frame (get-buffer helm-buffer))
			(progn
				(funcall maybe_delete_other_windows)
				(switch-to-buffer helm-buffer))
			(when helm-autoresize-mode (funcall maybe_delete_other_windows))
			(display-buffer helm-buffer
				(list
					nil
					(cons 'window-height helm::default_window_height)
					(cons 'window-width helm::default_window_width)))
			(run-hooks 'helm-window-configuration-hook))))

; No warnings in Emacs built --without-x.
(defvar x-display-name)

(defun helm-display-buffer-in-own-frame (&optional resume)
"Display helm-buffer in a separate frame.

See `helm::buffer_height_in_own_frame' and `helm::buffer_width_in_own_frame'
to configure frame size."
	(setq helm--buffer-in-new-frame-p t)
	(let* (
		(pos (window-absolute-pixel-position))
		(half-screen-size (/ (display-pixel-height x-display-name) 2))
		tab-bar-mode
		display-buffer-alist
	)
		; Display minibuffer above or below only in initial session,
		; not on a session triggered by action, this way if user have
		; toggled minibuffer and header-line manually she keeps this
		; setting in next action.
		(unless (or helm-action-info resume)
			; Add the hook inconditionally, if
			; helm-echo-input-in-header-line is nil helm-hide-minibuffer-maybe
			; will have anyway no effect so no need to remove the hook.
			(add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)
			; We should be in helm-buffer or helm-action-buffer.
			(setq-local helm-echo-input-in-header-line
				(not (> (cdr pos) half-screen-size))))
		(display-buffer helm-buffer
			(list
				'display-buffer-pop-up-frame
				(cons
					'pop-up-frame-parameters
					(if resume
						helm--last-frame-parameters
						`(
							(width . ,helm::buffer_width_in_own_frame)
							(height . ,helm::buffer_height_in_own_frame)
							(left .
								,(-
									(car pos)
									(*
										(frame-char-width)
										(min (- (point) (pos-bol)) (length helm-prompt)))))
							; Try to put frame at the best possible place.
							; Frame should be below point if enough
							; place, otherwise above point and
							; current line should not be hidden
							; by helm frame.
							(top .
								,(if (> (cdr pos) half-screen-size)
									; Above point
									(-
										(cdr pos)
										; Add 2 lines to make sure there is always a gap.
										(* (+ helm::buffer_height_in_own_frame 2) (frame-char-height))
										; Account for title bar height too
										(nthcdr 2 (assq 'title-bar-size (frame-geometry))))
									; Below point
									(+ (cdr pos) (frame-char-height))))
							(title . "helm") ; Shouldn't this be 'name?
							(undecorated . ,helm::use_undecorated_frame)
							(background-color .
								,(or
									helm-frame-background-color
									(face-attribute 'default :background)))
							(foreground-color .
								,(or
									helm-frame-foreground-color
									(face-attribute 'default :foreground)))
							(alpha . ,(or helm-frame-alpha 100))
							(font . ,(cdr (assq 'font (frame-parameters))))
							(vertical-scroll-bars)
							(tool-bar-lines . 0)
							(menu-bar-lines . 0)
							(fullscreen)
							(visibility . t)
							(minibuffer . t)
						))))))
	(run-hooks 'helm-window-configuration-hook))

; Ensure to quit helm when user delete helm frame manually.
; If user deletes another frame keep session running.
(add-hook 'delete-frame-functions
	(fn_symbol "helm-delete-frame-hook"
		(lambda (frame)
			(and
				helm-alive-p
				; 'frame' is handling helm-buffer
				(get-buffer-window helm-buffer frame)
				(abort-recursive-edit)))))

; Initialize

(defun helm-init-selection-overlay (buffer)
	(if helm-selection-overlay
		; Make sure the overlay belongs to the helm buffer if
		; it's newly created
		(move-overlay helm-selection-overlay 1 1 (get-buffer buffer))
		(setq helm-selection-overlay (make-overlay 1 1 (get-buffer buffer)))
		(overlay-put helm-selection-overlay 'face 'helm-selection)
		(overlay-put helm-selection-overlay 'priority 1)))

(defun helm-suspend-update-toggle ()
"Enable or disable display update in helm.
This can be useful for example for quietly writing a complex
regex without helm constantly updating."
	(interactive)
	(setq helm-suspend-update-flag (not helm-suspend-update-flag))
	(when helm-suspend-update-flag (helm-kill-async-processes))
	(message "Helm update %s." (if helm-suspend-update-flag "suspended" "enabled")))

(defun helm-case-fold-search-toggle ()
"Toggle value of `helm-case-fold-search' between smart, t and nil.
This only toggles it in this helm-buffer, there is no way to change it now.
In the future, this should be a slot in the source, or there should be
case-fold-search-symbol of helm session to change its value."
	(interactive)
	(with-current-buffer helm-buffer
		(setq-local helm-case-fold-search
			(cond
				((eq helm-case-fold-search 'smart) t)
				(helm-case-fold-search nil)
				(t 'smart))))
	(helm-force-update t)
	; Also run helm-force-update after coming back to helm-buffer from helm-action-buffer.
	(when (helm-in-action-buffer) (setq helm-action-update t)))

(defun helm-match-toggle ()
"Toggle value of helm-source-match slot of current source, between default value
(helm multi matching - no mode-line message) and additional values from
helm-source-match-alist or `helm-match-alist'."
	(interactive)
	(with-helm-buffer
		(if-let ((source (or (helm-get-current-source) helm-default-source)))
			(let (
				(match (helm-source-match source))
				(match_alist (or (helm-source-match-alist source) helm-match-alist))
			)
				(setf (helm-source-match source)
					(cdr
						(or
							; Take the next value from match_alist.
							(nth 1 (memq (rassq match match_alist) match_alist))
							; Take the default.
							(car match_alist))))
				(helm-force-update t))
			(message "No current source."))))

(defun helm-delete-backward-no-update (n)
"Disable update and delete N chars backward.
Update is reenabled when idle for 0.5s."
	(interactive "p")
	; If update is suspended, just do normal [backspace] thing
	; (this should probably recall [backspace] keybind, not just delete char...).
	(if helm-suspend-update-flag
		(ignore-errors (delete-char (- n)))
		; Else suspend update and reenable it after 1 second, unless
		; it was already enabled in this 1 second period.
		(setq helm-suspend-update-flag t)
		(helm-kill-async-processes)
		(ignore-errors (delete-char (- n)))
		(run-with-idle-timer 0.5 nil #'helm-reenable-update)))

(defun helm-update-keymap (&optional source)
"Handle different keymaps in multiples sources.
Set keymap of current source, helm-default-keymap or helm-map."
	(setq source (or source (helm-get-current-source)))
	(setcdr helm--minor-mode-map-cons
		(or
			(if source
				(helm-source-keymap source)
				(buffer-local-value 'helm-default-keymap (get-buffer (helm-buffer-get))))
			helm-map))
	nil)

(defun helm-check-minibuffer-input ()
"Update helm if minibuffer contents are different than helm-pattern."
	(with-selected-window (or (active-minibuffer-window) (minibuffer-window))
		(let ((input (minibuffer-contents-no-properties)))
			(unless (string= input helm-pattern)
				(setq helm-pattern input)
				(helm-update)))))

; Narrowing candidates
(defun helm-candidate-number-limit (source)
"Apply candidate-number-limit attribute value.
This overrides `helm-candidate-number-limit' variable."
	(or
		(helm-source-candidate-number-limit source)
		(buffer-local-value 'helm-candidate-number-limit (get-buffer (helm-buffer-get)))
		99999999))

; Case fold search
(defun helm-set-case-fold-search (&optional pattern)
"Used to set the value of `case-fold-search' in helm.
Return t or nil depending on the value of `helm-case-fold-search' in `helm-buffer'
and PATTERN.
PATTERN defaults to `helm-pattern'."
	(with-current-buffer helm-buffer
		(if (eq helm-case-fold-search 'smart)
			(let (case-fold-search)
				(not (string-match "[[:upper:]]" (or pattern helm-pattern))))
			helm-case-fold-search)))

(defun helm-substring-from-range-list (str range_list)
"RANGE_LIST must: contain only ranges, be sorted, have every index in range of STR."
	(mapconcat
		(lambda (range) (substring-no-properties str (car range) (cdr range)))
		range_list))

; Update

(defun helm-update (&optional preselect)
"Update `helm-buffer' based on `helm-pattern'.
Argument PRESELECT is like :preselect arg of `helm'.
Return non-nil if preselection was successful."
	(helm-kill-async-processes)
	(let* (
		(buffer (get-buffer (helm-buffer-get)))
		(window (get-buffer-window buffer 0))
	)
		(with-current-buffer buffer
			(dolist (source helm-sources)
				(when (helm-source-async-p source)
					(setf (helm-source-async-status source) nil)))
			; When persistent action have been called
			; we have two windows even with `helm-full-frame'.
			; So go back to one window when updating if `helm-full-frame'
			; is non-nil.
			(and
				window
				helm-onewindow-p
				; We are not displaying helm-buffer in a frame and
				; helm-window is already displayed.
				(not helm--buffer-in-new-frame-p)
				(not (helm-in-action-buffer))
				(with-selected-window window (delete-other-windows)))
			(unwind-protect
				(let (sources matches preselect_succeeded)
					; These incomplete regexps hang helm forever so defer update.
					(unless (member helm-pattern helm-update-blacklist-regexps)
						; Collect sources ready to be updated.
						(setq sources
							(cl-loop
								for source in helm-sources
								when
									(and
										(or
											(not helm-source-filter)
											(memq source helm-source-filter))
										(>= (length helm-pattern) (helm-source-requires-pattern source)))
									collect source)))
					; When no sources to update erase buffer
					; to avoid duplication of header and candidates
					; when next chunk of update will arrive,
					; otherwise the buffer is erased after the results are computed.
					(unless sources (erase-buffer))
					; Compute matches without rendering the sources.
					; This prevent the helm-buffer flickering when constantly updating.
					(setq matches
						; Return a list of matches for each source in sources.
						;
						; The resulting value is a list of lists, e.g. ((a b c) (c d) (e f))
						; or (nil nil nil) for three sources when no matches found,
						; however this function can be interrupted by new input and in this
						; case returns plain nil, in this case `helm-update' is not rendering
						; the source, keeping previous candidates in display."
						; Emacs bug debbugs.gnu.org/47205, unexpected dbus-event
						; is triggered on dbus init. Ignoring the dbus-event works.
						(let ((while-no-input-ignore-events (cons 'dbus-event while-no-input-ignore-events)))
							; Same as `while-no-input' but returns either body or nil.
							(with-local-quit
								(catch 'interrupting_input_arrived
									(let* (
										(throw-on-input 'interrupting_input_arrived)
										(matches
											(mapcar
												(lambda (source)
													(let* (
														(helm-current-source source)
														(helm-match-fold-diacritics (helm-source-diacritics source))
														(helm-pattern
															(if-let ((fn (helm-source-pattern-transformer source)))
																(funcall fn helm-pattern)
																helm-pattern))
													)
														(if (helm-source-async-p source)
															(when-let (
																(process
																	; Bind inhibit-quit to ensure function terminate in case of
																	; quit from this local quit block and processes are added to
																	; helm-async-processes for further deletion (Bug#2113).
																	(let ((inhibit-quit t))
																		(funcall (helm-source-candidates source))))
															)
																; Candidates will be filtered later in process filter.
																(helm-async-process-make
																	process source helm-pattern (helm-set-case-fold-search))
																(set-process-filter process #'helm-async-process-filter)
																nil)
															(let (
																(candidates
																	(or
																		(gethash source helm-candidate-cache)
																		(let ((candidates (funcall (helm-source-candidates source))))
																			(cond
																				; Candidates function returns no candidates.
																				((or
																						(not candidates)
																						; Can happen when the output of a process
																						; is empty, and the candidates function call
																						; something like (split-string (buffer-string) "\n")
																						; which result in a list of one empty string (Bug#938).
																						; e.g (completing-read "test: " '(""))
																						(equal candidates '("")))
																					nil)
																				; Don't save in cache if source is 'volatile.
																				((helm-source-sync-volatile source) candidates)
																				(t (puthash source candidates helm-candidate-cache))))))
																(limit (helm-candidate-number-limit source))
																(grouped (helm-source-sync-grouped source))
															)
																(when candidates
																	(setq candidates
																		; Perform matching only if helm-pattern is not empty.
																		(if (string= helm-pattern "")
																			(if grouped
																				; This unfortunately can count separator
																				; lines, so number of candidates can be
																				; lower than it should, but I don't think
																				; it's worth it to exclude it them here
																				; just to have a nice round number of candidates.
																				; It will usually be like 9980 instead of 9999.
																				(ntake limit (apply #'append candidates))
																				(take limit candidates))
																			; Compute candidates according to pattern with their match function.
																			; The infamous "helm matching".
																			; See function `helm-match' for a simpler version.
																			; Most of code below could use helm-match, but
																			; idk I wrote this first, it works and is relatively
																			; performant, so I'll leave it like that.
																			(let* (
																				(sort_fn
																					(when-let ((sort_fn (helm-source-sort source)))
																						`(lambda (c_1 c_2) (,sort_fn (car c_1) (car c_2)))))
																				(match_part (helm-source-match-part source))
																				(match_on_real (helm-source-sync-match-on-real source))
																				; Create hash table to store candidates to search
																				; for duplicates.
																				(dups_hash (make-hash-table :test 'equal))
																				(case-fold-search (helm-set-case-fold-search))
																				; List (helm-pattern . match_fn).
																				(match_list
																					(cl-loop
																						for match_fn_list in (helm-source-match source)
																						; See doc of helm-source-match - this
																						; can return nil to not match anything.
																						for pattern = (funcall (car match_fn_list))
																						when pattern collect (cons pattern (nth 1 match_fn_list))))
																				(count 0)
																				match
																				result
																				(do
																					(lambda ()
																						(while (and (< count limit) (setq match (pop match_list)))
																							(let ((helm-pattern (car match)))
																								(setq match (cdr match))
																								(setq result
																									(nconc
																										result
																										(map_modify_list
																											#'cdr
																											(let (
																												(candidates_1
																													(cl-loop
																														for cand in candidates
																														while (< count limit)
																														for cand_to_collect = nil
																														; Check if candidate have already
																														; been found in previous loop.
																														unless (gethash cand dups_hash) do
																															(if match_on_real
																																(let* (
																																	(display (car cand))
																																	(real (cdr cand))
																																	(target
																																		(if match_part
																																			(helm-substring-from-range-list
																																				real
																																				(or
																																					(get-text-property 0 'match-part real)
																																					(funcall match_part real)))
																																			real))
																																	(match_range_list (funcall match target))
																																)
																																	(when match_range_list
																																		(unless (eq match_range_list t)
																																			; Try to highlight something close
																																			; to what was matched in 'target'.
																																			; This could just do (funcall match display) and highlight
																																			; matched parts... Hmm, idk which is better.
																																			(setq display
																																				; Mainly use temp buffer for case-fold-search to matter,
																																				; because string-search doesn't handle it.
																																				(withTempBuffer
																																					(insert display)
																																					(dolist (range match_range_list)
																																						(goto-char 1)
																																						(let (
																																							(str
																																								(substring-no-properties
																																									target
																																									(car range)
																																									(cdr range)))
																																						)
																																							; 'str' is never "", so search-forward
																																							; always safely moves us forward into the
																																							; uncertain future.
																																							(while (search-forward str nil t)
																																								(add-face-text-property
																																									(match-beginning 0)
																																									(point)
																																									'helm-match))))
																																					(buffer-string))))
																																		(setq cand_to_collect (cons target (cons display real)))))
																																(let (
																																	; Base string to match.
																																	(target (helm-get-display cand))
																																	(set_cand_to_collect
																																		(lambda (target_to_sort display)
																																			(setq cand_to_collect
																																				(cons
																																					target_to_sort
																																					(if (consp cand)
																																						(cons display (cdr cand))
																																						display)))))
																																)
																																	(if match_part
																																		(let* (
																																			(match_part_range_list
																																				(or
																																					(get-text-property 0 'match-part target)
																																					(funcall match_part target)))
																																			(match_part_target
																																				(helm-substring-from-range-list
																																					target
																																					match_part_range_list))
																																			(match_range_list
																																				(funcall match match_part_target))
																																		)
																																			(when match_range_list
																																				(unless (eq match_range_list t)
																																					(setq target (copy-sequence target))
																																					; Example:
																																					; Target:
																																					; ABCDEFGH
																																					; 01234567
																																					; Match part:
																																					;  BCD  G
																																					;  123  6
																																					; match_part_range_list = ((1 . 4) (6 . 7))
																																					; Match: CDG
																																					; match_range_list = ((1 . 4))
																																					; Actual parts to highlight: ((2 . 4) (6 . 7))
																																					(let* (
																																						(match_part_range (pop match_part_range_list))
																																						; number of chars not present
																																						; in string passed to 'match'
																																						; before 'real_start'.
																																						(offset (car match_part_range))
																																						; index in 'target' that ends part that we are
																																						; in (started by real_start, ended by this var)
																																						(real_this_part_end (cdr match_part_range))
																																						(range (car match_range_list))
																																					)
																																						(while range
																																							(let ((real_start (+ offset (car range))))
																																								; Jump to next match-part parts, until
																																								; we found the part that real_start belongs to.
																																								(while (>= real_start real_this_part_end)
																																									; [1] Jump to next match-part part.
																																									(setq
																																										match_part_range
																																											(pop match_part_range_list)
																																										offset
																																											(+
																																												offset
																																												(-
																																													(car match_part_range)
																																													real_this_part_end))
																																										real_this_part_end
																																											(cdr match_part_range)
																																										real_start (+ offset (car range))))
																																								; Now we have valid real_start.
																																								(let ((real_end (+ offset (cdr range))))
																																									(if (> real_end real_this_part_end)
																																										(progn
																																											(add-face-text-property
																																												real_start real_this_part_end
																																												'helm-match nil target)
																																											; Keep the same end,
																																											; but change start to be start of
																																											; next match-part part
																																											; (this will happen because check
																																											; above [1] will pass).
																																											(setq range
																																												(cons
																																													(- real_this_part_end offset)
																																													(cdr range))))
																																										(add-face-text-property
																																											real_start real_end
																																											'helm-match nil target)
																																										(setq range (pop match_range_list))))))))
																																				(funcall set_cand_to_collect match_part_target target)))
																																		; Simplest case - no match-part, no match-on-real,
																																		; simply match display part and highlight
																																		; parts of it that match decided.
																																		(when-let (
																																			(match_range_list (funcall match target))
																																		)
																																			(unless (eq match_range_list t)
																																				; Don't modify string in original candidate list.
																																				(setq target (copy-sequence target))
																																				(dolist (range match_range_list)
																																					(add-face-text-property
																																						(car range) (cdr range)
																																						'helm-match nil target)))
																																			(funcall set_cand_to_collect target target)))))
																														when cand_to_collect
																															collect
																																(progn
																																	(++ count)
																																	; Save candidate to later check for duplicates.
																																	(puthash cand t dups_hash)
																																	cand_to_collect)))
																											)
																												; Now sort_fn can be nil, so this saving candidate
																												; to sort is pointless if that's the case, but
																												; that's very rare, there almost always should be
																												; some sorting, so don't optimize for rare cases
																												; when there isn't any.
																												(if sort_fn
																													(sort candidates_1 sort_fn)
																													candidates_1)))))))))
																			)
																				(cond
																					((eq grouped 'separators)
																						(dolist (candidates_1 candidates)
																							(setq result
																								(nconc
																									result
																									(let (
																										(match_list match_list)
																										; Remove separator line.
																										(candidates (cdr candidates_1))
																										result
																									)
																										(funcall do)
																										(clrhash dups_hash)
																										(when result
																											; Add separator line.
																											(cons
																												(car candidates_1)
																												result)))))))
																					(grouped
																						(dolist (candidates candidates)
																							(let ((match_list match_list))
																								(funcall do))
																							(clrhash dups_hash)))
																					(t (funcall do)))
																				result))))
																; Run candidate-transformer even if there are
																; no candidates, because it might add some candidates
																; on its own, for example new file.
																(when-let ((candidate_transformer (helm-source-candidate-transformer source)))
																	(setq candidates (funcall candidate_transformer candidates)))
																candidates))))
												sources))
									)
										; See comments in `while-no-input' about resetting quit-flag.
										(cond
											((eq quit-flag throw-on-input) (setq quit-flag nil))
											(quit-flag nil)
											(t matches)))))))
					; If computing matches finished and is not interrupted
					; erase the helm-buffer and render results.
					(when matches ; nil only when interrupted by new input.
						(erase-buffer)
						(unless preselect (setq preselect helm-default-preselect))
						(let (
							first_source_max_index
							(preselect_fn
								(lambda (&optional no_follow)
									(goto-char (point-min))
									(helm-goto-next-candidate)
									(let ((start (point)))
										(if (functionp preselect)
											(setq preselect_succeeded (funcall preselect))
											(while
												(and
													(if (consp preselect)
														(and
															(re-search-forward (car preselect) nil t)
															(re-search-forward (cdr preselect) nil t))
														(re-search-forward preselect nil t))
													; If search fall on a non candidate line,
													; continue the loop until it match or fail.
													(helm-get-index)
													(progn (setq preselect_succeeded t) nil))))
										(if preselect_succeeded
											(helm-goto-candidate-start)
											(goto-char start)))
									(helm-select-line)
									(unless no_follow (helm-follow-execute-persistent-action-maybe))))
						)
							(cl-loop
								for source in sources
								for matches_ in matches
								when matches_
									do ; Display matches from source according to its settings.
									(let ((count (helm-insert-items matches_ source)))
										(unless first_source_max_index
											(setq first_source_max_index (1- count)))))
							(cond
								; No matches.
								((not first_source_max_index))
								; When PRESELECT is a number, always count it as preselection
								; succeeded. Not ideal but idk how to treat it.
								((or (not preselect) (setq preselect_succeeded (numberp preselect)))
									(let (
										(no_or_number_preselect_fn
											(lambda (&optional no_follow)
												(goto-char (point-min))
												(helm-goto-next-candidate)
												(let (
													(target_index
														(min
															; This is the max limit -
															; index of the last candidate.
															; Preselection by index
															; is supported only in first source.
															first_source_max_index
															(or
																preselect
																(and
																	(not helm-inhibit-move-to-first-candidate)
																	(get-text-property (point) 'helm-new)
																	1)
																0)))
												)
													(while (not (eq target_index (helm-get-index)))
														(forward-line 1)))
												(helm-select-line)
												(unless no_follow
													(helm-follow-execute-persistent-action-maybe))))
									)
										(if window
											(with-selected-window window
												(funcall no_or_number_preselect_fn))
											; Don't run persistent action, because it needs helm-window.
											; If helm-window will actually be displayed, it will
											; be run in helm-internal.
											(funcall no_or_number_preselect_fn t))))
								(window (with-selected-window window (funcall preselect_fn)))
								; Like above.
								(t (funcall preselect_fn t)))))
					preselect_succeeded)
				; When there is only one async source, run `helm-after-update-hook'
				; in `helm-async-process-filter', when there is more than one source,
				; run `helm-after-update-hook' now even if an async source is
				; present and running in BG.
				(or
					(helm-source-async-p (helm-get-source))
					; This check was added to not call the hook that use-default-as-input
					; adds and it doesn't colide with anything (I think) - showing actions
					; doesn't use this hook in any way.
					(helm-in-action-buffer)
					(run-hooks 'helm-after-update-hook))
				(setq helm--force-updating-p nil)))))

(cl-defun helm-force-update (&optional refresh (preselect t) is_interactive)
"If REFRESH is non-nil, clear cached candidates of sources of current helm session.

PRESELECT is like :preselect arg of `helm', or t to get regex from currently
selected candidate.

Selection of current candidate is preserved, if it still exists after
update, or moved to PRESELECT, if specified.

Interactive calls also call `helm-source-update' functions of sources of current
helm session."
	(interactive (list t t t))
	(with-helm-window
		(and
			(eq preselect t)
			(setq preselect (helm-get-selection t))
			(setq preselect (helm-get-regex-for-preselection preselect)))
		(setq helm--force-updating-p t)
		(when refresh
			(dolist (source helm-sources)
				(remhash source helm-candidate-cache)
				(when-let* (is_interactive (update_fn (helm-source-update source)))
					(helm-apply-fn-from-source source update_fn))))
		; Preserve helm-window's vertical scroll, if preselect was successful.
		(let ((line_count_from_window_top (line_count_backward (window-start))))
			(when (helm-update preselect)
				(recenter line_count_from_window_top)))))

; Keybindings for keymap for candidate lines in helm buffers.

(defun helm-down-mouse-1-and-3-base (event fn)
	(setq event (nth 1 event))
	(let ((window (posn-window event)))
		(if (eq (get-buffer helm-buffer) (window-buffer window))
			(with-selected-window window (funcall fn (posn-point event)))
			(message "Selection in helm-window not available while selecting action."))))

(defun helm-down-mouse-1 (event) "Select candidate." (interactive "e")
	(helm-down-mouse-1-and-3-base
		event
		(lambda (pos)
			(goto-char pos)
			(helm-goto-candidate-start)
			(helm-select-line)
			(helm-follow-execute-persistent-action-maybe))))

(defun helm-down-mouse-3 (event) "Toggle mark." (interactive "e")
	(helm-down-mouse-1-and-3-base
		event
		(lambda (pos)
			(save-excursion
				(goto-char pos)
				(helm-goto-candidate-start)
				(helm-toggle-visible-mark)))))

(defun helm-down-mouse-2 () "Show actions for selected candidate." (interactive)
	(if (helm-in-action-buffer)
		(helm-select-action) ; Just go back to helm-buffer (for no good reason).
		(let ((source (helm-get-current-source)))
			(if-let ((actions (helm-get-actions-from-source source)))
				(if (functionp actions)
					(message "Sole action: %s." (helm-symbol-name actions))
					(let (
						(action
							; This either returns a non-nil value or quits.
							(x-popup-menu
								t (list "Available actions" (cons "" actions))))
						(cand
							(helm-withprop-to-cand
								(helm-get-selection 'withprop)))
					)
						(setq helm-action-info
							(list
								source
								(cdr action)
								cand
								(helm-get-real cand))))
					; Don't confirm because now only confirmation with
					; [return] and similar non mouse keys is supported.
					(helm-maybe-exit-minibuffer t))
				(message "No actions available.")))))

(defconst helm-candidate-keymap
	(let ((keymap (make-sparse-keymap)))
		; Select candidate.
		(define-key keymap [down-mouse-1] #'helm-down-mouse-1)
		(define-key keymap [double-down-mouse-1] (lambda () (interactive) (helm-return t)))

		; Popup action menu to select action.
		(define-key keymap [down-mouse-2] #'helm-down-mouse-2)

		; Mark candidate.
		(define-key keymap [down-mouse-3] #'helm-down-mouse-3)
		(define-key keymap [double-down-mouse-3] #'helm-down-mouse-3)
		(define-key keymap [triple-down-mouse-3] #'helm-down-mouse-3)

		keymap))

; Insert items into helm buffer.

(defun helm-insert-items (items source &optional count)
"Insert ITEMS into current buffer. Return new number of candidates.

If COUNT is non-nil, resume inserting from candidate number COUNT.
Don't insert source header in that case.

ITEMS is a list of:
	Candidates in form (display . real) or display.
	DISPLAY will be inserted with newline appended and marked with these props:
		keymap - `helm-candidate-keymap';
		helm-index - candidate index in this helm buffer, starting from 0;
		helm-source - SOURCE;
		helm-real - REAL, but only if candidate is in (display . real) form.
	These are so called \"candidate lines\" usually identified by checking for
	existence of helm-index prop. These are the only items that can be selected.
	There must be at least 1 item like that in ITEMS.

	If SOURCE has `helm-source-noncandidate' non-nil, then in ITEMS can be strings
	with prop helm-noncandidate - inserted w/o adding newline or any props.
	It can't have helm-index and helm-source props, because they would be mistaken
	with normal candidate lines.
	If `helm-source-noncandidate' is \\='header and first ITEM is a noncandidate
	line, insert it instead of source header (don't add newline, don't add face,
	add \\='helm-header t prop)."
	(let (
		(noncandidate (helm-source-noncandidate source))
		(insert_cand
			(lambda (cand) ; Dynamic binding: count.
				(let ((start (point)) display real)
					(if (consp cand)
						(setq display (car cand) real (cdr cand))
						(setq display cand))
					(insert (propertize display 'read-only nil) ?\n)
					(when real (put-text-property start (point) 'helm-real real))
					; This is pretty weird, or I just didn't knew it worked like that:
					; 'keymap prop on the last char in line (but not on newline),
					; still extends to the entire line, meaning clicks on blank space
					; after the last char in line still belong to this keymap.
					; Also, keymap of the last char in buffer extends to the entire
					; blank space after the end of buffer.
					; Because of that, we need to just not mark the newline after
					; candidate with 'keymap.
					(put-text-property start (1- (point)) 'keymap helm-candidate-keymap)
					(put-text-property start (point) 'helm-index count)
					(put-text-property start (point) 'helm-source source))
				(++ count)))
		(is_noncandidate
			(lambda (item)
				(and
					(stringp item)
					(get-text-property 0 'helm-noncandidate item))))
		(maybe_insert_noncandidate
			(lambda (item)
				(when (funcall is_noncandidate item)
					(insert item)
					t)))
	)
		(unless count
			; This is the first batch of items.
			(setq count 0)
			; Insert header.
			(unless (bobp) (insert (propertize "\n" 'face 'helm-eob-line)))
			(let ((start (point)))
				(if (and (eq noncandidate 'header) (funcall is_noncandidate (car items)))
					(insert (pop items))
					(insert (helm-get-name source) ?\n)
					(add-face-text-property start (point) 'helm-source-header))
				; You can add 'display-line-numbers-disable t here to not display
				; line numbers on source headers.
				; I deleted this because helm doesn't support display-line-numbers-mode
				; well anyway. It could implement it itself, by just prepending a line
				; number string, though I don't need it now so maybe someday.
				(put-text-property start (point) 'helm-header t)))
		; This could be simpler but I optimized it a little because
		; 90% of the time it's used in the simplest form and this really should
		; be as fast as possible.
		(if (helm-source-multiline source)
			(if noncandidate
				(let (
					(after_noncandidate
						(not (get-text-property (1- (point)) 'helm-index)))
				)
					(dolist (item items)
						(if (funcall maybe_insert_noncandidate item)
							(setq after_noncandidate t)
							(unless after_noncandidate (insert helm-candidate-separator))
							(setq after_noncandidate nil)
							(funcall insert_cand item))))
				; This is fine because there should never be a trailing candidate
				; separator.
				(funcall insert_cand (pop item))
				(dolist (item items)
					(insert helm-candidate-separator)
					(funcall insert_cand item)))
			(if noncandidate
				(dolist (item items)
					(unless (funcall maybe_insert_noncandidate item)
						(funcall insert_cand item)))
				(dolist (item items) (funcall insert_cand item)))))
	count)

; Actions

(defvar helm-action-alist nil
"List (action_description . action_fn) - candidates of `helm-action-source'.
Updated only in `helm-select-action' as a cache for `helm-action-source'.
It probably shouldn't be used anywhere else.")

; This is a special source - it doesn't run in helm-buffer, doesn't run any init
; functions, cleanup, and more.
; It's only accessible through `helm-select-action', and everything is setup there.
(defconst helm-action-source
	(let (
		; Add matching method for action numbers.
		; The point is to be able to type action's number
		; and always get the "correct" result, because w/o this,
		; when pattern is "2" and candidates are:
		; "202 shorter desc"
		; "2   long action description"
		; this shorter action will be higher, which is not what is wanted here,
		; so perform quick number matching that has highest priority -
		; only one match is possible - exact number match.
		(match
			(cons
				(list
					(lambda ()
						; If pattern is a number.
						(when (string-match-p "\\`[1-9][0-9]*\\'" helm-pattern)
							helm-pattern))
					(lambda (cand)
						(when
							(string=
								helm-pattern
								(substring-no-properties
									cand 0 (string-search " " cand 1)))
							(list (cons 0 (length helm-pattern))))))
				helm-match-multi))
	)
		(helm-source-sync-make nil
			:name "Actions"
			:candidates (lambda () helm-action-alist)
			:volatile t
			; Candidates are already sorted by index, so don't
			; use alphabetical sorting.
			; Also ("2 - asd" "10 - zxc") sorted by string< results in
			; ("10 - zxc" "2 - asd").
			:sort #'helm-sort-length
			:match match
			:match-alist
				(list
					(cons "action" match)
					(cons "multi mix" helm-match-multi)
					helm-match-substring-cell
					helm-match-regex-cell
					helm-match-multi-single-cell)
			:candidate-number-limit 9999
			:follow 'never
			:nomark t)))

(defun helm-select-action ()
"Select an action for the currently selected candidate.
If action buffer is selected, back to the helm buffer."
	(interactive)
	(let (
		(source (helm-get-current-source))
		cand
		(set_prompt
			(lambda (prompt)
				(with-selected-window (minibuffer-window)
					(let ((inhibit-read-only t)) (erase-buffer))
					(insert
						(propertize prompt
							'face 'helm-minibuffer-prompt
							'field t
							'read-only t
							'rear-nonsticky t
							'front-sticky t)))))
	)
		(run-hooks 'helm-select-action-hook)
		(with-selected-frame (window-frame (helm-window))
			(prog1
				(cond
					((helm-in-action-buffer)
						(set-window-buffer
							(get-buffer-window helm-action-buffer t) helm-buffer)
						(kill-buffer helm-action-buffer)
						(setq helm-action-source-and-cand nil)
						; Restore prompt.
						(funcall set_prompt
							(buffer-local-value 'helm-prompt (get-buffer helm-buffer)))
						; Restore helm-pattern.
						(helm-set-pattern helm-action-pattern t)
						(setq helm-action-pattern nil)
						; Restore previous input handling method.
						(when-let (
							(delay
								(buffer-local-value 'helm-delay (get-buffer helm-buffer)))
						)
							(with-current-buffer (window-buffer (minibuffer-window))
								(remove-hook 'post-command-hook
									#'helm-instant-post-command-hook t)
								(add-hook 'post-command-hook
									#'helm-delay-post-command-hook 90 t)
								(helm-setup-idle-update delay)))
						(when helm-action-update
							(helm-force-update t)
							(setq helm-action-update nil)))
					((setq cand (helm-get-selection 'withprop))
						(let (
							(actions (helm-get-actions-from-source source))
							helm-onewindow-p
						)
							(if (functionp actions)
								(message "Sole action: %s." (helm-symbol-name actions))
								; Show actions.

								(setq helm-action-source-and-cand (cons source cand))
								(setq helm-action-pattern helm-pattern)
								; Create candidate list for helm-action-source.
								(setq helm-action-alist
									(let (
										(max_length (length (number-to-string (length actions))))
										(i 0)
									)
										(mapcar
											(lambda (action)
												(++ i)
												(cons
													(let ((i_str (number-to-string i)))
														(concat
															i_str
															(get_space_string
																(1+ (- max_length (length i_str))))
															(propertize (car action)
																'face 'helm-action)))
													(cdr action)))
											actions)))
								; Setup helm-action-buffer, similar to helm-buffer.
								(with-current-buffer (get-buffer-create helm-action-buffer)
									(setq buffer-undo-list t)
									; Don't display cursor.
									(setq cursor-type nil)
									(setq cursor-in-non-selected-windows t)

									(use-local-map helm-local-map)
									(setq-local helm-sources (list helm-action-source))
									(setq-local helm-default-source helm-action-source)
									(setq-local helm-selection-overlay nil)
									(helm-init-selection-overlay helm-action-buffer))
								(set-window-buffer
									(get-buffer-window helm-buffer t) helm-action-buffer)
								(funcall set_prompt helm--action-prompt)
								(setq helm-pattern "")
								(helm-update)
								; Switch to instant input handling.
								(when
									(buffer-local-value 'helm-delay (get-buffer helm-buffer))
									(cancel-timer helm-timer)
									(setq helm-timer nil)
									(with-current-buffer (window-buffer (minibuffer-window))
										(remove-hook 'post-command-hook
											#'helm-delay-post-command-hook t)
										(add-hook 'post-command-hook
											#'helm-instant-post-command-hook 90 t))))))
					(t (message "No actions available.")))
				(run-hooks 'helm-window-configuration-hook)))))

; Set min-widths for some mode-line parts.
(defconst helm-mode-line-candidates-display
	(list
		'min-width
		(list
			(pixel_to_canonical_char_width
				(get_string_pixel_width
					(propertize "000/0000" 'face 'mode-line))))))
(defconst helm-mode-line-marked-candidates-display
	(list
		'min-width
		(list
			(pixel_to_canonical_char_width
				(get_string_pixel_width
					(propertize "00/000" 'face 'mode-line))))))

(defun helm-mode-line-update () "Update mode-line and header-line for `helm-buffer'."
	(with-helm-window
		(let ((source (or (helm-get-current-source) helm-default-source)))
			(setq header-line-format
				(if-let* (source (source_local_value (helm-source-header-line source)))
					(let (
						(str
							(concat
								(if (functionp source_local_value)
									(helm-apply-fn-from-source source source_local_value)
									source_local_value)
								(get_space_string (window-body-width))))
					)
						(add-face-text-property 0 (length str) 'helm-header nil str)
						str)
					(default-value 'header-line-format)))
			(setq mode-line-format
				(propertize_no_copy
					(concat
						" "
						(when source
							(concat
								(propertize_no_copy
									(concat
										(if (is_buffer_empty)
											"0"
											(number-to-string (1+ (helm-get-index))))
										"/"
										(number-to-string (helm-get-candidate-number t)))
									'face
									(if helm-suspend-update-flag
										'helm-candidate-number-suspended
										'helm-candidate-number)
									'display helm-mode-line-candidates-display)
								"  "
								(when helm-marked-candidates
									(concat
										(propertize_no_copy
											; visible_marked_in_current_source/all_marked_in_current_source.
											; Cross marking isn't supported, so no much point
											; in showing marked candidates not from current source.
											(concat
												(number-to-string
													(cl-count-if
														(lambda (ov)
															(and
																(overlay-buffer ov)
																(eq source (overlay-get ov 'source))))
														helm-marked-candidates))
												"/"
												(number-to-string
													(cl-count-if
														(lambda (ov) (eq source (overlay-get ov 'source)))
														helm-marked-candidates)))
											'face 'helm-visible-mark
											'display helm-mode-line-marked-candidates-display)
										"  "))
								(and
									(helm-source-follow source)
									(not (eq (helm-source-follow source) 'never))
									"F  ")
								(when-let (
									(match_description
										(car
											(rassq
												(helm-source-match source)
												(or
													(helm-source-match-alist source)
													helm-match-alist))))
								)
									(concat "Match: " match_description "  "))))
						; Only show case sensitivity when it is different to the default.
						(let (
							(local_helm_case_fold_search
								(buffer-local-value 'helm-case-fold-search (get-buffer helm-buffer)))
						)
							(unless (eq (default-value 'helm-case-fold-search) local_helm_case_fold_search)
								(concat
									"Case: "
									(cond
										((eq local_helm_case_fold_search 'smart) "smart")
										(local_helm_case_fold_search "insensitive")
										(t "sensitive"))
									"  ")))
						(when prefix-arg
							(concat
								(propertize_no_copy (format "P %S" prefix-arg)
									'face 'helm-prefarg)
								"  "))
						(when helm-source-filter
							(concat
								"Filter: "
								(mapconcat #'helm-get-name helm-source-filter ", ")
								"  "))
						(when-let* (source (source_local_value (helm-source-mode-line source)))
							(if (functionp source_local_value)
								(helm-apply-fn-from-source source source_local_value)
								source_local_value))
						(get_space_string (window-body-width)))
					; Add this keymap to disallow selecting helm-window.
					; (Emacs by default adds some default keymap to every mode-line
					; with bindings for selecting mode-line's window.)
					'keymap
					'(keymap
						(mode-line keymap
							(mouse-1 . ignore)
							(down-mouse-1 . ignore)
							(drag-mouse-1 . ignore)
							(mouse-2 . ignore)
							(down-mouse-2 . ignore)
							(drag-mouse-2 . ignore)
							(mouse-3 . ignore)
							(down-mouse-3 . ignore)
							(drag-mouse-3 . ignore))))))
		(force-mode-line-update)))

; This should be used only in `post-command-hook'.
(defun helm--update-header-line ()
	(let ((buffer (helm-buffer-get)))
		(when (buffer-local-value 'helm-echo-input-in-header-line (get-buffer buffer))
			(with-selected-window (minibuffer-window)
				(let* (
					(beg (save-excursion (vertical-motion 0 (helm-window)) (point)))
					(end (save-excursion (end-of-visual-line) (point)))
					; The visual line where the cursor is.
					(cont (buffer-substring beg end))
					(pref
						(propertize " "
							'display
							(if
								(string-match-p
									(regexp-opt
										(list
											(buffer-local-value
												'helm-prompt (get-buffer helm-buffer))
											helm--action-prompt))
									cont)
								(list 'space :width helm-header-line-space-before-prompt)
								(propertize "->" 'face 'helm-header-line-left-margin))))
					(pos (- (point) beg))
				)
					; Increment pos each time we find a "%" up to current-pos (bug#1648).
					(cl-loop
						for c across (buffer-substring-no-properties beg (point))
						when (= c ?%) do (++ pos))
					; Increment pos when cursor is on a "%" to make it visible in header-line
					; i.e "%%|" and not "%|%" (bug#1649).
					(when (eq (char-after) ?%) (++ pos))
					(setq cont (replace-regexp-in-string "%" "%%" cont))
					(with-current-buffer buffer
						(setq header-line-format (concat pref cont " "))
						(put-text-property
							; Increment pos to handle the space before prompt (i.e `pref').
							(1+ pos) (+ 2 pos)
							'face
							; Don't just use cursor face, this can hide the current character.
							(list
								:inverse-video t
								:foreground (face-background 'cursor)
								:background (face-background 'default))
							header-line-format)
						(force-mode-line-update)))))))

(defun helm-hide-minibuffer-maybe ()
"Hide minibuffer contents in a helm session.
This function should normally go to `helm-minibuffer-set-up-hook'.
It has no effect if `helm-echo-input-in-header-line' is nil."
	(when (buffer-local-value 'helm-echo-input-in-header-line (get-buffer (helm-buffer-get)))
		(let ((overlay (make-overlay (point-min) (point-max) nil nil t)))
			(overlay-put overlay 'window (selected-window))
			(overlay-put overlay 'face
				(let ((background (face-background 'default)))
					(list :background background :foreground background))))
		(when highlightLine::mode (highlightLine::mode -1))
		; Don't display cursor.
		(setq cursor-type nil)
		(setq cursor-in-non-selected-windows t)))


(defun helm-set-selection-overlay-face ()
"Put face on helm-selection-overlay depending if we are on a marked candidate."
	(with-helm-window
		(overlay-put helm-selection-overlay 'face
			(if (helm-this-visible-mark)
				'helm-selection-marked
				'helm-selection))))

(defun helm-select-line ()
"Select candidate at point - move `helm-selection-overlay' to it.
This overlay is only used as a visual info for user. Selection,
marking, etc. are all based on point, not this overlay (like it once was)."
	(move-overlay
		helm-selection-overlay
		(point)
		(save-excursion (helm-goto-candidate-end) (point))))

; Resplit helm window

(defun helm-toggle-resplit-window ()
"Toggle resplit helm window, vertically or horizontally."
	(interactive)
	(if
		(not
			(and
				(length= (window-list nil 0) 2)
				(not (window-dedicated-p (get-buffer-window helm-current-buffer)))))
		(message "Current window configuration is not suitable for splitting.")
		(when helm-prevent-escaping-from-minibuffer
			(helm-prevent-switching-other-window nil))
		(unwind-protect
			(with-helm-window
				(cond
					((or helm-full-frame (one-window-p t))
						(message "Attempt to resplit a single window."))
					((helm-in-action-buffer)
						(message "Can't resplit while selecting actions."))
					(t
						(let ((before-height (window-height)))
							(delete-window)
							(set-window-buffer
								(select-window
									; If initial split was horizontal.
									(if (= (window-height) before-height)
										; Split window vertically with `helm-buffer' placed
										; on the good side according to value of helm::split_window_side.
										(progn
											(setq helm-split-window-state 'vertical)
											(if (memq helm::split_window_side '(above left))
												(split-window nil nil 'above)
												(split-window-below)))
										(setq helm-split-window-state 'horizontal)
										(if (memq helm::split_window_side '(above left))
											(split-window nil nil 'left)
											(split-window-right))))
								helm-buffer))))
				(helm::set_window_side_state))
			(when helm-prevent-escaping-from-minibuffer
				(helm-prevent-switching-other-window t)))))

(defun helm-toggle-full-frame (&optional arg)
"Toggle `helm-buffer' full-frame view."
	(interactive "p")
	(cond
		((helm-in-action-buffer)
			(message "Can't toggle full frame from action window."))
		; Only show this message in an interactive call.
		((and arg helm--buffer-in-new-frame-p)
			(message "Can't toggle full frame when using helm own frame."))
		((or
				helm-onewindow-p
				(buffer-local-value 'helm-full-frame (get-buffer helm-buffer)))
			(with-current-buffer helm-buffer (setq-local helm-full-frame nil))
			(setq helm-onewindow-p nil)
			; FIXME This is generally pretty bad, as now we have just one normal window
			; in frame, and before first 'helm-toggle-full-frame' call we could have
			; some more windows. We could save window configuration, but then some
			; helm hooks may not run, and also some helm commands create additional
			; windows and deletion of them randomly is not accounted for in some
			; of them as far as I know, so this should be fixed.
			(let ((split-window-preferred-function #'helm::split_window))
				(switch-to-buffer helm-current-buffer))
			(with-current-buffer helm-buffer (helm-display-buffer))
			(select-window (minibuffer-window)))
		(t
			(with-helm-window
				(delete-other-windows)
				(setq-local helm-full-frame t))
			(setq helm-onewindow-p t))))

(defun helm-swap-windows ()
"Swap window holding `helm-buffer' with other window."
	(interactive)
	(cond
		((not (length= (window-list nil 0) 2))
			(message "Current window configuration is not suitable for splitting."))
		((and helm-full-frame (one-window-p t))
			(message "Can't swap windows in a single window."))
		((helm-in-action-buffer)
			(message "Can't resplit while selecting actions."))
		(t
			(let* (
				(w1 (helm-window))
				(split-state (eq helm-split-window-state 'horizontal))
				(w1size (window-total-size w1 split-state))
				(b1 (window-buffer w1)) ; helm-buffer
				(s1 (window-start w1))
				(cur-frame (window-frame w1))
				(w2
					(with-selected-window w1
						; Don't try to display helm-buffer
						; in a dedicated window.
						(get-window-with-predicate
							(lambda (w) (not (window-dedicated-p w)))
							0
							cur-frame)))
				(w2size (window-total-size w2 split-state))
				(b2 (window-buffer w2)) ; probably helm-current-buffer
				(s2 (window-start w2))
				resize
			)
				(with-selected-frame cur-frame
					(let (
						(replace_buffer_in_window
							; Replace BUFFER1 by BUFFER2 in WINDOW registering BUFFER1.
							(lambda (window buffer_1 buffer_2)
								(when (get-buffer-window buffer_1)
									(unrecord-window-buffer window buffer_1)
									(set-window-buffer window buffer_2))))
					)
						(funcall replace_buffer_in_window w1 b1 b2)
						(funcall replace_buffer_in_window w2 b2 b1))
					(setq resize
						(cond
							; helm-window is smaller than other window.
							((< w1size w2size)
								(-
									(- (max w2size w1size) (min w2size w1size))))
							; helm-window is larger than other window.
							((> w1size w2size)
								(- (max w2size w1size) (min w2size w1size)))
							; windows have probably same size.
							))
					; Maybe resize the window holding helm-buffer.
					(when resize (window-resize w2 resize split-state))
					(set-window-start w1 s2 t)
					(set-window-start w2 s1 t))
				(helm::set_window_side_state)))))

(defun helm::set_window_side_state ()
"Set 'helm--window-side-state' and return the position of `helm-window'
from `helm-current-buffer'.
Possible values are \\='left \\='right \\='below or \\='above."
	(setq helm--window-side-state
		(let (
			(helm_window (helm-window))
			(helm_current_buffer_window (get-buffer-window helm-current-buffer t))
		)
			(find_in_list '(left right below above)
				(lambda (side)
					(eq
						helm_window
						(window-in-direction side helm_current_buffer_window t)))))))

; Persistent action

(defun helm-execute-persistent-action ()
"When `helm-full-frame' is non-nil, and `helm-buffer' is displayed in only
one window, the helm window is split to display
`helm-select-persistent-action-window' in other window to maintain visibility."
	(interactive)
	(when-let (
		((not (helm-in-action-buffer)))
		(source (helm-get-current-source))
	)
		(let* (
			(selection (helm-get-selection))
			(action
				(or
					(helm-source-persistent-action source)
					(when-let ((action (helm-source-persistent-action-if source)))
						(helm-apply-fn-from-source source action selection))))
		)
			(when action
				(let (
					(fn (if (functionp action) action (car action)))
					(maybe_split (and (consp action) (not (functionp action))))
					; Is next-window (from helm-window) a suitable window for PA?
					(no-suitable-win
						(when-let (
							((not helm--buffer-in-new-frame-p))
							(window (get-buffer-window helm-current-buffer))
						)
							(or
								(window-dedicated-p window)
								(window-parameter window 'window-side))))
				)
					(and
						helm-onewindow-p
						maybe_split
						(not helm--buffer-in-new-frame-p)
						(helm-toggle-full-frame))
					(with-helm-window
						(save-selected-window
							; Select the window that will be used for persistent action.
							(select-window
								(setq minibuffer-scroll-window
									(helm-persistent-action-display-window
										(and
											maybe_split
											(or helm-onewindow-p no-suitable-win)))))
							(let (
								(helm-in-persistent-action t)
								(display-buffer-alist '((".*" (display-buffer-same-window))))
								pop-up-windows
								pop-up-frames
								special-display-regexps
								special-display-buffer-names
							)
								(helm-apply-fn-from-source source fn selection))
							; A typical case is when a persistent action delete
							; the buffer already displayed in
							; `helm-persistent-action-display-window' and `helm-full-frame'
							; is enabled, we end up with the `helm-buffer'
							; displayed in two windows.
							(and
								helm-onewindow-p
								(length> (window-list) 1)
								(equal
									(buffer-name
										(window-buffer
											helm-persistent-action-display-window))
									(helm-buffer-get))
								(delete-other-windows)))))))))

(defun helm-persistent-action-display-window (&optional split)
"Set 'helm-persistent-action-display-window' and return the window
that will be used for persistent action.
If SPLIT is non-nil window may be split (wow!)."
	(with-helm-window
		(setq helm-persistent-action-display-window
			(cond
				((and
						helm-persistent-action-display-window
						(window-live-p helm-persistent-action-display-window)
						(not
							(memq helm-persistent-action-display-window
								(get-buffer-window-list helm-buffer))))
					helm-persistent-action-display-window)
				((and helm--buffer-in-new-frame-p helm-initial-frame)
					(with-selected-frame helm-initial-frame
						(let ((win (selected-window)))
							(if
								(or
									(window-dedicated-p win)
									(window-parameter win 'window-side))
								(next-window win 0)
								win))))
				((and split (window::split_window_sensibly)))
				((get-buffer-window helm-current-buffer))
				(t (previous-window nil 0))))))

; Scrolling - recentering

(defun helm-scroll-other-window-base (fn arg)
	(let ((minibuffer-scroll-window (helm-persistent-action-display-window)))
		(funcall fn (or arg helm-scroll-amount))))

(defun helm-scroll-other-window-up (&optional arg)
"Scroll other window upward ARG many lines.
When arg is not provided scroll `helm-scroll-amount' lines."
	(interactive "P")
	(helm-scroll-other-window-base #'scroll-other-window arg))

(defun helm-scroll-other-window-down (&optional arg)
"Scroll other window downward ARG many lines.
When arg is not provided scroll `helm-scroll-amount' lines."
	(interactive "P")
	(helm-scroll-other-window-base #'scroll-other-window-down arg))

(defun helm-recenter-top-bottom-other-window (&optional arg)
"Run `recenter-top-bottom' in other window.
Meaning of prefix ARG is the same as in `recenter-top-bottom'."
	(interactive "P")
	(with-selected-window (with-helm-window (helm-persistent-action-display-window))
		(recenter-top-bottom arg)))

; Utility: Visible Mark

(defun helm-clear-visible-mark ()
	(with-helm-buffer
		(mapc #'delete-overlay helm-marked-candidates)
		(setq helm-marked-candidates nil)))

(defun helm-this-visible-mark ()
	(cl-loop
		for ov in (overlays-at (point))
		when (overlay-get ov 'helm-visible-mark) return ov))

(defun helm-delete-visible-mark (overlay)
	(delete-overlay overlay)
	(setq helm-marked-candidates (delq overlay helm-marked-candidates)))

(defun helm-is-mark-visible ()
"Return non-nil if there is at least one marked candidate in currently
displayed ones.
Good for checking if better to call helm-unmark-all or mark some cands."
	(unless (helm-in-action-buffer)
		(find_in_list
			(buffer-local-value 'helm-marked-candidates (get-buffer helm-buffer))
			#'overlay-buffer)))

(defun helm-make-visible-mark () "Must be in helm-window."
	(let ((o (make-overlay (point) (save-excursion (helm-goto-candidate-end) (point)))))
		(overlay-put o 'priority 0)
		(overlay-put o 'before-string
			(propertize " "
				'display `((margin left-margin) ,helm-visible-mark-prefix)))
		(overlay-put o 'evaporate t)
		(overlay-put o 'face 'helm-visible-mark)

		(overlay-put o 'helm-visible-mark t)
		(overlay-put o 'source (helm-get-source))
		(overlay-put o 'withprop (helm-get-selection 'withprop))
		(overlay-put o 'real (helm-get-selection))
		(push o helm-marked-candidates)))

(defun helm-toggle-visible-mark (&optional arg)
"Toggle helm visible mark at point ARG times.
If ARG is negative toggle backward.
If arg is nil, just toggle mark on current line."
	(interactive "P")
	(with-helm-window
		(let ((source (helm-get-source)))
			(cond
				; Don't do anything if there is no candidate selected.
				((not source))
				((helm-source-nomark source)
					(message "Marking not allowed in this source."))
				((get-text-property (point) 'helm-new)
					(message "Marking new candidates not allowed."))
				(t
					(if (and arg (/= (setq arg (prefix-numeric-value arg)) 0))
						(let (
							(toggle_fn
								(lambda ()
									(let ((this_visible_mark (helm-this-visible-mark)))
										(cond
											(this_visible_mark
												(helm-delete-visible-mark this_visible_mark))
											((not (get-text-property (point) 'helm-new))
												(helm-make-visible-mark))))))
						)
							(if (< arg 0)
								(cl-loop
									repeat (- arg)
									do (funcall toggle_fn)
									until (eq (helm-get-index) 0)
									do
									(helm-goto-prev-candidate)
									(helm-goto-candidate-start))
								(cl-loop
									repeat arg
									do
									(funcall toggle_fn)
									(helm-goto-candidate-end)
									(while
										(not
											(or
												(helm-get-index)
												(helm-on-header)
												(eobp)))
										(forward-line 1))
									while (helm-get-index))
								(unless (helm-get-index)
									(helm-goto-prev-candidate)
									(helm-goto-candidate-start)))
							(helm-select-line))
						(if-let ((this_visible_mark (helm-this-visible-mark)))
							(helm-delete-visible-mark this_visible_mark)
							(helm-make-visible-mark)))
					(helm-update-window-margins))))))

(defun helm-mark-all ()
"Mark all visible unmarked candidates in current source."
	(interactive)
	(with-helm-window
		(when-let ((source (helm-get-source)))
			(if (helm-source-nomark source)
				(message "Marking not allowed in this source.")
				(helm-map-candidates-in-source
					(lambda ()
						(or
							(helm-this-visible-mark)
							(get-text-property (point) 'helm-new)
							(helm-make-visible-mark))))
				(helm-update-window-margins)))))

(defun helm-unmark-all ()
"Unmark all candidates in all sources of current helm session."
	(interactive)
	(with-helm-window
		(helm-clear-visible-mark)
		(set-window-margins nil helm-left-margin-width)))

(defun helm-toggle-all-marks ()
"Mark all visible candidates of current source or unmark all
candidates visible or invisible in all sources of current helm session."
	(interactive)
	(if (helm-is-mark-visible) (helm-unmark-all) (helm-mark-all)))

(defun helm-marked-candidates (&optional with-wildcard withprop)
"Return marked candidates of current source, if any, else, if there is current
selection, return it in a list.

When WITH-WILDCARD is non-nil, do some expansion similar to file-expand-wildcards.

WITHPROP non-nil means to return list of (helm-get-selection \\='withprop) calls,
but this only matters when WITH-WILDCARD is nil.

Don't use this function for helm action source, it doesn't use marking anyway."
	; Make sure there is current source and selection.
	(unless (helm-empty-buffer-p)
		(with-current-buffer helm-buffer
			(let ((source (helm-get-current-source)))
				(if with-wildcard
					(let (
						(handle_wildcards
							(lambda (real)
								(if
									(and
										(string-search "*" real)
										(not (file-exists-p real)))
									(or
										; Same as `file-expand-wildcards' but allow recursion.
										; Recursion happens when PATTERN starts with two stars.
										; Directories expansion is not supported.
										(let (
											(bn (helm-basename real))
											(wildcard_to_regex
												; Transform wilcard like \"**.{jpg,jpeg}\".
												(lambda (wilcard)
													(when
														(string-match
															".*\\(\\*\\{1,2\\}\\)\\.[{]\\(.*\\)[}]\\'"
															wilcard)
														(format ".*\\.\\(%s\\)$"
															(replace-regexp-in-string
																","
																"\\\\|"
																(match-string 2 wilcard))))))
											case-fold-search
										)
											(ignore-errors
												(if
													(and
														helm-file-globstar
														(string-match "\\`\\*\\{2\\}\\(.*\\)" bn))
													(helm-walk-directory (file-name-directory real)
														:path 'full
														:match
															(or
																(funcall wildcard_to_regex bn)
																(wildcard-to-regexp bn))
														:skip-subdirs t)
													(if-let ((regex (funcall wildcard_to_regex bn)))
														(helm-files-directory-files (file-name-directory real) t regex)
														; `file-expand-wildcards' fails to expand weird directories
														; like "[ foo.zz ] bar.*.avi", fallback to `directory-files'
														; in such cases.
														(or
															(file-expand-wildcards real t)
															(helm-files-directory-files
																(file-name-directory real)
																t
																(wildcard-to-regexp bn)))))))
										; Avoid returning a not expanded wildcard fname.
										; e.g assuming "/tmp" doesn't contain "*.el"
										; return nil when real is "/tmp/*.el".
										(when
											(or
												(string-match-p helm--url-regexp real)
												(not (string-match-p "[[*?]" real)))
											(list real)))
									(list real))))
					)
						(or
							(nreverse
								(mapcan
									(lambda (ov)
										(when (eq source (overlay-get ov 'source))
											(funcall handle_wildcards
												(overlay-get ov 'real))))
									helm-marked-candidates))
							(funcall handle_wildcards (helm-get-selection))))
					(or
						(nreverse
							(cl-loop
								with prop = (if withprop 'withprop 'real)
								for ov in helm-marked-candidates
								when (eq source (overlay-get ov 'source))
									collect (overlay-get ov prop)))
						(list (helm-get-selection (when withprop 'withprop)))))))))

; Restore marked candidates when helm updates display.
(add-hook 'helm-after-update-hook
	(fn_symbol "helm-revive-visible-mark"
		(lambda ()
			(when-let (
				((not (is_buffer_empty (get-buffer helm-buffer))))
				(window (get-buffer-window helm-buffer 0))
			)
				(with-selected-window window
					(save-excursion
						(dolist (ov helm-marked-candidates)
							(let ((source (overlay-get ov 'source)) found)
								(goto-char (point-min))
								(helm-goto-next-candidate)
								(let (header_pos)
									(while
										(and
											(not (setq found (eq source (helm-get-source))))
											(setq header_pos (helm-get-next-header-pos)))
										(goto-char header_pos)
										(helm-goto-next-candidate)))
								(when found
									(let* (
										; This can be not a string!
										(ov-real (overlay-get ov 'real))
										(ov-ml-str
											(if-let (
												((stringp ov-real))
												(multiline (helm-source-multiline source))
												((numberp multiline))
											)
												; Assume display have been computed
												; against real e.g. kill-ring.
												(helm--multiline-get-truncated-candidate
													ov-real multiline)
												(overlay-get ov 'withprop)))
										(end
											(save-excursion
												(goto-char (or (helm-get-next-header-pos) (point-max)))
												(helm-goto-prev-candidate)
												(pos-eol)))
										start
									)
										(while
											(and
												(search-forward ov-ml-str end t)
												(save-excursion
													(goto-char (setq start (match-beginning 0)))
													(and
														(helm-get-index)
														(not (helm-this-visible-mark)))))
											(when
												(let (
													; This can be nil if candidate have only a display value.
													(real (get-text-property start 'helm-real))
												)
													(or
														(not real)
														; Check if real value of current candidate is the same
														; than the one stored in overlay.
														; This is needed when some cands have same display names.
														; Using equal allow testing any type of value for real cand.
														; Obviously this is only approximate, because we are
														; not using equal-including-properties.
														(equal ov-real real)))
												(save-excursion
													(goto-char start)
													(helm-goto-candidate-end)
													(move-overlay ov start (point)))))))))))))))

(defun helm-next-visible-mark (&optional prev)
"Move to next helm visible mark.
If PREV is non-nil move to precedent."
	(interactive)
	(with-helm-window
		(unless (is_buffer_empty)
			(goto-char
				(let (
					(caret (point))
					(point_list
						(sort
							(map_modify_list
								#'overlay-start
								(cl-delete-if-not
									#'overlay-buffer
									(copy-sequence helm-marked-candidates)))
							#'<))
				)
					(cond
						; Rule out special cases.
						((not point_list) caret)
						((and prev (<= caret (car point_list)))
							(car (last point_list)))
						((< (car (last point_list)) caret)
							(if prev (car (last point_list)) (car point_list)))
						((and (not prev) (>= caret (car (last point_list))))
							(car point_list))
						(t
							(nth
								(if prev
									(cl-loop
										for pt in point_list
										for i from 0
										when (<= caret pt) return (1- i))
									(cl-loop
										for pt in point_list
										for i from 0
										when (< caret pt) return i))
								point_list)))))
			(helm-select-line)
			(helm-follow-execute-persistent-action-maybe))))

(defun helm-prev-visible-mark () "Move to previous helm visible mark."
	(interactive)
	(helm-next-visible-mark t))

; Follow-mode: Automatic execution of persistent-action

(defun helm-follow-mode (&optional arg)
"Execute persistent action every time the cursor is moved.

This mode is source local, i.e. It applies on current source only.

This mode can be enabled or disabled interactively at anytime during a helm session."
	(interactive '(toggle))
	(with-helm-buffer
		(let ((source (or (helm-get-current-source) helm-default-source)))
			(cond
				((not source) (message "No current source."))
				((eq (helm-source-follow source) 'never)
					(message "Follow mode not allowed in this source."))
				; Leave updating mode-line to post-command-hook.
				((and
						(setf (helm-source-follow source)
							(cond
								((eq arg 'toggle) (not (helm-source-follow source)))
								((not (and (numberp arg) (<= arg 0))))))
						(not (helm-empty-buffer-p)))
					(helm-follow-execute-persistent-action-maybe))))))

(defun helm-follow-execute-persistent-action-maybe (&optional delay)
"Execute persistent action in mode `helm-follow-mode'.

This happen after: DELAY or the 'follow-delay value of current source
or `helm-follow-input-idle-delay' or `helm-input-idle-delay' secs,
or instantly if value of one of those is 'instant."
	(let ((source (helm-get-current-source)))
		(or
			(memq (helm-source-follow source) '(nil never))
			(helm-empty-buffer-p)
			(let (
				(first_non_nil_value
					(or
						delay
						(helm-source-follow-delay source)
						helm-follow-input-idle-delay
						helm-input-idle-delay))
			)
				(if (eq first_non_nil_value 'instant)
					(helm-execute-persistent-action)
					(run-with-idle-timer first_non_nil_value nil
						(lambda ()
							(when helm-alive-p
								(helm-execute-persistent-action)))))))))

; Auto-resize mode

(defun helm::autoresize_hook (&optional max-height min-height)
	(when-let ((window (helm-window)))
		(with-selected-window window
			(let ((frame_height (frame-height)))
				(fit-window-to-buffer nil
					(/
						(*
							frame_height
							(or max-height helm-autoresize-max-height))
						100)
					(/
						(*
							frame_height
							(or min-height helm-autoresize-min-height))
						100))))))

(define-minor-mode helm-autoresize-mode
"Auto resize helm window when enabled.
helm window is re-sized according to `helm-autoresize-max-height'
and `helm-autoresize-min-height'.

See `fit-window-to-buffer' for more infos."
	:global t
	(if helm-autoresize-mode
		(progn
			(add-hook 'helm-after-update-hook 'helm::autoresize_hook)
			(add-hook 'helm-window-configuration-hook 'helm::autoresize_hook))
		(remove-hook 'helm-after-update-hook 'helm::autoresize_hook)
		(remove-hook 'helm-window-configuration-hook 'helm::autoresize_hook)))


(defun helm-match-internal (candidates &optional sort use_match_data)
	(let (
		(match_fn_index (if use_match_data 2 1))
		(dups_hash (make-hash-table :test 'equal))
		(result (make-list (length candidates) nil))
		(i 0)
	)
		(dolist (match_fn (helm-source-match helm-current-source))
			(when-let ((helm-pattern (funcall (car match_fn))))
				(setq match_fn (nth match_fn_index match_fn))
				(let ((candidates_i candidates))
					(map_modify_list
						(lambda (current_value)
							(let ((cand (pop candidates_i)))
								(or
									; If already matched, skip.
									current_value
									(let ((cached_value (gethash cand dups_hash 'new)))
										(if (eq cached_value 'new)
											; Not a duplicate - proceed with matching.
											(puthash
												cand
												(when-let ((new_value (funcall match_fn cand)))
													(if sort
														(cons i new_value)
														new_value))
												dups_hash)
											; Any other value - reuse it.
											cached_value)))))
						result))
				; This is cache per match_fn.
				(clrhash dups_hash))
			(++ i))
		result))

(defun helm-match (candidates &optional sort use_match_data source)
"Match CANDIDATES based on SOURCE's matching settings.
Return list where every element is a result of matching its corresponding
candidate; element's value can be:
	list of ranges - substrings of candidate to highlight, sorted and
	not overlapping, like ((1 . 3) (4 . 5))

	t - candidate was matched but don't highlight anything

	nil - candidate not matched.

Actually, return nil if after transformation helm-pattern is an empty string.
This can be interpreted as matching everyting with no highlighting.

This function handles duplicates well (reuses matching results on found duplicates).

UPDATE: Added SORT arg - if non-nil, changes the return value to a list of
(match_fn_index . t_or_list_of_ranges), so CANDIDATES can later
be sorted based on a function that matched a candidate.

UPDATE: Added USE_MATCH_DATA arg - if non-nil, changes used matching functions
to ones returning list of `match-data's, so now list of ranges are lists
of match datas or just nil, never t. You can find 0-length matches with something
like (lambda (match_data) (= (car match_data) (nth 1 match_data)))."
	(unless source (setq source (helm-get-current-source)))
	(let* (
		(helm-current-source source)
		(helm-match-fold-diacritics (helm-source-diacritics source))
		(helm-pattern
			(if-let ((fn (helm-source-pattern-transformer source)))
				(funcall fn helm-pattern)
				helm-pattern))
	)
		(unless (string= helm-pattern "")
			(let ((case-fold-search (helm-set-case-fold-search)))
				(helm-match-internal candidates sort use_match_data)))))


(defun helm-get-max-length (candidates)
"Return length of the longest candidate in CANDIDATES.
CANDIDATES must be a list of strings."
	(cl-loop for cand in candidates maximize (length cand)))


(defun helm-map-candidates-in-source (fn)
"Map over all candidates in current source and execute FN with no args.
Must be run in helm-window.
You can throw stuff to exit out of this loop, this is safe."
	(save-excursion
		(helm-goto-first-candidate)
		(while
			(progn
				(funcall fn)
				(helm-goto-candidate-end)
				(while (not (or (helm-get-index) (helm-on-header) (eobp)))
					(forward-line 1))
				(helm-get-index)))))

(defun helm-mark-some-toggle (pred_fn)
"Mark or unmark candidates in current source, for which PRED_FN returns non-nil.

If selection is on marked cand, unmark, else mark.
This is intended to use in interactive functions,
like marking similar buffers or files; it sends a message at the end.

PRED_FN is used as arg to `helm-map-candidates-in-source', so it is called with
no args, source can't be multiline. Helm window is selected."
	(with-helm-window
		(let ((i 0) fn msg)
			(if (helm-this-visible-mark)
				(setq
					fn
						(lambda ()
							(when-let ((mark (helm-this-visible-mark)))
								(++ i)
								(helm-delete-visible-mark mark)))
					msg "unmarked")
				(setq
					fn
						(lambda ()
							(unless (helm-this-visible-mark)
								(++ i)
								(helm-make-visible-mark)))
					msg "marked"))
			; Dynamic binding strikes again.
			(helm-map-candidates-in-source `(lambda () (when (,pred_fn) (,fn))))
			(helm-update-window-margins)
			(message "%d candidate%s %s." i (if (= i 1) "" "s") msg))))



(defun helm-delete-candidate-force-base (delete_fn 0_or_1)
	(helm-force-update
		t
		(with-helm-window
			(if helm-marked-candidates
				(progn
					(mapc delete_fn (helm-marked-candidates))
					(helm-unmark-all)
					nil)
				(funcall delete_fn (helm-get-selection))
				(max 0 (- (helm-get-index) 0_or_1))))))

(defun helm-add-delete-binds-force (map delete_fn)
"Add binds for shift-delete and shift-backspace to map.
These binds delete marked candidates and preselect the current candidate
if it hasn't been deleted, or if it has been, just go to the first line.
If there are no marked candidates, delete candidate at point
and try to stay on the same line.
Uses 'helm-force-update'.
'delete_fn' is called with a candidate and should delete it,
such that candidates function of current source will not return it,
e.g. (lambda (cand) (setq kill-ring (delete cand kill-ring)))."
	(define-key map [S-delete]
		`(lambda () (interactive)
			(helm-delete-candidate-force-base #',delete_fn 0)))
	(define-key map [S-backspace]
		`(lambda () (interactive)
			(helm-delete-candidate-force-base #',delete_fn 1)))
	nil)

(defun helm-add-delete-binds
	(map delete_fn &optional non_volatile final_transformer_fn marked_transformer_fn)
"Similar to 'helm-add-delete-binds-force' but doesn't clear cache and
has some extra features.

If 'non_volatile' is nil, don't use cache:
	Call 'delete_fn' with 1 arg - candidate; return value is ignored.
	Call 'final_transformer_fn' with no args, return value is ignored.
Else:
	Call 'delete_fn' with 2 args - candidate and cache;
	return value must be the new cache.
	Call 'final_transformer_fn' with 1 arg: cache;
	return value must be the new cache.

MARKED_TRANSFORMER_FN is used to transform marked candidates."
	(let (
		(delete_candidate_base_fn
			`(lambda (0_or_1)
				(helm-force-update
					nil
					(with-helm-window
						,(if non_volatile
							`(let* (
								(source (helm-get-source))
								(cache (gethash source helm-candidate-cache))
							)
								(if helm-marked-candidates
									(let ((marked (helm-marked-candidates)))
										,(when marked_transformer_fn
											`(setq marked
												(,marked_transformer_fn marked)))
										(dolist (cand marked)
											(setq cache (,delete_fn cand cache)))
										,(when final_transformer_fn
											`(setq cache
												(,final_transformer_fn cache)))
										(puthash source cache helm-candidate-cache)
										(helm-unmark-all)
										(helm-get-regex-for-preselection))
									(setq cache (,delete_fn (helm-get-selection) cache))
									,(when final_transformer_fn
										`(setq cache (,final_transformer_fn cache)))
									(puthash source cache helm-candidate-cache)
									(max 0 (- (helm-get-index) 0_or_1))))
							`(if helm-marked-candidates
								(let ((marked (helm-marked-candidates)))
									,(when marked_transformer_fn
										`(setq marked (,marked_transformer_fn marked)))
									(mapc #',delete_fn marked)
									,(when final_transformer_fn
										(list final_transformer_fn))
									(helm-unmark-all)
									(helm-get-regex-for-preselection))
								(,delete_fn (helm-get-selection))
								,(when final_transformer_fn (list final_transformer_fn))
								(max 0 (- (helm-get-index) 0_or_1))))))))
	)
		(define-key map [S-delete]
			`(lambda () (interactive) (,delete_candidate_base_fn 0)))
		(define-key map [S-backspace]
			`(lambda () (interactive) (,delete_candidate_base_fn 1))))
	nil)


(defun helm-source-history-make (history_var &optional constructor &rest args)
"Create simple helm source (of class helm-source-sync or its derivative)
with HISTORY_VAR as its source of candidates.
Use CONSTRUCTOR or `helm-source-sync-make'.
Default settings:
helm-source-candidates - (lambda () HISTORY_VAR)
helm-source-sync-volatile - t
helm-source-keymap -
	if it is nil, make sparse one inheriting from helm-map,
	else use the existing one;
	call `helm-add-delete-binds-force' on it."
	(unless (plist-member args :candidates)
		(setq args (nconc (list :candidates `(lambda () ,history_var)) args)))
	(unless (plist-member args :volatile)
		(setq args (nconc (list :volatile t) args)))
	(let ((keymap (plist-get args :keymap)))
		(unless keymap
			(setq keymap (make-sparse-keymap))
			(set-keymap-parent keymap helm-map))
		(helm-add-delete-binds-force
			keymap
			`(lambda (cand)
				; Use delete to delete every equal one,
				; using delq would probably delete only one,
				; which is not fine for history vars that keep
				; duplicates.
				(setq ,history_var (delete cand ,history_var))))
		(setq args (nconc (list :keymap keymap) args)))
	(apply (or constructor #'helm-source-sync-make) nil args))


(defun helm-add-goto-bindings (keymap get_fn &optional key_next key_previous)
"Add bindings in KEYMAP for KEY_NEXT (default [C-S-down]) and KEY_PREVIOUS
(default [C-S-up]).
GET_FN is a function called with no args that should return an object
for current selection, usually a part of a selected candidate."
	(unless key_next (setq key_next [C-S-down]))
	(unless key_previous (setq key_previous [C-S-up]))
	(let (
		(base_fn
			`(lambda (n)
				(with-helm-window
					(let ((start (point)))
						(cl-loop
							with end_fn = (if (> n 0) #'eobp #'bobp)
							with current = (,get_fn)
							do (forward-line n)
							if (helm-get-index)
								; Until found different cand.
								while (equal current (,get_fn))
							else when (or (helm-on-header) (funcall end_fn))
								return
									; Move to the closest candidate in the same source.
									(if (< n 0)
										(helm-goto-next-candidate)
										(helm-goto-prev-candidate)
										(helm-goto-candidate-start)))
						(when (/= start (point))
							(helm-select-line)
							(helm-follow-execute-persistent-action-maybe))))))
	)
		(define-key keymap key_next `(lambda () (interactive) (,base_fn 1)))
		(define-key keymap key_previous `(lambda () (interactive) (,base_fn -1))))
	nil)


(defmacro helm-with-display-candidates (candidates &rest body)
"CANDIDATES - list of one line strings (without a newline),
not using replacing display property, so they can be compared with equal
to find and count duplicates."
	(declare (debug t))
	(let ((buffer (make-symbol "")) (window (make-symbol "")))
		`(let ((,buffer (temp-buffer-window-setup helm-marked-buffer-name)))
			(with-current-buffer ,buffer
				(setq cursor-in-non-selected-windows nil)
				(maphash
					(lambda (cand count)
						(setq cand (copy-sequence cand))
						(remove-text-properties 0 (length cand) '(keymap) cand)
						(insert
							(if (= count 1)
								cand
								(concat cand " (" (number-to-string count) ")"))
							?\n))
					(let* (
						(candidates ,candidates)
						(duplicates
							(make-hash-table :test 'equal :size (length candidates)))
					)
						(dolist (cand candidates)
							(puthash
								cand (1+ (or (gethash cand duplicates) 0)) duplicates))
						duplicates)))
			(let (
				(,window
					(temp-buffer-window-show
						,buffer
						'(display-buffer-below-selected
							(window-height . fit-window-to-buffer))))
			)
				(set-window-dedicated-p ,window t)
				(unwind-protect
					(progn ,@body)
					(delete-window ,window)
					(kill-buffer ,buffer))))))


(defvar helm-show-marked-recursion-depth)
(defvar helm-show-marked-window)

(defun helm-show-marked-escape-key-hook ()
	(when (<= (recursion-depth) helm-show-marked-recursion-depth)
		(remove_hook 'escape-key-hook #'helm-show-marked-escape-key-hook)
		(when (window-live-p helm-show-marked-window)
			; Don't kill buffer, user may want to do something with it after
			; quitting helm.
			(quit-window nil helm-show-marked-window)
			t)))

; Probably could add something as prefix, like show candidates of all sources
; or something.
; Also candidates could be aligned into columns closer to each other,
; but that would require some work. E.g.
; original:
; helm-core.el                                   [modification time] [size]
; helm-files.el                                  [modification time] [size]
; aligned:
; helm-core.el  [modification time] [size]
; helm-files.el [modification time] [size]
(defun helm-show-marked ()
"Display marked candidates (of current source) in a temp buffer and window,
without exiting helm.
Press escape to quit this window."
	(interactive)
	; Use saved source if we are in helm-action-buffer,
	; or default if in helm-buffer and it is empty.
	(let ((helm_buffer (get-buffer helm-buffer)))
		(if-let (
			(source
				(if (helm-in-action-buffer)
					(car helm-action-source-and-cand)
					(or
						(helm-get-current-source)
						(buffer-local-value 'helm-default-source helm_buffer))))
		)
			(if-let (
				(display_list
					(cl-loop
						for ov in (buffer-local-value 'helm-marked-candidates helm_buffer)
						when (eq source (overlay-get ov 'source))
							collect (overlay-get ov 'withprop)))
			)
				(let ((buffer (temp-buffer-window-setup "*helm-show-marked*")))
					(with-current-buffer buffer
						(add_hook 'escape-key-hook #'helm-show-marked-escape-key-hook 25)
						(setq cursor-in-non-selected-windows nil)
						(let (
							(get_display
								(lambda (display)
									(setq display (copy-sequence display))
									; Remove some text properties.
									(remove-text-properties 0 (length display) '(keymap) display)
									(remove_face_text_property display 'helm-match)
									display))
						)
							; If not multiline.
							(if (not (helm-source-multiline source))
								(dolist (display display_list)
									(insert (funcall get_display display) ?\n))
								(insert (funcall get_display (car display_list)) ?\n)
								(dolist (display (cdr display_list))
									(insert
										helm-candidate-separator
										(funcall get_display display)
										?\n)))))
					(setq helm-show-marked-window (temp-buffer-window-show buffer))
					(set-window-dedicated-p helm-show-marked-window t)
					(setq helm-show-marked-recursion-depth (recursion-depth)))
				(message "No marked candidates in current source."))
			(message "No current source."))))

(provide 'helm-core)
