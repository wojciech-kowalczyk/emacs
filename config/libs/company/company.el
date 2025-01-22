; -*- lexical-binding:nil -*-

; Modules for retrieving completion candidates are called backends,
; modules for displaying them are frontends.
;
; Backends are distributed in separate files and can be used individually.
;
; If you want to start a specific backend, call it interactively or use
; `company-begin-backend'.
; For example company-abbrev will prompt for and insert an abbrev.
;
; Sometimes it is a good idea to go to tesco or mix several backends together, for example
; to enrich company-gtags with company-buffer-search results (to emulate local variables).
; To do this, add a list with both backends as an element in `company-backends'.

(define_face 'company::popup::face '((t :background "#46484A"))
"Face used for the popup.")

(define_face 'company::popup::selection_face '((t :background "#113A5C"))
"Face used for the selection in the popup.")

(define_face 'company::popup::deprecated_face '((t :strike-through t))
"Face used for the deprecated items.")

(define_face 'company::popup::search_face '((t :foreground "gold1"))
"Face used for the matched parts of items while searching in the popup.")

(define_face 'company::popup::search_selection_face
	'((t :inherit company::popup::search_face))
"Face used for the matched parts of selected item while searching in the popup.")

(define_face 'company::popup::search_input_face '((t :background "grey15"))
"Face used for the search string in the popup - user input.
Should have a background to show trailing spaces.")

; Not used now because mouse-highlight is broken. When it will be fixed,
; this should be used again.
; TODO Well, now it's partially fixed - mouse-highlight
; is fine, but now mouse-face even though it doesn't specify :foreground,
; discards :foreground of normal, not mouse-face face, and sets :foreground
; to the value of :foreground under mouse pointer when it entered text
; marked with mouse-face, so it looks bad. And mouse-face can't even
; be applied on separate characters to specify :foreground on each of them,
; because mouse-face must be the same across all chars to highlight all of them,
; so it's impossible to solve this using elisp.
(define_face 'company::popup::mouse_face '((t :inherit highlight))
"Face used for the popup item under the mouse.")

(define_face 'company::popup::common_face '((t :foreground "#589DF6"))
"Face used for the common completion in the popup.")

(define_face 'company::popup::common_selection_face
	'((t :inherit company::popup::common_face))
"Face used for the selected common completion in the popup.")

(define_face 'company::popup::annotation_face '((t :foreground "#8C8C8C"))
"Face used for the completion annotation in the popup.")

(define_face 'company::popup::annotation_selection_face
	'((t :inherit company::popup::annotation_face))
"Face used for the selected completion annotation in the popup.")

(define_face 'company::popup::scrollbar_face '((t :background "gray33"))
"Face used for the popup scrollbar - the entire bar.")

(define_face 'company::popup::scrollbar_track_face '((t :background "gray28"))
"Face used for the popup scrollbar - moving part.")

;(defface company-preview '((default :inherit (company::popup::selection_face company::popup::face))) "Face used for the completion preview.")
;(defface company-preview-common '((default :inherit company::popup::common_selection_face)) "Face used for the common part of the completion preview.")
;(defface company-preview-search '((default :inherit company::popup::common_selection_face)) "Face used for the search string in the completion preview.")
;
;(defface company-echo nil "Face used for completions in the echo area.")
;(defface company-echo-common '((t (:foreground "firebrick1"))) "Face used for the common part of completions in the echo area.")

(defconst company-frontends
	(let ((value '(company::popup::frontend)))
		; The list of active frontends (visualizations).
		; Each frontend is a function that takes one argument.
		; It is called with one of the following arguments:
		;
		; `start': When the visualization should start.
		;
		; `end': When the visualization should end.
		;
		; `update': When the data has been updated.
		;
		; `pre-command': Before every command that is executed while the
		; visualization is active.
		;
		; `post-command': After every command that is executed while the
		; visualization is active.
		;
		; `unhide': When an asynchronous backend is waiting for its completions.
		; Only needed in frontends which hide their visualizations in `pre-command'
		; for technical reasons.
		;
		; The visualized data is stored in `company-prefix', `company-candidates',
		; `company-common', `company-selection', `company-point' and
		; `company-search-string'.
		;
		; echo -							company-echo-frontend
		; echo, strip common -				company-echo-strip-common-frontend
		; show echo meta-data in echo -		company-echo-metadata-frontend
		; popup -							company::popup::frontend
		; popup, multiple only -			company::popup::unless_just_one_frontend
		; popup, multiple only, delayed -	company::popup::unless_just_one_frontend_with_delay
		; preview -							company-preview-frontend
		; preview, unique only -			company-preview-if-just-one-frontend
		; preview, common -					company-preview-common-frontend
		; custom function.

		(if (or (and (memq 'company::popup::unless_just_one_frontend value)
					 (memq 'company::popup::frontend value))
				(and (memq 'company::popup::unless_just_one_frontend_with_delay value)
					 (memq 'company::popup::frontend value))
				(and (memq 'company::popup::unless_just_one_frontend_with_delay value)
					 (memq 'company::popup::unless_just_one_frontend value)))
			(user-error "Popup frontend cannot be used more than once"))
		(if (or (and (memq 'company-preview-if-just-one-frontend value)
					 (memq 'company-preview-frontend value))
				(and (memq 'company-preview-if-just-one-frontend value)
					 (memq 'company-preview-common-frontend value))
				(and (memq 'company-preview-frontend value)
					 (memq 'company-preview-common-frontend value)))
			(user-error "Preview frontend cannot be used twice"))
		(if (and (memq 'company-echo value) (memq 'company-echo-metadata-frontend value))
			(user-error "Echo area cannot be used twice"))
		; Preview must come last.
		(dolist (frontend '(company-preview-if-just-one-frontend company-preview-frontend company-preview-common-frontend))
			(if (cdr (memq frontend value))
				(setq value (append (delq frontend value) (list frontend)))))
		value))


(defconst company::popup::max_height 11 "The maximum number of lines in the popup.")

;(defconst company::popup::min_potential_height company::popup::max_height
;"Ensure visibility of this number of candidates.
;When that many lines are not available between caret and the bottom of the
;window, display the popup above caret.
;If this is = company::popup::max_height, then changing number of candidates
;won't cause popup to change sides.")

(defconst company::popup::flip_when_above nil
"Whether to flip the popup when it's above the current line.")

(defconst company::popup::max_width most-positive-fixnum
"The maximum width of the popup's inner area.
This doesn't include the margins and the scroll bar.")

(defconst company::popup::min_width 0
"The minimum width of the popup's inner area.
This doesn't include the margins and the scroll bar.")

(defconst company::popup::width_grow_only nil
"When non-nil, the popup width is not allowed to decrease.")

; No left margin, because then it doesn't make sense for icon to be on the center.
; So use icon margin to adjust left margin.
(defconst company::popup::right_margin_width 1
"Width of margin columns to show on the right side of popup.
Simply string with that many spaces is used for that margin.")

(defconst company::popup::offset_display 'lines
"Method using which the popup displays scrolling position.
`scrollbar' means draw a scrollbar to the right of the items.
`lines' means wrap items in lines with \"before\" and \"after\" counters.
`nil' means don't use offset display.")

(defconst company::popup::align_annotations t
"When non-nil, align annotations to the right popup border.")

(defconst company::popup::annotation_padding nil
"Non-nil to specify the padding before annotation.
Depending on the value of `company::popup::align_annotations', the default
padding is either 0 or 1 space. This variable allows to override that
value to increase the padding. When annotations are right-aligned, it sets
the minimum padding, and otherwise just the constant one.")

(defvar company::auto_update_doc nil
"If non-nil, update the documentation buffer on each selection change.
Currently it's too slow to use practically.")

; Now it is just calling fit-window-to-buffer, but it could be more permanent.
(defvar company::fit_doc_window t
"If non-nil, call fit-window-to-buffer with company::doc_window
every time new doc buffer is shown there.")
(defvar company::fit_location_window t
"Like 'company::fit_doc_window' but for 'company::show_location' window.")

(defconst company-backends '(company-capf)
;	'(company-bbdb
;	  company-semantic
;	  company-cmake
;	  company-capf
;	  company-clang
;	  company-files
;	  (company-gtags company-etags company-keywords)
;	  company-oddmuse
;	  company-buffer-search
;	  company-ispell
;	)
"List of symbols representing backends, and/or lists of merged backends.

The list of active backends (completion engines).

Only one backend is used at a time. The choice depends on the order of
the items in this list, and on the values they return in response to the
`prefix' command (see below). But a backend can also be a \"grouped\" one (see below).

`company-begin-backend' can be used to start a specific backend,
`company-other-backend' will skip to the next matching backend in the list.

Each backend is a function that takes a variable number of arguments.
The first argument is the command requested from the backend.
It is one of the following:

`prefix': The backend should return the text to be completed.
It must be text immediately before point.
Returning nil from this command passes control to the next backend.
Instead of a string, the backend may return a cons (PREFIX . LENGTH)
where LENGTH is a number used in place of PREFIX's length when
comparing against `company::min_prefix_length'.
LENGTH can also be just t, and in the latter case the test automatically succeeds.

`candidates': The second argument is the prefix to be completed.
The return value should be a list of candidates that match the prefix.

Non-prefix matches are also supported (candidates that don't start with the
prefix, but match it in some backend-defined way). Backends that use this
feature must disable cache (return t to `no-cache') and might also want to
respond to `match'.

Optional commands
=================

`sorted': Return t here to indicate that the candidates are sorted and will
not need to be sorted again.

`duplicates': If non-nil, company will take care of removing duplicates
from the list.

`no-cache': Usually company doesn't ask for candidates again as completion
progresses, unless the backend returns t for this command.  The second
argument is the latest prefix.

`ignore-case': Return t here if the backend returns case-insensitive matches.
This value is used to determine the longest common prefix
(as used in `company-complete-common'),
and to filter completions when fetching them from cache.

`meta': The second argument is a completion candidate.
Return a (short) documentation string for it.

`doc-buffer': The second argument is a completion candidate.
Return a buffer with documentation for it. Preferably use `company-doc-buffer'.
If not all buffer contents pertain to this candidate, return a cons of buffer
and window start position.

`location': The second argument is a completion candidate.
Return a cons of buffer and buffer location, or of file and line number where
the completion candidate was defined.

`annotation': The second argument is a completion candidate.
Return a string to be displayed inline with the candidate in the popup.
If duplicates are removed by company, candidates with equal string values will
be kept if they have different annotations. For that to work properly, backends
should store the related information on candidates using text properties.

`deprecated': The second argument is a completion candidate.
Return non-nil if the completion candidate is deprecated.

`match': The second argument is a completion candidate.
Return a positive integer, the index after the end of text matching
`prefix' within the candidate string. Alternatively, return a list
of (CHUNK-START . CHUNK-END) elements, where CHUNK-START and CHUNK-END
are indexes within the candidate string. The corresponding regions are
used when rendering the popup.
This command only makes sense for backends that provide non-prefix completion.

`require-match': If this returns t, the user is not allowed to enter
anything not offered as a candidate. Please don't use that value in normal
backends. The default value nil gives the user that choice with
`company::require_match'. Return value `never' overrides that option the
other way around (using that value will indicate that the returned set of
completions is often incomplete, so this behavior will not be useful).

`init': Called once for each buffer. The backend can check for external
programs and files and load any required libraries. Raising an error here
will show up in message log once, and the backend will not be used for
completion.

`post-completion': Called after a completion candidate has been inserted
into the buffer. The second argument is the candidate.
Can be used to modify it, e.g. to expand a snippet.

`kind': The second argument is a completion candidate.
Return a symbol describing the kind of the candidate.
Refer to `company::vscode_icons_map_vector' for the possible values.

The backend should return nil for all commands it does not support or
does not know about. It should also be callable interactively and use
`company-begin-backend' to start itself in that case.

Grouped backends
================

An element of `company-backends' can also be a list of backends.
The completions from backends in such groups are merged,
but only from those backends which return the same `prefix'.

If a backend command takes a candidate as an argument (e.g. `meta'), the
call is dispatched to the backend the candidate came from.  In other
cases (except for `duplicates' and `sorted'), the first non-nil value among
all the backends is returned.

The group can also contain keywords. Currently, `:with' and `:separate'
keywords are defined. If the group contains keyword `:with', the backends
listed after this keyword are ignored for the purpose of the `prefix' command.
If the group contains keyword `:separate', the candidates that come from
different backends are sorted separately in the combined list.

Asynchronous backends
=====================

The return value of each command can also be a cons (:async . FETCHER)
where FETCHER is a function of one argument, CALLBACK. When the data
arrives, FETCHER must call CALLBACK and pass it the appropriate return
value, as described above. That call must happen in the same buffer as
where completion was initiated.

True asynchronous operation is only supported for command `candidates', and
only during idle completion. Other commands will block the user interface,
even if the backend uses the asynchronous calling convention.")

; ======================================= Sorting =======================================

(defun company::sort::occurrence::prefer_closest_above (position match_start match_end)
"Give priority to the matches above position, then those below position."
	(if (< match_start position)
		(- position match_end)
		(- match_start (window-start))))

(defun company::sort::occurrence::prefer_any_closest (position _match_start match_end)
"Give priority to the matches closest to the point."
	(abs (- position match_end)))

(defconst company::sort::occurrence::weight_fn
	#'company::sort::occurrence::prefer_any_closest
"Function to weight matches in `company::sort::occurrence'.
It's called with three arguments: caret position, the beginning and the end of the match.
Options:
company::sort::occurrence::prefer_closest_above,
company::sort::occurrence::prefer_any_closest.")

(defun company::sort::occurrence (candidates)
"Sort candidates according to their occurrences.
Searches for each in the currently visible part of the current buffer and
prioritizes the matches according to `company::sort::occurrence::weight_fn'.
The rest of the list is appended unchanged.
Keywords and function definition names are ignored."
	(let* (
		(w-start (window-start))
		(w-end (window-end))
		(start-point (point))
		occurs
		(noccurs
			(save-excursion
				(cl-delete-if
					(lambda (candidate)
						(goto-char w-start)
						(and
							(not (equal candidate ""))
							(search-forward candidate w-end t)
							; ^^^ optimize for large lists where most elements won't have
							; a match.
							(catch 'done
								(goto-char (1- start-point))
								(let (
									(occurence_predicate
										(lambda ()
											(defvar comint-last-prompt)
											(save-match-data
												(let (
													(beg (match-beginning 0))
													(end (match-end 0))
													(comint-last-prompt
														(bound-and-true-p comint-last-prompt))
												)
													(save-excursion
														(goto-char end)
														; Workaround for python-shell-completion-at-point's
														; behavior:
														; github.com/company-mode/company-mode/issues/759
														; github.com/company-mode/company-mode/issues/549
														(when (derived-mode-p 'inferior-python-mode)
															(let ((lbp (line-beginning-position)))
																(setq comint-last-prompt (cons lbp lbp))))
														(and
															(not
																(memq
																	(get-text-property (1- (point)) 'face)
																	'(
																		font-lock-function-name-face
																		font-lock-keyword-face
																	)))
															(let (
																(prefix
																	(company--prefix-str
																		(company::call_backend 'prefix)))
															)
																(and
																	(stringp prefix)
																	(= (length prefix) (- end beg))))
															(throw 'done t)))))))
								)
									(while (search-backward candidate w-start t)
										(funcall occurence_predicate))
									(goto-char start-point)
									(while (search-forward candidate w-end t)
										(funcall occurence_predicate))))
							(push
								(cons
									candidate
									(funcall company::sort::occurrence::weight_fn
										start-point
										(match-beginning 0)
										(match-end 0)))
								occurs)))
					candidates)))
	)
		(nconc
			(map_modify_list #'car (sort occurs (lambda (e1 e2) (<= (cdr e1) (cdr e2)))))
			noccurs)))


(defun company::sort::length (str_list)
"Sort by string length ascending (shortest first)."
	(sort str_list (lambda (str_1 str_2) (< (length str_1) (length str_2)))))


(defun company::sort::same_case_prefix (candidates)
"Prefer candidates with the exact same prefix.
If a backend returns case insensitive matches, candidates with the an exact
prefix match (same case) will be prioritized."
	(cl-loop
		for candidate in candidates
		if (string-prefix-p company-prefix candidate)
			collect candidate into same-case
		else collect candidate into other-case
		finally return (append same-case other-case)))

(defun company::sort::backend_importance (candidates)
"Sort candidates as two priority groups.
If company-backend is a symbol, return candidates.
If it's is a list, move candidates from backends before
keyword :with to the front. Candidates from the rest of the backends
in the group, if any, will be left at the end."
	(if (symbolp company-backend)
		candidates
		(if-let ((low-priority (cdr (memq :with company-backend))))
			(sort candidates
				(lambda (c1 c2)
					(and
						(let ((b2 (get-text-property 0 'company-backend c2)))
							(and b2 (memq b2 low-priority)))
						(let ((b1 (get-text-property 0 'company-backend c1)))
							(or (not b1) (not (memq b1 low-priority)))))))
			candidates)))

(defconst company::transformer_list '(company::sort::length)
"Functions to change the list of candidates received from backends.

Each function gets called with the return value of the previous one.
The first one gets passed the list of candidates, already sorted and without duplicates.
Of course this var can be nil.

Options: company::sort::length, company::sort::occurrence,
company::sort::backend_importance, company::sort::same_case_prefix.")

(defvar company::min_prefix_length 2 "The minimum prefix length for idle completion.")

(defconst company::abort_manual_when_too_short nil
"If enabled, cancel a manually started completion when the prefix gets
shorter than both `company::min_prefix_length' and the length of the
prefix it was started from.")

(defconst company::abort_on_unique_match t
"If non-nil, typing a full unique match aborts completion.

You can still invoke `company-complete' manually to run the `post-completion'
handler, though.

If it's nil, completion will remain active until you type a prefix that
doesn't match anything or finish it manually, e.g. with enter.")

(defconst company::require_match nil
"If enabled, disallow non-matching input.
This can be a function do determine if a match is required.

nil - off,
t - on,
company::is_action_explicit - on, if user interaction took place,
predicate function.")

(defvar company::idle_delay t
"The idle delay in seconds until completion starts automatically.
The prefix still has to satisfy `company::min_prefix_length' before that
happens. The value of nil means no idle completion.
t means no delay (different than 0.01, because executed right there).

Could be a function after some very small changes in source.")

; Commented because company::popup::unless_just_one_frontend_with_delay is out-of-date.
;(defconst company::popup::idle_delay 0
;"The idle delay in seconds until popup is shown when using
;`company::popup::unless_just_one_frontend_with_delay'.
;The value of nil means no idle completion - only manual invocation will start completion.
;0 means no delay.")

; This is not great, explicit call from a command that wants to start
; completion would be better.
; Unfortunately, changing it would require a rework of this pre/post-command
; system on which auto-completion is based now.
(defconst company::begin_command_list '(self-insert-command)
;	'(
;		self-insert-command
;		org-self-insert-command
;		orgtbl-self-insert-command
;		c-scope-operator
;		c-electric-colon
;		c-electric-lt-gt
;		c-electric-slash
;	)
"A list of commands after which idle completion is allowed.
See company::idle_delay.")

(defvar company::selection_wrap_around t
"If enabled, selecting item before first or after last wraps around.")

(defconst company::async_redisplay_delay 0.005
"Delay before redisplay when fetching candidates asynchronously.

You might want to set this to a higher value if your backends respond
quickly, to avoid redisplaying twice per each typed character.")

(defconst company::async_wait_time 0.03
"Pause between checks to see if the value's been set when turning an
asynchronous call into synchronous.")

(defconst company::async_timeout 2
"Maximum wait time for a value to be set during asynchronous call.")


(defvar company-completion-started-hook nil
"Hook run when company starts completing.
The hook is called with one argument that is non-nil if the completion was
started manually.")

(defvar company-completion-cancelled-hook nil
"Hook run when company cancels completing.
The hook is called with one argument that is non-nil if the completion was
aborted manually.")

(defvar company-completion-finished-hook nil
"Hook run when company successfully completes.
The hook is called with the selected candidate as an argument.

If you indend to use it to post-process candidates from a specific
backend, consider using the `post-completion' command instead.")

(defvar company-after-completion-hook nil
"Hook run at the end of completion, successful or not.
The hook is called with one argument which is either a string or a symbol.")

; ======================================= Keymaps =======================================

(defun company-unrecord-this-command ()
	(setq prefix-arg current-prefix-arg)
	(setq unread-command-events
		(nconc (listify-key-sequence (this-command-keys-vector)) unread-command-events)))

(defun company-abort-and-unrecord-this-command () (interactive)
	(company-abort)
	(company-unrecord-this-command))

(defconst company-keymap (make-sparse-keymap)
"Keymap that is enabled during an active completion.")

(defvar company-keymap-active nil
"Non-nil if some company keymap is active.
car of company package's entry in `minor-mode-map-alist'.")

(defconst company-keymap-cell (cons 'company-keymap-active company-keymap))

(push company-keymap-cell minor-mode-map-alist)

(define-key company-keymap [C-home] #'company-select-first)
(define-key company-keymap [C-end] #'company-select-last)
(define-key company-keymap [prior] #'company::popup::scroll_previous_page) ; page up
(define-key company-keymap [next] #'company::popup::scroll_next_page) ; page down
(define-key company-keymap [?\C-s] #'company::show_location)
(define-key company-keymap [?\C-f] #'company-filter-candidates)
(define-key company-keymap [?\C-\S-f] #'company-search-candidates)
(define-key company-keymap [?\C-a] #'company::show_doc_buffer)
(define-key global-map [?\C-\S-a] #'company::quit_doc_window)
(define-key company-keymap [?\A-a] #'company::toggle_auto_doc)

(define-key company-keymap [tab]
	(fn_symbol "company-key-tab"
		(lambda () (interactive)
			(company-complete-selection)
			(let ((position_before_move (point)))
				(if (inside_code)
					(skip-syntax-forward "w_")
					(syntax::withTextSyntax (skip-syntax-forward "w_")))
				(delete-region position_before_move (point)))
			(funcall after_move_hook_fn))))

(define-key company-keymap [return]
	(fn_symbol "company-key-enter"
		(lambda () (interactive)
			(company-complete-selection)
			(funcall after_move_hook_fn))))

(define-key global-map [?\C-\s] #'company::manual_begin)
(define-key company-keymap [?\C-\s] #'company-complete-common)

(define-key company-keymap [escape] #'company-abort)

(define-key company-keymap [up] #'company-select-previous-or-abort)
(define-key company-keymap [down] #'company-select-next-or-abort)

(define-key company-keymap [S-up]
	(fn_symbol "company-key-up"
		(lambda () (interactive) (company-abort) (call-interactively #'key::up))))
(define-key company-keymap [S-down]
	(fn_symbol "company-key-down"
		(lambda () (interactive) (company-abort) (call-interactively #'key::down))))

; Add company-abort call before many commands.
; Every command that wants to abort completion should be here
; (if it doesn't need some special handling like mouse events in some areas).
(bind_many_keys company-keymap
	[
		[left] [S-left] [C-left] [C-S-left] [A-left] [A-S-left] [A-C-S-left]
		[right] [S-right] [C-right] [C-S-right] [A-right] [A-S-right] [A-C-S-right]

		[C-up] [C-S-up] [A-C-S-up] [A-up] [A-S-up]
		[C-down] [C-S-down] [A-C-S-down] [A-down] [A-S-down]

		[home] [S-home] [C-home] [C-S-home] [A-home] [A-S-home] [A-C-S-home]
		[end] [S-end] [C-end] [C-S-end] [A-end] [A-S-end] [A-C-S-end]
	]
	#'company-abort-and-unrecord-this-command)

; Do the same with some mouse commands.
; Do this here because they should also have bindings on fringes.
(dolist (
	key
	'(
		; Double and triple, as well as normal and drag versions of mouse commands
		; I think shouldn't be defined here, because global-map will already be in
		; effect when they are run.
		down-mouse-1 S-down-mouse-1
		C-down-mouse-1 C-S-down-mouse-1
		A-down-mouse-1 A-S-down-mouse-1 A-C-S-down-mouse-1

		down-mouse-4 S-down-mouse-4 down-mouse-5 S-down-mouse-5
	)
)
	(mouse::bind_in_fringes company-keymap (vector key)
		#'company-abort-and-unrecord-this-command))


(defvar company-search-keymap (make-keymap)
"Keymap used for incrementally searching the completion candidates.")

(define-key company-search-keymap [t] #'company-search-other-char)

(set-char-table-range (nth 1 company-search-keymap) (cons ?\s (max-char))
	#'company-search-printing-char)
(dotimes (i 10)
	(define-key
		company-search-keymap
		(vector (intern (concat "kp-" (number-to-string i))))
		#'company-search-printing-char))

(bind_many_keys
	company-search-keymap
	[[backspace] [S-backspace] [delete] [S-delete]]
	#'company-search-delete-char)
(define-key company-search-keymap [return] (lookup-key company-keymap [return]))
(define-key company-search-keymap [tab] (lookup-key company-keymap [tab]))
(define-key company-search-keymap [?\C-\s] #'company-complete-common)
(define-key company-search-keymap [escape]
	(fn_symbol "company-search-key-escape"
		(lambda () (interactive) (company-search-mode -1))))
(define-key company-search-keymap [up] #'company-search-repeat-backward)
(define-key company-search-keymap [down] #'company-search-repeat-forward)
(bind_many_keys company-search-keymap [[?\C-f] [?\C-\S-f]]
	(fn_symbol "company-search-key-toggle-filtering"
		(lambda () (interactive)
			(company::search::set_filtering (not company-search-filtering)))))

; ======================================== Icons ========================================

(defconst company::vscode_icons_map_vector
	[
		(array . "symbol-array.svg")
		(boolean . "symbol-boolean.svg")
		(class . "symbol-class.svg")
		(color . "symbol-color.svg")
		(constant . "symbol-constant.svg")
		(constructor . "symbol-method.svg")
		(enum-member . "symbol-enumerator-member.svg")
		(enum . "symbol-enumerator.svg")
		(event . "symbol-event.svg")
		(field . "symbol-field.svg")
		(file . "symbol-file.svg")
		(folder . "folder.svg")
		(interface . "symbol-interface.svg")
		(keyword . "symbol-keyword.svg")
		(method . "symbol-method.svg")
		(function . "symbol-method.svg")
		(module . "symbol-namespace.svg")
		(numeric . "symbol-numeric.svg")
		(operator . "symbol-operator.svg")
		(property . "symbol-property.svg")
		(reference . "references.svg")
		(snippet . "symbol-snippet.svg")
		(string . "symbol-string.svg")
		(struct . "symbol-structure.svg")
		(text . "symbol-key.svg")
		(type-parameter . "symbol-parameter.svg")
		(unit . "symbol-ruler.svg")
		(value . "symbol-enumerator.svg")
		(variable . "symbol-variable.svg")
		(nil . "symbol-misc.svg")
	]
)

(defconst company::icon_margin_width 3 "In columns, must be at least 3.")

(defconst company::icon_margin_hash_table
	; Variables in this let are for customization.
	(let (
		(icons_map_vector company::vscode_icons_map_vector)
		(icons_dir (concat (file-name-directory load-file-name) "icons/vscode-dark/"))
		(preferred_icon_pixel_size 26)
	)

		(let* (
			(icon_margin_pixel_width (* (default-font-width) company::icon_margin_width))
			(icon_pixel_size (min preferred_icon_pixel_size icon_margin_pixel_width))
			(icon_space_pixel_width (- icon_margin_pixel_width icon_pixel_size))
			(icon_left_space_pixel_width (/ icon_space_pixel_width 2))
			(icon_right_space_pixel_width
				(- icon_space_pixel_width icon_left_space_pixel_width))
			; [1] This is one of a few places trying to control line wrapping.
			; This one is only for word-wrap non-nil.
			; Use "x", because it has word syntax, so it won't separate symbols
			; into smaller ones, so it won't cause word-wrap to do anything.
			;
			; FIXME There is actually another problem with these spacing strings -
			; when lines are wrapped, word-wrap is nil and this left_space_str is the
			; first char in a wrapped line, then if this space, which can be very thin,
			; can fit at the end of previous wrapped line (visual line, not logical one),
			; then it will be fit there. This obviously looks bad.
			; Example (| are window borders, L is left_space_str, R right_space_str,
			; I icon):
			; no overlay:
			; |loooooooong_li | <- this last "space" (on screen, not in buffer)
			; |ne             |    has < width than "n" from the next line.
			; with overlay:
			; |loooooooong_liL| <- L is left_space_str, moved here because it fits.
			; |IRcandidate    |
			;
			; This can be somewhat easily solved by examining the distance between the end
			; of last char in a wrapped line and the right window text area border, and
			; adding padding to fill this space if it's < than first char on the
			; continuation line and < than left_space_str's width, or just always adding
			; this padding.
			; I always use word-wrap now, and I don't think this is a problem there
			; because this left_space_str is now a part of larger "word" that won't be
			; moved to the previous line.
			(left_space_str
				(propertize "x" 'display `(space :width (,icon_left_space_pixel_width))))
			(right_space_str
				(propertize "x" 'display `(space :width (,icon_right_space_pixel_width))))
			(unselected_background (face-attribute 'company::popup::face :background))
			(selected_background
				(face-attribute 'company::popup::selection_face :background))
			(icon_list_basic_base
				(list
					:type 'svg
					:width icon_pixel_size
					:height icon_pixel_size
					:ascent 'center
					:background))
			(unselected_icon_list_base
				(append icon_list_basic_base (list unselected_background)))
			(selected_icon_list_base
				(append icon_list_basic_base (list selected_background)))
			(get_icon_margin_string
				(lambda (filename icon_list_base) ; filename like "variable.svg"
					(concat
						left_space_str
						(propertize "x" ; Same here ([1]).
							'display
							(nconc
								(list 'image :file (concat icons_dir filename))
								icon_list_base))
						right_space_str)))
			(icon_margin_hash_table
				(make-hash-table :test 'eq :size (length icons_map_vector)))
		)
			(mapc
				(lambda (type_and_file)
					(puthash
						(car type_and_file)
						(cons
							(funcall get_icon_margin_string
								(cdr type_and_file) unselected_icon_list_base)
							(funcall get_icon_margin_string
								(cdr type_and_file) selected_icon_list_base))
						icon_margin_hash_table))
				icons_map_vector)
			icon_margin_hash_table))
"Hash table mapping kinds of candidates to conses of strings (unselected . selected)
with 3 spaces - first and last are spacing, the middle one is icon.
E.g. (gethash (company::call_backend 'kind candidate) company::icon_margin_hash_table)
will return strings to use in popup's icon margin for a candidate.")

; Currently unused (and probably out-of-date) text-icons.

;(defconst company-text-icons-mapping
;	'((array "a" font-lock-type-face)
;	  (boolean "b" font-lock-builtin-face)
;	  (class "c" font-lock-type-face)
;	  (color "#" success)
;	  (constant "c" font-lock-constant-face)
;	  (constructor "c" font-lock-function-name-face)
;	  (enum-member "e" font-lock-builtin-face)
;	  (enum "e" font-lock-builtin-face)
;	  (field "f" font-lock-variable-name-face)
;	  (file "f" font-lock-string-face)
;	  (folder "d" font-lock-doc-face)
;	  (interface "i" font-lock-type-face)
;	  (keyword "k" font-lock-keyword-face)
;	  (method "m" font-lock-function-name-face)
;	  (function "f" font-lock-function-name-face)
;	  (module "{" font-lock-type-face)
;	  (numeric "n" font-lock-builtin-face)
;	  (operator "o" font-lock-comment-delimiter-face)
;	  (property "p" font-lock-variable-name-face)
;	  (reference "r" font-lock-doc-face)
;	  (snippet "S" font-lock-string-face)
;	  (string "s" font-lock-string-face)
;	  (struct "%" font-lock-variable-name-face)
;	  (text "w" shadow)
;	  (type-parameter "p" font-lock-type-face)
;	  (unit "u" shadow)
;	  (value "v" font-lock-builtin-face)
;	  (variable "v" font-lock-variable-name-face)
;	  (nil "." shadow))
;"Mapping of the text icons.
;The format should be an alist of (KIND . CONF) where CONF is a list of the
;form (ICON FG BG) which is used to propertize the icon to be shown for a
;candidate of kind KIND. FG can either be color string or a face from which
;we can get a color string (using the :foreground face-property). BG must be
;of the same form as FG or a cons cell of (BG . BG-WHEN-SELECTED) which each
;should be of the same form as FG.
;
;The only mandatory element in CONF is ICON, you can omit both the FG and BG
;fields without issue.
;
;When BG is omitted and `company-text-icons-add-background' is non-nil, a BG
;color is generated using a gradient between the active popup color and the FG color.")
;
;(defconst company-text-face-extra-attributes '(:weight bold)
;"Additional attributes to add to text/dot icons faces.
;If non-nil, an anonymous face is generated.
;
;Affects `company-text-icons-margin' and `company-dot-icons-margin'.")
;
;(defconst company-text-icons-format " %s " "Format string for printing the text icons.")
;
;(defconst company-text-icons-add-background nil
;"Generate a background color for text/dot icons when none is given.
;See `company-text-icons-mapping'.")
;
;(defun company-text-icons--face (fg bg selected)
;	(let* ((extract_property
;				; Try to extract PROPERTY from FACE.
;				; If FACE isn't a valid face return FACE as is.
;				; If FACE doesn't have PROPERTY return nil.
;				(lambda (face property)
;					(if (facep face)
;						(let ((value (face-attribute face property)))
;							(unless (eq value 'unspecified)
;								value))
;						face)))
;		   (fg-color (funcall extract_property fg :foreground)))
;		`(,@company-text-face-extra-attributes
;			,@(and fg-color (list :foreground fg-color))
;			,@(let* ((bg-is-cons (consp bg))
;					 (bg (if bg-is-cons (if selected (cdr bg) (car bg)) bg))
;					 (bg-color (funcall extract_property bg :background))
;					 (popup-bg-color
;						(funcall extract_property
;							(if selected
;								'company::popup::selection_face
;								'company::popup::face)
;							:background)))
;				(cond
;					((and company-text-icons-add-background selected (not bg-is-cons) bg-color popup-bg-color)
;						; Adjust the coloring of the background when *selected* but user hasn't
;						; specified an alternate background color for selected item icons.
;						(list
;							:background
;							(apply #'color-rgb-to-hex
;								(nth 0
;									(color-gradient
;										(color-name-to-rgb popup-bg-color)
;										(color-name-to-rgb bg-color)
;										2)))))
;					(bg
;						; When background is configured we use it as is, even if it doesn't
;						; constrast well with other candidates when selected.
;						(if bg-color
;							(list :background bg-color)))
;					((and company-text-icons-add-background fg-color popup-bg-color)
;						; Lastly attempt to generate a background from the foreground.
;						(list
;							:background
;							(apply #'color-rgb-to-hex
;								(nth 0
;									(color-gradient
;										(color-name-to-rgb popup-bg-color)
;										(color-name-to-rgb fg-color)
;										10))))))))))
;
;(defun company-text-icons-margin (candidate selected)
;"Margin function which returns unicode icons."
;	(if-let ((kind (company::call_backend 'kind candidate))
;			 (conf
;				(or
;					(alist-get kind company-text-icons-mapping)
;					(alist-get t company-text-icons-mapping))))
;		(cl-destructuring-bind (icon &optional fg bg) conf
;			(propertize (format company-text-icons-format icon)
;				'face (company-text-icons--face fg bg selected)))))
;
;(defconst company-dot-icons-format "● " "Format string for `company-dot-icons-margin'.")
;
;(defun company-dot-icons-margin (candidate selected)
;"Margin function that uses a colored dot to display completion kind."
;	(if-let ((kind (company::call_backend 'kind candidate))
;			 (conf (or (assoc-default kind company-text-icons-mapping)
;					   (assoc-default t company-text-icons-mapping))))
;		(cl-destructuring-bind (_icon &optional fg bg) conf
;			(propertize company-dot-icons-format
;				'face (company-text-icons--face fg bg selected)))))

; ================================ Functions for backends ================================

(defun company-grab-line (regex &optional expression)
"Return a match string for regex if it matches text before point.
If expression is non-nil, return the match string for the respective
parenthesized expression in regex.
Matching is limited to the current line."
	(when (looking-back regex (pos-bol))
		(or (match-string-no-properties (or expression 0)) "")))

(defun company-grab-symbol ()
	(buffer-substring (point) (save-excursion (skip-syntax-backward "w_") (point))))

(defun company::get_prefix_after_member_access ()
"Return prefix (symbol) at caret, in a cons (prefix_string . t) if this prefix
is right after \".\", \"->\" or \"::\"; else as just a string."
	(when-let ((symbol_string (company-grab-symbol)))
		(save-excursion
			(forward-char (- (length symbol_string)))
			(if (looking-back "\\.\\|->\\|::" (- (point) 2))
				(cons symbol_string t)
				symbol_string))))

(defun company::substitute_prefix (prefix_string candidate_list)
	; delete-dups is necessary, for example:
	; candidates: ("asd" "ASd"), prefix: "As",
	; candidates after prefix substitution: ("Asd" "Asd").
	(delete-dups
		(let ((prefix_length (length prefix_string)))
			(mapcar
				(lambda (candidate_string)
					; If strings match exactly.
					(if
						(eq
							(compare-strings
								prefix_string
								0
								prefix_length
								candidate_string
								0
								prefix_length)
							t)
						candidate_string
						(concat
							prefix_string
							(substring candidate_string prefix_length))))
				candidate_list))))

; =============================== Functions for the people ===============================

(defun company::call_frontends (command)
	(mapcar (lambda (frontend) (funcall frontend command)) company-frontends))

(defvar-local company-backend nil)

(defvar-local company-prefix nil)

(defvar-local company-candidates nil)
(defvar-local company-candidates-length nil)
(defvar-local company-candidates-cache nil)
(defvar-local company-candidates-predicate nil)

(defvar-local company-common nil)

(defconst company-selection-default 0 "The default value for `company-selection'.")

(defvar-local company-selection company-selection-default)
(defvar-local company-selection-changed nil)

(defvar-local company--manual-action nil "Non-nil if manual completion took place.")
(defvar-local company--manual-prefix nil)

(defvar-local company-point-max nil)
(defvar-local company-point nil)

(defvar company-timer nil)

(defun company-cancel (&optional result)
	(let ((prefix company-prefix) (backend company-backend))
		(setq company-selection company-selection-default)
		(setq company-backend nil)
		(setq company-prefix nil)
		(setq company-candidates nil)
		(setq company-candidates-length nil)
		(setq company-candidates-cache nil)
		(setq company-candidates-predicate nil)
		(setq company-common nil)
		(setq company-selection-changed nil)
		(setq company--manual-action nil)
		(setq company--manual-prefix nil)
		(setq company-point-max nil)
		(setq company-point nil)
		(setq company--multi-uncached-backends nil)
		(setq company--multi-min-prefix nil)
		(when company-timer (cancel-timer company-timer) (setq company-timer nil))
;		(company-echo-cancel t)
		(company-search-mode -1)
		(company::call_frontends 'end)
		(kill-local-variable 'company-keymap-active)
		(when prefix
			(if (stringp result)
				(let ((company-backend backend))
					(run-hook-with-args 'company-completion-finished-hook result)
					(company::call_backend 'post-completion result))
				(run-hook-with-args 'company-completion-cancelled-hook result))
			(run-hook-with-args 'company-after-completion-hook result)))
	nil)

(defun company-abort () (interactive) (company-cancel 'abort))

(defun company::force_sync (fn args backend)
	(let ((value (apply fn args)))
		(if (eq (car-safe value) :async)
			(let ((res 'trash) (start (time-to-seconds)))
				(funcall (cdr value) (lambda (result) (setq res result)))
				(while (eq res 'trash)
					(if (> (- (time-to-seconds) start) company::async_timeout)
						(error "Company: backend %s async timeout with args %s"
							backend args)
						; XXX: Reusing the trick from company--fetch-candidates here
						; doesn't work well: sit-for isn't a good fit when we want to
						; ignore pending input (results in too many calls).
						; FIXME: We should deal with this by standardizing on a kind of
						; Future object that knows how to sync itself. In most cases (but
						; not all), by calling accept-process-output, probably.
						(sleep-for company::async_wait_time)))
				res)
			value)))

(defun company::prefix_string (prefix) (or (car-safe prefix) prefix))

(defun company::is_good_prefix (prefix min_length)
	(and
		prefix
		(or
			(eq (cdr-safe prefix) t)
			(>= (or (cdr-safe prefix) (length prefix)) min_length))))

(defun company::preprocess_candidates (candidates)
	(unless (company::call_backend 'sorted)
		(setq candidates (sort candidates #'string<)))
	(when (company::call_backend 'duplicates)
		(let ((candidates_1 candidates) (extras 'unk))
			(while candidates_1
				(setcdr candidates_1
					(let ((candidate_1 (pop candidates_1)))
						(while
							(let ((candidate_2 (car candidates_1)))
								(if (equal candidate_1 candidate_2)
									(let (
										(call_backend_annotation_and_kind
											(lambda (candidate)
												(cons
													(company::call_backend
														'annotation candidate)
													(company::call_backend
														'kind candidate))))
									)
										(when (eq extras 'unk)
											(setq extras
												(list
													(funcall
														call_backend_annotation_and_kind
														candidate_1))))
										(let (
											(extra_2
												(funcall call_backend_annotation_and_kind
													candidate_2))
										)
											(if (member extra_2 extras)
												t
												(push extra_2 extras)
												nil)))
									(setq extras 'unk)
									nil))
							(pop candidates_1))
						candidates_1)))))
	candidates)


(defvar-local company--multi-uncached-backends nil)
(defvar-local company--multi-min-prefix nil)

(defun company--multi-backend-adapter (backends command &rest args)
	(let ((separate (memq :separate backends)))
	    (when (eq command 'prefix)
			(setq backends (butlast backends (length (memq :with backends)))))
		(setq backends (cl-remove-if #'keywordp backends))
	    (cond
			((eq command 'candidates)
				(let* (
					(prefix (car args))
					(min-length (or company--multi-min-prefix 0))
					(pair_list
						(cl-loop
							for backend in backends
							when
								(let (
									(bp
										(let ((company-backend backend))
											(company::call_backend 'prefix)))
								)
									(and
										; It's important that the lengths match.
										(equal (company::prefix_string bp) prefix)
										; One might override min-length, another not.
										(or
											(company::is_good_prefix bp min-length)
											(progn
												(push backend
													company--multi-uncached-backends)
												nil))))
								collect
									(cons
										(funcall backend 'candidates prefix)
										`(lambda (candidates)
											,(when separate
												`(setq candidates
													(let ((company-backend ',backend))
														(company::preprocess_candidates
															candidates))))
											; Don't tag the candidates received from the
											; first backend in the group.
											,(unless (eq backend (car backends))
												`(setq candidates
													(mapcar
														(lambda (str)
															(propertize str
																'company-backend
																',backend))
														candidates)))
											candidates))))
				)
					(if
						(cl-loop
							for pair in pair_list
							thereis (eq :async (car-safe (car pair))))
						(cons
							:async
							`(lambda (callback)
								(let (
									list
									(pending (mapcar #'car ',pair_list))
									(finisher
										`(lambda ()
											(unless pending
												(,callback
													(apply #'append (nreverse list))))))
								)
									(dolist (pair ',pair_list)
										(push nil list)
										(let* (
											(val (car pair))
											(this-finisher
												`(lambda (res)
													(setq pending (delq ',val pending))
													(setcar ',list (,(cdr pair) res))
													(,finisher)))
										)
											(if (eq :async (car-safe val))
												(funcall (cdr val) this-finisher)
												(funcall this-finisher val)))))))
						(apply #'nconc
							(mapcar
								(lambda (pair) (funcall (cdr pair) (car pair)))
								pair_list)))))
			((eq command 'set-min-prefix) (setq company--multi-min-prefix (car args)))
			((eq command 'sorted) separate)
			((eq command 'duplicates) (not separate))
			((and
					(eq command 'no-cache)
					(let ((uncached company--multi-uncached-backends) found)
						(dolist (backend backends)
							(and
								(member backend uncached)
								(company::is_good_prefix
									(let ((company-backend backend))
										(company::call_backend 'prefix))
									(or company--multi-min-prefix 0))
								(setq
									found t
									company--multi-uncached-backends
										(delete
											backend company--multi-uncached-backends))))
						found))
				t)
			((memq command '(prefix ignore-case no-cache require-match))
				(cl-loop
					for backend in backends
					thereis (company::force_sync backend (cons command args) backend)))
			(t
				(let ((arg (car args)))
					(when (> (length arg) 0)
						(apply
							(or (get-text-property 0 'company-backend arg) (car backends))
							command args)))))))

(defun company::call_backend_raw (&rest args)
	(if (functionp company-backend)
		(apply company-backend args)
		(apply #'company--multi-backend-adapter company-backend args)))

(defun company::call_backend (&rest args)
	(company::force_sync #'company::call_backend_raw args company-backend))

(defun company::is_action_explicit ()
"Return whether explicit completion action was taken by the user."
	(or company--manual-action company-selection-changed))

(defun company::set_selection (selection &optional force-update)
"Set SELECTION for company candidates.
This will update `company-selection' and related variable.
Only update when the current selection is changed, but optionally always update
if FORCE-UPDATE."
	(let ((offset (if company-selection-default 0 1)))
		(setq selection (+ selection offset))
		(setq selection
			(if company::selection_wrap_around
				(mod selection (+ company-candidates-length offset))
				(max 0 (min (+ company-candidates-length offset -1) selection))))
		(setq selection (unless (< selection offset) (- selection offset))))
	(when (or force-update (not (eq selection company-selection)))
		(setq company-selection selection)
		(setq company-selection-changed t)
		(company::call_frontends 'update))
	nil)

(defun company::update_candidates (candidates)
	(setq company-candidates-length (length candidates))
	(if company-selection-changed
		; Try to restore the selection
		(let (
			(selected
				(when company-selection (nth company-selection company-candidates)))
		)
			(setq company-candidates candidates)
			(when selected
				(setq company-selection 0)
				(catch 'found
					(while candidates
						(let ((candidate (pop candidates)))
							(and
								(string= candidate selected)
								(equal
									(company::call_backend 'annotation candidate)
									(company::call_backend 'annotation selected))
								(throw 'found t)))
						(++ company-selection))
					(setq company-selection company-selection-default)
					(setq company-selection-changed nil))))
		(setq company-selection company-selection-default)
		(setq company-candidates candidates))
	; Calculate common.
	; We want to support non-prefix completion, so filtering is the
	; responsibility of each respective backend, not ours.
	; On the other hand, we don't want to replace non-prefix input in
	; `company-complete-common', unless there's only one candidate.
	(setq company-common
		(if (cdr company-candidates)
			(let* (
				(completion-ignore-case (company::call_backend 'ignore-case))
				(common (try-completion "" company-candidates))
			)
				(when (string-prefix-p company-prefix common completion-ignore-case)
					common))
			(car company-candidates)))
	nil)

(defun company::require_match ()
	(let ((backend_require_match (company::call_backend 'require-match)))
		(or
			(eq backend_require_match t)
			(and
				(not (eq backend_require_match 'never))
				(if (functionp company::require_match)
					(funcall company::require_match)
					(eq company::require_match t))))))

(defun company::calculate_candidates (prefix ignore-case)
	(let ((candidates (cdr (assoc prefix company-candidates-cache))))
		(or
			candidates
			(and
				company-candidates-cache
				(let (
					(len (length prefix))
					(completion-ignore-case ignore-case)
					prev
				)
					(cl-loop
						for i to len
						when
							(setq prev
								(cdr
									(assoc
										(substring prefix 0 (- len i))
										company-candidates-cache)))
							return
								(progn
									(setq candidates (all-completions prefix prev))
									t))))
			; No cache match, call the backend.
			(let (
				(refresh-timer
					(run-with-timer company::async_redisplay_delay nil
						(lambda ()
							(when company-candidates (company::call_frontends 'unhide))
							(let (inhibit-redisplay) (redisplay))
							(when company-candidates
								(company::call_frontends 'pre-command)))))
			)
				(setq candidates
					(company::preprocess_candidates
						(let* (
							(non-essential (not (company::is_action_explicit)))
							(inhibit-redisplay t)
							(c
								(if
									(or
										company-selection-changed
										; FIXME: This is not ideal, but we have not managed
										; to deal with these situations in a better way yet.
										(company::require_match))
									(company::call_backend 'candidates prefix)
									(company::call_backend_raw 'candidates prefix)))
						)
							(if (eq (car c) :async)
								(let ((res 'none))
									(funcall (cdr c)
										(lambda (candidates)
											(when (eq res 'none)
												(push 'company-foo unread-command-events))
											(setq res candidates)))
									(if flyspell-mode
										(while (and (eq res 'none) (not (input-pending-p)))
											(sleep-for company::async_wait_time)) ; TODO what is this crap
										(while (and (eq res 'none) (sit-for 0.5 t))))
									(while
										(member
											(car unread-command-events)
											'(company-foo (t . company-foo)))
										(pop unread-command-events))
									(prog1 (and (consp res) res) (setq res 'exited)))
								c))))
				; If the backend is synchronous, no chance for the timer to run.
				(cancel-timer refresh-timer)
				; Save in cache.
				(push (cons prefix candidates) company-candidates-cache)))
		; Apply predicate.
		(when (or company-candidates-predicate company::transformer_list)
			(setq candidates (copy-sequence candidates)))
		(when company-candidates-predicate
			(setq candidates (cl-delete-if-not company-candidates-predicate candidates)))
		; Apply transformers.
		; There was originally copy-sequence, but I think it's not needed here.
		(dolist (transformer company::transformer_list)
			(setq candidates (funcall transformer candidates)))
		candidates))

; ========================== Starting and continuing completion ==========================

(defun company::get_prefix_min_length ()
"Return minimum prefix length that should not end completion."
	(if company--manual-prefix
		(if company::abort_manual_when_too_short
			; Must not be less than minimum or initial length.
			(min company::min_prefix_length (length company--manual-prefix))
			0)
		company::min_prefix_length))

(defun company::abort_if_unique_match (candidate_list prefix ignore_case)
"Abort if there is only one match, and it matches prefix exactly, and user wants
such behaviour.
Return non-nil in that case."
	(when
		(and
			company::abort_on_unique_match
			candidate_list
			(not (cdr candidate_list))
			(or
				(eq
					(compare-strings
						(car candidate_list) nil nil prefix nil nil ignore_case)
					t)
				; I added this special case - abort if point is in the middle of a symbol
				; that is the only candidate. In other words abort regardless of where
				; point is in the symbol we are completing.
				(let (
					(full_symbol_at_point
						(buffer-substring-no-properties
							(- (point) (length prefix))
							(save-excursion (skip-syntax-forward "w_") (point))))
				)
					(and
						(not (equal full_symbol_at_point prefix))
						(eq
							(compare-strings
								(car candidate_list) nil nil full_symbol_at_point
								nil nil ignore_case)
							t))))
			(not (eq (company::call_backend 'kind (car candidate_list)) 'snippet)))
		; Handle it like completion was aborted, to differentiate from
		; user calling one of company's commands to insert the candidate,
		; not to trigger template expansion, etc.
		(company-cancel 'unique)
		t))

(defun company::after_starting_or_continuing_completion ()
	(if (not company-candidates)
		(setq company-backend nil)
		(setq company-point (point))
		(setq company-point-max (point-max))
		(setcdr company-keymap-cell company-keymap)
		(company::call_frontends 'update))
	nil)

(defun company::auto_begin ()
	(when
		(and
			company-mode ; company-mode must be on.
			(not company-candidates)) ; Completion must not already be started.
		(or
			buffer-read-only
			overriding-local-map
			mark-active
			; Check if in the middle of entering a key combination (like C-c c).
			(and
				(not (equal (this-command-keys-vector) []))
				(keymapp (key-binding (this-command-keys-vector))))
			(cl-loop
				with prefix_min_length = (company::get_prefix_min_length)
				; Prefer manual override
				for backend in
					(if company-backend (list company-backend) company-backends)
				for prefix =
					(if (symbolp backend)
						(let ((company-backend backend))
							(company::call_backend 'prefix))
						(company--multi-backend-adapter backend 'prefix))
				when (company::is_good_prefix prefix prefix_min_length)
					return
						(let ((ignore_case (company::call_backend 'ignore-case)))
							; Keep this undocumented, esp. while only 1 backend needs it.
							(company::call_backend 'set-min-prefix prefix_min_length)
							(setq company-prefix (company::prefix_string prefix))
							(setq company-backend backend)
							(let (
								(candidate_list
									(company::calculate_candidates
										company-prefix ignore_case))
							)
								(cond
									((company::abort_if_unique_match
											candidate_list company-prefix ignore_case))
									(candidate_list ; We got completions.
										(when company--manual-action
											(setq company--manual-prefix prefix))
										(company::update_candidates candidate_list)
										(run-hook-with-args
											'company-completion-started-hook
											(company::is_action_explicit))
										(company::call_frontends 'start)))))))
		(company::after_starting_or_continuing_completion)
		company-candidates)) ; Return non-nil if active.

(defun company-assert-enabled ()
	(unless company-mode
		(kill-local-variable 'company-keymap-active)
		(user-error "Company not enabled")))

(defun company::manual_begin ()
"Start the completion interface.
Unlike `company-complete-selection' or `company-complete',
this command doesn't cause any immediate changes to the buffer text."
	(interactive)
	(company-assert-enabled)
	(setq company--manual-action t)
	(prog1
		(or
			company-candidates
			(let ((company::min_prefix_length 0)) (company::auto_begin)))
		(unless company-candidates (setq company--manual-action nil))))

; Functions for interactive calling of backends (currently unused).

(defvar-local company-callback nil)

(defun company-remove-callback (_)
	(remove-hook 'company-completion-finished-hook company-callback t)
	(remove-hook 'company-completion-cancelled-hook 'company-remove-callback t)
	(remove-hook 'company-completion-finished-hook 'company-remove-callback t))

(defun company-begin-backend (backend &optional callback)
"Start a completion at point using backend.
On succesful completion, run callback if it's non-nil.
Both backend and callback should be symbols with function definitions."
	(when (setq company-callback callback)
		(add-hook 'company-completion-finished-hook company-callback nil t))
	(add-hook 'company-completion-cancelled-hook 'company-remove-callback nil t)
	(add-hook 'company-completion-finished-hook 'company-remove-callback nil t)
	(setq company-backend backend)
	; Return non-nil if active.
	(if (company::manual_begin)
		t
		(message "Cannot complete at point.")
		nil))

(defun company-other-backend (&optional backward) (interactive "P")
	(company-assert-enabled)
	(let (
		(after
			(if company-backend
				(cdr (member company-backend company-backends))
				company-backends))
		(before (cdr (member company-backend (reverse company-backends))))
	)
		(company-cancel)
		(cl-loop
			for backend in
				(if backward
					(append before (reverse after))
					(append after (reverse before)))
			until (company-begin-backend backend)))
	(unless company-candidates (message "No other backend.")))


(defun company::pre_command ()
	(when company-candidates (company::call_frontends 'pre-command))
	(when company-timer
		(cancel-timer company-timer)
		(setq company-timer nil))
;	(company-echo-cancel t)
	(kill-local-variable 'company-keymap-active)
	nil)

(defun company::post_command ()
	(when (and company-candidates (null this-command))
		; Happens when the user presses `C-g' while inside
		; `flyspell-post-command-hook', for example.
		; Or any other `post-command-hook' function that can call `sit-for',
		; or any quittable timer function.
		(company-abort)
		(setq this-command 'company-abort))
	; If should get new prefix and calculate candidates.
	(unless (eq (point) company-point)
		(when company-candidates ; If completion is running.
			(when (company::call_backend 'no-cache company-prefix)
				; Don't complete existing candidates, fetch new ones.
				(setq company-candidates-cache nil))
			(let* (
				(new_prefix (company::call_backend 'prefix))
				(ignore_case (company::call_backend 'ignore-case))
				(candidate_list
					(and
						(company::is_good_prefix
							new_prefix (company::get_prefix_min_length))
						(setq new_prefix (company::prefix_string new_prefix))
						(=
							(- (point) (length new_prefix))
							(- company-point (length company-prefix)))
						(company::calculate_candidates new_prefix ignore_case)))
			)
				(cond
					((company::abort_if_unique_match
							candidate_list new_prefix ignore_case))
					((consp candidate_list)
						; Incremental match (normal branch continuing completion).
						(setq company-prefix new_prefix)
						(company::update_candidates candidate_list))
					; Input that produced empty candidate_list.
					; Require match or cancel.
					((and
							(> (point) company-point)
							(> (point-max) company-point-max)
							(equal
								(buffer-substring
									(- company-point (length company-prefix))
									company-point)
								company-prefix))
						(cond
							((and
									(or
										(not (company::require_match))
										; Don't require match if the new prefix
										; doesn't continue the old one, and the latter
										; was a match.
										(not (stringp new_prefix))
										(<= (length new_prefix) (length company-prefix)))
									(member company-prefix company-candidates))
								; Last input was a success, but we're treating it as an
								; abort + input anyway, like the `unique' case below.
								(company-cancel 'non-unique))
							((company::require_match)
								; Wrong incremental input, but required match.
								(delete-region company-point (point))
								(message "Matching input is required.")
								company-candidates)
							(t (company-cancel))))
					(t (company-cancel)))))
		(company::after_starting_or_continuing_completion))
	(cond
		(company-candidates
			(company::call_frontends 'post-command)
			(when company::auto_update_doc (company::show_doc_buffer t))
			(setq-local company-keymap-active t)) ; Activate keymap.
		((and company::idle_delay (memq this-command company::begin_command_list))
			(let (
				(auto_begin
					(lambda ()
						(let ((non-essential t))
							(when (company::auto_begin) (company::post_command)))))
			)
				(if (numberp company::idle_delay)
					(setq company-timer
						(run-with-timer (max 0.01 company::idle_delay) nil
							; Old company-idle-begin.
							`(lambda ()
								(and
									(eq ,(current-buffer) (current-buffer))
									(eq ,(selected-window) (selected-window))
									(=
										,(buffer-chars-modified-tick)
										(buffer-chars-modified-tick))
									(= ,(point) (point))
									(,auto_begin)))))
					(funcall auto_begin)))))
	nil)

; ========================================= Mode =========================================

(defun company-is-active () company-candidates)

(define-minor-mode company-mode
"Fast & portable ai-enhanced completion in your basement.
Completion starts automatically, depending on the values
`company::idle_delay' and `company::min_prefix_length'.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'. If these commands are
called before `company::idle_delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'. These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'. If you want to start a specific backend, call
it interactively or use `company-begin-backend'.

By default, the completions list is sorted alphabetically, unless the
backend chooses otherwise, or `company::transformer_list' changes it later.

keymap during active completions (`company-keymap'):

\\{company-keymap}"
	:lighter nil

	(if company-mode
		(progn
			(company::call_frontends 'init)
			(add-hook 'pre-command-hook #'company::pre_command nil t)
			(add-hook 'post-command-hook #'company::post_command nil t)
			(when (boundp 'yas-keymap-disable-hook)
				(add-hook 'yas-keymap-disable-hook #'company-is-active nil t)))
		(remove-hook 'pre-command-hook #'company::pre_command t)
		(remove-hook 'post-command-hook #'company::post_command t)
		(when (boundp 'yas-keymap-disable-hook)
			(remove-hook 'yas-keymap-disable-hook #'company-is-active t))
		(company-cancel)))

(define_globalized_minor_mode 'global-company-mode 'company-mode
	(lambda () (when (/= ?\s (aref (buffer-name) 0)) (company-mode))))

; ======================================== Search ========================================

(defun company::search_words_regex (input)
	(mapconcat
		(lambda (word) (concat "\\(" (regexp-quote word) "\\)"))
		(split-string input " +" t)
		".*"))

(defun company::search_words_in_any_order_regex (input)
	(mapconcat
		(lambda (words) (mapconcat #'identity words ".*"))
		(get_permutations_quick
			(map_modify_list
				(lambda (word) (concat "\\(" (regexp-quote word) "\\)"))
				(split-string input " +" t)))
		"\\|"))

(defun company::search_flex_regex (input)
	(if (= 0 (length input))
		""
		(concat
			(regexp-quote (char-to-string (aref input 0)))
			(mapconcat
				(lambda (char)
					(let ((char_str (char-to-string char)))
						(concat "[^" char_str "]*" (regexp-quote char_str))))
				(substring input 1)))))

(defconst company::search_regex_fn #'company::search_words_in_any_order_regex
"Function to construct the search regexp from input.
It's called with one argument, the current search input.
It must return either a regexp without groups, or one where
groups don't intersect and each one wraps a part of the input string.
Options:
regexp-quote - exact match,
company::search_words_regex - words separated with spaces,
company::search_words_in_any_order_regex - words separated with spaces, in any order,
company::search_flex_regex - all characters in given order, with anything in between.")

(defvar-local company-search-string "")

(defvar company-search-filtering nil
"Non-nil to filter the completion candidates by the search string.")

(defvar-local company--search-old-selection 0)
(defvar-local company--search-old-changed nil)

(defun company--search (text lines)
	(cl-loop
		with regex = (funcall company::search_regex_fn text)
		for i from 0
		for line in lines
		when (string-match regex line) return i))

(defun company--search-update-predicate (search_string) "Only for filtering."
	(let (
		(company-candidates-predicate
			(let ((regex (funcall company::search_regex_fn search_string)))
				(unless (string= regex "")
					`(lambda (candidate) (string-match ,regex candidate)))))
	)
		(when-let (
			(candidate_list
				(company::calculate_candidates
					company-prefix
					(company::call_backend 'ignore-case)))
		)
			(company::update_candidates candidate_list))))

(defun company--search-update-string (new)
	(let* (
		(selection (or company-selection 0))
		(pos (company--search new (nthcdr selection company-candidates)))
	)
		(when pos
			(setq company-search-string new)
			(company::set_selection (+ selection pos) t))))

(defun company-search-printing-char () (interactive)
	(let (
		(search_string
			(concat
				company-search-string
				(let ((event-type (event-basic-type last-command-event)))
					(if (characterp event-type)
						(char-to-string last-command-event)
						; Handle key press on the keypad.
						(let ((name (symbol-name event-type)))
							(if (string-match "kp-\\([0-9]\\)" name)
								(match-string 1 name)
								(error "Unexpected printing char input")))))))
	)
		(when company-search-filtering
			(company--search-update-predicate search_string))
		(company--search-update-string search_string)))

(defun company-search-repeat-forward ()
"Repeat the incremental search in completion candidates forward."
	(interactive)
	(if (or company-search-filtering (string= company-search-string ""))
		(company-select-next 1)
		(let* (
			(selection (or company-selection 0))
			(pos
				(company--search
					company-search-string (nthcdr (1+ selection) company-candidates)))
		)
			(cond
				(pos (company::set_selection (+ selection pos 1) t))
				(company::selection_wrap_around
					(setq pos (company--search company-search-string company-candidates))
					(unless (eq pos selection)
						(company::set_selection pos t)))))))

(defun company-search-repeat-backward ()
"Repeat the incremental search in completion candidates backwards."
	(interactive)
	(if (or company-search-filtering (string= company-search-string ""))
		(company-select-previous 1)
		(let* (
			(selection (or company-selection 0))
			(pos
				(company--search
					company-search-string
					(nthcdr
						(- company-candidates-length selection)
						(reverse company-candidates))))
		)
			(cond
				(pos (company::set_selection (- selection pos 1) t))
				; If there are no matches above selection, select last match.
				(company::selection_wrap_around
					(let ((candidate_list_1 (nthcdr (1+ selection) company-candidates)))
						(setq pos
							(company--search company-search-string candidate_list_1))
						; If there are no matches below selection, so selection is on the
						; only match, do nothing.
						(when pos
							(while
								(progn
									(+= selection pos 1)
									(and
										(setq candidate_list_1
											(nthcdr (1+ pos) candidate_list_1))
										(setq pos
											(company--search
												company-search-string
												candidate_list_1)))))
							(company::set_selection selection t))))))))

(defun company::search::set_filtering (value)
"Use this to switch between searching and filtering when company-search-mode is on."
	(unless (eq company-search-filtering value)
		(setq company-search-filtering value)
		(if company-search-filtering
			(company--search-update-predicate company-search-string)
			(when-let (
				(candidate_list
					(company::calculate_candidates
						company-prefix
						(company::call_backend 'ignore-case)))
			)
				(company::update_candidates candidate_list)))
		(company--search-update-string company-search-string)))

(defun company-search-abort () "Abort searching the completion candidates."
	(interactive)
	(company-search-mode -1)
	(when company--search-old-selection
		(company::set_selection company--search-old-selection t))
	(setq company-selection-changed company--search-old-changed))

(defun company-search-other-char () "Abort search and recall command."
	(interactive)
	(company-search-mode -1)
	(company-unrecord-this-command))

(defun company-search-delete-char () (interactive)
	(unless (string= company-search-string "")
		(let ((ss (substring company-search-string 0 -1)))
			(when company-search-filtering (company--search-update-predicate ss))
			(company--search-update-string ss))))


(define-minor-mode company-search-mode
"Search mode for completion candidates.
Don't start this directly, use `company-search-candidates' or
`company-filter-candidates'."
	:lighter nil

	(let (keymap)
		(if company-search-mode
			(if (not (company::manual_begin))
				(setq company-search-mode nil)
				(setq company--search-old-selection company-selection)
				(setq company--search-old-changed company-selection-changed)
				(company::call_frontends 'update)
				(setq keymap company-search-keymap))
			(setq company-search-filtering nil)
			(kill-local-variable 'company-search-string)
			(kill-local-variable 'company--search-old-selection)
			(kill-local-variable 'company--search-old-changed)
			(when company-backend
				(when-let (
					(candidate_list
						(company::calculate_candidates
							company-prefix
							(company::call_backend 'ignore-case)))
				)
					(company::update_candidates candidate_list))
				(company::call_frontends 'update))
			(setq keymap company-keymap))
		(kill-local-variable 'company-keymap-active)
		(setcdr company-keymap-cell keymap)))

(defun company-search-candidates ()
"Start searching the completion candidates incrementally.

\\<company-search-keymap>Search can be controlled with the commands:
- `company-search-repeat-forward' (\\[company-search-repeat-forward])
- `company-search-repeat-backward' (\\[company-search-repeat-backward])
- `company-search-abort' (\\[company-search-abort])
- `company-search-delete-char' (\\[company-search-delete-char])

Regular characters are appended to the search string.

Customize `company::search_regex_fn' to change how the input
is interpreted when searching."
	(interactive)
	(company-search-mode))

(defun company-filter-candidates ()
"Start filtering the completion candidates incrementally."
	(interactive)
	(company-search-mode)
	; This doesn't use company::search::set_filtering, because that is only
	; needed when company-search-string isn't empty.
	(setq company-search-filtering t))

; Selecting candidates.

(defun company-select-next (n)
"Select the nth next candidate in the list.

When `company-selection-default' is nil, add a special pseudo candidates
meant for no selection."
	(interactive "p")
	(when (company::manual_begin)
		(company::set_selection
			(+ n (or company-selection company-selection-default -1)))))

(defun company-select-previous (n)
"Select the nth previous candidate in the list.

When `company-selection-default' is nil, add a special pseudo candidates
meant for no selection."
	(interactive "p")
	(company-select-next (- n)))

(defun company-select-next-or-abort (n)
"Select the nth next candidate if more than one, else abort
and invoke the normal binding.
With ARG, move by that many elements."
	(interactive "p")
	; Not sure why it's different than in ...previous-or-abort,
	; probably because of default selection.
	(if (or (not company-selection) (> company-candidates-length 1))
		(company-select-next n)
		(company-abort-and-unrecord-this-command)))

(defun company-select-previous-or-abort (n)
"Select the nth previous candidate if more than one, else abort
and invoke the normal binding.
With ARG, move by that many elements."
	(interactive "p")
	(if (> company-candidates-length 1)
		(company-select-previous n)
		(company-abort-and-unrecord-this-command)))

(defun company-select-first () "Select the first completion candidate." (interactive)
	(company::set_selection 0))

(defun company-select-last () "Select the last completion candidate." (interactive)
	(company::set_selection (1- company-candidates-length)))

(defun company::insert_candidate (candidate_string)
	(when (/= (length candidate_string) 0)
		(setq candidate_string (substring-no-properties candidate_string))
		(unless (string= company-prefix candidate_string)
			(delete-char (- (length company-prefix)))
			(insert candidate_string))))

(defun company-complete-selection () "Insert the selected candidate." (interactive)
	(and
		(company::manual_begin)
		company-selection
		(let ((selected_candidate (nth company-selection company-candidates)))
			(company::insert_candidate selected_candidate)
			(company-cancel selected_candidate))))

(defun company-complete-common () "Insert the common part of all candidates."
	(interactive)
	(when (company::manual_begin)
		(if
			(and
				(not (cdr company-candidates))
				(equal company-common (car company-candidates)))
			(company-complete-selection)
			(company::insert_candidate company-common))))

; Used by backends.
(defun company-doc-buffer (&optional str)
	(let ((buffer (get-buffer-create "*company-doc*")))
		(with-current-buffer buffer
			(erase-buffer)
			(fundamental-mode)
			(when str (save-excursion (insert str))))
		buffer))

(defvar company::doc_window nil
"Last doc window.
company::show_doc_buffer will try to reuse this window.")

; TODO ogarnąć tworzenie tych buforów w backendach - muszą współgrać z myTab - mają zaczynać się "*"
; i zrobić dobrze żeby set-window-dedicated wziąść pod uwagę, dodatkowo mode-line dostosować.
; wiadomo, ma być fajne - do std:: można dać jakiś link do cppreference, itp.

(defun company::show_doc_buffer (&optional auto)
"Show the documentation buffer for the selection.
auto non-nil means to do nothing if company::doc_window isn't alive."
	(interactive)
	(let ((is_window_live (window-live-p company::doc_window)))
		(when (or is_window_live (not auto))
			(let (
				(doc_buffer
					(company::call_backend
						'doc-buffer (nth (or company-selection 0) company-candidates)))
				window_start
			)
				(if (not doc_buffer)
					(message "No documentation available.")
					(if (consp doc_buffer)
						(setq
							doc_buffer (get-buffer (car doc_buffer))
							window_start (cdr doc_buffer))
						(setq
							doc_buffer (get-buffer doc_buffer)
							window_start 1))
					(setq other-window-scroll-buffer doc_buffer)
					(if
						(not
							(or
								is_window_live
								(setq company::doc_window
									(window::split_window_sensibly))))
						(when-let ((fallback_window (display-buffer doc_buffer t)))
							(set-window-start fallback_window window_start)
							(setq other-window-scroll-buffer doc_buffer)
							(when company::fit_doc_window
								(fit-window-to-buffer fallback_window)))
						(set-window-dedicated-p company::doc_window nil)
						(set-window-buffer company::doc_window doc_buffer)
						(set-window-dedicated-p company::doc_window t)
						(set-window-start company::doc_window window_start)
						(setq other-window-scroll-buffer doc_buffer)
						(when company::fit_doc_window
							(fit-window-to-buffer company::doc_window)))))))
	nil)

(defun company::quit_doc_window (&optional arg) (interactive "P")
	(when (window-live-p company::doc_window)
		(quit-window arg company::doc_window))
	nil)

(defun company::toggle_auto_doc () (interactive)
	(setq company::auto_update_doc (not company::auto_update_doc))
	(message (if company::auto_update_doc "On" "Off"))
	nil)

(defun company::show_location ()
"Display a buffer in a new window showing the selected candidate in context."
	(interactive)
	; TODO some backends (elip-completion-at-point) don't call find-file-noselect
	; with nowarn, so some unwanted messages/warnings show up.
	; TODO also some stupid shit forgot to wrap function with save-excursion
	; so now elisp backend moves point in selected window, so fix this shit.
	(let* (
		(buffer_or_file
			(company::call_backend
				'location
				(nth (or company-selection 0) company-candidates)))
		(position_or_line (cdr buffer_or_file))
	)
		(if (not position_or_line)
			(message "No location available.")
			(let (
				(buffer
					(if (bufferp (car buffer_or_file))
						; Make sure it's a buffer, not buffer name.
						(get-buffer (car buffer_or_file))
						(find-file-noselect (car buffer_or_file) t)))
				; Try to create a new window and if it isn't possible,
				; fallback to some non-selected existing window.
				(window (window::split_window_sensibly))
			)
				(if window
					(set-window-buffer window buffer)
					(setq window (display-buffer buffer t)))
				(setq other-window-scroll-buffer buffer)
				(with-selected-window window
					(save-restriction
						(widen)
						(if (bufferp (car buffer_or_file))
							(goto-char position_or_line)
							(goto_line position_or_line)))
					(recenter)
					(when company::fit_location_window (fit-window-to-buffer))))))
	nil)

(defun company-begin-with (candidates &optional prefix-length require-match callback)
"Start a completion at point.
CANDIDATES is the list of candidates to use and PREFIX-LENGTH is the length
of the prefix that already is in the buffer before point.
It defaults to 0.

CALLBACK is a function called with the selected result if the user
successfully completes the input.

Example: (company-begin-with '(\"foo\" \"foobar\" \"foobarbaz\"))."
	(company-begin-backend
		(fn_symbol "temp_backend"
			`(lambda (command &optional arg)
				(cl-case command
					(prefix
						(when (eq (point) (marker-position ,(copy-marker (point) t)))
							(buffer-substring (- (point) (or ,prefix-length 0)) (point))))
					(candidates (all-completions arg ',candidates))
					(require-match ',require-match))))
		callback))

; ======================================== Popup ========================================

(defvar-local company::popup::offset 0
"Current scrolling state of the popup.
Represented by the index of the first visible completion candidate from the
candidates list.")

(defvar-local company::popup::current_width 0
"Only used when company::popup::width_grow_only is on.")

(defvar-local company::popup::force_update nil
"Set to t when company-prefix changes, so post-command in
company::popup::frontend will recompute popup.")

(defvar-local company::popup::overlay_vector nil
"Vector of overlays:
	one overlay prepended with newline if entire overlay is after end of buffer

	one overlay for one line, except if overlay is partially after end of buffer,
	then to overlay on last buffer line, string representing popup's lines after
	end of buffer is appended.

This is actually always a vector in buffers with company initialized, not nil.")

(defvar-local company::popup::overlay_vector_length 0)

(defvar-local company::popup::overlay_before_string_vector nil
"Vector with length company::popup::overlay_vector_length,
storing 'before-string attribute for popup overlays -
propertized strings representing popup and it's surroundings.")

(defvar-local company::popup::covered_line_string_vector nil
"Vector with length company::popup::overlay_vector_length,
storing lines currently covered by popup, with tabs turned to spaces
(so it's indistinguishable from original line).")

(defvar-local company::popup::candidate_and_annotation_info_vector nil
"Vector with offset 4 - candidate_str, candidate_width, annotation_str, annotation_width.
annotation_string may be nil.")
(defvar-local company::popup::candidate_and_annotation_info_vector_length 0)

(defvar-local company::popup::line_string_vector nil
"Vector with strings representing only popup's contents, not surrounding lines.")
(defvar-local company::popup::line_string_vector_length 0
"Number of lines that popup covers.")

(defvar-local company::popup::guard_popup_recreation_vector nil
"4 elements only to use when popup overlay exists:
0 - height - line count that popup will have if there will be enough candidates.
1 - is_above - if popup is above caret
2 - window-hscroll
3 - window-body-width
This guards next popup's recomputation - some commands like 'other-window
or such that don't move caret, don't scroll enough to affect popup's height,
don't change window's size and don't modify buffer, so shouldn't cause
recomputation.")


(defconst company::popup::NEWLINE_AFTER_BUFFER_END_STRING
	(propertize "\n"
		'face
			(list
				:extend t
				:background (face-attribute 'default :background nil t))))

(defun company::popup::scroll_next_page () "Select the candidate one page further."
	(interactive)
	(let (company::selection_wrap_around)
		(company::set_selection (+ company-selection company::popup::max_height -2))))

(defun company::popup::scroll_previous_page () "Select the candidate one page earlier."
	(interactive)
	(let (company::selection_wrap_around)
		(company::set_selection (- company-selection company::popup::max_height -2))))

(defconst company-popup-line-keymap
	(let ((keymap (make-sparse-keymap)))
		(define-key keymap [wheel-up] #'company::popup::scroll_previous_page)
		(define-key keymap [wheel-down] #'company::popup::scroll_next_page)
		keymap))

(defconst company-popup-surroundings-keymap
	(let ((keymap (make-sparse-keymap)))
		; Bind down-mouse-... keys at parts of lines that popup doesn't cover,
		; to a commands calling original command with a new event, generated
		; after company-abort, so after popup's overlay deletion.
		; Mouse clicks at that parts get events with position in them always
		; saying that mouse clicked on overlay-start char.
		; This recalling with a new event solves this.
		(mapc
			(lambda (key_modifier)
				(let* (
					(key (intern (concat key_modifier "down-mouse-1")))
					(key_vector (vector key))
				)
					(define-key keymap key_vector
						`(lambda () (interactive)
							(company-abort)
							(when-let ((key_binding (key-binding ,key_vector)))
								(setq prefix-arg current-prefix-arg)
								(setq this-command key_binding)
								(funcall key_binding
									(list
										',key
										(let (
											(mouse_pixel_coords
												(cdr (mouse-pixel-position)))
										)
											(posn-at-x-y
												(car mouse_pixel_coords)
												(cdr mouse_pixel_coords)
												(selected-window)
												t)))))))))
			KEY_MODIFIER_VECTOR)
		keymap))

(defun company::popup::frontend (command)
"Company frontend - popup in buffer made of overlays."
	(let (
		(popup_delete
			(lambda ()
				(dotimes (i company::popup::overlay_vector_length)
					(delete-overlay (aref company::popup::overlay_vector i)))
				(setq company::popup::overlay_vector_length 0)))
		(popup_show
			(lambda ()
				(dotimes (i company::popup::overlay_vector_length)
					(overlay-put (aref company::popup::overlay_vector i) 'display
						; Epic hack, because if that would be just "", then overlay 1,
						; that end at the start of overlay 2, will somehow include
						; 'before-string of overlay 2 in overlay's 1 'display "",
						; so before-string of overlay 1 will be invisible.
						; The same happens with 'invisible t instead of 'display "",
						; different 'fields and different front-advance and rear-advance
						; attributes. This 'display (propertize " " 'invisible t) is
						; the first solution I found to prevent this. I hope it's not
						; very problematic for display engine.
						(propertize " " 'invisible t))
					(overlay-put
						(aref company::popup::overlay_vector i)
						'before-string
						(aref company::popup::overlay_before_string_vector i)))))
	)
		(cl-case command
			(start (setq company::popup::current_width 0))
			(end (funcall popup_delete) (setq company::popup::offset 0))
			(update (setq company::popup::force_update t))
			(unhide
				(when (aref company::popup::overlay_vector 0)
					; If popup is above caret.
					(when (aref company::popup::guard_popup_recreation_vector 1)
						; Sleight of hand: if the current line wraps, we adjust the
						; start of the overlay so that the popup does not zig-zag,
						; but don't update the popup's background. This seems just
						; non-annoying enough to avoid the work required for the latter.

						; This is very old, I never tested this, so it's certainly wrong.
						(save-excursion
							(vertical-motion 1)
							(when
								(/=
									(point)
									(overlay-start
										(aref company::popup::overlay_vector 0)))
								(move-overlay
									(aref company::popup::overlay_vector 0)
									(point)
									(overlay-end
										(aref company::popup::overlay_vector 0))))))
					(funcall popup_show)))
			(pre-command
				; Old company::popup::hide_temp.
				; Hide popup temporarily, for movement in commands to work correctly.
				(dotimes (i company::popup::overlay_vector_length)
					(overlay-put
						(aref company::popup::overlay_vector i) 'before-string nil)
					(overlay-put
						(aref company::popup::overlay_vector i) 'display nil)))
			(post-command
				(let* (
					(position_info (posn-at-point))
					; = 0 if at window first line.
					(available_line_count_above (cdr (posn-col-row position_info)))
					(window_column_count (get_window_last_column))
					(hscroll (window-hscroll))
					; Popup height assuming there are enough candidates to fill it.
					height_if_enough_candidates
					is_above ; If popup should be above caret.
					is_popup_start_after_buffer_end
					width
				)
					; Set height_if_enough_candidates and is_above.
					(let (
						(available_line_count_below
							; -1 to not count caret's line.
							; Could use something more precise that window-screen-lines,
							; but this is probably precise enough.
							; Floor to count only fully visible lines.
							(- (floor (window-screen-lines)) 1 available_line_count_above))
					)
						(if
							(and
								; If space below is not enough for popup.
								; Old version instead of company::popup::max_height:
								; (min company::popup::max_height company-candidates-length).
								(< available_line_count_below company::popup::max_height)
								; In some very small windows where there's not enough space
								; on both sides, just choose side with more space.
								(< available_line_count_below available_line_count_above))
							(setq
								height_if_enough_candidates
									(min
										company::popup::max_height
										available_line_count_above)
								is_above t)
							(setq height_if_enough_candidates
								(min
									company::popup::max_height
									available_line_count_below))))

					; Don't recompute popup only if all below are true:
					;	candidates hadn't changed
					;	(if they had, then company::popup::force_update will be t)
					;
					;	popup overlay already exists
					;
					;	height hadn't changed (this checks vertical scrolling)
					;
					;	popup side hadn't changed
					;
					;	window-hscroll hadn't changed (not every window-hscroll change
					;	should trigger popup's recomputation, but it really isn't important)
					;
					;	window-body-width hadn't changed (similar, it's even less important).
					(unless
						(and
							(not company::popup::force_update)
							(aref company::popup::overlay_vector 0)
							(=
								(aref company::popup::guard_popup_recreation_vector 0)
								height_if_enough_candidates)
							(eq
								(aref company::popup::guard_popup_recreation_vector 1)
								is_above)
							(=
								(aref company::popup::guard_popup_recreation_vector 2)
								hscroll)
							(=
								(aref company::popup::guard_popup_recreation_vector 3)
								(window-body-width)))
						(funcall popup_delete)
						; Save data guarding next popup's recomputation.
						(aset company::popup::guard_popup_recreation_vector 0
							height_if_enough_candidates)
						(aset company::popup::guard_popup_recreation_vector 1 is_above)
						(aset company::popup::guard_popup_recreation_vector 2 hscroll)
						(aset company::popup::guard_popup_recreation_vector 3
							(window-body-width))

						; Set line_covered_by_popup..., is_popup_start_after_buffer_end
						; and company::popup::overlay_vector.
						(let* (
							(true_height ; True final height of popup.
								(min
									height_if_enough_candidates
									; Add one line for displaying search string if
									; search mode is on.
									(if company-search-mode
										(1+ company-candidates-length)
										company-candidates-length)))
							(popup_start_line_index
								(if is_above
									(- available_line_count_above true_height)
									(1+ available_line_count_above)))
							(make_popup_line_overlay
								(lambda (start end)
									(let ((overlay (make-overlay start end)))
										; Beat outline's folding overlays.
										; And Flymake (53). And Flycheck (110).
										(overlay-put overlay 'priority 111)
										(overlay-put overlay 'window (selected-window))
										overlay)))
						)
							(save-excursion
								(setq is_popup_start_after_buffer_end
									(<
										(move-to-window-line popup_start_line_index)
										popup_start_line_index))
								(if is_popup_start_after_buffer_end
									(progn
										(setq company::popup::overlay_vector_length 1)
										(aset company::popup::overlay_vector 0
											(funcall make_popup_line_overlay (point) (point))))
									(cl-loop
										; Line start after the line on which popup overlay ends.
										with line_start_position_after_popup_end =
											(save-excursion (vertical-motion true_height) (point))
										with get_line_create_overlay =
											(lambda (line_start_position line_end_position)
												(let (
													(line_covered_by_popup_str
														(save-excursion
															(buffer_substring_tabs_to_spaces
																line_start_position
																line_end_position)))
												)
													; Keep the original value unmodified, for no special
													; reason.
													(when-let (
														(line_prefix
															(get-text-property
																0 'line-prefix line_covered_by_popup_str))
													)
														(setq line_covered_by_popup_str
															(concat line_prefix line_covered_by_popup_str))
														(remove-text-properties
															0 (length line_covered_by_popup_str)
															'(line-prefix) line_covered_by_popup_str))
													(aset
														company::popup::covered_line_string_vector
														company::popup::overlay_vector_length
														line_covered_by_popup_str))
												(aset
													company::popup::overlay_vector
													company::popup::overlay_vector_length
													(funcall make_popup_line_overlay
														line_start_position line_end_position))
												(++ company::popup::overlay_vector_length)
												nil)
										for line_start_position = (point)
										; If popup ends after end of buffer, then last overlay will
										; contain last real line and will append it's own lines.
										when (= 0 (vertical-motion 1))
											return
												(funcall get_line_create_overlay
													line_start_position (point))
										do
										(funcall get_line_create_overlay
											line_start_position (if (bolp) (1- (point)) (point)))
										while (< (point) line_start_position_after_popup_end)))))

						; This part fills company::popup::line_string_vector and sets
						; company::popup::line_string_vector_length.
						; company::popup::line_string_vector contains only popup's
						; contents, no surroundings.
						(let (
							(candidate_line_count
								(if company-search-mode
									(1- height_if_enough_candidates)
									height_if_enough_candidates))
							(selection_index (or company-selection 0))
							top_line_offset_display_str
							bottom_line_offset_display_str
							search_line_str
							(search_line_str_width 0)
							scrollbar_bounds
						)
							(when company-search-mode
								(setq search_line_str
									(concat
										(propertize
											(if company-search-filtering
												"Filter: "
												"Search: ")
											'face 'company::popup::face)
										(propertize company-search-string
											'face 'company::popup::search_input_face)))
								(setq search_line_str_width (string-width search_line_str)))

							; Set offset and maybe scroll.
							(if (eq company::popup::offset_display 'lines)
;								(progn
;									(cond
;										((< selection_index company::popup::offset)
;											; Try to make selected candidate be at the top.
;											(setq company::popup::offset selection_index))
;										; If selection_index is below or at last popup line.
;										((>= selection_index (+ company::popup::offset candidate_line_count -1))
;											; Try to make selected candidate be at the bottom.
;											(setq company::popup::offset (- selection_index (1- candidate_line_count)))))
;									; Otherwise try to preserve offset.
;									(if (<= company::popup::offset 1)
;										(let ((candidate_below_popup_count (- company-candidates-length candidate_line_count)))
;											(setq company::popup::offset 0)
;											(if (<= candidate_below_popup_count 0)
;												(setq candidate_line_count company-candidates-length)
;												; Add (probably) bottom line.
;												(-- candidate_line_count)
;												; If selection is not at line that would turn to a bottom offset display line.
;												(if (/= selection_index (+ company::popup::offset candidate_line_count))
;													(setq bottom_line_offset_display_str
;														; 1+ because of added bottom line.
;														(concat "(" (number-to-string (1+ candidate_below_popup_count)) ")"))
;													(+= company::popup::offset 2)
;													(setq top_line_offset_display_str (concat "(" (number-to-string company::popup::offset) ")"))
;													(unless (= candidate_below_popup_count 1)
;														; Add bottom line.
;														(-- candidate_line_count)
;														(setq bottom_line_offset_display_str
;															(concat
;																"("
;																(number-to-string
;																	(- company-candidates-length company::popup::offset candidate_line_count))
;																")"))))))
;										; Add top line.
;										(-- candidate_line_count)
;										(let ((candidate_below_popup_count (- company-candidates-length company::popup::offset candidate_line_count)))
;											(cond
;												((= candidate_below_popup_count 0)
;													(setq top_line_offset_display_str (concat "(" (number-to-string company::popup::offset) ")")))
;												((< candidate_below_popup_count 0)
;													; Try to scroll up.
;													(+= company::popup::offset candidate_below_popup_count)
;													; If there is not/exactly enough candidates to fill popup,
;													; show them all without top and bottom lines.
;													(if (<= company::popup::offset 1)
;														(setq company::popup::offset 0
;															  candidate_line_count company-candidates-length)
;														(setq top_line_offset_display_str (concat "(" (number-to-string company::popup::offset) ")"))))
;												(t ; (> candidate_below_popup_count 0)
;													; Add (probably) bottom line.
;													(-- candidate_line_count)
;													; If selection is not at line that would turn to a bottom offset display line.
;													(if (/= selection_index (+ company::popup::offset candidate_line_count))
;														(setq bottom_line_offset_display_str
;															; 1+ because of added bottom line.
;															(concat "(" (number-to-string (1+ candidate_below_popup_count)) ")"))
;														(+= company::popup::offset 2)
;														(setq top_line_offset_display_str (concat "(" (number-to-string company::popup::offset) ")"))
;														(unless (= candidate_below_popup_count 1)
;															; Add bottom line.
;															(-- candidate_line_count)
;															(setq bottom_line_offset_display_str
;																(concat
;																	"("
;																	(number-to-string
;																		(- company-candidates-length company::popup::offset candidate_line_count))
;																	")")))))))))

								; This branch is a mess. ok
								(let (
									(maybe_delete_top_offset_display_line
										(lambda () ; Dynamic binding: candidate_line_count.
											(when (<= company::popup::offset 1)
												(setq company::popup::offset 0)
												(++ candidate_line_count))))
									(get_candidate_below_popup_count
										; Invisible candidates below popup.
										(lambda () ; Dynamic binding: candidate_line_count.
											(-
												company-candidates-length
												candidate_line_count
												company::popup::offset)))
								)
									; Assuming top offset line is needed (not sure if that assumption is
									; right, but after some testing it seems to work), if there are not
									; enough candidates to fill the popup without changing the offset,
									; then set the offset to be able to display all candidates with top
									; offset line and without bottom one.
									; In other words, scroll up to fill the entire popup.
									(when (< (funcall get_candidate_below_popup_count) 0)
										; Yes, this can make offset negative, but this first call to
										; maybe_delete_top_offset_display_line a couple lines below will
										; set it to 0, and nothing before will be broken because of that.
										(setq company::popup::offset
											(1+ (- company-candidates-length candidate_line_count))))
									; Subtract first and last line - these probably display offset.
									(-= candidate_line_count 2)
									(setq company::popup::offset
										(max
											(min selection_index company::popup::offset)
											; Index of the candidate in first candidate line (so offset),
											; if candidate in last candidate line would have index =
											; selection_index.
											(- selection_index (1- candidate_line_count))))
									(funcall maybe_delete_top_offset_display_line)
									; Delete bottom line if it's not needed.
									(when (<= (funcall get_candidate_below_popup_count) 1)
										(++ candidate_line_count)
										; It's a fix for this exact situation, from this max couple
										; lines above. Probably should be done in some clearer way.
										(when (= selection_index (1- company-candidates-length))
											(-- company::popup::offset)
											; Maybe also delete top line if now it's not needed.
											(funcall maybe_delete_top_offset_display_line)))
									; Add top line if it's needed.
									(when (> company::popup::offset 0)
										(setq top_line_offset_display_str
											(concat "(" (number-to-string company::popup::offset) ")")))
									; Add bottom line if it's needed.
									(let (
										(candidate_below_popup_count
											(funcall get_candidate_below_popup_count))
									)
										(when (> candidate_below_popup_count 0)
											(setq bottom_line_offset_display_str
												(concat
													"("
													(number-to-string candidate_below_popup_count)
													")")))))

								(setq company::popup::offset
									(cond
										; If there is not enough candidates to fill popup with current
										; scrolling, reduce scrolling to fill popup.
										((<
												company-candidates-length
												(+ company::popup::offset candidate_line_count))
											(max
												0
												(1+ (- company-candidates-length candidate_line_count))))
										((< selection_index company::popup::offset) selection_index)
										(t
											(max
												company::popup::offset
												(+ selection_index 1 (- candidate_line_count))))))
								(when
									(and
										(eq company::popup::offset_display 'scrollbar)
										(> company-candidates-length candidate_line_count))
									(setq scrollbar_bounds
										(let* (
											(size
												(ceiling
													(*
														candidate_line_count
														(float candidate_line_count))
													company-candidates-length))
											(lower
												(floor
													(*
														candidate_line_count
														(float company::popup::offset))
													company-candidates-length))
											(upper (+ lower size -1))
										)
											(cons lower upper)))
									; We will display scrollbar, so reserve 1 column for it,
									; so pretend that window is 1 column less wide,
									; so later when there will be adjusting popup's width
									; to window, we still will have that 1 column free.
									(-- window_column_count)))

							(setq selection_index
								(when company-selection
									(- company-selection company::popup::offset)))
							; Set this to company-candidates-length if there are not enough
							; candidates to fill entire popup.
							(setq candidate_line_count
								(min candidate_line_count company-candidates-length))
							(setq company::popup::candidate_and_annotation_info_vector_length
								(* candidate_line_count 4))
							(let (
								; Initial max known width without margins.
								(width_no_margins
									(max
										search_line_str_width
										; These only contain digits, so it's fine to use
										; lengths (I think).
										(length top_line_offset_display_str)
										(length bottom_line_offset_display_str)))
								(annotation_padding_length
									(or
										company::popup::annotation_padding
										(if company::popup::align_annotations 1 0)))
								(candidates (nthcdr company::popup::offset company-candidates))
								; I never encountered these, so I don't use this.
;								(replace_non_printable_chars
;									(lambda (str)
;										(replace-regexp-in-string
;											"\\([^[:graph:] ]\\)\\|\\(\ufeff\\)\\|[[:multibyte:]]"
;											(lambda (match)
;												(cond
;													((match-beginning 1)
;														; FIXME: Better char for 'non-printable'?
;														; We shouldn't get any of these, but sometimes
;														; we might. The official "replacement character"
;														; is not supported by some fonts. "\ufffd"
;														"?")
;													; Zero-width non-breakable space.
;													((match-beginning 2) "")
;													((> (string-width match) 1)
;														(concat
;															(make-string
;																(1- (string-width match)) ?\ufeff)
;															match))
;													(t match)))
;											str)))
							)

								; Fill company::popup::candidate_and_annotation_info_vector,
								; clean candidate strings,
								; get annotation strings from backend and clean them,
								; calculate popup's width without margins -
								; max (candidate width + annotation width +
								; annotation padding length).
								(let ((candidates_1 candidates))
									(dotimes (i candidate_line_count)
										(let* (
											(candidate_str (pop candidates_1))
											(annotation_str
												(company::call_backend 'annotation candidate_str))
											candidate_width
											(annotation_width 0)
										)
;											(setq candidate_str
;												(funcall replace_non_printable_chars candidate_str))
											(setq candidate_width (string-width candidate_str))
											(when annotation_str
;												(setq annotation_str
;													(funcall replace_non_printable_chars annotation_str))
												(setq annotation_width (string-width annotation_str)))
											(let ((index (* i 4)))
												(aset
													company::popup::candidate_and_annotation_info_vector
													index
													candidate_str)
												(aset
													company::popup::candidate_and_annotation_info_vector
													(++ index)
													candidate_width)
												(aset
													company::popup::candidate_and_annotation_info_vector
													(++ index)
													annotation_str)
												(aset
													company::popup::candidate_and_annotation_info_vector
													(1+ index)
													annotation_width))
											(setq width_no_margins
												(max
													width_no_margins
													(+
														candidate_width
														(if annotation_str
															(+
																annotation_width
																annotation_padding_length)
															0)))))))
								(setq width_no_margins (max width_no_margins company::popup::min_width))
								(when company::popup::width_grow_only ; The only use of this var.
									(setq width_no_margins
										(max width_no_margins company::popup::current_width))
									(setq company::popup::current_width width_no_margins))
								; Limit width if it doesn't fit in window.
								(setq width_no_margins
									(min
										width_no_margins
										(-
											window_column_count
											company::popup::right_margin_width
											company::icon_margin_width)))

								(let (
									(width_no_left_margin
										(+ width_no_margins company::popup::right_margin_width))
									(popup_line_index 0)
								)
									(setq company::popup::line_string_vector_length candidate_line_count)

									; Maybe add search line.
									(when search_line_str
										(++ popup_line_index)
										(++ company::popup::line_string_vector_length)
										(aset company::popup::line_string_vector 0
											(concat
												(propertize
													(get_space_string company::icon_margin_width)
													'face 'company::popup::face)
												search_line_str
												(propertize
													(get_space_string
														(max
															0
															(-
																width_no_left_margin
																search_line_str_width)))
													'face 'company::popup::face))))

									; Maybe add top and bottom offset display lines.
									(let (
										(get_finalized_offset_display_line
											; Dynamic binding: width_no_left_margin.
											(lambda (offset_display_line_str)
												(propertize_no_copy
													(concat
														(get_space_string company::icon_margin_width)
														offset_display_line_str
														(get_space_string
															(max
																0
																(-
																	width_no_left_margin
																	(length offset_display_line_str)))))
													'face 'company::popup::face
													'keymap company-popup-line-keymap)))
									)
										(when top_line_offset_display_str
											(++ popup_line_index)
											(++ company::popup::line_string_vector_length)
											(aset
												company::popup::line_string_vector
												(1- popup_line_index)
												(funcall get_finalized_offset_display_line
													top_line_offset_display_str)))
										(when bottom_line_offset_display_str
											(++ company::popup::line_string_vector_length)
											(aset
												company::popup::line_string_vector
												(1- company::popup::line_string_vector_length)
												(funcall get_finalized_offset_display_line
													bottom_line_offset_display_str))))

									; Fill company::popup::line_string_vector with candidate lines -
									; propertized strings (icon_margin + candidate + annotation +
									; right_margin).
									(let (
										(popup_line_index_1 popup_line_index)
										(absolute_candidate_index company::popup::offset)
									)
										(dotimes (candidate_line_index candidate_line_count)
											(aset
												company::popup::line_string_vector
												popup_line_index_1
												(let* (
													(index (* candidate_line_index 4))
													(candidate_str
														(aref
															company::popup::candidate_and_annotation_info_vector
															index))
													(candidate_width
														(aref
															company::popup::candidate_and_annotation_info_vector
															(++ index)))
													(annotation_str
														(aref
															company::popup::candidate_and_annotation_info_vector
															(++ index)))
													(annotation_width
														(aref
															company::popup::candidate_and_annotation_info_vector
															(1+ index)))
													(is_selected (eq candidate_line_index selection_index))
													(icon_margin_str
														(gethash
															(company::call_backend 'kind (pop candidates))
															company::icon_margin_hash_table))
													line_str
												)
													(setq icon_margin_str
														(if is_selected (cdr icon_margin_str) (car icon_margin_str)))
													(let (
														(get_string_with_removed_props
															(lambda (str props)
																; Don't modify the original string
																; (maybe unnecessary, not sure).
																(setq str (copy-sequence str))
																(remove-text-properties 0 (length str) props str)
																str))
													)
														; Deal with candidate_str's faces.

														; Remove faces.
														(setq candidate_str
															(funcall get_string_with_removed_props
																candidate_str
																'(face nil font-lock-face nil mouse-face nil)))
														; Add common face.
														(dolist (
															range
;															(let (
;																(company-common
;																	(when company-common
;																		(funcall replace_non_printable_chars
;																			company-common)))
;															)
															(or
																(let (
																	(matches
																		(company::call_backend 'match candidate_str))
																)
																	(cond
																		((and
																			matches
																			company-common
																			(listp matches)
																			(= 1 (length matches))
																			(= 0 (car (car matches)))
																			(>
																				(length company-common)
																				(cdr (car matches))))
																			nil)
																		((integerp matches) `((0 . ,matches)))
																		(t matches)))
																(and
																	company-common
																	`((0 . ,(length company-common)))))
														)
															(put-text-property
																(car range)
																(cdr range)
																'face
																(if is_selected
																	'company::popup::common_selection_face
																	'company::popup::common_face)
																candidate_str))
														; Add match face.
														(and
															company-search-mode
															(let ((regex (funcall company::search_regex_fn company-search-string)))
																(and (not (string= regex "")) (string-match regex candidate_str)))
															(let (
																(match_data (match-data))
																(add_search_face
																	(lambda ()
																		(add-face-text-property
																			(car match_data)
																			(nth 1 match_data)
																			(if is_selected
																				'company::popup::search_selection_face
																				'company::popup::search_face)
																			nil
																			candidate_str)))
															)
																; This split to highlighting entire match vs submatches doesn't
																; really make sense, but it looks a little better in most
																; usecases of \(\) regex constructs.
																(if (length= match_data 2)
																	(funcall add_search_face)
																	(while (setq match_data (nthcdr 2 match_data))
																		(when (car match_data) (funcall add_search_face))))))
														; Add deprecated face.
														(when (company::call_backend 'deprecated candidate_str)
															(add-face-text-property
																0
																(length candidate_str)
																'company::popup::deprecated_face
																t
																candidate_str))

														; Deal with annotation_str's faces.
														(when annotation_str
															; Remove faces.
															(setq annotation_str
																(funcall get_string_with_removed_props
																	annotation_str
																	'(font-lock-face nil mouse-face nil)))
															; Add face.
															(propertize_no_copy annotation_str
																'face
																(if is_selected
																	'company::popup::annotation_selection_face
																	'company::popup::annotation_face))))

													; Concatenate icon margin, candidate, annotation and right margin
													; into complete line.
													(setq line_str
														(concat
															icon_margin_str
															(let* (
																; Min width that candidate, and possibly annotation, need.
																(desired_min_width_no_margins
																	(if annotation_str
																		(+
																			candidate_width
																			annotation_padding_length
																			annotation_width)
																		candidate_width))
																; If that is too much, truncate.
																(truncate (> desired_min_width_no_margins width_no_margins))
															)
																(if (or truncate (not company::popup::align_annotations))
																	(let (
																		(line_no_margins_str
																			(if annotation_str
																				(concat
																					candidate_str
																					(get_space_string annotation_padding_length)
																					annotation_str)
																				candidate_str))
																	)
																		(if truncate
																			(truncate-string-to-width
																				line_no_margins_str width_no_margins nil ?\s)
																			; Add spaces to reach right margin.
																			(concat
																				line_no_margins_str
																				(get_space_string
																					(-
																						width_no_margins
																						desired_min_width_no_margins)))))
																	(concat
																		candidate_str
																		; Add padding.
																		(get_space_string
																			(-
																				width_no_margins
																				candidate_width
																				annotation_width))
																		annotation_str)))
															(get_space_string company::popup::right_margin_width)))
													; Add global selected face.
													(when is_selected
														(add-face-text-property
															0
															(length line_str)
															'company::popup::selection_face
															t
															line_str))
													; Add global default face, because we don't want
													; to inherit it from overlay's surroundings.
													(add-face-text-property
														0 (length line_str) 'company::popup::face t line_str)
													(put-text-property
														0
														(length line_str)
														'mouse-face
														'company::popup::mouse_face
														line_str)
													(propertize_no_copy line_str
														'pointer 'arrow
														'keymap
														(let ((keymap (make-sparse-keymap)))
															(set-keymap-parent keymap company-popup-line-keymap)
															(define-key keymap [down-mouse-1]
																`(lambda () (interactive)
																	(company::set_selection ,absolute_candidate_index)
																	(company-complete-selection)))
															(define-key keymap [down-mouse-3]
																`(lambda () (interactive)
																	(company::set_selection ,absolute_candidate_index)))
															keymap))
													line_str))
											(++ absolute_candidate_index)
											(++ popup_line_index_1)))

									; Set popup full width and maybe add scrollbar.
									(setq width
										(+
											company::icon_margin_width
											width_no_margins
											company::popup::right_margin_width))
									(when scrollbar_bounds
										; Add scrollbar column.
										(++ width)
										; Restore correct value.
										(++ window_column_count)
										; Add scrollbar.
										(dotimes (candidate_line_index candidate_line_count)
											(aset company::popup::line_string_vector popup_line_index
												(concat
													(aref company::popup::line_string_vector popup_line_index)
													(propertize " "
														'face
														(if
															(in_range_inclusive
																candidate_line_index
																(car scrollbar_bounds)
																(cdr scrollbar_bounds))
															'company::popup::scrollbar_face
															'company::popup::scrollbar_track_face))))
											(++ popup_line_index))))))

						(let* (
							; Popup left column relative to mostleft visual column, visible
							; or not. That means column that (vertical-motion 0) moves to
							; (unless at eob).
							(left_column
								(+
									hscroll
									(max
										0
										(-
											(car (posn-col-row position_info))
											(round (line-number-display-width 'columns))
											(string-width company-prefix)
											company::icon_margin_width))))
							(right_column (+ left_column width))
							(merge_and_add_line_to_overlay_before_string
								; Merge popup's line at index popup_line_index with it's
								; surroundings - line at index covered_line_index and save
								; result to company::popup::overlay_before_string_vector at
								; index covered_line_index.
								; These propertize_no_copy calls in here add
								; company-popup-surroundings-keymap to parts of lines not
								; covered by popup.
								(lambda (covered_line_index popup_line_index)
									(let (
										(overlay_before_string
											(let* (
												(covered_line
													(aref
														company::popup::covered_line_string_vector
														covered_line_index))
												(covered_line_width (string-width covered_line))
											)
												; If line reaches after left_column.
												(if (> covered_line_width left_column)
													(concat
														(propertize_no_copy
															(truncate-string-to-width
																covered_line
																left_column
																nil
																; Just for safety, if last char is somehow very
																; wide.
																?\s)
															'keymap company-popup-surroundings-keymap)
														(aref company::popup::line_string_vector popup_line_index)
														(when (> covered_line_width right_column)
															(propertize_no_copy
																(truncate-string-to-width
																	covered_line
																	covered_line_width
																	right_column
																	?\s)
																'keymap company-popup-surroundings-keymap)))
													(concat
														(propertize_no_copy
															(concat
																covered_line
																(get_space_string
																	(- left_column covered_line_width)))
															'keymap company-popup-surroundings-keymap)
														(aref
															company::popup::line_string_vector
															popup_line_index)))))
									)
										(add-face-text-property
											0 (length overlay_before_string) 'default t overlay_before_string)
										(aset
											company::popup::overlay_before_string_vector
											covered_line_index
											overlay_before_string)
										nil)))
						)

							; Maybe move popup to the left to fit it in window.
							(let ((window_last_visible_column (+ window_column_count hscroll)))
								(when (> right_column window_last_visible_column)
									(setq left_column (- window_last_visible_column width))
									(setq right_column window_last_visible_column)
									; If now we are too much to the left, move to the column 0.
									(when (< left_column 0)
										(setq left_column 0)
										(setq right_column width))))

							; Fill company::popup::overlay_before_string_vector with lines
							; from company::popup::line_string_vector surrounded with contents
							; of matching lines from company::popup::covered_line_string_vector.
							(cond
								; If popup should be flipped.
								((and is_above company::popup::flip_when_above)
									; Here we don't need to worry about end of buffer.
									(let ((i 0) (j (1- company::popup::line_string_vector_length)))
										(while (< i company::popup::line_string_vector_length)
											(funcall merge_and_add_line_to_overlay_before_string i j)
											(++ i)
											(-- j))))
								(is_popup_start_after_buffer_end
									(aset company::popup::overlay_before_string_vector 0
										(let (
											(overlay_before_string
												; I don't quite remember what is this space for,
												; probably because cursor is displayed at the end
												; of overlay instead of at the start or some
												; other weird overlay behaviour.
												(let ((first_space_and_newline_str " \n"))
													(put-text-property
														0 1 'cursor t first_space_and_newline_str)
													first_space_and_newline_str))
											(left_padding_str (get_space_string left_column))
										)
											(dotimes (i company::popup::line_string_vector_length)
												(setq overlay_before_string
													(concat
														overlay_before_string
														left_padding_str
														(aref company::popup::line_string_vector i)
														company::popup::NEWLINE_AFTER_BUFFER_END_STRING)))
											(add-face-text-property
												0
												(length overlay_before_string)
												'default
												t
												overlay_before_string)
											overlay_before_string)))
								; Most common branch - popup is under caret, caret isn't at last
								; buffer line.
								(t
									(let ((i 0))
										; Fill lines before end of buffer.
										(while (< i company::popup::overlay_vector_length)
											(funcall merge_and_add_line_to_overlay_before_string i i)
											(++ i))
										; If some lines must be after end of buffer, append them to
										; the last existing line.
										(when (< i company::popup::line_string_vector_length)
											; = (1- company::popup::overlay_vector_length)
											(let ((last_overlay_before_string_vector_index (1- i)))
												(aset
													company::popup::overlay_before_string_vector
													last_overlay_before_string_vector_index
													(concat
														(aref
															company::popup::overlay_before_string_vector
															last_overlay_before_string_vector_index)
														(let (
															(left_padding_str (get_space_string left_column))
															(string_to_append
																company::popup::NEWLINE_AFTER_BUFFER_END_STRING)
														)
															(while (< i company::popup::line_string_vector_length)
																(setq string_to_append
																	(concat
																		string_to_append
																		left_padding_str
																		(aref company::popup::line_string_vector i)
																		company::popup::NEWLINE_AFTER_BUFFER_END_STRING))
																(++ i))
															(add-face-text-property
																0 (length string_to_append) 'default t string_to_append)
															string_to_append)))))))))))
				(funcall popup_show))
			(init
				(setq company::popup::guard_popup_recreation_vector (make-vector 4 nil))
				; This is completely safe - this max_height is really max.
				(setq company::popup::overlay_vector
					(make-vector company::popup::max_height nil))
				(setq company::popup::overlay_before_string_vector
					(make-vector company::popup::max_height nil))
				(setq company::popup::covered_line_string_vector
					(make-vector company::popup::max_height nil))
				(setq company::popup::candidate_and_annotation_info_vector
					(make-vector (* company::popup::max_height 4) nil))
				(dotimes (i (* company::popup::max_height 2))
					(aset
						company::popup::candidate_and_annotation_info_vector
						(1+ (* i 2))
						0))
				(setq company::popup::line_string_vector
					(make-vector company::popup::max_height nil))))))

; Commented section below is out-of-date.

;(defun company--show-inline-p ()
;	(and
;		(not (cdr company-candidates))
;		company-common
;		(not (eq t (compare-strings company-prefix nil nil (car company-candidates) nil nil t)))
;		(or
;			(eq (company::call_backend 'ignore-case) 'keep-prefix)
;			(string-prefix-p company-prefix company-common))))
;
;(defun company::popup::unless_just_one_frontend (command)
;"`company::popup::frontend', but not shown for single candidates."
;	(unless (and (memq command '(post-command unhide)) (company--show-inline-p))
;		(company::popup::frontend command)))
;
;(defvar company::popup::timer nil "Only for company::popup::unless_just_one_frontend_with_delay.")
;
;(defun company::popup::unless_just_one_frontend_with_delay (command)
;"`company::popup::frontend', but shown after a delay.
;Delay is determined by `company::popup::idle_delay'."
;	(when (and (memq command '(pre-command end)) company::popup::timer)
;		(cancel-timer company::popup::timer)
;		(setq company::popup::timer nil))
;	(cl-case command
;		(post-command
;			(if (or company::popup::timer (overlayp company::popup::overlay_vector))
;				(if (not (overlayp company-preview-overlay))
;					(company::popup::unless_just_one_frontend command)
;					(let (company::popup::timer) (company::call_frontends 'pre-command))
;					(company::call_frontends 'post-command))
;				(setq company::popup::timer
;					(run-with-timer
;						company::popup::idle_delay
;						nil
;						(lambda ()
;							(if company-candidates
;								(company::popup::unless_just_one_frontend_with_delay 'post-command)))))))
;		(unhide
;			(if (overlayp company::popup::overlay_vector)
;				(company::popup::unless_just_one_frontend command)))
;		(t (company::popup::unless_just_one_frontend command))))
;
;; ========================================= Preview =========================================
;
;(defun company-strip-prefix (str) (substring str (length company-prefix)))
;
;(defvar-local company-preview-overlay nil)
;
;(defun company-preview-show-at-point (pos completion)
;	(company-preview-hide)
;	(let* ((company-common
;				(and
;					company-common
;					(string-prefix-p company-prefix company-common)
;					company-common))
;		   (common (company--common-or-matches completion)))
;		(setq completion (copy-sequence (company--pre-render completion)))
;		(add-face-text-property 0 (length completion) 'company-preview nil completion)
;		(cl-loop
;			for (beg . end) in common
;			do (add-face-text-property beg end 'company-preview-common nil completion))
;		; Add search string
;		(if (string-match
;				(funcall company::search_regex_fn company-search-string)
;				completion)
;			(pcase-dolist (`(,mbeg . ,mend) (company--search-chunks))
;				(add-face-text-property mbeg mend 'company-preview-search nil completion)))
;		(if (string-prefix-p company-prefix completion
;				(eq (company::call_backend 'ignore-case) 'keep-prefix))
;			(setq completion (company-strip-prefix completion)))
;		(if (string-prefix-p "\n" completion)
;			(setq completion
;				(concat
;					(propertize " " 'face 'company-preview)
;					"\n"
;					(substring completion 1))))
;		(if (and (equal pos (point)) (not (equal completion "")))
;			(add-text-properties 0 1 '(cursor 1) completion))
;		(let* ((beg pos)
;			   (pto company::popup::overlay_vector)
;			   (ptf-workaround (and pto (char-before pos) (eq pos (overlay-start pto)))))
;			; Try to accommodate for the popup overlay,
;			; which may start at the same position if it's at eol.
;			(when ptf-workaround
;				(-- beg)
;				(setq completion (concat (buffer-substring beg pos) completion)))
;			(setq company-preview-overlay (make-overlay beg pos))
;			(let ((ov company-preview-overlay))
;				(overlay-put ov (if ptf-workaround 'display 'after-string) completion)
;				(overlay-put ov 'window (selected-window))))))
;
;(defun company-preview-hide ()
;	(when company-preview-overlay
;		(delete-overlay company-preview-overlay)
;		(setq company-preview-overlay nil)))
;
;(defun company-preview-frontend (command)
;"`company-mode' frontend showing the selection as if it had been inserted."
;	(pcase command
;		(`pre-command (company-preview-hide))
;		(`unhide
;			(if company-selection
;				(let* ((current (nth company-selection company-candidates))
;					   (company-prefix
;							(if (equal current company-prefix)
;								; Would be more accurate to compare lengths,
;								; but this is shorter.
;								current
;								(buffer-substring
;									(- company-point (length company-prefix))
;									(point)))))
;					(company-preview-show-at-point (point) current))))
;		(`post-command
;			(if company-selection
;				(company-preview-show-at-point
;					(point) (nth company-selection company-candidates))))
;		(`end (company-preview-hide))))
;
;(defun company-preview-if-just-one-frontend (command)
;"`company-preview-frontend', but only shown for single candidates."
;	(if (or (not (memq command '(post-command unhide))) (company--show-inline-p))
;		(company-preview-frontend command)))
;
;(defun company-preview-common--show-p ()
;"Returns whether the preview of common can be showed or not"
;	(and
;		company-common
;		(or
;			(eq (company::call_backend 'ignore-case) 'keep-prefix)
;			(string-prefix-p company-prefix company-common))))
;
;(defun company-preview-common-frontend (command)
;"`company-mode' frontend preview the common part of candidates."
;	(if (or (not (memq command '(post-command unhide))) (company-preview-common--show-p))
;		(pcase command
;			(`pre-command (company-preview-hide))
;			((or 'post-command 'unhide)
;				(company-preview-show-at-point (point) company-common))
;			(`end (company-preview-hide)))))
;
;; ========================================= Echo =========================================
;
;(defvar-local company-echo-last-msg nil)
;
;(defvar company-echo-timer nil)
;
;(defvar company-echo-delay .01)
;
;(defconst company-echo-truncate-lines t
;"Whether frontend messages written to the echo area should be truncated.")
;
;(defun company-echo-show (&optional getter)
;	(let ((last-msg company-echo-last-msg)
;		  (message-log-max nil)
;		  (message-truncate-lines company-echo-truncate-lines))
;		(if getter
;			(setq company-echo-last-msg (funcall getter)))
;		; Avoid modifying the echo area if we don't have anything to say, and we
;		; didn't put the previous message there (thus there's nothing to clear).
;		(if (member company-echo-last-msg '(nil ""))
;			(unless (member last-msg '(nil ""))
;				(message ""))
;			(message "%s" company-echo-last-msg))))
;
;(defun company-echo-show-soon (&optional getter delay)
;	(company-echo-cancel)
;	(setq company-echo-timer
;		(run-with-timer (or delay company-echo-delay) nil 'company-echo-show getter)))
;
;(defun company-echo-cancel (&optional unset)
;	(if company-echo-timer
;		(cancel-timer company-echo-timer))
;	(if unset
;		(setq company-echo-timer nil)))
;
;(defun company-echo-format ()
;	(let ((selection (or company-selection 0)))
;		(let ((limit (window-body-width (minibuffer-window)))
;			  (len -1)
;			  (candidates (nthcdr selection company-candidates))
;			  comp
;			  msg)
;			(while candidates
;				(setq
;					comp
;						(propertize (company-reformat (company--clean-string (pop candidates)))
;							'face 'company-echo)
;					len (+ len 1 (length comp)))
;				(let ((beg 0) (end (string-width (or company-common ""))))
;					; FIXME: Add support for the `match' backend action, and thus, non-prefix matches.
;					(add-text-properties beg end '(face company-echo-common) comp))
;				(if (>= len limit)
;					(setq candidates nil)
;					(push comp msg)))
;			(mapconcat #'identity (nreverse msg) " "))))
;
;(defun company-echo-strip-common-format ()
;	(let ((selection (or company-selection 0)))
;		(let ((limit (window-body-width (minibuffer-window)))
;			  (len (+ (length company-prefix) 2))
;			  (candidates (nthcdr selection company-candidates))
;			  comp
;			  msg)
;			(while candidates
;				(setq comp (company-strip-prefix (pop candidates))
;					  len (+ len 2 (length comp)))
;				(if (>= len limit)
;					(setq candidates nil)
;					(push (propertize comp 'face 'company-echo) msg)))
;			(concat
;				(propertize company-prefix 'face 'company-echo-common)
;				"{"
;				(mapconcat #'identity (nreverse msg) ", ")
;				"}"))))
;
;(defun company-echo-hide ()
;	(unless (equal company-echo-last-msg "")
;		(setq company-echo-last-msg "")
;		(company-echo-show)))
;
;(defun company-echo-frontend (command)
;"`company-mode' frontend showing the candidates in the echo area."
;	(pcase command
;		(`post-command (company-echo-show-soon 'company-echo-format 0))
;		(`end (company-echo-hide))))
;
;(defun company-echo-strip-common-frontend (command)
;"`company-mode' frontend showing the candidates in the echo area."
;	(pcase command
;		(`post-command (company-echo-show-soon 'company-echo-strip-common-format 0))
;		(`end (company-echo-hide))))
;
;(defvar-local company-last-metadata nil)
;
;(defun company-echo-metadata-frontend (command)
;"`company-mode' frontend showing the documentation in the echo area."
;	(pcase command
;		(`post-command
;			(company-echo-show-soon
;				(lambda ()
;					(let ((selected (nth (or company-selection 0) company-candidates)))
;						(unless (eq selected (car company-last-metadata))
;							(setq company-last-metadata (cons selected (company::call_backend 'meta selected))))
;						(cdr company-last-metadata)))))
;		(`unhide (company-echo-show))
;		(`end (company-echo-hide))))


; Some version and diagnostic info.

(defun company-diagnostic () "Pop a buffer with information about completions at point."
	(interactive)
	(let* (
		(bb company-backends)
		(mode (symbol-name major-mode))
		backend
		(prefix
			(cl-loop
				for b in bb
				thereis
					(let ((company-backend b))
						(setq backend b)
						(company::call_backend 'prefix))))
		(c-a-p-f completion-at-point-functions)
		cc
		annotations
	)
		(when (or (stringp prefix) (consp prefix))
			(let ((company-backend backend))
				(condition-case nil
					(setq
						cc
							(company::call_backend 'candidates
								(company::prefix_string prefix))
						annotations
							(mapcar
								(lambda (c)
									(cons c (company::call_backend 'annotation c)))
								cc))
					(error (setq annotations 'error)))))
		(pop-to-buffer (get-buffer-create "*company-diagnostic*"))
		(setq buffer-read-only nil)
		(erase-buffer)
		(insert
			(format "Emacs %s (%s) of %s on %s"
				emacs-version
				system-configuration
				(format-time-string "%Y-%m-%d" emacs-build-time)
				emacs-build-system)
			"\n\ncompany-backends: " (pp-to-string bb)
			"\nUsed backend: " (pp-to-string backend)
			"\n")
		(when (memq 'company-capf (if (listp backend) backend (list backend)))
			(insert "Value of c-a-p-f: " (pp-to-string c-a-p-f)))
		(insert
			"Major mode: " mode
			"\nPrefix: " (pp-to-string prefix)
			"\nCompletions:")
		(unless cc (insert " none"))
		(if (eq annotations 'error)
			(insert "(error fetching)")
			(save-excursion
				(dolist (c annotations)
					(insert "\n  " (prin1-to-string (car c)))
					(when (cdr c) (insert " " (prin1-to-string (cdr c)))))))
		(special-mode)))

; ==================================== Initialization ====================================

(let (initialized_backend_list)
	(dolist (backend (flatten-tree company-backends))
		(unless (memq backend initialized_backend_list)
			(condition-case err
				(progn
					(require backend)
					(push backend initialized_backend_list))
				(error
					(error "Company backend '%s' could not be initialized:\n%s"
						backend (error-message-string err)))))))

(provide 'company)
