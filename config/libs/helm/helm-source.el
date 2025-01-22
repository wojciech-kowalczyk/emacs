; -*- lexical-binding:nil -*-

; Helm sources - cl-structs - records.

(defun helm-source-define-constructor
	(sub_struct_name base_struct_name &optional doc body_fn)
"Define constructor of sub struct objects, named SUB_STRUCT_NAME-make,
taking optional argument SOURCE and rest ARGS.
BODY_FN is called with no args, but \"source\" and \"args\" parameters
are dynamically bound, so it may use them.
Constructor returns new source object, so BODY_FN return value is ignored,
it should modify \"source\" variable to change return value of constructor.
DOC should be nil if no additional doc is provided."
	(declare (doc-string 3))
	(fset (intern (concat sub_struct_name "-make"))
		`(lambda (&optional source &rest args)
			,(concat
				"Constructor of "
				sub_struct_name
				".

If source is nil, create "
				sub_struct_name
				" one.
Otherwise it should be a source of a derived cl-struct to set up."
				(when doc (concat "\n\n" doc)))
			(unless source
				(setq source
					(apply #',(intern (concat sub_struct_name "--make"))
						:allow-other-keys t args)))
			(setq source (apply #',(intern (concat base_struct_name "-make")) source args))
			,(when body_fn (list body_fn))
			source)))

(defvar helm-pattern)
(defvar helm-current-source)

(cl-proclaim '(optimize (safety 0))) ; Not sure if this does anything.

; This shouldn't be a final struct.
(cl-defstruct (helm-source (:constructor helm-source--make) (:copier nil))

"Main struct to define helm sources.
Every source struct should inherit from this."

	; Mandatory

	(name nil
		:type string/function
		:documentation
"Mandatory name of the source.
A string or function with no args returning string, used in source header.")

	(candidates nil
		:type function
		:documentation
"Mandatory function called with no parameters.

For sync sources:
	It should return a list of candidates (or nil).

	Members must be strings or (DISPLAY . REAL) pairs.
	In case of (DISPLAY . REAL) pairs, the DISPLAY string is shown
	in the Helm buffer, but the REAL one is used as action
	argument when the candidate is selected. This allows a more
	readable presentation for candidates which would otherwise be,
	for example, too long or have a common part shared with other
	candidates which can be safely replaced with an abbreviated
	string for display purposes.

	Note that if the (DISPLAY . REAL) form is used then pattern
	matching is done on the displayed string, not on the real value.

	This function, generally should not compute candidates according to
	`helm-pattern' which defeat all the helm's matching mechanism
	i.e. multiple pattern matching.
	But it can do that - for example if you want to use this slot
	to do all matching by itself, set this: :match nil, :match-alist nil,
	:volatile t.

	UPDATE: It's role can be different if `helm-source-sync-grouped' is non-nil.

For async sources:
	It should return process (`processp') with no filter and buffer,
	or nil to act like process returned no candidates, for example
	when helm-pattern is an invalid regex.")

	(action #'identity
		:type function/alist
		:documentation
"Mandatory action(s) - a single function or an alist of
(display_string . function) cells.
Function is called with one parameter: the selected candidate.
You can call 'helm-marked-candidates' to get marked candidates
and just ignore passed selected candidate.

An action other than the default can be chosen from this list
of actions for the currently selected candidate (by default
with TAB). The display_string is shown in the completions
buffer and the function is invoked when an action is
selected. The first action of the list is the default.")

	(match helm-match-multi
		:type list
		:documentation
"Mandatory list of lists with functions:
	first function - \"prepare\" function.
		Called with no args, should return nil to match nothing - skip to
		the next matching function, or some other value to proceed with matching.
		This other value is the new let-bound value for helm-pattern,
		so return helm-pattern if you don't want to change it
		(it's safe, helm-pattern is never nil).
		This temporary helm-pattern value can be anything you want (outside of nil),
		it's usually used as a cache, e.g. with list of split patterns.

	second function - actual matching function.
		Called with one parameter - a candidate to match.
		It should use helm-pattern (let-bound value from prepare function)
		as a pattern.
		Should return nil if candidate wasn't matched,
		t if it was matched but don't highlight anything in it,
		or a list of ranges that it matched in a candidate,
		sorted and not overlapping.

	third (optional) function - matching function with match data.
		Like the second function but this one should return a list of match
		datas for each match. This is similar to the list of ranges returned
		by the second function, but has a little bit different format
		and includes submatches when using regexes with subexpressions
		(see `match-data' and example below).

		E.g. candidate = \"asdfg\":
			string-search (substring):
				pattern = \"sdf\",
				the second function returns ((1 . 4)),
				this one returns ((1 4)).

			string-match (regex):
				pattern = \"s(d)(q)?f(g)x?(p)?\",
				first - ((1 . 5)),
				second - ((1 5 2 3 nil 4 5)).

			multi match (multi regex), mixed:
				pattern = \"f (sd) f?\",
				first - from ((3 . 4) (1 . 3) (0 . 0)) simplified to ((1 . 4)),
				second - ((3 4) (1 3 1 3) (0 0)).

		For now this is only used for replacing matches, so if your source
		doesn't care about this, it can have only prepare and match functions
		specified, leaving this third one nil.

When helm-pattern is an empty string, helm doesn't perform any matching,
so these functions don't need to handle such case.

For async sources, this slot is optional if helm-async-candidate-transformer,
and helm-highlight-current-line are not used.

Functions should be listed in order of precedence, e.g.
most strictly matching one should come first for
best results to come first.
Like (list #'string-match-fn #'fuzzy-match-fn).
Actually, this precedence matters only if helm-candidate-number-limit
will be reached, because then matching stops, and functions are run
from first to last, on whole list of candidates, so for example
if there are 1000 candidates, if first function matched 900, then
second function will only be allowed to match 100 candidates.
If this second function matched 100 candidates, matching instantly stops
and later candidates will probably be sorted in candidate-transformer.")

	; Optional

	(sort #'helm-sort-length-alpha
		:type function
		:documentation "Sort predicate function. Nil to not helm-sort.")

	(match-alist nil
		:type alist
		:documentation
"Value overriding `helm-match-alist' for this source.
If you just want to add some matching options,
don't forget to append `helm-match-alist' to this alist.
:match slot of this source should always be a part of this alist of course.")

	(keymap nil
		:type keymap
		:documentation
"Nil or keymap for this source.
Pretty much always should inherit from helm-map.")

	(persistent-action nil
		:type function/cons
		:documentation
"Can be either a function called with one parameter (the
selected candidate) or a cons cell where first element is this
same function, to prefer splitting helm window for persistent action,
instead of using helm-current-buffer's window, when helm-onewindow-p
is non-nil or helm-current-buffer is dedicated or side.")

	(persistent-action-if nil
		:type function
		:documentation
"Similar to persistent action but it is a function that should
return an object suitable for persistent action when called ,
i.e. a function or a cons cell.")

	(follow nil
		:type symbol
		:documentation
"Enable `helm-follow-mode' for this source only.
Set to 'never to never enable it.
See `helm-follow-mode' for more infos.")

	(follow-delay nil
		:type integer/symbol
		:documentation
"`helm-follow-mode' will execute persistent-action after this delay.
Can also be 'instant, to execute it instantly.
Otherwise value of `helm-follow-input-idle-delay' is used if non-nil,
If none of these are non-nil, use `helm-input-idle-delay'.")

	(multiline nil
		:type bool/integer
		:documentation
"Allow multiline candidates.
When non-nil candidates will be separated by `helm-candidate-separator'.
You can customize the color of this separator with `helm-separator' face.
Value of multiline can be an integer which specify the maximum size of the
multiline string to display, if multiline string is longer than this value
it will be truncated.")

	(requires-pattern 0
		:type integer
		:documentation
"Integer specifying the required length of input before doing any computations.
This is useful in case of sources with lots of candidates.")

	(candidate-transformer nil
		:type function
		:documentation
"Funtion called with one arg - list of candidates in helm-buffer.

Candidates will be in the same format as :candidates returned them.

This transformer is run on the candidate list which is already
filtered by the current pattern and limited by
'helm-candidate-number-limit'.

It can be used to transform the candidate list dynamically, for
example, based on the current pattern.

In some cases it may also be more efficent to perform candidate
transformation here, instead of with `candidates'
even if this transformation is done every time the pattern is
changed. For example, if a candidate set is very large then
`candidates' transforms every candidate while only
some of them will actually be displayed due to the limit
imposed by `helm-candidate-number-limit'.

For async sources this is list of candidates outputted by the process
(split with `helm-source-async-separator').")

	(display-to-real nil
		:type function
		:documentation
"Transform the selected candidate when passing it to action.

Function called with one parameter, the selected candidate.

Avoid recomputing all candidates with candidates
or candidate-transformer to give a new value to REAL,
instead the selected candidate is transformed only when passing it
to action. This works (and make sense) only with plain string
candidates, it will NOT work when candidate is a cons cell, in this
case the real value of candidate will be used.")

	(action-transformer nil
		:type function
		:documentation
"It's a function called with one arg when the action list from
the source is requested (it is when selecting action or showing action buffer).
The arg is the list of actions.

The function should return a transformed action list.

This can be used to customize the list of actions based on the
currently selected candidate.
Not called with selected candidate to not call helm-get-selection
when it is unknown if this function will even use it.")

	(pattern-transformer nil
		:type function
		:documentation
"It's a function called with one argument
before computing matches. Its argument is `helm-pattern'.
Function should return transformed `helm-pattern'
(it must not modify it, as it is not let-bound yet).

Unfortunately, as an optimization, when helm-pattern after transforming it
with this function is an empty string, helm returns all candidates skipping
any matching. It's fine for 99% of sources, but if you want to perform matching
with your specified :match function even with empty helm-pattern,
you must transform it with this :pattern-transformer to not be empty.
This transformation matters only when performing matching on
candidates returned from :candidates, later helm-pattern is restored,
so you can just set helm-pattern to something like \"\\0\" to
pass this info to your :match function, or just skip playing with
all this and do everything by yourself in :candidates function,
setting :volatile t and :match nil.")

	(candidate-number-limit nil
		:type integer
		:documentation
"Override `helm-candidate-number-limit' only for this source.")

	(diacritics nil
		:type bool
		:documentation "Ignore diacritics when matching.")

	(match-part nil
		:type bool/function
		:documentation
"Allow matching only some part of candidate.
If source has this attribute non-nil, match is computed only
on part of candidate returned by the call of function provided
by this attribute, or 'match-part property of matching string.
The function have one arg - candidate,
and return a list of conses with indexes, that specify substrings of candidate
to match. Like (list (cons 0 2) (cons 3 5)).
If this is non-nil and it isn't a function, then every candidate must
have 'match-part prop. When candidate is (display . real), then normally
display must have this prop, as this is the string that will be matched,
but if :match-on-real of this source is non-nil, then real must have this prop.

On async sources, as matching is done by the backend, this have
no effect apart for highlighting matches.")

	(nomark nil
		:type bool
		:documentation
"Don't allow marking candidates when this attribute is present.")

	(noncandidate nil
		:type bool/symbol
		:documentation
"Non-nil to allow using noncandidate lines in this source.
These are strings with \\='helm-noncandidate non-nil prop,
that are amongst items passed to `helm-insert-items'.
If this field is a symbol \\='header and first item is a noncandidate line,
use it instead of source header line - insert it with \\='helm-header t prop,
don't append newline and don't add helm-source-header face;
so if you want it to look like source header, add this face yourself.")

	(mode-line nil
		:type string/function
		:documentation
"String or function called with no args returning string.
String will be appended to mode-line in helm-window.
String shouldn't start with spacing, and it should probably end in spacing
(normally 2 spaces) to easily be able to append new strings.")

	(header-line nil
		:type string/function
		:documentation
"Source local `header-line-format'.
Can also be a function called with no args.
This string probably should start with 1 space.
helm-header face will be prepended to this string.")

	(cleanup nil
		:type function
		:documentation
"Function called with no parameters when helm-buffer is closed.
It is useful for killing unneeded candidates buffer.

Note that the function is executed BEFORE performing action.")

	(init nil
		:type function
		:documentation
"Function called with no parameters when helm is started.
It is useful for collecting current state information which can be
used to create the list of candidates later.
It's also called when force updating.")

	(update nil
		:type function
		:documentation
"Function called with no parameters before :init function
when `helm-force-update' is called.")

	(resume nil
		:type function
		:documentation
"Function called with no parameters at end of initialization
when `helm-resume' is started.")

	(before-init nil
		:type function
		:documentation
"Function run at beginning of initilization of this source.
i.e before the creation of `helm-buffer'.")

	(after-init nil
		:type function
		:documentation
"Function run at end of initilization of this source.
i.e after the creation of `helm-buffer'.")

	(confirm nil
		:type bool/string/function
		:documentation
"One of:
	nil - never request confirmation.

	non-nil, not a function - request confirmation when exiting with new
	(with helm-new text prop) candidate. If this is a string, use it as message,
	else use \"confirm\".

	Function called 2 args returning one of the above.
	First arg is the selected candidate (in the same format as after
	candidate-transformer, that is (display . real) or display),
	second arg is the action function.

Probably most sources that use helm-new candidates want to set this to non-nil.

This \"confirmation\" is just pressing [return] or this-command-keys-vector
the second time, so it's not invasive at all, also mouse selection doesn't
use any confirmation (for now).")

	(auto-resize nil
		:type bool/integer
		:documentation
"Non-nil to call shrink-window-if-larger-than-buffer on helm-window after
computing candidates for the first time, if helm-pattern is an empty string,
multiline attribute is nil, there is only one source and there will be less
candidates than default helm-window vertical size, which is around 17 lines.
Multiline attribute is checked in constructor.
Leave it as nil if you are sure such conditions will never be met
to not waste time checking this;
e.g. when using obarray or using source that will always be used with
at least one other source.
Also leave it when there are functions adding candidates after
their first computation, like skipping boring buffers of files,
because then this size adjustment will be a problem
(unless you handle resizing helm-window in such functions yourself).
Actually this can be an integer - number of lines to add
to the number of candidates.
Set this to 1 if source adds 1 helm-new candidate.
If helm-window is actually resized, set helm-scroll-margin to 0.
After writing all of this, I guess that sometimes it's better
to use helm-after-window-setup-hook and do with the helm-window
what you want by yourself."))

(defun helm-multiline-transformer (candidates)
	(let ((offset (helm-source-multiline helm-current-source)))
		(map_modify_list
			(lambda (cand)
				(let (display real)
					(if (consp cand)
						(setq display (car cand) real (cdr cand))
						(setq display cand real cand))
					(cons
						(helm--multiline-get-truncated-candidate display offset)
						real)))
			candidates)))

(defun helm-source-make (&optional source &rest args)
"Constructor of helm-source.

If source is nil, create helm-source one.
Otherwise it should be a source of a derived cl-struct to set up."
	(unless source (setq source (apply #'helm-source--make :allow-other-keys t args)))
	(when-let ((multiline (helm-source-multiline source)))
		(setf (helm-source-auto-resize source) nil)
		(when (numberp multiline)
			(setf (helm-source-candidate-transformer source)
				(if-let ((original_fn (helm-source-candidate-transformer source)))
					`(lambda (candidates)
						(helm-multiline-transformer (,original_fn candidates)))
					#'helm-multiline-transformer))))
	source)

(cl-defstruct
	(helm-source-sync
		(:copier nil)
		(:constructor helm-source-sync--make)
		(:include helm-source))

"One of two main helm source types.
Every source must inherit either this, or async source."

	; Optional

	(volatile nil
		:type bool
		:documentation
"Indicates the source assembles the candidate list dynamically,
so it shouldn't be cached within a single Helm invocation.
It is only applicable to synchronous sources,
because asynchronous sources are not cached.")

	(match-on-real nil
		:type bool
		:documentation "Match the real value of candidates when non-nil.")

	(grouped nil
		:type bool/symbol
		:documentation
"If non-nil, return value of helm-source-candidates of this source must be
a list of lists - candidates grouped in lists. This makes helm always display
those candidates in groups, sorting is also done only inside those groups.

If value is \\='separators, then first element of every group must be
a candidate separator - noncandidate line which is omitted by matching and
sorting. After matching, candidates passed to candidate-transformer will be
one list made by (apply #'append results), so there may be separators
among them.

For example, helm-occur uses this to group candidates by buffer.")

	(adaptive nil
		:type bool
		:documentation
"Non-nil to use helm-adaptive-history.
You will need to add helm-adaptive-sort to candidate-transformer."))

(helm-source-define-constructor "helm-source-sync" "helm-source")


(cl-defstruct
	(helm-source-async
		(:copier nil)
		(:constructor helm-source-async--make)
		(:include helm-source
			(candidate-transformer #'helm-async-candidate-transformer)
			(mode-line #'helm-async-mode-line)))

"One of two main helm source types.
Every source must inherit either this, or sync source.

Most important slot is :candidates, it must be a function
with no args that must return a process ('processp') or nil.
The process buffer and filter should be nil.

Default candidate-transformer function is `helm-async-candidate-transformer'
which does highlighing and sorting.

Default mode-line function is `helm-async-mode-line' showing process status.

Remember to add these defaults somewhere in functions in these slots
if you will override the defaults."

	; Mandatory

	(separator "\n"
		:type regex
		:documentation "Mandatory candidate separator used in process output.")

	; Optional

	(match-strict nil
		:type bool
		:documentation
"If non-nil, allow matching in `helm-async-candidate-transformer' to discard
candidates if they are not matched.
If nil, this matching is only for sorting and highlighting.")

	(status nil
		:type string
		:documentation
"Nil or string to show as message in mode-line.
Should be set in sentinel function of every async source by
`helm-async-set-finished'."))

(helm-source-define-constructor "helm-source-async" "helm-source")

(defun helm-async-mode-line ()
	(when-let ((status (helm-source-async-status helm-current-source)))
		(concat status "  ")))

(defun helm-async-set-finished (source)
	(setf (helm-source-async-status source) helm-async-status-finished-message)
	(helm-mode-line-update))

(defun helm-async-get-default-sentinel (&rest success_exit_codes)
	`(lambda (process event)
		(and
			(or
				(string= event "finished\n")
				(memq (process-exit-status process) ',success_exit_codes))
			(helm-get-process-info process)
			(helm-async-set-finished ',helm-current-source))))

(provide 'helm-source)
