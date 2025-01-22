; -*- lexical-binding:nil -*-

; From helm-core.el
(defvar helm-pattern)
(defvar helm-match-fold-diacritics)

(defvar helm-match-regex-type 'pcre
"Regex type that user types into the minibuffer.
\\='pcre or \\='elisp.
In the future there may be more options.
Can be set locally in helm-buffer by some commands.")

(defconst helm-match-mixed t
"Usually helm matches this way:
	first, simple substring search, e.g. (string-search pattern candidate);

	second, regex match, e.g. (string-match pattern candidate);

	final, multi match, splitting pattern with this regex
	(normally, this can be changed): \"[ \\f\\t\\n\\r\\v]+\",
	then handling negation - if a part of pattern prefixed with \"!\"
	matches candidate, this candidate is discarded,
	then handling other patterns - regex matching.

This variable allows customizing how this final regex matching is done -
if non-nil, use mixed pattern matching - order of sub patterns
doesn't matter, e.g. candidate: \"foobarbaz\"
pattern \"foo bar\" matches, pattern \"bar foo\" also matches.
This unfortunately isn't exactly translatable to regex, because it is simply
matching every subpattern from the start of the candidate every time,
not something like \"foo.*bar|bar.*foo\", well, in this case it actually
is basically the same, but in this case: \"foo.*oo|oo.*foo\" it isn't.
This could be just regex matching, but it's rarely useful and is much, much
slower - calculating regex for 10 part pattern takes like 10 seconds,
so there is no point even trying to match anything with it.
UPDATE: Now it isn't simply regex matching from the start every time,
now, in order to support patterns like \"foo oo\", the procedure involves
2 loops - outer - \"for each\" loop - regex in split_pattern,
inner - match from the start of candidate, if found match is overlapping
with some other, set \"point\" to end of found match and try to find
another match that doesn't overlap (matching against substring of candidate
starting from \"point\"). If after arriving at the end of candidate with \"point\"
we still don't have a valid match, return nil - don't match this candidate.
So now \"foo oo\" equals regex \"foo.*oo|oo.*foo\", but other stuff
like \".+ .+\" is still different (in most regex engines),
but this is not that important or useful.

If nil, use fixed pattern matching - order of sub patterns matters,
e.g.
pattern \"foo bar\" -> regex \"foo.*bar\" matches,
pattern \"bar foo\" -> regex \"bar.*foo\" doesn't match.
This version is translatable to regex.

Unfortunately there is no way to implement negation through regex alone.

Also permutation matching (mixed patterns) solely through regex
is very inefficient, so some commands that use external processes
that use regex will pass regex to match anything and later will perform
correct multi matching.")


(defun helm-match-is-valid-regex (regex)
	(ignore-error invalid-regexp (string-match regex "") t))

(defun helm-match-get-patterns (pattern)
"Return (list_of_negated_strings . list_of_strings_to_match)."
	(let (negation_list match_list)
		(dolist (str (split-string pattern))
			(if (= ?! (aref str 0))
				(push (substring-no-properties str 1) negation_list)
				(push str match_list)))
		(cons
			(nreverse negation_list)
			(nreverse match_list))))

(defun helm-match--match-base (candidate match_fn)
"MATCH_FN is called with patterns to match and candidate;
it should return nil if candidate was not matched,
else list of ranges representing parts of candidate to highlight
(not sorted or merged)."
	(let ((patterns helm-pattern))
		(and
			; Return nil if match for a negation was found, else return t.
			(cl-loop
				for regex in (car patterns)
				never (string-match-p regex candidate))
			(or
				; Return t if there are no patterns to match.
				(not (setq patterns (cdr patterns)))
				; Match patterns.
				(when-let ((range_list (funcall match_fn patterns candidate)))
					; Delete 0-length ranges.
					(setq range_list
						(cl-delete-if
							(lambda (range) (= (car range) (cdr range)))
							range_list))
					(or
						; If after removing 0-length ranges, there are no more ranges,
						; return t - match but don't highlight.
						; For regexes matching empty strings, like "^" or ".*".
						(not range_list)
						; Merge and sort ranges in range_list to represent parts
						; of candidate to highlight.
						; E.g. from '((1 . 3) (2 . 3) (4 . 5) (1 . 2)) to '((1 . 3) (4 . 5)).
						; I think this is still the best and most efficient way.
						; Obviously this would also benefit from being moved to C immensly.
						(let (
							(sorted_length 0)
							(first_unsorted_range range_list)
							i
						)
							; Merge overlapping ranges.
							(while (setq i (cdr first_unsorted_range))
								(let (
									(current (car first_unsorted_range))
									(absolute_index sorted_length)
								)
									(while i
										(let ((range (car i)))
											(setq i (cdr i))
											(++ absolute_index)
											; Unless ranges are not overlapping.
											(unless
												(or
													(>= (car current) (cdr range))
													(<= (cdr current) (car range)))
												; Found overlapping ranges - merge them, remove
												; them from range_list, move result of their merger
												; to the first unsorted position in range_list
												; and start searching for overlapping ranges again.
												(setcar current (min (car current) (car range)))
												(setcdr current (max (cdr current) (cdr range)))
												(delete_from_list_by_index range_list absolute_index)
												(setq absolute_index sorted_length)
												(setq i (cdr first_unsorted_range))))))
								(setq first_unsorted_range (cdr first_unsorted_range))
								(++ sorted_length))
							; Sort ascendingly.
							(sort
								range_list
								(lambda (range_1 range_2) (< (car range_1) (car range_2)))))))))))

(defun helm-match--match-data-base (candidate match_fn)
	(let ((patterns helm-pattern))
		(and
			(cdr patterns) ; If there is anything to match.
			; Return nil if match for a negation was found.
			(cl-loop
				for regex in (car patterns)
				never (string-match-p regex candidate))
			(funcall match_fn (cdr patterns) candidate))))

; Fixed order.

(defun helm-match-fixed-get-regex (pattern &optional match_negation)
	(if-let (
		(split
			(if match_negation
				(split-string pattern)
				(cdr (helm-match-get-patterns pattern))))
	)
		(mapconcat
			(if helm-match-fold-diacritics #'char-fold-to-regexp #'identity)
			split
			".*")
		""))

(defun helm-match-fixed-match (candidate)
	(helm-match--match-base
		candidate
		(lambda (patterns candidate)
			(cl-loop
				with range_list = nil
				with index = 0
				for regex in patterns
				if (string-match regex candidate index)
					do
					(push
						; Regexes matching empty strings are ok here.
						(cons (match-beginning 0) (setq index (match-end 0)))
						range_list)
				else return nil
				finally return range_list))))

(defun helm-match-fixed-match-data (candidate)
	(helm-match--match-data-base
		candidate
		(lambda (patterns candidate)
			(cl-loop
				with index = 0
				for regex in patterns
				if (string-match regex candidate index)
					collect (progn (setq index (match-end 0)) (match-data))
				else return nil))))

; Mixed order (permutations).

(defun helm-match-mixed-get-regex (pattern &optional match_negation)
"Return a regex.

If MATCH_NEGATION is non-nil, match parts of PATTERN prefixed with \"!\"
just like \"!\" was an oridinary character,
else ignore parts prefixed with \"!\" (act as if they didn't exist in PATTERN).

E.g. pattern = \"foo bar !baz\", MATCH_NEGATION is nil,
return \"bar.*foo\\|foo.*bar\".

PATTERN with 8 elements takes around 0.038s to calculate,
9 elements - around 0.5s, 10 elements - 7.5s;
so message that pattern is too complicated when it has more that 6
elements to match and return empty string."
	(if-let (
		(split
			(if match_negation
				(split-string pattern)
				(cdr (helm-match-get-patterns pattern))))
	)
		(if (length> split 6)
			(progn
				(message
					"Pattern can have at most 6 parts when using mixed multi matching.")
				"")
			(when helm-match-fold-diacritics
				(map_modify_list #'char-fold-to-regexp split))
			(mapconcat
				(lambda (perm) (mapconcat #'identity perm ".*"))
				(get_permutations_quick split)
				"\\|"))
		""))

(defun helm-match-mixed-match (candidate)
	(helm-match--match-base
		candidate
		(lambda (patterns candidate)
			(cl-loop
				with range_list = nil
				with this_match_start = nil
				with this_match_end = nil
				for regex in patterns
				if
					(cl-loop
						with index = 0
						unless (string-match regex candidate index)
							; No (maybe next) match found - return nil.
							return nil
						do
						(setq this_match_start (match-beginning 0))
						(setq this_match_end (match-end 0))
						when
							; If found match is overlapping, go to next loop to try
							; to find other match that doesn't overlap
							; (simulate "foo.*oo").
							(cl-loop
								for (start . end) in range_list
								always
									(or
										(>= this_match_start end)
										(<= this_match_end start)))
							return t
						; This max is because only (match-end 0) doesn't protect
						; against regexes matching empty string - infinite loop,
						; so move at least one char at a time.
						; Also this loop above makes it impossible to call
						; string-match with index > (length candidate), because
						; (match-beginning 0) will always be > than any range end
						; in range_list.
						do (setq index (max (1+ this_match_start) this_match_end)))
					do (push (cons this_match_start this_match_end) range_list)
				else return nil
				finally return range_list))))

(defun helm-match-mixed-match-data (candidate)
	(helm-match--match-data-base
		candidate
		(lambda (patterns candidate)
			(cl-loop
				with match_data_list = nil
				for regex in patterns
				if
					(cl-loop
						with index = 0
						unless (string-match regex candidate index)
							; No (maybe next) match found - return nil.
							return nil
						for this_match_start = (match-beginning 0)
						for this_match_end = (match-end 0)
						when
							; If found match is overlapping, go to next loop to try
							; to find other match that doesn't overlap
							; (simulate "foo.*oo").
							(cl-loop
								for match_data in match_data_list
								always
									(or
										(>= this_match_start (nth 1 match_data))
										(<= this_match_end (car match_data))))
							return t
						; This max is because only (match-end 0) doesn't protect
						; against regexes matching empty string - infinite loop,
						; so move at least one char at a time.
						; Also this loop above makes it impossible to call
						; string-match with index > (length candidate), because
						; (match-beginning 0) will always be > than any range end
						; in match_data_list.
						do (setq index (max (1+ this_match_start) this_match_end)))
					collect (match-data) into match_data_list
				else return nil
				finally return match_data_list))))

(defun helm-match-get-regex (&optional pattern match_negation)
"Return regex created from PATTERN according to `helm-match-mixed'.

This doesn't support negation, because it's not possible to support it
using just regex, so just ignore negated strings if MATCH_NEGATION is nil,
else match them like \"!\" was not a special character.

PATTERN defaults to `helm-pattern'."
	(funcall
		(if helm-match-mixed #'helm-match-mixed-get-regex #'helm-match-fixed-get-regex)
		(or pattern helm-pattern) match_negation))

; First match - match pattern as a substring of candidate.

(defconst helm-match-substring
	(list
		(lambda () (if case-fold-search (downcase helm-pattern) helm-pattern))
		(lambda (candidate)
			(when case-fold-search (setq candidate (downcase candidate)))
			(when-let ((start (string-search helm-pattern candidate)))
				(list (cons start (+ start (length helm-pattern))))))
		(lambda (candidate)
			(when case-fold-search (setq candidate (downcase candidate)))
			(when-let ((start (string-search helm-pattern candidate)))
				(list (list start (+ start (length helm-pattern)))))))
"Match functions for helm matching, simply matching substring.")

(defconst helm-match-substring-cell (list "substring" helm-match-substring))

; Second match - regex.

(let (
	(match_fn
		(lambda (candidate)
			(when (string-match helm-pattern candidate)
				(or
					(= (match-beginning 0) (match-end 0))
					(list (cons (match-beginning 0) (match-end 0)))))))
	(match_data_fn
		(lambda (candidate)
			(when (string-match helm-pattern candidate)
				(or
					(= (match-beginning 0) (match-end 0))
					(list (match-data))))))
)
	(defconst helm-match-regex-single
		(list
			(lambda ()
				; Translate and check if helm-pattern is a valid regex.
				(ignore-error invalid-regexp
					(let (
						(pattern
							(if (eq helm-match-regex-type 'pcre)
								(rxt-pcre-to-elisp helm-pattern)
								helm-pattern))
					)
						(string-match pattern "")
						(if helm-match-fold-diacritics
							(char-fold-diacritics pattern)
							pattern))))
			match_fn
			match_data_fn)
"Match functions for helm matching, matching regex,
to use as the sole matching method.")

	(defconst helm-match-regex-multi
		(list
			(lambda ()
				; Is pattern a regex and not just a simple string.
				; Length comparison because regexp-quote only works through
				; adding backslashes (I think), so if it added any - it's
				; probably a regex.
				(when
					(/=
						(length helm-pattern)
						(length
							(if (eq helm-match-regex-type 'pcre)
								(rxt-quote-pcre helm-pattern)
								(regexp-quote helm-pattern))))
					(funcall (car helm-match-regex-single))))
			match_fn
			match_data_fn)
"Match functions for helm matching, matching regex,
to use as the matching method after substring."))

(defconst helm-match-regex-cell (list "regex" helm-match-regex-single))

; Combination of substring and regex matching.

(defconst helm-match-substring-and-regex-cell
	(list "substring and regex" helm-match-substring helm-match-regex-multi))

; Multi match with substring and regex.

(defvar helm-match--multi-match-fn)
(defvar helm-match--multi-match-data-fn)

(defconst helm-match-multi
	(list
		helm-match-substring
		helm-match-regex-multi
		; Multi match - negation and multi regex match.
		(list
			; Prepare function for helm multi matching (third, final stage).
			; Return nil to match nothing, t to match everything or a cons to
			; actually proceed with matching.
			(lambda ()
				; No matching should be performed on empty pattern,
				; so don't handle that case here.
				(let ((patterns (helm-match-get-patterns helm-pattern)))
					(and
						(or
							(car patterns)
							(and
								; If pattern had only blanks, return nil instead of
								; exactly matching it, because helm-match-substring would
								; have already return non-nil if that match was found.
								; No matching should be performed on empty pattern,
								; so don't need to return t when pattern is "" here.
								(cdr patterns)
								; Don't perform multi matching when pattern is a regex
								; without "blanks" and there is no negation.
								; This would be a waste because previous step
								; is regex matching, so this would be exactly the same.
								(string-match-p
									split-string-default-separators helm-pattern)))
						; Translate.
						(if (eq helm-match-regex-type 'pcre)
							(ignore-error invalid-regexp
								(map_modify_list #'rxt-pcre-to-elisp (car patterns))
								(map_modify_list #'rxt-pcre-to-elisp (cdr patterns))
								t)
							t)
						; Check if every subpattern is a valid regex.
						(cl-loop
							for regex in (car patterns)
							always (helm-match-is-valid-regex regex))
						(cl-loop
							for regex in (cdr patterns)
							always (helm-match-is-valid-regex regex))
						(progn
							(when helm-match-fold-diacritics
								(map_modify_list #'char-fold-to-regexp (car patterns))
								(map_modify_list #'char-fold-to-regexp (cdr patterns)))
							; Also cache functions.
							(if helm-match-mixed
								(setq
									helm-match--multi-match-fn
										#'helm-match-mixed-match
									helm-match--multi-match-data-fn
										#'helm-match-mixed-match-data)
								(setq
									helm-match--multi-match-fn
										#'helm-match-fixed-match
									helm-match--multi-match-data-fn
										#'helm-match-fixed-match-data))
							patterns))))
			(lambda (candidate) (funcall helm-match--multi-match-fn candidate))
			(lambda (candidate) (funcall helm-match--multi-match-data-fn candidate))))
"Match functions for helm matching (helm-source-match slot).")

; Multi match single - just multi regex match and negation.

(defconst helm-match-multi-single
	(list
		(lambda ()
			; No matching should be performed on empty pattern,
			; so don't handle that case here.
			(catch 'return
				(let ((patterns (helm-match-get-patterns helm-pattern)))
					(and
						(or
							(car patterns)
							(nthcdr 2 patterns)
							(and
								(cdr patterns)
								; We have only 1 regex to match, so use normal regex
								; matching on it.
								(let ((helm-pattern (car (cdr patterns))))
									; Select regex matching functions.
									(setq helm-match--multi-match-fn
										(nth 1 helm-match-regex-single))
									(setq helm-match--multi-match-data-fn
										(nth 2 helm-match-regex-single))
									(throw 'return
										(funcall (car helm-match-regex-single))))))
						; Proceed with multi matching.
						(or
							(not (eq helm-match-regex-type 'pcre))
							; Translate.
							(ignore-error invalid-regexp
								(map_modify_list #'rxt-pcre-to-elisp (car patterns))
								(map_modify_list #'rxt-pcre-to-elisp (cdr patterns))
								t))
						; Check if every subpattern is a valid regex.
						(cl-loop
							for regex in (car patterns)
							always (helm-match-is-valid-regex regex))
						(cl-loop
							for regex in (cdr patterns)
							always (helm-match-is-valid-regex regex))
						(progn
							(when helm-match-fold-diacritics
								(map_modify_list #'char-fold-to-regexp (car patterns))
								(map_modify_list #'char-fold-to-regexp (cdr patterns)))
							; Also cache functions.
							(if helm-match-mixed
								(setq
									helm-match--multi-match-fn
										#'helm-match-mixed-match
									helm-match--multi-match-data-fn
										#'helm-match-mixed-match-data)
								(setq
									helm-match--multi-match-fn
										#'helm-match-fixed-match
									helm-match--multi-match-data-fn
										#'helm-match-fixed-match-data))
							patterns)))))
		(lambda (candidate) (funcall helm-match--multi-match-fn candidate))
		(lambda (candidate) (funcall helm-match--multi-match-data-fn candidate))))

(defconst helm-match-multi-single-cell (list "multi" helm-match-multi-single))

; Match alist - user chooses from these what matching method to use.

(defvar helm-match-alist
	(list
		(cons nil helm-match-multi)
		helm-match-substring-cell
		helm-match-regex-cell
		helm-match-multi-single-cell)
"Alist of additional helm matching objects for user to toggle between.
car - description or nil (no message).
cdr - list of (prepare_fn match_fn match_data_fn).")

; Some useful sorting predicates.

(defun helm-sort-length (str_1 str_2) "Sort predicate - shorter string wins."
	(< (length str_1) (length str_2)))

(defun helm-sort-alpha (str_1 str_2)
"Sort predicate like `string<', but downcase strings so case won't matter.
It's mainly to not split candidates into two groups, where first start
with uppercase letters.
Often used as the initial sorting for candidates."
	(string< (downcase str_1) (downcase str_2)))

(defun helm-sort-alpha-generic (cand_1 cand_2)
"Like `helm-sort-alpha' but handle candidates in (display . real) form."
	(helm-sort-alpha (helm-get-display cand_1) (helm-get-display cand_2)))

(let ((fn (merge_sort_predicates #'helm-sort-length #'helm-sort-alpha)))
	(add_fn_doc fn
"Sort predicate, where the more important one is `helm-sort-length'
and the other is `helm-sort-alpha'.
It's equivalent to (sort (sort list #'helm-sort-alpha) #'helm-sort-length),
so don't use it if candidates are already sorted by helm-sort-alpha.")
	(fset (intern "helm-sort-length-alpha") fn))

; Translating elisp from/to pcre

(defun helm-match-pattern-to-pcre (pattern)
"Also fold diacritics if `helm-match-fold-diacritics' is non-nil.
Should be wrapped in ignore-error invalid-regexp."
	(cond
		(helm-match-fold-diacritics
			(rxt-elisp-to-pcre
				(char-fold-to-regexp
					(if (eq helm-match-regex-type 'pcre)
						(rxt-pcre-to-elisp pattern)
						pattern))))
		((eq helm-match-regex-type 'pcre) pattern)
		(t (rxt-elisp-to-pcre pattern))))

(defun helm-match-patterns-to-multi (patterns)
"Very specific, for translating cdr of helm-match-get-patterns to pcre."
	(ignore-error invalid-regexp
		(if helm-match-mixed
			(helm-match-pattern-to-pcre (car patterns))
			(mapconcat #'helm-match-pattern-to-pcre patterns ".*"))))

(provide 'helm-match)
