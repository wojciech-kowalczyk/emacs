; -*- lexical-binding:nil -*-

; Iterators

(defun helm-iter-list (list_ &optional cycle)
"Return an iterator object from list_.
The iterator die and return nil when it reach end of list_.
When CYCLE is specified the iterator never ends."
	(let ((list_i (make-symbol "list_i")))
		(set list_i list_)
		`(lambda ()
			(let ((el (car ,list_i)))
				(setq ,list_i ,(if cycle `(or (cdr ,list_i) ',list_) `(cdr ,list_i)))
				el))))

(defun helm-iter-circular (list_) "Infinite iteration on list_." (helm-iter-list list_ t))

(defun helm-iter-next (iterator) "Return next element of ITERATOR."
	(and iterator (funcall iterator)))

; Multiline transformer

(defun helm--multiline-get-truncated-candidate (candidate offset)
"Truncate CANDIDATE when its length is > than OFFSET."
	(with-temp-buffer
		(insert candidate)
		(goto-char (point-min))
		(if (and offset (> (buffer-size) offset))
			(let ((end-str "[...]"))
				(concat
					(buffer-substring
						(point)
						(save-excursion
							(forward-char offset)
							(setq
								end-str (if (looking-at "\n")
								end-str (concat "\n" end-str)))
							(point)))
					end-str))
			(buffer-string))))

; List processing

(defun helm-fast-remove-dups (list_)
"Remove duplicates elements in list.

This is same as `remove-duplicates' but with memoisation.
It is much faster, especially in large lists.
NOTE: Comparison of special Elisp objects (e.g., markers etc.)
fails because their printed representations which are stored in
hash-table can't be compared with with the real object in SEQ.
This is a bug in `puthash' which store the printable
representation of object instead of storing the object itself,
this to provide at the end a printable representation of hashtable itself."
	(cl-loop
		with cont = (make-hash-table :test 'equal)
		for elm in list_
		unless (gethash elm cont)
			collect (puthash elm elm cont)))

(defun helm-remove-if-match (regex list_)
"Remove all elements of list_ that match regex."
	(cl-loop
		for str in list_
		unless (string-match-p regex str)
			collect str))

(defun helm-append-at-nth (seq elm index)
"Append ELM (seq) at INDEX (int) in SEQ (list).
When INDEX is > to the SEQ length ELM is added at end of SEQ.
When INDEX is 0 or negative, ELM is added at beginning of SEQ.

Examples:

    (helm-append-at-nth \\='(a b c d) \\='(z) 2)
    =>(a b z c d)

    (helm-append-at-nth \\='(a b c d) \\='((x . 1) (y . 2)) 2)
    =>(a b (x . 1) (y . 2) c d)

    But this is not working:
    (helm-append-at-nth \\='(a b c d) \\='(x . 1) 2)
    =>Wrong type argument: listp, 1"
	(setq index (min (max 0 index) (length seq)))
	(if (= 0 index)
		(append elm seq)
		(let ((end-part (nthcdr index seq)))
			(append (butlast seq (length end-part)) elm end-part))))

; Strings processing.

(defun helm-symbol-name (obj)
	(if
		(or
			(and (consp obj) (functionp obj))
			(byte-code-function-p obj)
			(subr-native-elisp-p obj))
		"Anonymous"
		(symbol-name obj)))

(defun helm-stringify (string_or_number_or_symbol)
"Return the representation of string_or_number_or_symbol as a string."
	(cond
		((stringp string_or_number_or_symbol)
			string_or_number_or_symbol)
		((numberp string_or_number_or_symbol)
			(number-to-string string_or_number_or_symbol))
		((symbolp string_or_number_or_symbol)
			(symbol-name string_or_number_or_symbol))))

(defun helm-url-unhex-string (str)
"Same as `url-unhex-string' but ensure STR is completely decoded."
	(unless str (setq str ""))
	(with-temp-buffer
		(insert str)
		(while
			(progn
				; Restart from beginning until string is completely decoded.
				(goto-char 1)
				(re-search-forward "%[A-Za-z0-9]\\{2\\}" nil t))
			(replace-match
				(byte-to-string
					(string-to-number (substring (match-string 0) 1) 16))
				t t))
		(decode-coding-string (buffer-string) 'utf-8)))

(defun helm-read-answer (prompt answer_str)
"Prompt user for an answer.
PROMPT is the prompt to present user the different possible answers,
ANSWER-LIST is string, where every char is a possible answer.
If user enters an answer which is one of ANSWER-LIST, return it,
otherwise keep prompting for a valid answer.

Example:
(cl-case
	(helm-read-answer \"Funny prompt? [y,n,!,q]\" \"yn!q\")
	(?y \"yes\")
	(?n \"no\")
	(?! \"all\")
	(t \"quit\"))"
	(setq prompt (propertize prompt 'face 'minibuffer-prompt))
	(let (answer)
		(while
			(unless
				(and
					(characterp (setq answer (read-key prompt)))
					(find_in_vector_= answer_str answer))
				(message "Wrong answer, valid are [%s]."
					(mapconcat #'char-to-string answer_str ","))
				(sit-for 1)
				t))
		answer))

; Files routines

(defun helm-file-name-extension (file)
"Returns FILE extension if it is not a number."
	(when-let (
		(file_extension (file-name-extension file))
		; See string-to-number doc on why this is necessary.
		((not (cl-loop for c across file_extension always (= c ?0))))
		((= 0 (string-to-number file_extension)))
	)
		file_extension))

(defun helm-basename (fname &optional ext)
"Print FNAME with any leading directory components removed.
If specified, also remove filename extension EXT.
If FNAME is a directory EXT arg is ignored.

Arg EXT can be specified as a string, a number or `t' .
When specified as a string, this string is stripped from end of FNAME.
e.g. (helm-basename \"tutorial.el.gz\" \".el.gz\") => tutorial.
When `t' no checking of `file-name-extension' is done and the first
extension is removed unconditionally with `file-name-sans-extension'.
e.g. (helm-basename \"tutorial.el.gz\" t) => tutorial.el.
When a number, remove that many times extensions from FNAME until FNAME ends
with its real extension which is by default \".el\".
e.g. (helm-basename \"tutorial.el.gz\" 2) => tutorial
To specify the extension where to stop use a cons cell where the cdr is a regexp
matching extension e.g. (2 . \\\\.py$).
e.g. (helm-basename \"~/ucs-utils-6.0-delta.py.gz\" \\='(2 . \"\\\\.py\\\\\\='\"))
=>ucs-utils-6.0-delta."
	(let (
		(non-essential t)
		(ext-regexp
			(cond
				((consp ext) (cdr ext))
				((numberp ext) "\\.el\\'")
				(t ext)))
		result
	)
		(cond
			((or (null ext) (file-directory-p fname))
				(file-name-nondirectory (directory-file-name fname)))
			((or (numberp ext) (consp ext))
				(cl-dotimes (_ (if (consp ext) (car ext) ext))
					(let ((bn (file-name-nondirectory (or result fname))))
						(when (setq result (file-name-sans-extension bn))
							(when (string-match-p ext-regexp bn) (cl-return)))))
				result)
			((eq ext t)
				(file-name-sans-extension (file-name-nondirectory fname)))
			((stringp ext)
				(replace-regexp-in-string
					(concat (regexp-quote ext) "\\'")
					""
					(file-name-nondirectory fname))))))

(defun helm-w32-prepare-filename (file)
"Convert filename FILE to something usable by external w32 executables."
	(string-replace "/" "\\"
		(replace-regexp-in-string ; Strip cygdrive paths
			"/cygdrive/\\(.\\)" "\\1:" file)))

; Same as `vc-directory-exclusion-list'.
(defvar helm-walk-ignore-directories
	'(
		"SCCS/" "RCS/" "CVS/" "MCVS/" ".svn/" ".git/" ".hg/" ".bzr/"
		"_MTN/" "_darcs/" "{arch}/" ".gvfs/"
	))

(cl-defun helm-walk-directory
	(directory &key (path 'basename) directories match skip-subdirs noerror)
"Walk through DIRECTORY tree.

Argument PATH can be one of basename, relative, full, or a
function called on file name, default to basename.

Argument DIRECTORIES when t return also directories names,
otherwise skip directories names, with a value of `only' returns
only subdirectories, i.e. files are skipped.

Argument MATCH is a regexp matching files or directories.

Argument SKIP-SUBDIRS when t will skip
`helm-walk-ignore-directories', otherwise if it is given as a
list of directories, this list will be used instead of
`helm-walk-ignore-directories'.

Argument NOERROR when t will skip directories which are not accessible."
	(let (
		(fn
			(cl-case path
				(basename #'file-name-nondirectory)
				(relative #'file-relative-name)
				(full #'identity)
				(t path))) ; A function.
	)
		(setq skip-subdirs
			(if (listp skip-subdirs)
				skip-subdirs
				helm-walk-ignore-directories))
		(cl-labels (
			(ls-rec (dir)
				(unless (file-symlink-p dir)
					(mapcan
						(lambda (f)
							(unless (member f '("./" "../"))
								; A directory.
								; Expand to avoid infloop on directory symlinks.
								(if (string-suffix-p "/" f)
									(let (
										(filename
											(expand-file-name
												(substring-no-properties f 0 -1) dir))
									)
										(unless
											(or
												(member f skip-subdirs)
												(and
													noerror
													(not (file-accessible-directory-p filename))))
											(if
												(and
													directories
													(or (null match) (string-match-p match f)))
												(cons
													(concat (funcall fn filename) "/")
													(ls-rec filename))
												(ls-rec filename))))
									; A regular file.
									(and
										(not (eq directories 'only))
										(or (null match) (string-match-p match f))
										(list (funcall fn (expand-file-name f dir)))))))
						(sort (file-name-all-completions "" dir) #'string<))))
		)
			(ls-rec directory))))

(provide 'helm-lib)
