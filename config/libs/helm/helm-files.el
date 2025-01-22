; -*- lexical-binding:nil -*-

(unless (eq system-type 'windows-nt) (require 'tramp))
(require 'ffap)
(require 'dired-aux)
(require 'dired-x)
(require 'dired-async)
(require 'filenotify)
(require 'image-mode)
(require 'image-dired)
(require 'find-cmd)

(declare-function w32-shell-execute "ext:w32fns.c" (operation document &optional parameters show-flag))
(declare-function eshell-reset "esh-mode.el")
(declare-function eshell/cd "em-dirs.el")
(declare-function eshell-next-prompt "em-prompt.el")
(declare-function helm-ls-git "ext:helm-ls-git")
(declare-function helm-hg-find-files-in-project "ext:helm-ls-hg")
(declare-function helm-get-default-program-for-file "helm-external")
(declare-function term-line-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-input "term")
(declare-function term-next-prompt "term")
(declare-function term-process-mark "term")
(declare-function bookmark-prop-get "bookmark")
(declare-function comint-next-prompt "comint")
(declare-function comint-delete-input "comint")
(declare-function comint-send-input "comint")
(declare-function comint-goto-process-mark "comint")
(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-get-completion-function "tramp")
(declare-function tramp-make-tramp-file-name "tramp")
(declare-function ffap-fixup-url "ffap")
(declare-function ffap-url-at-point "ffap")
(declare-function ffap-file-at-point "ffap")
(declare-function dired-create-files "dired-aux")
(declare-function dired-goto-file "dired")
(declare-function dired-move-to-filename "dired")
(declare-function dired-move-to-end-of-filename "dired")
(declare-function dired-get-filename "dired")
(declare-function dired-async-processes "ext:dired-async.el")
(declare-function wfnames-setup-buffer "ext:wfnames.el")

(defvar recentf-list)
(defvar dired-async-mode)
(defvar org-directory)
(defvar tramp-archive-enabled)
(defvar wfnames-buffer)

; Internal vars

(defvar helm-ff-default-directory nil)
(defconst helm-ff-url-regexp
	"\\`\\(news\\(post\\)?\\|nntp\\|mailto\\|file\\|ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\):"
"Same as `ffap-url-regexp' but match earlier possible url.")
; helm-ff-tramp-file-name-regexp is based on old version of
; tramp-file-name-regexp i.e. "\\`/\\([^[/:]+\\|[^/]+]\\):" but it seems
; it is wrong and a simpler regexp is enough, let's try it and watch out!
; Old: [^[/:]+\\|[^/]+]
; New: [^/:|]+
(defvar helm-ff-tramp-file-name-regexp "\\`/\\([^/:|]+\\):")
(defvar helm-ff-tramp-method-regexp "[/|]:\\([^:]*\\)")
(defvar helm-ff--tramp-methods
	(sort (mapcar #'car (when (boundp 'tramp-methods) tramp-methods)) #'string<))

; User variables

(desktop::add_to_globals 'file-name-history)

(defvar helm-ff-history nil
"History of default directories for helm-ff.")

(desktop::add_to_globals 'helm-ff-history)

(defconst helm-tramp-verbose 0
"Just like `tramp-verbose' but specific to Helm.
When set to 0 don't show tramp messages in Helm.
If you want to have the default tramp messages set it to 3.")

(defconst helm-ff-newfile-prompt-p t
"Whether prompt or not when creating new file.
This sets `ffap-newfile-prompt'.")

(defconst helm-ff-avfs-directory "~/.avfs"
"The default avfs directory, usually \\='~/.avfs'.
When this is set you will be able to expand archive filenames
with persistent-action inside an avfs directory mounted with mountavfs.
See <http://sourceforge.net/projects/avf/>.")

(defconst helm-ff-file-compressed-list '("gz" "bz2" "zip" "7z")
"Minimal list of compressed files extension.")

(defconst helm-ff-printer-list nil
"A list of available printers (names) on your system.
When non-nil let you choose a printer to print file.
Otherwise when nil the variable `printer-name' will be used.")

(defconst helm-tooltip-hide-delay 60
"Hide tooltips automatically after this many seconds.")

(defvar helm-files-default-skip-boring t
"Non-nil to skip boring files.
I.e. the files matching regexes in `helm-files-boring-regex'.
Toggle this with `helm-files-skip-boring-toggle' and `helm-ff-skip-boring-toggle'.")

(defconst helm-ff-skip-git-ignored-files nil
"Non-nil to skip git ignored files.
This take effect only in `helm-ff'.
Check is not done on remote files.
Note that when non-nil this will slow down slightly `helm-ff'.
Git exe must be available, otherwise helm-ff will error.")

(defconst helm-ff-candidate-number-limit 5000
"The `helm-candidate-number-limit' for `helm-ff' and friends.
Note that when going one level up with 'helm-ff-up-one-level' the
length of directory will be used instead if it is higher than this value.
This is to avoid failing to preselect the previous directory/file
if this one is situated lower than `helm-ff-candidate-number-limit'
num candidate.")

(defconst helm-ff-bookmark-prefix "helm-ff "
"bookmark name prefix of `helm-ff' sessions.")

(defconst helm-ff-guess-ffap-filenames nil
"Use ffap to guess local filenames at point in `helm-ff'.
This doesn't disable url or mail at point, see
`helm-ff-guess-ffap-urls' for this.")

(defconst helm-ff-guess-ffap-urls t
"Use ffap to guess local urls at point in `helm-ff'.
This doesn't disable guessing filenames at point, see
`helm-ff-guess-ffap-filenames' for this.
See also `ffap-url-unwrap-remote' that may override this variable.")

(defvar helm-ff-no-preselect nil)

(defconst helm-ff-allow-non-existing-file-at-point nil
"Use non existing file-at-point as initial input in `helm-ff'.
Default is nil.")

(defconst helm-ff-ignore-thing-at-point nil
"Use only `default-directory' as default input in `helm-ff'.
I.e. text under cursor in `current-buffer' is ignored.")

(defconst helm-mounted-network-directories nil
"A list of directories used for mounting remotes filesystem.

When nil `helm-file-on-mounted-network-p' always return nil
otherwise check if a file is in one of these directories.

Remote filesystem are generally mounted with sshfs.")

(defconst helm-modes-using-escaped-strings '(shell-mode eshell-mode term-mode)
"Modes that requires string's insertion to be escaped.")

(defconst helm-ff-allow-recursive-deletes nil
"When non-nil, don't prompt for recursive deletion of directories.
When nil, will ask for recursive deletion.
Note that when deleting multiple directories you can answer !
when prompted to avoid being asked for next directories, so it
is probably better to not modify this variable.")

(defconst helm-ff-delete-files-function #'helm-delete-marked-files
"The function to use by default to delete files.

Default is to delete files synchronously (helm-delete-marked-files),
other choice is to delete files asynchronously (helm-delete-marked-files-async).

BE AWARE that when deleting async you will not be warned about
recursive deletion of directories, IOW non-empty directories will
be deleted with no warnings in background!

It is the function that will be used when using `helm-ff'.")

(defconst helm-ff-default-sort-method nil
"Sort method to use when initially listing a directory.

Can be:
nil - alphabetically (a to z)
newest - newest to oldest
size - largest to smallest
ext - alphabetically (a to z) but for extensions.

When searching in a directory, sorting based on match equality is used,
so this sorting will be secondary.
E.g. helm-pattern = foo/bar/ -> helm-ff-default-sort-method;
helm-pattern = foo/bar/file ->
match based sorting mostly overriding the helm-ff-default-sort-method.

This is the default and can be changed in helm session with:
helm-ff-sort-by-size
helm-ff-sort-by-newest
helm-ff-sort-by-ext
helm-ff-sort-alpha
and helm-ff-sort-reverse-toggle for normal/reversed sorting.")

(defconst helm-ff-default-sort-reverse nil
"Non-nil to reverse sorting.
This is the default setting, use helm-ff-sort-reverse-toggle to change it
in helm session.")

(defconst helm-ff-default-sort-dirs-first t
"Non-nil to show directories first (last when reversed sorting)
when initially listing directory.")

(defconst helm-ff-image-rotate-program "exiftran"
"External program used to rotate images (string).
When nil, fallback to `image-rotate' without modification
of exif data i.e. rotation is not persistent.

Options: exiftran, mogrify, jpegtran.")

(defconst helm-ff-image-rotate-switch '("-i")
"List of strings representing options used with `helm-ff-image-rotate-program'.
If you are using Mogrify or Jpegtran mandatory option is \"-rotate\",
with Exiftran mandatory option is \"-i\".")

(defconst helm-ff-preferred-shell-mode 'shell-mode
"Shell to use to switch to a shell buffer from `helm-ff'.
Possible values are `shell-mode', `eshell-mode' and `term-mode'.
This affects 'helm-ff-switch-to-shell'.")

(defconst helm-rsync-options '("-a" "-z" "-h" "-s" "--info=all2")
"Rsync options to use with HFF Rsync action.
Note: Using \"--info=all2\" allows having the name of the file
currently transfered in an help-echo in mode-line, if you use
\"--info=progress2\" you will not have this information.")

(defconst helm-ff-rsync-progress-bar-style 'bar
"Style of progress-bar for rsync action.
Value can be either 'bar or 'text.")

(defconst helm-ff-rsync-progress-bar-info '(percent size speed remain)
"Infos shown at end of Rsync progress bar.

Valid value is a list containing one or more elements:
'percent, 'size, 'speed, 'remain.
When set to nil show nothing at end of progress bar.
This has no effect when `helm-ff-rsync-progress-bar-style' is text.

'size - Show the amount of data copied
'percent - Show the percentage of data copied
'speed - Show the current speed of transfer
'remain - Show the time remaining")

(defconst helm-trash-default-directory nil
"The default trash directory.
You probably don't need to set this when using a Linux system using
standard settings.
Should be the directory file name i.e. don't add final slash.
When nil helm will compute a default value according to freedesktop specs.
It is generally \"~/.local/share/Trash\".

Don't have any effect on systems that have function 'system-move-file-to-trash'.")

(defconst helm-dwim-target nil
"Default target directory for file actions.

Define the directory where you want to start navigating for the
target directory when copying, renaming, etc.
You can use the `default-directory' of `next-window', the visited directory,
the current `default-directory' or have completion on all the directories
belonging to each visible windows.

Options:
nil - Use visited directory (default)
'next-window - Directory belonging to next window
'completion - Completion on directories belonging to each window
'default-directory - Use initial directory or `default-directory'.")

; FIXME This is now broken on Windows - moving to trash a directory that has a
; directory (or maybe any file, idk) with watcher, signals error and hangs emacs forever.
; This may be a bug with file-notify package because it seems that file is never
; translated from c:/... format to C:\... and is passed to system function,
; that later causes error "Invalid argument" "c:/...".
; Though "simpler" actions like deleting or creating file in watched
; directory works fine even with this "wrong" format, so I'm maybe that's not it,
; but some other bug in file-notify package.
(defconst helm-ff-use-notify nil
"Watch directories visited with `helm-ff' when non-nil.
If your system have no file notification package available turn this
to nil to avoid error messages when using `helm-ff'.")

(defconst helm-ff-inotify-unsupported-methods '("adb")
"Tramp methods unsupported by file-notify.
List of strings.")

(defconst helm-ff-image-cache-max-len 5
"The last seen image number to keep in cache.")

(defconst helm-ff-slideshow-delay 3
"Delay in seconds between each image in slideshow.")

(defconst helm-ff-follow-blacklist-file-exts '("gpg" "doc" "docx" "mp3" "ogg")
"File extensions we don't want to follow when helm-follow-mode is enabled.
Note that image files are always followed even if their extensions is
present in this list.")

(defconst helm-ff-edit-marked-files-fn #'helm-ff-marked-files-in-dired
"A function to edit filenames in a special buffer.

By default `wfnames' package is used to avoid wdired which
doesn't always work with all emacs versions and also is quite
clumsy about default-directory among other things.
If you still want to use it, helm is still providing
`helm-ff-marked-files-in-dired'.

Options:
helm-ff-wfnames - Use Wfnames package to edit filenames
helm-ff-marked-files-in-dired - Use Wdired package to edit filenames.")

(defconst helm-ff-ignore-following-on-directory nil
"In follow mode ignore silently directories when non-nil.")

(defconst helm-ff-tramp-fancy nil
"Colorize remote files when non-nil.
Be aware that a non-nil value will make tramp display very slow.")

(defconst helm-files-boring-regex
	(concat
		"\\(?:"
		(mapconcat
			#'identity
			(append
				(mapcar
					(lambda (f) (concat (regexp-quote f) "\\'"))
					completion-ignored-extensions)
				(list
					"\\`\\.#" ; Lock files.
					"\\`#.*#\\'" ; Auto-save files.
					))
			"\\)\\|\\(?:")
		"\\)")
"Regex matching boring files.
Called on basename (plus slash if file is a directory).
Build by default using 'completion-ignored-extensions'.")


(define_face 'helm-ff-directory '((t :foreground "yellow green"))
"Face used for directories in `helm-ff'.")

(define_face 'helm-ff-symlink '((t :foreground "yellow1"))
"Face used for symlinks in `helm-ff'.")

(define_face 'helm-ff-directory-symlink '((t :foreground "yellow3"))
"Face used for symlinks in `helm-ff'.")

(define_face 'helm-ff-invalid-symlink '((t :foreground "red"))
"Face used for invalid symlinks in `helm-ff'.")

(define_face 'helm-ff-file '((t :inherit font-lock-builtin-face))
"Face used for \"normal\" existing file names in `helm-ff'.
These are not: exe, suid, socket, pipe, backup, auto save, lock.")

(define_face 'helm-ff-exe '((t :foreground "green"))
"Face used for executable files in `helm-ff'.")

(define_face 'helm-ff-suid '((t :foreground "white"))
"Face used for suid files in `helm-ff'.")

(define_face 'helm-ff-socket '((t :foreground "DeepPink"))
"Face used for socket files in `helm-ff'.")

(define_face 'helm-ff-pipe '((t :foreground "yellow" :background "black"))
"Face used for named pipes and character device files in `helm-ff'.")

(define_face 'helm-ff-backup-file '((t :inherit shadow))
"Face used for backup files in `helm-ff'.")

(define_face 'helm-ff-auto-save-file '((t :inherit helm-ff-backup-file))
"Face used for auto save files in `helm-ff'.")

(define_face 'helm-ff-lock-file '((t :inherit helm-ff-backup-file))
"Face used for lock files in `helm-ff'.
These are prepended with \".#\".")

(define_face 'helm-ff-url '((t))
"Face used for urls in `helm-ff'.")

(define_face 'helm-ff-tramp-method '((t))
"Face used for tramp methods in `helm-ff'.")


(define_face 'helm-ff-nofile '((t :inherit italic))
"Face used in combination with other face(s), for files that don't exist
(or some other things I guess, like incomplete tramp methods or something).
It has higher priority than base face, like helm-ff-file or helm-ff-directory,
but it has lower than non base faces, like helm-ff-extension or helm-ff-basename.")

(define_face 'helm-ff-basename '((t :inherit bold))
"Face used for file (not directories, though it could be changed) basename part
in absolute filenames. It has the highest priority.")

(define_face 'helm-ff-extension '((t :foreground "#d8e4e8"))
"Face used for file extensions in `helm-ff'.")


(define_face 'helm-ff-size '((t :foreground "RosyBrown")))

(define_face 'helm-ff-access-time '((t :foreground "coral")))

(define_face 'helm-ff-modification-time '((t :foreground "orange2")))

(define_face 'helm-ff-creation-time '((t :foreground "DarkOrange3")))


(define_face 'helm-ff-rsync-progress '((t :inherit font-lock-warning-face))
"Face used for rsync mode-line indicator.")

(define_face 'helm-ff-rsync-progress-1 '((t :background "CadetBlue" :foreground "black"))
"Face used for rsync progress bar percentage and proc name.")

(define_face 'helm-ff-rsync-progress-2 '((t :background "orange"))
"Face used for rsync progress bar progress.")

(define_face 'helm-ff-rsync-progress-3 '((t :background "white"))
"Face used for rsync progress bar background.")


(define_face 'helm-delete-async-message '((t :foreground "yellow"))
"Face used for message in mode-line before and after async deleting files.")


; Keymaps

; Make some common exiting commands
; (mainly because they are shared with 'helm-files-keymap').
(mapc
	(lambda (symbol)
		(fset
			(intern (concat (symbol-name symbol) "-command"))
			(helm-make-action-command symbol)))
	(list
		#'helm-ff-grep
		#'helm-ff-zgrep
		#'helm-ff-pdfgrep
		#'helm-ff-rename
		#'helm-ff-copy
		#'helm-ff-symlink
		#'helm-ff-hardlink
		#'helm-ff-delete-files
		#'helm-ff-ediff-files
		#'helm-ff-ediff-merge-files
		#'helm-ff-open-file-externally
		#'helm-ff-open-file-with-default-program
		#'helm-ff-edit-marked-files
		#'helm-ff-insert-as-org-link
		#'helm-ff-properties))

; helm-ff-default-keymap is defined in helm-mode.el.
(set-keymap-parent helm-ff-default-keymap helm-map)
(define-key helm-ff-default-keymap [S-return] #'helm-return)
(define-key helm-ff-default-keymap [A-backspace] #'helm-ff-up-one-level)
(define-key helm-ff-default-keymap [?\C-:] #'helm-ff-complete-tramp-methods)
; Exchange normal minibuffer history with a better version for files.
(define-key helm-ff-default-keymap [?\C-h] #'helm-file-name-history)
(define-key helm-ff-default-keymap [?\C-\S-h] #'helm-ff-history)
(define-key helm-ff-default-keymap [?\A-h] #'helm-minibuffer-history)
(define-key helm-ff-default-keymap [?\C-s ?1] #'helm-ff-sort-alpha)
(define-key helm-ff-default-keymap [?\C-s ?2] #'helm-ff-sort-by-newest)
(define-key helm-ff-default-keymap [?\C-s ?3] #'helm-ff-sort-by-size)
(define-key helm-ff-default-keymap [?\C-s ?4] #'helm-ff-sort-by-ext)
(define-key helm-ff-default-keymap [?\C-s ?r] #'helm-ff-sort-reverse-toggle)
(define-key helm-ff-default-keymap [?\C-s ?d] #'helm-ff-sort-dirs-first-toggle)
(define-key helm-ff-default-keymap [?\C-s ?t] #'helm-ff-thumbnails-toggle)
(define-key helm-ff-default-keymap [?\C-s ?b] #'helm-ff-skip-boring-toggle)
(define-key helm-ff-default-keymap [?\C-s ?s] #'helm-files-substitute-in-file-name)
; No need to bind helm-files-expand-file-name because helm-pattern
; is expanded in every helm-update.
; Well, maybe helm-ff-set-pattern doesn't perform the exact same expansion like
; expand-file-name, so bind it anyway, also this way it's more consistent.
(define-key helm-ff-default-keymap [?\C-s ?e] #'helm-files-expand-file-name)

(defconst helm-files-default-keymap
	(let ((map (make-sparse-keymap)))
		(set-keymap-parent map helm-map)
		(define-key map [A-backspace] #'helm-files-up-one-level)
		(define-key map [?\C-s ?f] #'helm-files-filter-toggle)
		(define-key map [?\C-s ?b] #'helm-files-skip-boring-toggle)
		(define-key map [?\C-s ?s] #'helm-files-substitute-in-file-name)
		(define-key map [?\C-s ?e] #'helm-files-expand-file-name)
		map))

(defconst helm-files-keymap
	(let ((map (make-sparse-keymap)))
		(set-keymap-parent map helm-files-default-keymap)
		; Non exiting.
		(define-key map [?\C-p] #'helm-ff-properties-persistent)
		; Goto next/previous dir.
		; Only useful in a couple of sources, like helm-fd,
		; where files are listed with recursive listing of subdirs, so user can
		; jump to next/prev dir.
		(helm-add-goto-bindings
			map
			(lambda ()
				(or
					(file-name-directory (get-text-property (point) 'helm-real))
					"/")))
		; Exiting.
		(define-key map [?\C-\S-p] #'helm-ff-properties-command)
		(define-key map [f2] #'helm-ff-rename-command)
		(define-key map [?\A-c] #'helm-ff-copy-command)
		(define-key map [?\A-d] #'helm-ff-delete-files-command)
		(define-key map [?\A-s ?s] #'helm-ff-symlink-command)
		(define-key map [?\A-s ?h] #'helm-ff-hardlink-command)
		(define-key map [?\C-e] #'helm-ff-ediff-files-command)
		(define-key map [?\C-\S-e] #'helm-ff-ediff-merge-files-command)
		(define-key map [?\C-o] #'helm-ff-open-file-externally-command)
		(define-key map [?\C-\S-o] #'helm-ff-open-file-with-default-program-command)
		(define-key map [?\C-d] #'helm-ff-edit-marked-files-command)
		(when helm-grep-exe (define-key map [?\C-g] #'helm-ff-grep-command))
		(when helm-zgrep-exe (define-key map [?\A-z] #'helm-ff-zgrep-command))
		(when helm-pdfgrep-exe (define-key map [?\A-p] #'helm-ff-pdfgrep-command))
		map))

; I'm pretty sure some actions (like multi rename) still could be used here,
; even if they are less viable.
(defconst helm-files-actions
	(delq nil
		(list
			(cons "Find file" #'helm-ff-action)
			(cons "Find file other window" #'helm-ff-other-window)
			(cons "Find file other frame" #'helm-ff-other-frame)
			(cons
				"Find file in Dired"
				(lambda (file)
					(setq file
						(directory-file-name
							(helm-files-expand-and-save-to-history file)))
					(dired (file-name-directory file))
					(dired-goto-file file)))
			(unless (eq system-type 'windows-nt)
				(cons "Find file as root" #'helm-ff-as-root))
			(cons "Attach file(s) to mail buffer" #'helm-ff-mail-attach-files)
			(cons "Marked files in Dired (prefix - wdired)" #'helm-ff-edit-marked-files)
			(when helm-grep-exe (cons "Grep file(s)" #'helm-ff-grep))
			(when helm-zgrep-exe (cons "Zgrep file(s)" #'helm-ff-zgrep))
			(when helm-pdfgrep-exe (cons "Pdfgrep file(s)" #'helm-ff-pdfgrep))
			(cons "Insert as org link" #'helm-ff-insert-as-org-link)
			(cons "Checksum file" #'helm-ff-checksum)
			(cons "Ediff" #'helm-ff-ediff-files)
			(cons "Ediff Merge" #'helm-ff-ediff-merge-files)
			(cons "View file" #'helm-ff-view-file)
			(cons "Insert file" #'helm-ff-insert-file)
			(cons "Show file properties" #'helm-ff-properties)
			(cons "Add marked files to file-cache" #'helm-ff-cache-add-file)
			(cons "Delete file(s) (prefix - reverse trash)" #'helm-ff-delete-files)
			(cons "Copy file(s) (prefix - follow to destination)" #'helm-ff-copy)
			(cons "Rename file(s) (prefix - follow to destination)" #'helm-ff-rename)
			(cons "Symlink files(s) (prefix - follow to destination)" #'helm-ff-symlink)
			(cons "Relsymlink file(s) (prefix - follow to destination)" #'helm-ff-relsymlink)
			(cons "Hardlink file(s) (prefix - follow to destination)" #'helm-ff-hardlink)
			(cons "Open file externally (prefix - choose program)" #'helm-ff-open-file-externally)
			(cons "Open file with default program" #'helm-ff-open-file-with-default-program)
			(cons "Find file in hex dump" #'helm-ff-hexl-find-file))))


(defvar helm-file-name-basename-history nil
"History variable for some sources that don't want absolute filenames like
in `file-name-history', but basenames.
Should be updated before every use by `helm-file-name-basename-history-update'.
Used only for reading, every write to it will be discarded by
`helm-file-name-basename-history-update'.")

(defun helm-file-name-basename-history-update ()
	(setq helm-file-name-basename-history (mapcar #'helm-basename file-name-history)))

(defun helm-files-add-extension-face (str)
	(when-let ((ext (helm-file-name-extension str)))
		(add-face-text-property
			(- (length str) (length ext)) (length str)
			'helm-ff-extension nil str))
	nil)

(defun helm-files-add-basename-face (str &optional basename)
	(add-face-text-property
		(- (length str) (length (or basename (file-name-nondirectory str)))) (length str)
		'helm-ff-basename nil str)
	nil)

; Icons

; TODO Find some better, more stylistically matching icons for files and dirs,
; they are now very different.

(defun helm-files-icon-for-dir (basename)
	(if-let (
		(raw_icon
			(cdr
				(assoc
					basename
					all-the-icons-dir-icon-alist
					#'string-match-p)))
	)
		(apply (car raw_icon) (cdr raw_icon))
		(all-the-icons-octicon "file-directory")))

(defun helm-ff-prepend-icon (display icon)
	(propertize_no_copy (concat (all-the-icons-align icon) display)
		'match-part (list (cons 3 (+ (length display) 3)))))

(defun helm-ff-prepend-icon-new-file (display)
	(propertize_no_copy
		(helm-ff-prepend-icon display
			(all-the-icons-material
				(if (string-suffix-p "/" display) "create_new_folder" "note_add")))
		'helm-new t))


(defun helm-files-first-existing-dir (file)
"Return first directory in path 'file' that exists, or nil if such directory
couldn't be found. That can happen for example when path starts with a drive
letter of a drive that doesn't exist on Windows.
'file' should be an absolute expanded path."
	(while
		(let ((new_file (file-name-directory (substring-no-properties file 0 -1))))
			(if (or (not new_file) (>= (length new_file) (length file)))
				(setq file nil)
				(setq file new_file)
				(not (file-exists-p file)))))
	file)

(defun helm-files-lock-file-p (file)
	(let ((basename (file-name-nondirectory file)))
		(and
			(string-prefix-p ".#" basename)
			(file-exists-p
				(concat
					; Basedir.
					(substring-no-properties file 0 (- (length basename)))
					(substring-no-properties basename 2))))))

(defun helm-files-w32-pathname-transformer (filepath_list)
"Change undesirable features of windows pathnames to ones more acceptable
to other candidate transformers."
	; This may be useful, but not for me now.
	; (replace-regexp-in-string "/cygdrive/\\(.\\)" "\\1:" x)
	(mapcar (lambda (x) (string-replace "\\" "/" x)) filepath_list))

(defun helm-files-basename-with-slash (file)
"Like `helm-basename', but if FILE ends with a slash,
return basename with appended slash.
Good for showing basenames while still showing if file is a directory."
	(if (string-suffix-p "/" file)
		(concat (file-name-nondirectory (substring-no-properties file 0 -1)) "/")
		(file-name-nondirectory file)))


; Now 90% of this function is copied from helm-ff-source's action transformer, so there
; are some useless checks, but idk why some actions are only in helm-ff-source,
; like helm-ff-query-replace, seems wrong, so when I'll add them to helm-files-actions,
; these checks will be useful.
(defun helm-files-action-transformer (actions)
	(let ((candidate (helm-get-selection)))
		(cond
			((string-match "^http\\|^ftp" candidate)
				(cons (cons "Browse with browser" #'browse-url) actions))
			((not (file-exists-p candidate))
				(cl-remove-if
					(lambda (cell)
						(memq
							(cdr cell)
							; Not sure if helm-ff-edit-marked-files or helm-ff-rsync
							; should be here.
							'(
								helm-ff-view-file
								helm-ff-insert-file
								helm-ff-query-replace-filenames
								helm-ff-query-replace
								helm-ff-query-replace-regex
								helm-ff-serial-rename
								helm-ff-open-file-with-default-program
								helm-ff-open-file-externally
								helm-ff-mail-attach-files
								helm-ff-hexl-find-file
								helm-ff-in-file
								helm-ff-cache-add-file
								helm-ff-grep
								helm-ff-zgrep
								helm-ff-pdfgrep
								helm-ff-ediff-files
								helm-ff-ediff-merge-files
								helm-ff-delete-files
								helm-ff-touch-files
								helm-ff-copy
								helm-ff-rename
								helm-ff-backup
								helm-ff-multi-copy
								helm-ff-chmod
								helm-ff-print
								helm-ff-locate
								helm-ff-checksum
							)))
					actions))
			((file-directory-p candidate)
				(cl-remove-if
					(lambda (cell)
						(memq
							(cdr cell)
							'(
								helm-ff-insert-file
								helm-ff-in-file
								helm-ff-checksum
							)))
					actions))
			((member (file-name-extension candidate) '("el" "elc"))
				(append
					actions
					(list (cons "Load elisp file" #'load-file))))
			((string-match "\\.html?$" candidate)
				(append
					actions
					(list (cons "Browse url file" #'browse-url-of-file))))
			(t actions))))

; Display format:
; longest_filepath 00.00.0000  00:00  0000.0B  L R ...
; where L is the lock icon, R is the remote icon, ... will be some other
; icons in the future.
(defun helm-files-highlight (files)
"A basic transformer for helm files sources.
files should be a list of filepaths, where dirs end with a slash.
Return list of (display . filepath).
display is a full path with match-part prop, icon and possibly some additional
info like size and modification time.
Source should set match-part to t."
	(let* (
		(candidates (make-list (length files) nil))
		(max_length
			(cl-loop
				with candidates_i = candidates
				for file in files
				for abbrev = (abbreviate-file-name file)
				maximize (length abbrev)
				do
				(setcar candidates_i (cons abbrev file))
				(setq candidates_i (cdr candidates_i))))
	)
		(dolist (cand candidates)
			(let* (
				(display (car cand))
				(file (cdr cand))
				(basename (file-name-nondirectory file))
				(is_remote
					(or (file-remote-p file) (helm-file-on-mounted-network-p file)))
				(attributes
					; Call file-attributes only if:
					; - file is not remote
					; - helm-ff-tramp-fancy is non-nil and file is remote and connected.
					(unless
						(and
							is_remote
							(not
								(and
									helm-ff-tramp-fancy
									(file-remote-p file nil t))))
						(file-attributes file)))
				(is_dir (string-suffix-p "/" file))
				(type (file-attribute-type attributes))
			)
				(setcar cand
					(if (not attributes)
						(if is_remote
							(concat
								(let (icon)
									(if is_dir
										(setq
											display (propertize display 'face 'helm-ff-directory)
											icon
												(helm-files-icon-for-dir
													(file-name-nondirectory
														(substring-no-properties display 0 -1))))
										(setq
											display (propertize display 'face 'helm-ff-file)
											icon
												(all-the-icons-icon-for-file
													(file-name-nondirectory display)))
										(helm-files-add-extension-face display)
										(helm-files-add-basename-face display basename))
									(helm-ff-prepend-icon display icon))
								(get_space_string (1+ (- max_length (length display))))
								(all-the-icons-material "wifi"))
							(let (icon_name)
								(if is_dir
									(setq
										display
											(propertize display
												'face '(helm-ff-nofile helm-ff-directory))
										icon_name "create_new_folder")
									(setq
										display
											(propertize display
												'face '(helm-ff-nofile helm-ff-file))
										icon_name "note_add")
									(helm-files-add-extension-face display)
									(helm-files-add-basename-face display basename))
								(helm-ff-prepend-icon
									display
									(all-the-icons-material icon_name))))
						(cond
							((stringp type)
								(setq display
									(if-let ((truename (helm-ff-valid-symlink-p file)))
										(let (
											(truename_to_display
												(copy-sequence (abbreviate-file-name truename)))
											icon face
										)
											(if is_dir
												(progn
													(setq face 'helm-ff-directory-symlink)
													(propertize_no_copy truename_to_display
														'face 'helm-ff-directory)
													(setq icon
														(all-the-icons-octicon
															"file-symlink-directory")))
												(setq face 'helm-ff-symlink)
												(propertize_no_copy truename_to_display
													'face 'helm-ff-file)
												(helm-files-add-extension-face truename_to_display)
												(helm-files-add-basename-face truename_to_display)
												(setq icon
													(all-the-icons-icon-for-file
														(file-name-nondirectory truename_to_display))))
											(let (
												(str
													; Don't show any properties because
													; 2 full filepaths will be shown -
													; they are too long.
													(concat
														(all-the-icons-align icon)
														(propertize display 'face face)
														" -> "
														truename_to_display))
											)
												(propertize_no_copy str
													'match-part
														(list
															(cons 3 (+ (length display) 3))
															(cons
																(-
																	(length str)
																	(length truename_to_display)
																	; Add one space.
																	1)
																(length str))))))
										(setq display
											(propertize display 'face 'helm-ff-invalid-symlink))
										(helm-ff-prepend-icon
											display (all-the-icons-material "error")))))
							(type
								(setq display
									(concat
										(helm-ff-prepend-icon
											(propertize display 'face 'helm-ff-directory)
											(helm-files-icon-for-dir
												(file-name-nondirectory
													(substring-no-properties display 0 -1))))
										(get_space_string (1+ (- max_length (length display))))
										(propertize_no_copy
											(format-time-string "%d.%m.%Y  %H:%M"
												(file-attribute-modification-time
													attributes))
											'face 'helm-ff-modification-time))))
							(t
								; Choose face and decide if extension should be coloured.
								(let (face color_extension)
									(cond
										((backup-file-name-p basename)
											(setq face 'helm-ff-backup-file))
										((auto-save-file-name-p basename)
											(setq face 'helm-ff-auto-save-file))
										((helm-files-lock-file-p file)
											(setq face 'helm-ff-lock-file)
											(setq color_extension t))
										(t
											(setq face
												(let ((modes (file-attribute-modes attributes)))
													(cond
														; A character device file.
														((find_in_vector_= "cp" (aref modes 0))
															'helm-ff-pipe)
														; A socket file.
														((= ?s (aref modes 0)) 'helm-ff-socket)
														; An executable file.
														((= ?x (aref modes 3)) 'helm-ff-exe)
														; An executable file with suid.
														((= ?s (aref modes 3)) 'helm-ff-suid)
														; A file.
														(t 'helm-ff-file))))
											(setq color_extension t)))
									(setq display (propertize display 'face face))
									(when color_extension (helm-files-add-extension-face display)))
								(helm-files-add-basename-face display basename)
								; Add attributes (modification time and size).
								(setq display
									(let (
										(size
											(helm-file-human-size
												(file-attribute-size attributes)))
									)
										(concat
											(helm-ff-prepend-icon
												display
												(all-the-icons-icon-for-file basename))
											(get_space_string
												(1+ (- max_length (length display))))
											(propertize_no_copy
												(format-time-string "%d.%m.%Y  %H:%M"
													(file-attribute-modification-time
														attributes))
												'face 'helm-ff-modification-time)
											(get_space_string (- 9 (length size)))
											size)))))
						(let (
							; Unless we show a symlinked path, which may be very long,
							; we should be at this column now -
							; longest_filepath 00.00.0000  00:00  0000.0B
							; |  max_length  ||           27            |
							(column (+ max_length 27))
						)
							; Add lock icon if file is locked.
							(when (file-locked-p file)
								(setq display
									(concat
										display
										; Add 2 spaces.
										(get_space_string
											(+ (max 0 (- column (length display))) 2))
										(all-the-icons-material "lock_outline"))))
							(+= column 3) ; 1 icon and 2 spaces.
							; Add remote icon if file is remote.
							(when is_remote
								(setq display
									(concat
										display
										; Add 1 space.
										(get_space_string
											(1+ (max 0 (- column (length display)))))
										(all-the-icons-material "wifi")))))
						display))))
		candidates))


(defun helm-files-substitute-in-file-name ()
"Set helm-pattern to (substitute-in-file-name helm-pattern)."
	(interactive)
	(let ((substituted (substitute-in-file-name helm-pattern)))
		(and
			substituted
			(not (string= substituted helm-pattern))
			(helm-set-pattern substituted))))

(defun helm-files-expand-file-name ()
"Set helm-pattern to (expand-file-name helm-pattern)."
	(interactive)
	(let ((expanded (expand-file-name helm-pattern)))
		(and
			expanded
			(not (string= expanded helm-pattern))
			(helm-set-pattern expanded))))

(defun helm-files-append-slash-to-dir (file)
"If file is `file-directory-p', return it with appended slash, else just return it.
In most cases shouln't be used, because it's slow, especially on remote files."
	(if (file-directory-p file) (concat file "/") file))

(defun helm-files-append-slash-to-dirs (files)
"List modifying transformer using `helm-files-append-slash-to-dir'."
	(map_modify_list #'helm-files-append-slash-to-dir files))

(defun helm-files-directory-files (directory &optional full match nosort count)
"Like 'directory-files' but directories end with a slash.
Unnecessarily slow, to be rewritten in C with the same performance
as 'directory-files'."
	(helm-files-append-slash-to-dirs
		(directory-files directory full match nosort count)))

(defun helm-current-directory ()
"Return current-directory name at point.
Useful in dired buffers when there are inserted subdirs."
	(if (eq major-mode 'dired-mode)
		(expand-file-name (dired-current-directory))
		default-directory))

(defun helm-ff-regex-for-preselection (file)
"Return regex that should preselect candidate 'file'.
'file' can be a basename or a full path, can end with a slash or not.
These things matter but differ depending on helm source, so caller must
take care of them.
Actually final slash doesn't matter very much, so if uncertain, remove it
(helm-basename does that)."
	(concat "^\\( . \\)?" (regexp-quote file)))

; ===================================== Base sources ====================================

; Base source

(cl-defstruct
	(helm-source-files-base
		(:copier nil)
		(:constructor helm-source-files-base--make)
		(:include helm-source-sync))
	(skip-boring helm-files-default-skip-boring)
	mode-line-skip-boring)

(defun helm-files-mode-line-skip-boring-set (source)
	(setf (helm-source-files-base-mode-line-skip-boring source)
		(concat
			(if (helm-source-files-base-skip-boring source) "skip" "show")
			" boring")))

(helm-source-define-constructor "helm-source-files-base" "helm-source-sync" nil
	(lambda () (helm-files-mode-line-skip-boring-set source)))

; Normal source for files

(defun helm-files-confirm (cand _action) "Confirm nonexistent files."
	(memq
		'helm-ff-nofile
		(let ((prop (get-text-property 3 'face (car cand))))
			(if (listp prop) prop (list prop)))))

(cl-defstruct
	(helm-source-files
		(:copier nil)
		(:constructor helm-source-files--make)
		(:include helm-source-files-base
			(match-part t)
			(action helm-files-actions)
			(action-transformer #'helm-files-action-transformer)
			(persistent-action #'helm-files-persistent-action)
			(keymap helm-files-keymap)
			(mode-line #'helm-files-mode-line)
			(confirm #'helm-files-confirm)))
	filter ; nil, 'file or 'dir
	mode-line-filter
	; Contains candidates after initial transforming (like "\" to "/")
	; and highlighting (helm-files-highlight), but before
	; boring files discarding and file/dir filtering.
	cache)

(defun helm-files-mode-line-filter-set (source)
	(setf (helm-source-files-mode-line-filter source)
		(cl-case (helm-source-files-filter source)
			(file "files only, ")
			(dir "dirs only, "))))

(helm-source-define-constructor "helm-source-files" "helm-source-files-base"
"Amongst ARGS there can be :w32 - if non-nil and system-type = windows-nt,
as the first step of candidate transformer,
pass candidates to `helm-files-w32-pathname-transformer'."
	(lambda ()
		(helm-files-mode-line-filter-set source)
		(setf (helm-source-candidates source)
			`(lambda ()
				(,(if (and (eq system-type 'windows-nt) (plist-get args :w32))
					#'helm-files-candidate-transformer-w32
					#'helm-files-candidate-transformer)
					(,(helm-source-candidates source)))))))

(defun helm-files-mode-line ()
	(concat
		(helm-source-files-mode-line-filter helm-current-source)
		(helm-source-files-base-mode-line-skip-boring helm-current-source)))

(defun helm-files-candidate-transformer (candidates)
	(helm-files-skip-boring
		(helm-files-filter
			(setf (helm-source-files-cache helm-current-source)
				(helm-files-highlight candidates))
			helm-current-source)
		helm-current-source))

(defun helm-files-candidate-transformer-w32 (candidates)
	(helm-files-candidate-transformer (helm-files-w32-pathname-transformer candidates)))

; Some skipping/filter functions.

(defun helm-files-toggle-base (update_fn &optional source)
	(unless source
		(setq source
			(or
				(helm-get-current-source)
				(buffer-local-value 'helm-default-source (get-buffer helm-buffer)))))
	(funcall update_fn source)
	(helm-force-update)
	nil)

(defun helm-files-skip-boring-toggle-base (update_cache_fn)
	(helm-files-toggle-base
		`(lambda (source)
			(setf (helm-source-files-base-skip-boring source)
				(not (helm-source-files-base-skip-boring source)))
			(helm-files-mode-line-skip-boring-set source)
			(,update_cache_fn source))))

(defun helm-files-skip-boring (candidates source)
	(if (helm-source-files-base-skip-boring source)
		(cl-remove-if
			(lambda (cand)
				(string-match-p
					helm-files-boring-regex
					(helm-files-basename-with-slash (cdr cand))))
			candidates)
		candidates))

(defun helm-files-skip-boring-toggle () (interactive)
	(helm-files-skip-boring-toggle-base
		(lambda (source)
			(puthash
				source
				; Skip or include boring files.
				(helm-files-skip-boring (helm-source-files-cache source) source)
				helm-candidate-cache))))

(defun helm-ff-skip-boring-toggle () (interactive)
	(helm-files-skip-boring-toggle-base
		(lambda (_source)
			; Force recomputation by clearing cache with candidates.
			(clrhash helm-ff--list-directory-cache))))


(defun helm-files-filter (candidates source)
	(if (helm-source-files-filter source)
		(cl-remove-if
			(if (eq (helm-source-files-filter source) 'file)
				(lambda (cand) (string-suffix-p "/" (cdr cand)))
				(lambda (cand) (not (string-suffix-p "/" (cdr cand)))))
			candidates)
		candidates))

(defun helm-files-filter-toggle ()
"Toggle filtering between none, files only and dirs only."
	(interactive)
	(helm-files-toggle-base
		(lambda (source)
			(setf (helm-source-files-filter source)
				(cl-case (helm-source-files-filter source)
					(file 'dir)
					(dir nil)
					(t 'file)))
			(helm-files-mode-line-filter-set source)
			(puthash
				source
				(helm-files-filter (helm-source-files-cache source) source)
				helm-candidate-cache))))

; ================================== Small sync sources ==================================

; Recentf

(defconst helm-recentf-source
	(helm-source-files-make nil
		:name "Recentf"
		:candidates (lambda () recentf-list)
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-files-keymap)
				(helm-add-delete-binds
					map
					(lambda (cand cache)
						(setq recentf-list (delete cand recentf-list))
						(delete cand cache))
					t)
				map))
"Set `recentf-max-saved-items' to a bigger value if default is too small.")

(defun helm-recentf () (interactive)
	(helm
		:sources (list helm-recentf-source)
		:helm-default-keymap helm-files-default-keymap
		:helm-case-fold-search helm-file-name-case-fold-search))

; Files in current dir. Pretty useless.

(defconst helm-files-in-current-dir-source
	(helm-source-files-make nil
		:name "Files from current directory"
		:candidates
			(lambda ()
				(let (
					(dir
						(with-current-buffer helm-current-buffer
							(helm-current-directory)))
				)
					(when (file-accessible-directory-p dir)
						(helm-files-directory-files
							dir t directory-files-no-dot-files-regexp))))))
		; There could be some candidate-transformer to make it possible to add files
		; to helm-source-files sources.
		; Like helm-add-new-candidate with some basic highlighting.
;		:confirm t))

; List of files gleaned from every dired buffer

; (dired '("~/" "~/.emacs.d/.emacs-custom.el" "~/.emacs.d/.emacs.bmk"))

(defconst helm-files-in-all-dired-source
	(helm-source-files-make nil
		:name "Files in dired buffers"
		:candidates
			(lambda ()
				(mapcan
					(lambda (cell)
						(when (buffer-live-p (cdr cell))
							(let (
								(dir
									(buffer-local-value 'dired-directory
										(cdr cell)))
							)
								(if (listp dir)
									(cdr dir)
									(helm-files-directory-files
										(car cell)
										t
										directory-files-no-dot-files-regexp)))))
					dired-buffers))))

; ================================= Small async sources =================================

; Keymaps

(let ((base_keymap (make-sparse-keymap)))
	; Override these toggle functions with correct implementation
	; for async sources.
	(define-key base_keymap [?\C-s ?b] #'helm-files-process-skip-boring-toggle)
	(define-key base_keymap [?\C-s ?f] #'helm-files-process-filter-toggle)
	(defconst helm-files-process-keymap
		(let ((keymap (copy-sequence base_keymap)))
			(set-keymap-parent keymap helm-files-keymap)
			keymap))
	(set-keymap-parent base_keymap helm-files-default-keymap)
	(defconst helm-files-process-default-keymap base_keymap))

; Transformers

(defun helm-files-process-skip-boring (candidates source)
	(if (helm-source-files-process-skip-boring source)
		(cl-remove-if
			(lambda (cand)
				(string-match
					helm-files-boring-regex
					(helm-files-basename-with-slash cand)))
			candidates)
		candidates))

(defun helm-files-process-filter (candidates source)
	(if (helm-source-files-process-filter source)
		(cl-remove-if
			(if (eq (helm-source-files-process-filter source) 'file)
				(lambda (cand) (string-suffix-p "/" cand))
				(lambda (cand) (not (string-suffix-p "/" cand))))
			candidates)
		candidates))

(defun helm-files-process-candidate-transformer (candidates)
	(helm-async-candidate-transformer
		(helm-files-highlight
			(helm-files-process-skip-boring
				(helm-files-process-filter candidates helm-current-source)
				helm-current-source))))

; Extensive copying from helm-source-files and its functions,
; because there's no multiple inheritance mechanism (this source have to inherit
; from helm-source-async, but it also wants to inherit from helm-source-files,
; but that's not possible).

(cl-defstruct
	(helm-source-files-process
		(:copier nil)
		(:constructor helm-source-files-process--make)
		(:include helm-source-async
			(candidate-transformer #'helm-files-process-candidate-transformer)
			(match-part t)
			(action helm-files-actions)
			(action-transformer #'helm-files-action-transformer)
			(persistent-action #'helm-files-persistent-action)
			(keymap helm-files-process-keymap)
			(mode-line #'helm-files-process-mode-line)
			(requires-pattern 2)
			(candidate-number-limit 9999)
			(confirm #'helm-files-confirm)))
	(skip-boring helm-files-default-skip-boring)
	mode-line-skip-boring
	filter ; nil, 'file or 'dir
	mode-line-filter)

(defun helm-files-process-mode-line-skip-boring-set (source)
	(setf (helm-source-files-process-mode-line-skip-boring source)
		(concat
			(if (helm-source-files-process-skip-boring source) "skip" "show")
			" boring  ")))

(defun helm-files-process-mode-line-filter-set (source)
	(setf (helm-source-files-process-mode-line-filter source)
		(cl-case (helm-source-files-process-filter source)
			(file "files only, ")
			(dir "dirs only, "))))

(helm-source-define-constructor "helm-source-files-process" "helm-source-async"
"Amongst ARGS there can be :w32 - if non-nil and system-type = windows-nt,
as the first step of candidate transformer,
pass candidates to `helm-files-w32-pathname-transformer'."
	(lambda ()
		(helm-files-process-mode-line-skip-boring-set source)
		(helm-files-process-mode-line-filter-set source)
		(and
			(eq system-type 'windows-nt)
			(plist-get args :w32)
			(setf (helm-source-async-candidate-transformer source)
				`(lambda (candidates)
					(,(helm-source-async-candidate-transformer source)
						(helm-files-w32-pathname-transformer candidates)))))))

(defun helm-files-process-mode-line ()
	(concat
		(helm-source-files-process-mode-line-filter helm-current-source)
		(helm-source-files-process-mode-line-skip-boring helm-current-source)
		(helm-async-mode-line)))

; Toggle functions

(defun helm-files-process-skip-boring-toggle () (interactive)
	(helm-files-toggle-base
		(lambda (source)
			(setf (helm-source-files-process-skip-boring source)
				(not (helm-source-files-process-skip-boring source)))
			(helm-files-process-mode-line-skip-boring-set source))))

(defun helm-files-process-filter-toggle ()
"Toggle filtering between none, files only and dirs only."
	(interactive)
	(helm-files-toggle-base
		(lambda (source)
			(setf (helm-source-files-process-filter source)
				(cl-case (helm-source-files-process-filter source)
					(file 'dir)
					(dir nil)
					(t 'file)))
			(helm-files-process-mode-line-filter-set source))))

; Locate

; I have no idea how it works so idk if dirs returned by locate end with a slash,
; if not, candidate transformer should be added to ensure they are.
; Also this "Locate" section is untested (because I don't have locate).

(defconst helm-locate-db-file-regexp "m?locate\\.db$"
"Default regexp to match locate database.
If nil, search in all files.")

(defconst helm-locate-db-filename "locate.db"
"The basename of the locatedb file you use locally in your directories.
When this is set and helm finds such a file in the directory from
where you launch locate, it will use this file and will not
prompt you for a db file.
Note that this happen only when locate is launched with a prefix arg.")

(defconst helm-locate-exe (executable-find "locate"))

(defconst helm-locate-options '("-e" "-A"))

(defconst helm-locate-create-db-command "updatedb -l 0 -o '%s' -U '%s'"
"Command used to create a locale locate db file.")

(defconst helm-locate-project-list nil
"A list of directories, your projects.
When set, allow browsing recursively files in all directories of
this list with `helm-projects-find-files'.")


(defconst helm-locate-source
	(helm-source-files-process-make nil
		:name "Locate"
		:candidates
			; Initialize async locate process.
			(lambda ()
				(let (
					(default-process-coding-system
						; Fix `default-process-coding-system' in locate for Windows.
						(if-let (
							((eq system-type 'windows-nt))
							((boundp 'w32-ansi-code-page))
							(code-page-eol
								(intern (format "cp%s-%s" w32-ansi-code-page "dos")))
							((ignore-errors (check-coding-system code-page-eol)))
						)
							(cons code-page-eol code-page-eol)
							default-process-coding-system))
				)
					(make-process
						:name "helm-locate"
						:command
							(nconc
								(list helm-locate-exe)
								helm-locate-options
								(when (helm-set-case-fold-search) (list "-i"))
								(split-string-shell-command helm-pattern))
						:sentinel (helm-async-get-default-sentinel))))))

(defun helm-locate-create-db (db-name directory)
"Function used to create a locale locate db file.
Argument DB-NAME name of the db file.
Argument DIRECTORY root of file system subtree to scan."
	(format helm-locate-create-db-command db-name (expand-file-name directory)))

(defun helm-locate-1 (localdb from-ff default)
"Generic function to run Locate.
Prefix arg LOCALDB when (4) search and use a local locate db file
when it exists or create it, when (16) force update of existing
db file even if exists.
See `helm-locate-with-db' and `helm-locate'."
	(let (
		(locdb
			(when localdb
				(or
					; Try to find if a local locatedb file is available.
					; The search is done in `helm-ff-default-directory'
					; or falls back to `default-directory' if FROM-FF is nil.
					(when-let (
						((equal localdb '(4)))
						helm-locate-db-filename
						(f
							(locate-dominating-file
								(or
									(and from-ff helm-ff-default-directory)
									default-directory)
								helm-locate-db-filename))
					)
						(expand-file-name helm-locate-db-filename f))
					(helm-read-file-name
						"Create locatedb file: "
						:input
							(expand-file-name "locate.db"
								(or helm-ff-default-directory default-directory))
						:preselect helm-locate-db-file-regexp
						:type 'file
						:test
							(when helm-locate-db-file-regexp
								(lambda (file)
									; Select only locate db files and directories
									; to allow navigation.
									(or
										(string-match helm-locate-db-file-regexp file)
										(string-suffix-p "/" file))))))))
	)
		(and
			locdb
			(or (equal localdb '(16)) (not (file-exists-p locdb)))
			(cond
				((file-directory-p locdb) (message "The locatedb should be a file."))
				((=
						0
						(shell-command (helm-locate-create-db locdb helm-ff-default-directory)))
					(message "New locatedb file \"%s\" created." locdb))
				(t (error "Failed to create locatedb file \"%s\"" locdb))))
		(helm-locate-with-db locdb default)))

(defun helm-locate-with-db (db &optional default)
"Run locate -d DB.
If DB is nil use locate without -d option.
Argument DB can be given as a string or list of db files.
See also `helm-locate'."
	(when (stringp db) (setq db (list db)))
	(let (
		(helm-locate-options
			(if db
				(nconc
					(list
						"-d"
						(concat
							"'"
							(mapconcat
								#'identity
								; Remove directories marked by error.
								(cl-loop
									for i in db
									unless (file-directory-p i)
										; expand-file-name to resolve abbreviated fnames
										; not expanding inside single quotes i.e. '%s'.
										collect (expand-file-name i))
								":")
							"'"))
					helm-locate-options)
				helm-locate-options))
	)
		(helm-file-name-basename-history-update)
		(helm
			:sources (list helm-locate-source)
			:default default
			:history 'helm-file-name-basename-history
			:helm-delay t
			:helm-default-keymap helm-files-process-default-keymap
			:helm-case-fold-search helm-file-name-case-fold-search)))

(defun helm-locate (arg)
"helm for locate.
Note: you can add locate options after entering pattern.
See \\='man locate' for valid options.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it if
it doesn't exists.

To create a user specific db, use \"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by `helm-locate-db-file-regexp'."
	(interactive "P")
	(setq helm-ff-default-directory default-directory)
	(helm-locate-1 arg nil (thing-at-point 'filename)))

(defun helm-projects-find-files (update)
"Find files with locate in `helm-locate-project-list'.
With a prefix arg refresh the database in each project."
	(interactive "P")
	(unless (executable-find "updatedb")
		(error "Unsupported locate version"))
	(unless helm-locate-project-list
		(error "No projects found, please setup `helm-locate-project-list'"))
	(helm-locate-with-db
		(mapcar
			(lambda (p)
				(let (
					(db
						(expand-file-name
							helm-locate-db-filename
							(file-name-as-directory p)))
				)
					(or
						(and (not update) (file-exists-p db))
						(= 0 (shell-command (helm-locate-create-db db p)))
						(error "Failed to create locatedb file `%s'" db))
					db))
			helm-locate-project-list)))

; Find

; Also untested because I don't use find.

(defconst helm-findutils-skip-boring-files t
"Non-nil to ignore boring files in find command results.")

(defconst helm-findutils-search-full-path nil
"Non-nil to search in full path with shell command find.
I.e. use the -path/ipath arguments of find instead of -name/iname.")

(defconst helm-find-noerrors nil
"Prevent showing error messages in helm-buffer when non-nil.")

; name will be set before every helm call.
(defconst helm-findutils-source
	(helm-source-files-process-make nil
		:candidates
			; Asynchronously fetch candidates for 'helm-find'.
			; Additional find options can be specified after a "*" separator.
			(lambda ()
				(let* (
					process-connection-type
					(process
						(start-file-process-shell-command "helm-find" helm-buffer
							(concat
								(let* (
									(default-directory
										(or
											(file-remote-p default-directory 'localname)
											default-directory))
									(patterns+options
										(split-string helm-pattern "\\(\\`\\| +\\)\\* +"))
									ignored-dirs
									(ignored-files
										(when helm-findutils-skip-boring-files
											(cl-loop
												for f in completion-ignored-extensions
												if (string-suffix-p "/" f)
													do
													(push
														(substring-no-properties f 0 -1)
														ignored-dirs)
												else
													collect (concat "*" f))))
									(name-or-iname
										(if (helm-set-case-fold-search (car patterns+options))
											(if helm-findutils-search-full-path
												'ipath 'iname)
											(if helm-findutils-search-full-path
												'path 'name)))
								)
									(find-cmd
										(and
											ignored-dirs
											(list 'prune (cons 'name ignored-dirs)))
										(and
											ignored-files
											(list 'not (cons 'name ignored-files)))
										`(and
											,@(mapcar
												(lambda (pattern)
													(list name-or-iname (concat "*" pattern "*")))
												(split-string (car patterns+options)))
											,@(and
												(cdr patterns+options)
												(list (concat (nth 1 patterns+options) " "))))))
								(when helm-find-noerrors "2> /dev/null"))))
				)
					(set-process-sentinel process
						`(lambda (process event)
							(helm-process-deferred-sentinel-hook
								process event (helm-default-directory))
							(,(helm-async-get-default-sentinel) process event)))
					process))
		:candidate-transformer
			(lambda (candidates)
				(helm-files-process-candidate-transformer
					(map_modify_list
						(if-let ((f (file-remote-p default-directory)))
							`(lambda (i) (expand-file-name (concat ,f i)))
							#'expand-file-name)
						candidates)))))

(defun helm-find-1 (dir)
	; Not sure it that's needed (maybe for some action function).
	(setq helm-ff-default-directory dir)

	(setf (helm-source-name helm-findutils-source) (concat "Find (" dir ")"))
	(helm-file-name-basename-history-update)
	(let ((default-directory dir))
		(helm
			:sources (list helm-findutils-source)
			:default (thing-at-point 'filename)
			:history 'helm-file-name-basename-history
			:helm-delay t
			:helm-default-keymap helm-files-process-default-keymap
			:helm-case-fold-search helm-file-name-case-fold-search)))

(defun helm-find (arg)
"helm for the find shell command.

Recursively find files whose names are matched by all specified
globbing PATTERNs under the current directory using the external
program specified in `find-program' (usually \"find\").
Every input PATTERN is silently wrapped into two stars: *PATTERN*.

With prefix argument, prompt for a directory to search.

When user option `helm-findutils-search-full-path' is non-nil,
match against complete paths, otherwise, against file names
without directory part.

The (possibly empty) list of globbing PATTERNs can be followed by
the separator \"*\" plus any number of additional arguments that
are passed to \"find\" literally."
	(interactive "P")
	(helm-find-1
		(if arg
			(file-name-as-directory (read-directory-name "Default directory: "))
			default-directory)))

; fd (similar to gnu find)

; helm-pattern must be an actual pattern, not options.
;
; Negation is supported as a secondary filter when multi matching.
;
; Even though fd displays dirs as ending with a slash, it performs matching like
; there is no trailing slash, so every pattern matching final part of path and trailing
; slash will fail. This is pretty crapping annoying and should be changed, but it would
; require investigating possibly complicated regexes, so for now leave this stupid
; behavior.

(defconst helm-fd-exe (executable-find "fd"))

(defconst helm-fd-options
	'("--no-ignore" "--hidden" "--color=never" "--path-separator=/"))

(defvar helm-fd-match-path 'off
"nil or \\='off to match the entire path, not only basename.
nil means user turned it off,
t means user turned it on,
\\='on means auto turned on (pattern contains \"/\"),
\\='off means auto turned off (pattern doesn't contain \"/\").")

(defvar helm-fd-max-depth nil
"Number >= 1 to limit directory traversal depth. Nil - no limit.")

(defvar helm-fd-min-depth nil
"Number >= 1 to search from this depth. Nil - search from default dir.")

(defun helm-fd-match-path-toggle ()
"Toggle between: auto, force off, force on; path matching."
	(interactive)
	(setq helm-fd-match-path
		(cond
			((memq helm-fd-match-path '(on off)) nil)
			(helm-fd-match-path 'off)
			(t t)))
	; This will update to correct auto value if helm-fd-match-path is now 'off.
	(helm-force-update))

(defun helm-fd-depth-set-base (symbol msg arg)
	(set symbol
		(let (
			(number
				(if arg ; Use prefix if given.
					(max 0 (prefix-numeric-value arg))
					(setq msg (concat msg " depth: "))
					(let (str)
						(let (
							(default
								(when (symbol-value symbol)
									(number-to-string (symbol-value symbol))))
						)
							(while
								(progn
									(setq str
										(string-trim
											(read-from-minibuffer
												msg nil nil nil nil default)))
									(not (string-match-p "\\`[0-9]*\\'" str)))
								(message "Enter a positive number or nothing.")
								(sit-for 1)))
						(string-to-number str))))
		)
			(and (/= number 0) number)))
	(helm-force-update))

(defun helm-fd-max-depth-set (arg) (interactive "P")
	(helm-fd-depth-set-base 'helm-fd-max-depth "Max" arg))

(defun helm-fd-min-depth-set (arg) (interactive "P")
	(helm-fd-depth-set-base 'helm-fd-min-depth "Min" arg))

(defconst helm-fd-default-keymap
	(let ((keymap (make-sparse-keymap)))
		(set-keymap-parent keymap helm-files-process-default-keymap)
		(define-key keymap [?\C-o] #'helm-fd-match-path-toggle)
		(define-key keymap [?\C-,] #'helm-fd-min-depth-set)
		(define-key keymap [?\C-.] #'helm-fd-max-depth-set)
		keymap))


; --extension raczej nie ma sensu, bo multi match \.exe$ itp. to ogarnia.
; Teoretycznie można by kiedyś dodać --size, --changed-within i --changed-before,
; ale wydaje mi się to bardzo mało użyteczne, więc na razie nie.

; name will be set before every helm call.
(defconst helm-fd-source
	(helm-source-files-process-make nil
		:candidates
			(lambda ()
				(let* (
					match_args
					(pattern_args
						; Do this first to quickly recognize if helm-pattern is an invalid
						; regex. If it is and only regex matching is used, don't even call fd.
						(cond
							((string= helm-pattern "") (list helm-pattern))
							; Substring.
							((eq (helm-source-match helm-current-source) (cdr helm-match-substring-cell))
								(setq match_args (list "--fixed-strings"))
								(list (string-replace "/" "\\" helm-pattern)))
							; Multi single.
							((eq (helm-source-match helm-current-source) (cdr helm-match-multi-single-cell))
								(let ((patterns (helm-match-get-patterns helm-pattern)))
									(cond
										((cdr patterns)
											(setq patterns
												(helm-match-patterns-to-multi (cdr patterns)))
											(when patterns
												(list (string-replace "/" "\\\\" patterns))))
										((car patterns) (list "")))))
							(t
								(let (
									(pcre_regex
										(ignore-error invalid-regexp
											; It's regex so quote all backslashes.
											; I really hope this is a safe way to replace
											; / with \ but I'm not 100% sure.
											; Even though fd can display filepaths with / as separator,
											; it always matches \ anyway, so this replacement is necessary.
											(string-replace "/" "\\\\"
												(helm-match-pattern-to-pcre helm-pattern))))
								)
									(if (eq (helm-source-match helm-current-source) (cdr helm-match-regex-cell))
										; Regex.
										(when pcre_regex (list pcre_regex))
										; Multi match.
										(list
											(concat
												; AFAIK there is no way to use substring
												; and regex matching in the same call to
												; fd, so quote helm-pattern.
												; This doesn't signal.
												(rxt-quote-pcre (string-replace "/" "\\" helm-pattern))
												(when pcre_regex (concat "|" pcre_regex))
												(when-let (
													; If helm-pattern contains parts to match.
													(patterns
														(cdr (helm-match-get-patterns helm-pattern)))
													((setq patterns
														(helm-match-patterns-to-multi patterns)))
												)
													(setq patterns (string-replace "/" "\\\\" patterns))
													(concat "|" patterns)))))))))
					process-connection-type
				)
					(when pattern_args
						(make-process
							:name "helm-fd"
							:command
								(append
									(cons helm-fd-exe helm-fd-options)
									(list
										(concat
											"--max-results="
											(number-to-string
												(helm-candidate-number-limit helm-current-source))))
									; fd uses smart case by default.
									(unless (eq helm-case-fold-search 'smart)
										(list
											(if helm-case-fold-search
												"--ignore-case"
												"--case-sensitive")))
									(cl-case (helm-source-files-process-filter helm-current-source)
										(dir (list "--type=directory"))
										; 'file here means not dir, so everything else.
										; Actually, idk how symlinks to dirs are treated,
										; so this may be slightly wrong.
										(file
											(list
												"--type=file"
												"--type=symlink"
												"--type=socket"
												"--type=pipe"
												"--type=block-device"
												"--type=char-device")))
									(cond
										; Auto match-path.
										((memq helm-fd-match-path '(on off))
											(if (string-search "/" helm-pattern)
												(progn
													(setq helm-fd-match-path 'on)
													(list "--full-path"))
												(setq helm-fd-match-path 'off)
												nil))
										; Force on.
										(helm-fd-match-path (list "--full-path")))
									(when helm-fd-max-depth
										(list
											(concat
												"--max-depth="
												(number-to-string helm-fd-max-depth))))
									(when helm-fd-min-depth
										(list
											(concat
												"--min-depth="
												(number-to-string helm-fd-min-depth))))
									match_args
									(list "--")
									pattern_args)
							:sentinel (helm-async-get-default-sentinel)
							:stderr (helm-async-get-stderr "helm-fd")))))
		:sort #'helm-sort-alpha
		; Discard non matched candidates - handle negation and different multi matching
		; (--and arguments perform simple matching from the start of the string
		; every time, so patterns like "helm" --and "helm" don't make any sense
		; (are always matched), unlike helm multi matching, so discard candidates
		; matched by fd that way.
		:match-strict t
		:candidate-transformer
			(lambda (candidates)
				(setq candidates (helm-files-process-skip-boring candidates helm-current-source))
				(setq candidates (helm-files-highlight candidates))
				(let ((match_basename (memq helm-fd-match-path '(off nil))))
					(dolist (cand candidates)
						(setq cand (car cand)) ; Take display part.
						(let* (
							(match_part (get-text-property 0 'match-part cand))
							(first_cell (car match_part))
						)
							; Remove parts like added symlink destination.
							(setcdr match_part nil)
							; Adjust end if file is a dir - fd doesn't match that.
							(when (= (aref cand (1- (cdr first_cell))) ?/)
								(setcdr first_cell (1- (cdr first_cell))))
							(when match_basename
								(setcar first_cell
									(-
										(cdr first_cell)
										(length
											(file-name-nondirectory
												(substring-no-properties
													cand
													(car first_cell)
													(cdr first_cell))))))))))
				(helm-async-candidate-transformer candidates))
		:keymap
			(let ((keymap (make-sparse-keymap)))
				(set-keymap-parent keymap helm-files-process-keymap)
				(define-key keymap [?\C-o] #'helm-fd-match-path-toggle)
				(define-key keymap [?\C-,] #'helm-fd-min-depth-set)
				(define-key keymap [?\C-.] #'helm-fd-max-depth-set)
				keymap)
		:mode-line
			(lambda ()
				(concat
					; From helm-files-process-mode-line, to put "finished" message
					; at the end.
					(helm-source-files-process-mode-line-filter helm-current-source)
					(helm-source-files-process-mode-line-skip-boring helm-current-source)

					"match path: "
					(cond
						((eq helm-fd-match-path 'on) "auto on")
						((eq helm-fd-match-path 'off) "auto off")
						(helm-fd-match-path "force on")
						(t "force off"))
					"  ("
					(if helm-fd-min-depth
						(number-to-string helm-fd-min-depth)
						"0")
					", "
					(if helm-fd-max-depth
						(number-to-string helm-fd-max-depth)
						"∞")
					")  "
					(helm-async-mode-line)))
		:requires-pattern 0
		:resume (lambda () (setq helm-ff-default-directory default-directory))))

(defun helm-fd () "helm for fd." (interactive)
	(setq helm-ff-default-directory default-directory)
	(setf (helm-source-name helm-fd-source)
		(concat "fd (" (abbreviate-file-name default-directory) ")"))
	(helm-file-name-basename-history-update)
	(helm
		:sources (list helm-fd-source)
		:default (thing-at-point 'filename t)
		:history 'helm-file-name-basename-history
		:helm-delay t
		:helm-default-keymap helm-fd-default-keymap
		:helm-case-fold-search helm-file-name-case-fold-search))

(defun helm-ff-fd (_candidate) "Run fd shell command from `helm-ff'."
	(helm-files-save-to-file-name-history helm-ff-default-directory)
	(if (file-remote-p helm-ff-default-directory)
		(message "Fd not supported on remote directories.")
		(let ((default-directory helm-ff-default-directory)) (helm-fd))))

; es (Everything)

; This command is too complex to support its matching and sorting
; options, so this only provides:
; filtering by file, dir, all
; limited number of candidates
; helm's case fold search
; skipping boring files
; highlighting by helm-files-highlight, but actually this also should
; be scrapped, because of possibility of candidates being much more
; that filepaths - many columns of different info about files.
; files in "emacs" format - separators are "/" and dirs end with "/".
;
; This tried to provide some more features by taking double quoted part
; of helm-pattern and interpreting it as a file, replacing "/" with "\"
; and translating regex (if applicable) for highlighting,
; but after taking a look of how many options "es" have and many of them
; include paths, so user would have to write main path in quotes with "/"
; and options with "\", it doesn't really make sense, and I can't just
; replace "/" with "\" "globally", because some es command start not with
; "-" or "--", but with "/", and also some of these "/" could be in regexes,
; so there would need to be extensive analysis of helm-pattern.
; So leave this with this minimal interface.
;
; Also turns out -highlight doesn't work like in grep - it's not an ansi color,
; so idk how to apply highlighting to this now...
; I tried in shell-mode and it also doesn't work there.

(defconst helm-es-exe (executable-find "es"))

(defconst helm-es-source
	(helm-source-files-process-make nil
		:name "Everything"
		:candidates
			(lambda ()
				(make-process
					:name "helm-es"
					:command
						(nconc
							(list
								"es"
								"-n"
								(number-to-string
									(helm-candidate-number-limit helm-es-source)))
							; -i = match case.
							(unless (helm-set-case-fold-search) (list "-i"))
							(cl-case (helm-source-files-process-filter helm-es-source)
								(dir (list "/ad"))
								(file (list "/a-d")))
							(split-string-shell-command helm-pattern))
					:sentinel (helm-async-get-default-sentinel)))
		:match nil
		:match-part nil
		:sort nil
		:candidate-transformer
			(lambda (candidates)
				; Append slash to dirs if needed.
				(cl-case (helm-source-files-process-filter helm-es-source)
					(dir (map_modify_list (lambda (dir) (concat dir "/")) candidates))
					(file)
					(t (helm-files-append-slash-to-dirs candidates)))
				(helm-files-highlight
					(helm-files-process-skip-boring candidates helm-es-source)))
		:w32 t)) ; Replace "\" with "/" in files.

(defun helm-es () "helm for Everything (es)." (interactive)
	; Not sure it that's needed (maybe for some action function).
	(setq helm-ff-default-directory default-directory)

	(helm-file-name-basename-history-update)
	(helm
		:sources (list helm-es-source)
		:default (thing-at-point 'filename)
		:history 'helm-file-name-basename-history
		:helm-delay t
		:helm-default-keymap helm-files-process-default-keymap
		:helm-case-fold-search helm-file-name-case-fold-search))

; Gid THIS IS NOW BROKEN

(defconst helm-gid-exe (executable-find "gid")
"For Mac OS X users, if you install GNU coreutils, the name `gid'
might be occupied by `id' from GNU coreutils, and you should set
it to correct name (or absolute path). For example, if using
MacPorts to install id-utils, it should be `gid32'.")

(defconst helm-gid-db-file-name "ID"
"Name of a database file created by `mkid' command from `ID-utils'.")

; name will be set before every helm call.
(defconst helm-gid-source
	(helm-source-async-make nil
		:candidates
			(lambda ()
				(let* (
					(patterns (split-string helm-pattern))
					(cmd
						(concat
							helm-gid-exe
							" -r "
							(shell-quote-argument (car patterns))
							(mapconcat
								(lambda (p)
									(concat
										" | grep --color=never "
										(shell-quote-argument p)))
								(cdr patterns))))
					(process (start-process-shell-command "helm-gid" nil cmd))
				)
					(set-process-sentinel process (helm-async-get-default-sentinel))
					process))
		:candidate-transformer
			(lambda (candidates)
				(map_modify_list
					#'helm-grep--filter-candidate-1
					; "gid -r" may add dups in some rare cases.
					(helm-fast-remove-dups candidates)))
		:action
			(mapcar
				(lambda (cell)
					(cons
						(car cell)
						`(lambda (candidate)
							(,(cdr cell)
								(helm-files-expand-and-save-to-history candidate)))))
				helm-grep-actions)
		:persistent-action #'helm-grep-goto-line
		; Fortunately helm-grep-keymap doesn't have any actions that would need
		; helm-files-save-to-file-name-history (as of now).
		:keymap helm-grep-keymap
		:requires-pattern 2
		:candidate-number-limit 99999))

(defun helm-gid ()
"helm for `gid' command line of `ID-Utils'.
Need A database created with the command `mkid' above `default-directory'.
Need id-utils as dependency which provide `mkid', `gid' etc.
See <https://www.gnu.org/software/idutils/>."
	(interactive)
	(if-let ((db (locate-dominating-file default-directory helm-gid-db-file-name)))
		(progn
			(setf (helm-source-name helm-gid-source) (concat "Gid (" db ")"))
			(helm
				:sources (list helm-gid-source)
				:default (thing-at-point 'symbol)
				:use-default-as-input t
				:history 'helm-grep-history
				:helm-delay helm-grep-input-idle-delay
				:helm-truncate-lines helm-grep-truncate-lines))
		(message "No database found, create one with `mkid'.")))

; ======================================= helm-ff =======================================

(defvar helm-ff--show-thumbnails nil)
(defvar helm-ff--thumbnailed-directories nil)

(defvar helm-ff--list-directory-cache (make-hash-table :test 'equal)
"Cache for `helm-ff' candidates.")

(defun helm-ff-mode-line ()
	(concat
		(helm-source-ff-mode-line-base helm-current-source)
		"  Sort "
		(helm-source-ff-mode-line-sort-method helm-current-source)
		(helm-source-ff-mode-line-dirs-first helm-current-source)
		(helm-source-ff-mode-line-reverse helm-current-source)
		", "
		(helm-source-files-base-mode-line-skip-boring helm-current-source)))

(defun helm-ff-update ()
	(remhash helm-ff-default-directory helm-ff--list-directory-cache))

(cl-defstruct
	(helm-source-ff
		(:copier nil)
		(:constructor helm-source-ff--make)
		(:include helm-source-files-base
			(mode-line #'helm-ff-mode-line)
			(update #'helm-ff-update)
			(candidates #'helm-ff-get-candidates)
			(candidate-transformer #'helm-ff-maybe-show-thumbnails)
			(persistent-action-if #'helm-ff-persistent-action-if)
			(volatile t)
			(match-part t)
			(candidate-number-limit helm-ff-candidate-number-limit)))
	mode-line-base
	(dirs-first helm-ff-default-sort-dirs-first)
	mode-line-dirs-first
	(sort-method helm-ff-default-sort-method)
	mode-line-sort-method
	(reverse helm-ff-default-sort-reverse)
	mode-line-reverse)

(defun helm-ff-mode-line-dirs-first-set (source)
	(setf (helm-source-ff-mode-line-dirs-first source)
		(when (helm-source-ff-dirs-first source) ", dirs first")))

(defun helm-ff-mode-line-sort-method-set (source)
	(setf (helm-source-ff-mode-line-sort-method source)
		(cl-case (helm-source-ff-sort-method source)
			(size "by size")
			(newest "by newest")
			(ext "by extension")
			(t "alphabetically"))))

(defun helm-ff-mode-line-reverse-set (source)
	(setf (helm-source-ff-mode-line-reverse source)
		(when (helm-source-ff-reverse source) ", reversed")))

(helm-source-define-constructor "helm-source-ff" "helm-source-files-base" nil
	(lambda ()
		(helm-ff-mode-line-dirs-first-set source)
		(helm-ff-mode-line-sort-method-set source)
		(helm-ff-mode-line-reverse-set source)))

; Sorting

(defun helm-ff-sort-base (method mode-line)
	(let ((source (buffer-local-value 'helm-default-source (get-buffer helm-buffer))))
		(unless (eq method (helm-source-ff-sort-method source))
			(helm-files-toggle-base
				`(lambda (source)
					(setf (helm-source-ff-sort-method source) ',method)
					(helm-ff-mode-line-sort-method-set source)
					; Force recomputation by clearing cache with candidates.
					(clrhash helm-ff--list-directory-cache))
				source))))

(defun helm-ff-sort-alpha () (interactive) (helm-ff-sort-base nil "alphabetically"))
(defun helm-ff-sort-by-size () (interactive) (helm-ff-sort-base 'size "by size"))
(defun helm-ff-sort-by-newest () (interactive) (helm-ff-sort-base 'newest "by newest"))
(defun helm-ff-sort-by-ext () (interactive) (helm-ff-sort-base 'ext "by extension"))

(defun helm-ff-sort-reverse-toggle () (interactive)
	(helm-files-toggle-base
		(lambda (source)
			(setf (helm-source-ff-reverse source) (not (helm-source-ff-reverse source)))
			(helm-ff-mode-line-reverse-set source)
			; Force recomputation by clearing cache with candidates.
			(clrhash helm-ff--list-directory-cache))))

(defun helm-ff-sort-dirs-first-toggle () (interactive)
	(helm-files-toggle-base
		(lambda (source)
			(setf (helm-source-ff-dirs-first source)
				(not (helm-source-ff-dirs-first source)))
			(helm-ff-mode-line-dirs-first-set source)
			; Force recomputation by clearing cache with candidates.
			(clrhash helm-ff--list-directory-cache))))

(defconst helm-ff-source
	(helm-source-ff-make nil
		:name (lambda () (or (helm-source-ff-mode-line-base helm-ff-source) "Find files"))
		:confirm t
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-ff-default-keymap)

				; Non exiting.

				(define-key map [?\C-p] #'helm-ff-properties-persistent)
				(define-key map [?\A-l] #'helm-ff-image-rotate-left)
				(define-key map [?\A-r] #'helm-ff-image-rotate-right)
				(define-key map [?\C-=] #'helm-ff-image-increase-size)
				(define-key map [?\C--] #'helm-ff-image-decrease-size)
				; Copy helm-ff-default-directory.
				(define-key map [?\C-\S-c]
					(lambda () (interactive)
						(when helm-ff-default-directory
							(clipboard-add helm-ff-default-directory helm-buffer)
							(message "Copied \"%s\"." helm-ff-default-directory))))
				(define-key map [?\C-\S-\s] #'helm-ff-mark-similar-files)
				(define-key map [?\C-b] #'helm-ff-bookmark-set)
				; Open file externally.
				(define-key map [?\A-o]
					(lambda () (interactive)
						(let ((file (helm-get-selection)))
							(helm-files-save-to-file-name-history file)
							(if (helm-get-default-program-for-file file)
								(helm-ff-open-file-externally file)
								(message
									"Please configure an external program for \"%s\" files in `helm-external-programs-associations'."
									(file-name-extension file))))))
				; Save and kill file buffer.
				(define-key map [kp-insert]
					(lambda () (interactive)
						(let ((i 0) probably_need_refresh)
							(dolist (buffer (helm-marked-candidates))
								(setq buffer (get-file-buffer buffer))
								(cond
									((not buffer))
									((eq buffer (get-buffer helm-current-buffer))
										(message
											"Can't kill `helm-current-buffer' without quitting helm."))
									(t
										(setq probably_need_refresh t)
										(when
											(if current-prefix-arg
												(let ((use-short-answers t))
													(kill-buffer buffer))
												(with-current-buffer buffer (save_buffer))
												(kill-buffer buffer))
											(++ i)))))
							(if (= i 0)
								(message "No buffers killed.")
								(message "%d buffer%s killed." i (if (= i 1) "" "s")))
							; When nothing was saved or killed, don't recompute.
							; This allows for lightweight checking if file has
							; associated buffer.
							(when probably_need_refresh
								(clrhash helm-ff--list-directory-cache)
								(helm-force-update)))))

				; Delete marked.
				; Copied from helm-add-delete-binds.
				(let (
					(delete_candidate_base_fn
						(lambda (0_or_1)
							; If something was actually deleted.
							(when (helm-delete-marked-files nil t)
								(clrhash helm-ff--list-directory-cache)
								(helm-force-update
									nil
									(with-helm-window
										(if (not helm-marked-candidates)
											(max 0 (- (helm-get-index) 0_or_1))
											(helm-unmark-all)
											t))))))
				)
					(define-key map [S-delete]
						`(lambda () (interactive) (,delete_candidate_base_fn 0)))
					(define-key map [S-backspace]
						`(lambda () (interactive) (,delete_candidate_base_fn 1))))

				; Exiting.

				(define-key map [return] #'helm-ff-return)
				(define-key map [f2] #'helm-ff-rename-command)
				(define-key map [?\A-c] #'helm-ff-copy-command)
				(define-key map [?\A-C] (helm-make-action-command #'helm-ff-multi-copy))
				(define-key map [?\A-d] #'helm-ff-delete-files-command)
				(define-key map [?\A-s ?s] #'helm-ff-symlink-command)
				(define-key map [?\A-s ?h] #'helm-ff-hardlink-command)
				(define-key map [?\A-s ?r] (helm-make-action-command #'helm-ff-relsymlink))
				(when (executable-find "rsync")
					(define-key map [?\A-r] (helm-make-action-command #'helm-ff-rsync)))
				(when helm-grep-exe (define-key map [?\C-g] #'helm-ff-grep-command))
				(when helm-zgrep-exe (define-key map [?\A-z] #'helm-ff-zgrep-command))
				(when helm-pdfgrep-exe (define-key map [?\A-p] #'helm-ff-pdfgrep-command))
				(when helm-grep-rg-exe
					(define-key map [?\A-g] (helm-make-action-command #'helm-ff-rg)))
				(when helm-grep-git-exe
					(define-key map [?\A-G] (helm-make-action-command #'helm-ff-git-grep)))
				(when helm-gid-exe
					(define-key map [?\A-i] (helm-make-action-command #'helm-ff-gid)))
				(define-key map [?\C-d] #'helm-ff-edit-marked-files-command)
				(define-key map [?\A-e] (helm-make-action-command #'helm-ff-etags))
				(when helm-locate-exe
					(define-key map [?\C-l] (helm-make-action-command #'helm-ff-locate)))
				(define-key map [?\A-b] (helm-make-action-command #'helm-ff-browse-project))
				(when (executable-find "touch")
					(define-key map [?\A-t] (helm-make-action-command #'helm-ff-touch-files)))
				(define-key map [?\A-m] (helm-make-action-command #'helm-ff-chmod))
				(define-key map [?\A-s] (helm-make-action-command #'helm-ff-switch-to-shell))
				(define-key map [?\C-\S-i] (helm-make-action-command #'helm-insert-file-name-completion-at-point))
				(define-key map [?\C-o] #'helm-ff-open-file-externally-command)
				(define-key map [?\C-\S-o] #'helm-ff-open-file-with-default-program-command)
				(define-key map [S-f2] (helm-make-action-command #'helm-ff-query-replace-filenames))
				(define-key map [?\C-r] (helm-make-action-command #'helm-ff-query-replace))
				(define-key map [?\C-\S-r] (helm-make-action-command #'helm-ff-query-replace-regex))
				(define-key map [?\C-e] #'helm-ff-ediff-files-command)
				(define-key map [?\C-\S-e] #'helm-ff-ediff-merge-files-command)
				(define-key map [?\C-\S-p] #'helm-ff-properties-command)
				(when helm-fd-exe (define-key map [?\C-f] (helm-make-action-command #'helm-ff-fd)))
;				(define-key map [?\C-\S-f] (helm-make-action-command #'helm-ff-find-command))
				map)
		:action-transformer
			(lambda (actions)
				(let ((candidate (helm-get-selection)))
					(cond
						((not (file-exists-p candidate))
							(cl-remove-if
								(lambda (cell)
									(memq
										(cdr cell)
										; Not sure if helm-ff-edit-marked-files or helm-ff-rsync
										; should be here.
										'(
											helm-ff-view-file
											helm-ff-insert-file
											helm-ff-query-replace-filenames
											helm-ff-query-replace
											helm-ff-query-replace-regex
											helm-ff-serial-rename
											helm-ff-open-file-with-default-program
											helm-ff-open-file-externally
											helm-ff-mail-attach-files
											helm-ff-hexl-find-file
											helm-ff-in-file
											helm-ff-cache-add-file
											helm-ff-grep
											helm-ff-zgrep
											helm-ff-pdfgrep
											helm-ff-ediff-files
											helm-ff-ediff-merge-files
											helm-ff-delete-files
											helm-ff-touch-files
											helm-ff-copy
											helm-ff-rename
											helm-ff-backup
											helm-ff-multi-copy
											helm-ff-chmod
											helm-ff-print
											helm-ff-locate
											helm-ff-checksum
										)))
								actions))
						((file-directory-p candidate)
							(cl-remove-if
								(lambda (cell)
									(memq
										(cdr cell)
										'(
											helm-ff-insert-file
											helm-ff-in-file
											helm-ff-checksum
										)))
								actions))
						((member (file-name-extension candidate) '("el" "elc"))
							(append
								actions
								(list (cons "Load elisp file" #'load-file))))
						((string-match (image-file-name-regexp) candidate)
							(helm-append-at-nth
								actions
								(list
									; These two are pretty useless, its better to rotate through keybind.
									(cons "Rotate image right" #'helm-ff-image-rotate-right)
									(cons "Rotate image left" #'helm-ff-image-rotate-left)
									(cons "Slideshow with marked" #'helm-ff-start-slideshow-on-marked))
								6))
						((string-match "\\.html?$" candidate)
							(helm-append-at-nth
								actions
								(list (cons "Browse url file" #'browse-url-of-file))
								6))
						(t actions))))
		:action
			(delq
				nil
				(list
					(cons "Find file" #'helm-ff-action)
					(cons "Find file other window" #'helm-ff-other-window)
					(cons "Find file other frame" #'helm-ff-other-frame)
					(cons
						"Find file in Dired"
						; Put point on filename in dired buffer.
						(lambda (candidate)
							(unless (string-match helm-ff-url-regexp candidate)
								(helm-files-save-to-file-name-history candidate)
								(setq candidate (directory-file-name candidate))
								(dired (file-name-directory candidate))
								; If file doesn't exist (is not in dired buffer),
								; nothing bad happens.
								(dired-goto-file candidate))))
					(cons
						"Dired directory (prefix - use default dir)"
						(lambda (candidate)
							(if helm-current-prefix-arg
								(when helm-ff-default-directory
									(helm-files-save-to-file-name-history helm-ff-default-directory)
									(dired helm-ff-default-directory))
								(unless (string-match helm-ff-url-regexp candidate)
									(helm-files-save-to-file-name-history candidate)
									(when (file-directory-p candidate)
										(dired candidate))))))
					(cons "Find alternate file" #'helm-ff-find-alternate-file)
					(unless (eq system-type 'windows-nt)
						(cons "Find file as root" #'helm-ff-as-root))
					(cons "Show file properties" #'helm-ff-properties)
					(cons "View file" #'helm-ff-view-file)
					(cons "Insert file" #'helm-ff-insert-file)
					(cons "Checksum file" #'helm-ff-checksum)
					(cons "Marked files in Dired (prefix - wdired)" #'helm-ff-edit-marked-files)
					(cons "Query replace regex filenames" #'helm-ff-query-replace-filenames)
					(cons "Query replace" #'helm-ff-query-replace)
					(cons "Query replace regex" #'helm-ff-query-replace-regex)
					(cons "Serial rename files (prefix: 1 - copy, 2 - symlink)" #'helm-ff-serial-rename)
					(cons "Open file with default program" #'helm-ff-open-file-with-default-program)
					(cons "Open file externally (prefix - choose program)" #'helm-ff-open-file-externally)
					(cons "Attach file(s) to mail buffer" #'helm-ff-mail-attach-files)
					(cons "Find file in hex dump" #'helm-ff-hexl-find-file)
					(cons "Browse project" #'helm-ff-browse-project)
					(cons "Complete at point" #'helm-insert-file-name-completion-at-point)
					(cons "Insert as org link" #'helm-ff-insert-as-org-link)
;					(cons "Find shell command" #'helm-ff-find-command)
					(when helm-fd-exe (cons "Fd shell command" #'helm-ff-fd))
					(cons "Find files in file(s)" #'helm-ff-in-file)
					(cons "Add marked files to file-cache" #'helm-ff-cache-add-file)
					(when helm-grep-exe (cons "Grep file(s)" #'helm-ff-grep))
					(when helm-grep-rg-exe (cons "RG file(s)" #'helm-ff-rg))
					(when helm-zgrep-exe (cons "Zgrep file(s)" #'helm-ff-zgrep))
					(when helm-pdfgrep-exe (cons "Pdfgrep file(s)" #'helm-ff-pdfgrep))
					(when helm-grep-git-exe
						(cons "Git grep (prefix - from root)" #'helm-ff-git-grep))
					(when helm-gid-exe (cons "Gid" #'helm-ff-gid))
					(cons "Switch to shell" #'helm-ff-switch-to-shell)
					(cons "Etags (prefix - reload tag file)" #'helm-ff-etags)
					(cons "Ediff" #'helm-ff-ediff-files)
					(cons "Ediff Merge" #'helm-ff-ediff-merge-files)
					(cons "Delete file(s) (prefix - reverse trash)" #'helm-ff-delete-files)
					(when (executable-find "touch")
						(cons "Touch file(s)" #'helm-ff-touch-files))
					(cons "Copy file(s) (prefix - follow to destination)" #'helm-ff-copy)
					(cons "Rename file(s) (prefix - follow to destination)" #'helm-ff-rename)
					(cons "Backup file(s) (prefix - follow to destination)" #'helm-ff-backup)
					(cons "Symlink files(s) (prefix - follow to destination)" #'helm-ff-symlink)
					(cons "Relsymlink file(s) (prefix - follow to destination)" #'helm-ff-relsymlink)
					(cons "Hardlink file(s) (prefix - follow to destination)" #'helm-ff-hardlink)
					(when (executable-find "rsync")
						(cons "Rsync file(s) (prefix - edit command)" #'helm-ff-rsync))
					(cons "Copy file to dir(s)" #'helm-ff-multi-copy)
					(cons "Change mode on file(s)" #'helm-ff-chmod)
					(cons "Print file (prefix - refresh)" #'helm-ff-print)
					(when helm-locate-exe
						(cons "Locate (prefix - specify locate db)" #'helm-ff-locate)))))
"The main source to browse files.
Should not be used among other sources.")

(defun helm-ff-return ()
"If selected candidate is a directory, enter (expand) it,
else invoke normal [return] keybind for helm."
	(interactive)
	(if
		(let ((candidate (helm-get-selection)))
			(or
				(file-directory-p candidate)
				(helm-ff--invalid-tramp-name-p candidate)))
		(helm-execute-persistent-action)
		(helm-return)))

(defun helm-ff-open-file-with-default-program (file)
"Open FILE with the default platform on this platform."
	(setq file (helm-files-expand-and-save-to-history file))
	(let (process-connection-type)
		(if (eq system-type 'windows-nt)
			(w32-shell-execute "open" (helm-w32-prepare-filename file))
			(start-process "helm-ff-open-file-with-default-program"
				nil
				(cond
					((eq system-type 'gnu/linux) "xdg-open")
					((or
							(eq system-type 'darwin) ; Mac OS X
							(eq system-type 'macos)) ; Mac OS 9
						"open")
					((eq system-type 'cygwin) "cygstart"))
				file))))

(defun helm-ff-open-file-externally (_file)
"Open FILE with an external program.
Try to guess which program to use with `helm-get-default-program-for-file'.
If not found or a prefix arg is given, query the user which program to use."
	(let* (
		(files (helm-files-marked-candidates t))
		(fname (car files))
		(def-prog (helm-get-default-program-for-file fname))
		(program
			(if (and (not helm-current-prefix-arg) def-prog)
				def-prog
				; Prefix arg or no default program.
				; Always prompt to set this program as default.
				(setq def-prog nil)
				(helm-comp-read
					"Program: "
					(sort helm-external-commands-list #'helm-sort-alpha)
					:must-match t
					:name "Open file externally"
					:history 'helm-external-command-history)))
	)
		(when
			(and
				(not def-prog)
				; Don't try to record non-filenames associations (e.g urls).
				(file-exists-p fname)
				(y-or-n-p
					(format
						"Do you want to make \"%s\" the default program for this kind of files?"
						program)))
			(setq helm-external-programs-associations
				(assoc-delete-all
					(file-name-extension fname) helm-external-programs-associations))
			(push
				(cons
					(file-name-extension fname)
					(read-string "Program (maybe add args and confirm): " program))
				helm-external-programs-associations))
		(helm-run-or-raise program files)
		(run-hooks 'helm-ff-open-file-externally-after-hook)
		(setq helm-external-command-history
			(cl-delete-if-not #'executable-find helm-external-command-history))))

(defun helm-ff-as-root (candidate)
	(setq candidate (helm-files-expand-and-save-to-history candidate))
	(let (
		(buf (helm-basename candidate))
		(remote-path
			(concat
				"/"
				helm-su-or-sudo
				":"
				(if-let ((host (file-remote-p candidate 'host)))
					(concat
						host ":" (expand-file-name (file-remote-p candidate 'localname)))
					(concat ":" candidate))))
	)
		(if (not (get-buffer buf))
			(find-file remote-path)
			(set-buffer buf)
			(find-alternate-file remote-path))))

; Bookmark functions

(defun helm-ff-bookmark-jump (bookmark) "Bookmark handler for `helm-ff'."
	(let ((default_dir (bookmark-prop-get bookmark 'default-dir)))
		(helm-files-save-to-file-name-history default_dir)
		; Force tramp connection with `file-directory-p' before lauching
		; hff otherwise the directory name is inserted on top before
		; tramp starts and display candidates.
		(when (file-directory-p default_dir)
			(helm-ff-1
				default_dir
				(helm-ff-regex-for-preselection
					(helm-basename (bookmark-prop-get bookmark 'preselect)))))))

(defun helm-ff-bookmark-set () "Record `helm-ff' session in bookmarks." (interactive)
	(with-current-buffer helm-buffer
		(helm-files-save-to-file-name-history helm-ff-default-directory)
		(let (
			(bookmark-make-record-function
				(lambda ()
					(list
						(cons 'default-dir helm-ff-default-directory)
						(cons 'preselect (helm-get-selection))
						(cons 'handler #'helm-ff-bookmark-jump))))
		)
			(bookmark-set
				(concat
					helm-ff-bookmark-prefix
					(abbreviate-file-name helm-ff-default-directory)))))
	(message "Helm find files session bookmarked."))

(defun helm-dwim-target-directory ()
"Try to return a suitable directory according to `helm-dwim-target'."
	(with-selected-window
		(or
			(get-buffer-window helm-current-buffer)
			; Try next-window if current-buffer has been
			; killed during this session probably by C-d.
			(next-window (helm-window) 0))
		(let ((wins (remq (get-buffer-window helm-marked-buffer-name) (window-list))))
			(expand-file-name
				(cond
					; Provide completion on all the directory belonging to
					; visible windows if some.
					((and (cdr wins) (eq helm-dwim-target 'completion))
						(helm-comp-read
							"Browse target starting from: "
							(delete-dups
								(delq nil
									(nconc
										(list (car helm-ff-history) default-directory)
										(mapcar
											(lambda (w)
												(buffer-local-value
													'default-directory
													(window-buffer w)))
											wins))))))
					; Use default-directory of next-window.
					((and (cdr wins) (eq helm-dwim-target 'next-window))
						(buffer-local-value 'default-directory
							(window-buffer (next-window))))
					; Always use default-directory when only one window.
					((and (not (cdr wins)) (eq helm-dwim-target 'default-directory))
						default-directory)
					; Use the visited directory.
					((not (and (cdr wins) helm-dwim-target))
						; Using the car of helm-ff-history allow staying
						; in the directory visited instead of current.
						(or (car helm-ff-history) default-directory)))))))

; Rsync

(defconst helm-rsync-mode-line t
"Used in global-mode-string by helm rsync to recognize mode-line construct.
Always t.")

(defvar helm-rsync-command-history nil)
(defvar helm-rsync-process-buffer "*helm-rsync*")

(defun helm-ff-kill-rsync-process-clear-mode-line ()
	; Restore the mode line when Rsync finishes.
	(assq-delete-all 'helm-rsync-mode-line global-mode-string)
	(force-mode-line-update t)
	nil)

; Currently unused.
(defun helm-ff-kill-rsync-process () "Kill rsync process." (interactive)
	(if-let ((process (get-buffer-process helm-rsync-process-buffer)))
		(progn
			(delete-process process)
			(kill-buffer helm-rsync-process-buffer)
			(message "Process \"%s\" killed." process)
			(helm-ff-kill-rsync-process-clear-mode-line))
		(message "No live rsync process found."))
	nil)

; Main action

(defvar helm-ff-confirm-override-done nil
"Non-nil if user confirmed overriding of selected file.")
(defun helm-ff-confirm-override (cand _action)
"Function for helm-source-confirm.
Confirm when selecting existing file (not a directory)."
	(setq helm-ff-confirm-override-done nil)
	(setq cand (helm-get-real cand))
	(and
		(not (string-suffix-p "/" cand))
		(file-exists-p cand)
		(progn
			(setq helm-ff-confirm-override-done t)
			"confirm override")))

(defun helm-dired-create-files
	(file_creator_fn operation_name files new_name_fn &optional marker_char)
"`dired-create-files' but with some small changes."
	(let (
		dired-create-files-failures
		failures
		skipped
		(success-count 0)
		(total (length files))
	)
		(let (to overwrite-query overwrite-backup-query) ; for dired-handle-overwrite
			(dolist (from files)
				; Position point on the current file -- this is useful if
				; handling a number of files to show where we're working at.
				(dired-goto-file from)
				(setq to (funcall new_name_fn from))
				(when (equal to from)
					(setq to nil)
					(dired-log "Cannot %s to the same file: %s"
						(downcase operation_name) from))
				(if (not to)
					(push (dired-make-relative from) skipped)
					(let (
						; must determine if FROM is marked before file-creator
						; gets a chance to delete it (in case of a move).
						(actual-marker-char
							(cond
								((integerp marker_char) marker_char)
								(marker_char (dired-file-marker from)))) ; slow
						overwrite
						dired-overwrite-confirmed ; for dired-handle-overwrite
					)
						; My addition: if user takes action on only 1 file
						; and confirms override of only 1 file then don't
						; ask him twice.
						(cond
							((and (= total 1) helm-ff-confirm-override-done)
								(setq overwrite t)
								(setq dired-overwrite-confirmed t))
							((file-exists-p to)
								(setq overwrite t)
								(setq dired-overwrite-confirmed
									(let (
										(help-form
											(format-message
												(substitute-command-keys
"Type \\`SPC' or \\`y' to overwrite file `%s',
\\`DEL' or \\`n' to skip to next,
\\`ESC' or \\`q' to not overwrite any of the remaining files,
\\`!' to overwrite all remaining files with no more questions.")
												to))
									)
										(dired-query
											'overwrite-query "Overwrite `%s'?" to)))))
						; Handle the `dired-copy-file' file-creator specially
						; When copying a directory to another directory or
						; possibly to itself or one of its subdirectories.
						; e.g "~/foo/" => "~/test/"
						; or "~/foo/" =>"~/foo/"
						; or "~/foo/ => ~/foo/bar/")
						; In this case the 'name-constructor' have set the destination
						; TO to "~/test/foo" because the old emacs23 behavior
						; of `copy-directory' was to not create the subdirectory
						; and instead copy the contents.
						; With the new behavior of `copy-directory'
						; (similar to the `cp' shell command) we don't
						; need such a construction of the target directory,
						; so modify the destination TO to "~/test/" instead of
						; "~/test/foo/".
						(let ((destname (file-name-directory to)))
							(and
								(file-directory-p from)
								(file-directory-p to)
								(eq file_creator_fn 'dired-copy-file)
								(setq to destname))
							; If DESTNAME is a subdirectory of FROM, not a symlink,
							; and the method in use is copying, signal an error.
							(and
								(eq (file-attribute-type (file-attributes destname)) t)
								(eq file_creator_fn 'dired-copy-file)
								(file-in-directory-p destname from)
								(error "Cannot copy `%s' into its subdirectory `%s'"
									from to)))
						; Check, that `dired-do-symlink' does not create symlinks
						; on different hosts.
						(and
							(eq file_creator_fn 'make-symbolic-link)
							(not (equal (file-remote-p from) (file-remote-p to)))
							(error "Cannot symlink `%s' to `%s' on another host" from to))
						(condition-case err
							(progn
								(funcall file_creator_fn
									from to dired-overwrite-confirmed)
								(when overwrite
									; If we get here, file-creator hasn't been aborted
									; and the old entry (if any) has to be deleted
									; before adding the new entry.
									(dired-remove-file to))
								(++ success-count)
								(message "%s: %d of %d" operation_name success-count total)
								(dired-add-file to actual-marker-char))
							(file-error ; FILE-CREATOR aborted
								(progn
									(push (dired-make-relative from) failures)
									(dired-log "%s: `%s' to `%s' failed:\n%s"
										operation_name from to (error-message-string err)))))))))
		(cond
			(dired-create-files-failures
				(setq failures (nconc failures dired-create-files-failures))
				(dired-log-summary
					(format
						(ngettext
							"%s failed for %d file in %d requests"
							"%s failed for %d files in %d requests"
							(length failures))
						operation_name (length failures) total)
					failures))
			(failures
				(dired-log-summary
					(format
						(ngettext
							"%s: %d of %d file failed"
							"%s: %d of %d files failed"
							total)
						operation_name (length failures) total)
					failures))
			(skipped
				(dired-log-summary
					(format
						(ngettext
							"%s: %d of %d file skipped"
							"%s: %d of %d files skipped"
							total)
						operation_name (length skipped) total)
					skipped))
			(t
				(message
					(ngettext
						"%s: %d file done"
						"%s: %d files done"
						success-count)
					operation_name success-count))))
	(dired-move-to-filename))

(defun helm-ff-do-action (action must_match)
"ACTION is a symbol that can be one of:
copy, rename, symlink, relsymlink, hardlink or backup.
When action is rsync, rsync command must be available.

`dired-create-destination-dirs' will be temporarily set to \\='always,
because this function already asks for confirmation when appropriate
before selecting nonexisting dirs."
	(let* (
		(rsync-options
			(and
				(eq action 'rsync)
				helm-current-prefix-arg
				(cdr
					(split-string-shell-command
						(read-string "Run rsync like this: "
							(mapconcat #'identity (cons "rsync" helm-rsync-options) " ")
							'helm-rsync-command-history)))))
		(files (helm-files-marked-candidates t))
		(cand (unless (cdr files) (helm-get-selection))) ; For preselection.
		(prefarg helm-current-prefix-arg)
		(helm-inhibit-move-to-first-candidate t)
		; If HFF is using a frame use a frame as well.
		(helm-actions-inherit-frame-settings t)
		helm::use_frame_if_more_than_n_windows
		(dest
			(helm-with-display-candidates
				(map_modify_list #'car (helm-files-highlight files))
				(helm-read-file-name
					(format "%s %d file%s %s: "
						(if
							(and
								dired-async-mode
								(not (eq action 'rsync))
								(not prefarg))
							(concat "Async " (symbol-name action))
							(capitalize (symbol-name action)))
						(length files)
						(if (cdr files) "s" "")
						(if (memq action '(symlink relsymlink hardlink))
							"from" "to"))
					:preselect
						(when cand
							(helm-ff-regex-for-preselection
								(helm-basename cand)))
					:input (helm-dwim-target-directory)
					:default cand
					:must-match must_match)))
		(dest-dir-p (file-directory-p dest))
		(dest-dir (if dest-dir-p dest (file-name-directory dest)))
	)
		(if (eq action 'rsync)
			(progn
				; Old helm-rsync-copy-files.
				; Send FILES to DEST using Rsync with rsync-options.
				(and
					(not dest-dir-p)
					(not (file-directory-p dest-dir))
					(y-or-n-p (format "Create directory \"%s\"?" dest-dir))
					(make-directory dest-dir t))
				(when (or dest-dir-p (file-directory-p dest-dir))
					(if (get-process "helm-rsync")
						(progn
							(message
"Rsync process \"helm-rsync\" is running, helm doesn't support multiple of them, which should be changed.")
							nil)
						(let (
							(remote_to_rsync
								(lambda (file)
									(if (file-remote-p file)
										(let (
											(localname
												(shell-quote-argument
													(directory-file-name
														(expand-file-name
															(file-remote-p file 'localname)))))
											(user (file-remote-p file 'user))
											; Tramp name may contain port e.g. /ssh:host#2222:/foo.
											(host
												(replace-regexp-in-string "#[0-9]+" ""
													(file-remote-p file 'host)))
										)
											(concat
												(when user (concat user "@"))
												host
												":"
												localname))
										(shell-quote-argument (directory-file-name file)))))
						)
							(map_modify_list remote_to_rsync files)
							(setq dest (funcall remote_to_rsync dest)))
						; Assume global-mode-string is something like this: '("" ...).
						; Setup mode-line construct for process' filter function
						; to append a progress bar string to.
						(unless (assq 'helm-rsync-mode-line global-mode-string)
							(nconc global-mode-string '(helm-rsync-mode-line nil)))
						(make-process
							:name "helm-rsync"
							:buffer helm-rsync-process-buffer
							:command
								(append
									(cons "rsync" (or rsync-options helm-rsync-options))
									(when-let* (
										; Add automatically port specified in tramp
										; name unless user already specified it himself
										; with the -e option by editing command.
										rsync-options
										((cl-loop
											for arg in rsync-options
											never (string-prefix-p "-e" arg)))
										(host (file-remote-p dest 'host))
										((string-match "#\\([0-9]+\\)" host))
									)
										(list
											"-e"
											(concat "ssh -p " (match-string 1 host))))
									files
									(list dest))
							:sentinel
								`(lambda (process event)
									(if (string= event "finished\n")
										(message "%s copied %s file(s)."
											(capitalize (process-name process))
											,(length files))
										(error "Process %s %s with code %s"
											(process-name process)
											(process-status process)
											(process-exit-status process)))
									(helm-ff-kill-rsync-process-clear-mode-line))
							:filter
								(lambda (process output)
									(let (
										(mode_line_string
											(let ((inhibit-read-only t) fname progbar)
												(with-current-buffer (process-buffer process)
													(when (string-match comint-password-prompt-regexp output)
														; FIXME: Fully not tested and use
														; an agent or auth-source or whatever
														; to get password if available.
														(process-send-string
															process
															(concat
																(read-passwd (match-string 0 output))
																"\n")))
													; Extract the progress bar.
													(with-temp-buffer
														(insert output)
														(when (re-search-backward "[[:cntrl:]]" nil t)
															(setq progbar
																(buffer-substring-no-properties
																	(match-end 0) (point-max)))))
													; Insert the text, advancing the process marker.
													(save-excursion
														(goto-char (process-mark process))
														(insert output)
														(set-marker (process-mark process) (point)))
													(goto-char (process-mark process))
													; Extract the file name currently
													; copied (Imply --info=all2 or all1).
													(save-excursion
														(when (re-search-backward "^[^[:cntrl:]]" nil t)
															(setq fname
																(helm-basename
																	(buffer-substring-no-properties
																		(point) (pos-eol))))))
													; Now format the string for the mode-line.
													; progbar == "          2,83G  92%   98,65MB/s    0:00:02  "
													(propertize_no_copy
														(let ((infos (split-string progbar " " t)) percent info)
															(if (eq helm-ff-rsync-progress-bar-style 'text)
																(mapconcat #'identity infos " ")
																(setq info
																	(let (
																		(size (pop infos))
																		(percent_ (pop infos))
																		(speed (car infos))
																		(remain (nth 1 infos))
																	)
																		(mapconcat
																			(lambda (x)
																				(cl-case x
																					(size size)
																					(percent percent_)
																					(speed speed)
																					(remain remain)))
																			(if (listp helm-ff-rsync-progress-bar-info)
																				helm-ff-rsync-progress-bar-info
																				(list helm-ff-rsync-progress-bar-info))
																			", ")))
																(when (string-match "\\([0-9]+\\)%" progbar)
																	(setq percent
																		(string-to-number (match-string 1 progbar))))
																(if percent
																	(concat
																		(propertize
																			(capitalize
																				(replace-regexp-in-string
																					"<\\([0-9]+\\)>" "(\\1)"
																					(process-name process)))
																			'display '(height 0.9)
																			'face 'helm-ff-rsync-progress-1)
																		(propertize " "
																			'display (list 'space :width (list percent))
																			'face 'helm-ff-rsync-progress-2)
																		(propertize " "
																			'display (list 'space :width (list (- 100 percent)))
																			'face 'helm-ff-rsync-progress-3)
																		(propertize info
																			'display '(height 0.9)
																			'face 'helm-ff-rsync-progress-1))
																	"")))
														'help-echo (format "%s->%s" (process-name process) fname)))))
									)
										; When rsync progress bar stop for some reason (e.g. rsync
										; takes time to finalize writing file to disk), no output is
										; coming from filter process, as a result the progress bar
										; disapear for a while giving no information to user while
										; the rsync process continues, so keep printing the last valid
										; progress bar.
										(unless (string= mode_line_string "")
											; Update global-mode-string.
											(setcar
												(cdr (assq 'helm-rsync-mode-line global-mode-string))
												(concat
													(if (eq helm-ff-rsync-progress-bar-style 'text)
														(concat
															"["
															(propertize mode_line_string
																'face 'helm-ff-rsync-progress)
															"]")
														mode_line_string)
													"  "))
											; Finally update mode-line in all windows.
											(force-mode-line-update t))))))))
			; Execute ACTION on FILES to dest.
			; Argument prefarg when non-nil specifies to follow FILES to
			; dest for the actions copy and rename.

			(when (get-buffer dired-log-buffer) (kill-buffer dired-log-buffer))
			; When default-directory in current-buffer is an invalid directory,
			; (e.g. buffer-file directory have been renamed somewhere else)
			; be sure to use a valid value to give to dired-create-file.
			; make-process is creating a process buffer based on default-directory.
			(let (
				(default-directory
					(cl-loop
						for b in (buffer-list)
						for cd = (buffer-local-value 'default-directory b)
						when (file-directory-p cd) return cd))
				(dirflag
					(and
						(not (cdr files))
						(file-directory-p (car files))
						(not dest-dir-p)))
				(dired-async-state (unless dired-async-mode -1))
			)
				(and prefarg dired-async-mode (dired-async-mode -1))
				(unwind-protect
					(let ((dired-create-destination-dirs 'always))
						(helm-dired-create-files
							(cl-case action
								(copy #'dired-copy-file)
								(rename #'dired-rename-file)
								(symlink #'make-symbolic-link)
								(relsymlink #'dired-make-relative-symlink)
								(hardlink #'dired-hardlink)
								(backup #'backup-file))
							(capitalize (symbol-name action))
							files
							; If dest is a directory, build filename in this
							; directory, else use dest.
							(if (string-suffix-p "/" dest)
								`(lambda (from)
									(expand-file-name (helm-basename from) ,dest))
								`(lambda (_from) ,dest))
							(cl-case action
								((copy rename backup) dired-keep-marker-copy)
								(symlink dired-keep-marker-symlink)
								(relsymlink dired-keep-marker-relsymlink)
								(hardlink dired-keep-marker-hardlink))))
					(dired-async-mode dired-async-state))
				(add-to-history 'helm-ff-history dest-dir)
				; If prefarg is non-nil we should not be in async mode.
				(and
					prefarg
					(not (memq action '(symlink relsymlink hardlink)))
					(not (get-buffer dired-log-buffer))
					(let (
						(target (substring-no-properties dest 0 -1))
						(cands-to-mark
							; Transform filenames of files to abs of dest.
							; At this point files have been renamed/copied.
							; That means dest exists.
							(cl-loop
								for src in files
								for fname =
									(cond
										(dirflag target)
										(dest-dir-p (concat dest (helm-basename src)))
										(t dest))
								when (file-exists-p fname) collect fname))
					)
						; Only mark candidates if there are more than 1,
						; else just preselect the one cand.
						(when (cdr cands-to-mark)
							(helm::call_after_next_update
								; Add visible mark to all candidates in cands-to-mark.
								`(lambda ()
									(unless (helm-empty-buffer-p)
										(let ((cands-to-mark ',cands-to-mark))
											(with-helm-window
												(while
													(progn
														(goto-char (point-min))
														(forward-line 1)
														(while
															(if
																(string=
																	(car cands-to-mark)
																	(helm-get-selection))
																(progn
																	(helm-make-visible-mark)
																	(setq cands-to-mark
																		(cdr cands-to-mark))
																	nil)
																(forward-line 1)
																(not (eobp))))
														(and cands-to-mark (not (eobp)))))
												(goto-char (point-min))
												(helm-next-visible-mark)))))))
						(if (and dirflag (eq action 'rename))
							(helm-ff-1
								(file-name-directory target)
								(helm-ff-regex-for-preselection
									(file-name-nondirectory target)))
							(helm-ff-1
								dest-dir
								(helm-ff-regex-for-preselection
									(helm-basename (car files)))))))))))

(defun helm-ff-rsync (_candidate) "Rsync files from `helm-ff'."
	(helm-ff-do-action 'rsync nil)) ; Idk if this nil is correct.
(defun helm-ff-copy (_candidate) "Copy files from `helm-ff'."
	(helm-ff-do-action 'copy #'helm-ff-confirm-override))
(defun helm-ff-rename (_candidate) "Rename files from `helm-ff'."
	(helm-ff-do-action 'rename #'helm-ff-confirm-override))
(defun helm-ff-symlink (_candidate) "Symlink files from `helm-ff'."
	(helm-ff-do-action 'symlink nil)) ; Same
(defun helm-ff-relsymlink (_candidate) "Relsymlink files from `helm-ff'."
	(helm-ff-do-action 'relsymlink nil)) ; Same
(defun helm-ff-hardlink (_candidate) "Hardlink files from `helm-ff'."
	(helm-ff-do-action 'hardlink nil)) ; Same
(defun helm-ff-backup (_candidate)
"Backup files from `helm-ff'.
This reproduce the behavior of \"cp --backup=numbered from to\"."
	(helm-ff-do-action 'backup nil)) ; Same

; File attributes

(defun helm-directory-size (directory recursive)
"Return [size file_count dir_count],
where size is summed size, file_count is a number of not dirs,
dir_count is a number of dirs.
Symlinks are not followed, so their size is very low."
	(let ((file_count 0) (dir_count 0))
		(vector
			(cl-loop
				for file in
					(if recursive
						(directory-files-recursively directory "")
						(directory-files
							directory t directory-files-no-dot-files-regexp t))
				for attributes = (file-attributes file)
				sum (file-attribute-size attributes)
				do
				(if (eq (file-attribute-type attributes) t)
					(++ dir_count) (++ file_count)))
			file_count
			dir_count)))

(defun helm-split-mode-file-attributes (modes &optional string)
"Split MODES in a list of modes.
MODES is same as what (file-attribute-modes (file-attributes \"foo\")) would return."
	(let* (
		(type (substring-no-properties modes 0 1))
		(user (substring-no-properties modes 1 4))
		(group (substring-no-properties modes 4 7))
		(other (substring-no-properties modes 7 10))
		(octal
			; Numeric representation of permissions.
			; `file-modes-symbolic-to-number' interpret its MODES argument as what would
			; result when calling such mode on a file with chmod, BTW we have to remove
			; all "-" like read-file-modes does.
			(format "%o"
				(file-modes-symbolic-to-number
					(string-replace "-" "" (format "u=%s,g=%s,o=%s" user group other)))))
	)
		(if string
			(concat " " type " " user " " group " " other " " octal)
			(list type user group other octal))))

; This could be extended to change other properties of file, like read-only,
; hidden, and maybe default program for this kind of file.
(defun helm-ff-chmod (_candidate)
"Set file mode on marked files.
If no mode is specified in prompt, default mode will be the mode of
the car of marked files i.e. the first marked file."
	(let* (
		(marked (helm-files-marked-candidates))
		(marked_length (length marked))
		(file_modes_vector (make-vector marked_length nil))
	)
		(if-let (
			(invalid_file
				(cl-loop
					for file in marked
					for i from 0
					for file_attributes = (file-attributes file)
					unless file_attributes return file
					do (aset file_modes_vector i (file-attribute-modes file_attributes))))
		)
			(message "Couldn't access \"%s\" attributes." invalid_file)
			(let* (
				(mode (read-file-modes nil (car marked)))
				(symbolic_mode (file-modes-number-to-symbolic mode))
				(first_index 0)
				(i -1)
			)
				(when (string= symbolic_mode (aref file_modes_vector 0))
					(setq marked (cdr marked))
					(++ marked_length)
					(++ first_index)
					(++ i))
				(cond
					((not marked)
						(message "Mode is the same as before - nothing to do."))
					((helm-with-display-candidates
							(map_modify_list
								(lambda (cand)
									(concat
										(car cand)
										"  "
										(aref file_modes_vector (++ i))))
								(helm-files-highlight marked))
							(y-or-n-p (format "Change file mode to %s?" symbolic_mode)))
						(dolist (file marked)
							; 'nofollow to not follow symlinks.
							(set-file-modes file mode 'nofollow))
						(if (cdr marked)
							(message "Changed file mode to (approximately) %s on %d files."
								symbolic_mode
								marked_length)
							(message "Changed file mode of \"%s\" from %s to %s."
								(helm-files-basename-with-slash (car marked))
								(aref file_modes_vector first_index)
								; If file's new attributes can be read (idk, they should
								; probably always be readable, but just to be safe).
								; This is because for example on Windows, changing "others"
								; permissions doesn't do anything (idk how emacs actually
								; implements this and for now idc).
								(if-let ((file_attributes (file-attributes (car marked))))
									(file-attribute-modes file_attributes)
									(concat "(approximately) " symbolic_mode)))))
					(t (message "Operation aborted.")))))))

(defun helm-ff-other-window (_candidate)
"Keep current-buffer and open files in separate windows."
	(helm-window-show-buffers
		(map_modify_list #'find-file-noselect (helm-files-marked-candidates))
		t))

(defun helm-ff-other-frame (candidate)
	(find-file-other-frame (helm-files-expand-and-save-to-history candidate)))

(defun helm-ff-view-file (candidate)
	(view-file (helm-files-expand-and-save-to-history candidate)))

(defun helm-ff-insert-file (candidate)
	(insert-file (helm-files-expand-and-save-to-history candidate)))

(defun helm-ff-hexl-find-file (candidate)
	(hexl-find-file (helm-files-expand-and-save-to-history candidate)))

(defun helm-ff-find-alternate-file (candidate)
	(find-alternate-file (helm-files-expand-and-save-to-history candidate)))

(defun helm-ff-ediff-files-1 (candidate &optional merge)
"Generic function to ediff/merge files."
	(setq candidate (expand-file-name candidate))
	(let (
		(helm-dwim-target 'next-window)
		(bname (helm-basename candidate))
		(marked (helm-files-marked-candidates t))
		(fn (if merge #'ediff-merge-files #'ediff-files))
	)
		(if (length= marked 2)
			(funcall fn (car marked) (nth 1 marked))
			(helm-files-save-to-file-name-history candidate)
			(funcall fn
				candidate
				(helm-read-file-name
					(concat
						"Ediff "
						(when merge "merge ")
						"\""
						bname
						"\" with file: ")
					:input (helm-dwim-target-directory)
					:preselect (helm-ff-regex-for-preselection bname)
					; I added these.
					:must-match t
					:type 'file)))))

(defun helm-ff-ediff-files (candidate) (helm-ff-ediff-files-1 candidate))
(defun helm-ff-ediff-merge-files (candidate) (helm-ff-ediff-files-1 candidate t))

(defun helm-ff-grep (_candidate)
	(with-helm-default-directory helm-ff-default-directory
		(helm-grep (helm-files-marked-candidates t))))

(defun helm-ff-rg (_candidate)
	(with-helm-default-directory helm-ff-default-directory
		(helm-grep-rg (helm-files-marked-candidates t))))

(defun helm-ff-git-grep (_candidate)
"Default action to git-grep `helm-ff-default-directory'."
	(with-helm-default-directory helm-ff-default-directory
		(helm-files-save-to-file-name-history default-directory)
		(helm-grep-git-1 default-directory helm-current-prefix-arg)))


(defvar helm-ff-zgrep-cache (make-hash-table :test 'equal))
(defun helm-ff-zgrep (_candidate)
"Default action to zgrep files from `helm-ff'."
	(with-helm-default-directory helm-ff-default-directory
		(helm-grep
			(mapc
				#'helm-files-save-to-file-name-history
				(or
					(gethash default-directory helm-ff-zgrep-cache)
					(puthash
						default-directory
						(helm-walk-directory
							default-directory
							:directories nil
							:path 'full
							:match helm-zgrep-file-extension-regexp)
						helm-ff-zgrep-cache)))
			'zgrep)))

; Helm interface for pdfgrep
; pdfgrep program <http://pdfgrep.sourceforge.net/>
; and a pdf-reader (e.g xpdf) are needed.

(defvar helm-pdfgrep-targets)

(defconst helm-pdfgrep-source
	(helm-source-grep-make nil
		:name "PdfGrep"
		:candidates
			(lambda ()
				(let (
					(process
						(let (process-connection-type)
							(start-file-process-shell-command
								"helm-pdfgrep"
								nil
								(concat
									helm-pdfgrep-exe
									(mapconcat
										(append
											helm-pdfgrep-options
											(list helm-pattern)
											helm-pdfgrep-targets)
										#'shell-quote-argument
										" ")))))
				)
					(message nil) ; Idk if this is needed.
					(set-process-sentinel process (helm-async-get-default-sentinel))
					process))
		:action
			(lambda (candidate)
				(helm-files-save-to-file-name-history candidate)
				(unless (string= helm-pattern "")
					(add-to-history 'helm-grep-history helm-pattern))
				(let (
					(file
						(concat
							; Tramp fname.
							(file-remote-p (helm-default-directory))
							(helm-grep-cand-file candidate)))
					(line_number
						(string-to-number (helm-grep-cand-line-number candidate)))
				)
					(if helm-pdfgrep-read-fn
						(funcall helm-pdfgrep-read-fn file line_number)
						(find-file file)
						(if (derived-mode-p 'pdf-view-mode)
							(pdf-view-goto-page line_number)
							(doc-view-goto-page line_number)))))
		:nomark t))

(defun helm-ff-pdfgrep (_candidate) "Action to pdfgrep files from `helm-ff'."
	(with-helm-default-directory helm-ff-default-directory
		(setq helm-pdfgrep-targets
			(cl-delete-if
				(lambda (file)
					(not
						(or
							(file-directory-p file)
							(member (file-name-extension file) '("pdf" "PDF")))))
				(helm-files-marked-candidates t)))
		(helm
			:sources (list helm-pdfgrep-source)
			:history 'helm-grep-history
			:helm-delay helm-grep-input-idle-delay)))


(defun helm-ff-etags (_candidate) "Action to jump to etags from `helm-ff'."
	(when helm-ff-default-directory
		(helm-files-save-to-file-name-history helm-ff-default-directory))
	(let ((default-directory helm-ff-default-directory))
		(helm-etags helm-current-prefix-arg)))

(defun helm-ff-switch-to-shell (_candidate)
"Switch to a shell buffer and cd to `helm-ff-default-directory'.
Set your preferred shell mode in `helm-ff-preferred-shell-mode'.

With a numeric prefix arg switch to numbered shell buffer, if no
prefix arg provided and more than one shell buffer exists, provide
completions on those buffers. If only one shell buffer exists,
switch to this one, if no shell buffer exists or if the numeric
prefix arg shell buffer doesn't exists, create it and switch to it."
	(when helm-ff-default-directory
		(helm-files-save-to-file-name-history helm-ff-default-directory))
	; Jump to a shell buffer or open a new session.
	(if-let (
		((not helm-current-prefix-arg))
		(buffer
			(let (
				(bufs
					(cl-loop
						for b in (buffer-list)
						when
							(with-current-buffer b
								(when (eq major-mode helm-ff-preferred-shell-mode)
									(save-excursion
										(goto-char (point-min))
										(cl-case major-mode
											(shell-mode (comint-next-prompt))
											(eshell-mode (eshell-next-prompt))
											(term-mode (term-next-prompt)))
										(/= (point) (point-min)))))
							collect (buffer-name b)))
			)
				(if (cdr bufs)
					(helm-comp-read "Switch to shell buffer: " bufs :must-match t)
					(car bufs))))
	)
		; Display in same window by default to preserve the
		; historical behaviour.
		(pop-to-buffer buffer '(display-buffer-same-window))
		(cl-case helm-ff-preferred-shell-mode
			(eshell-mode (eshell helm-current-prefix-arg))
			(shell-mode
				(shell
					(when helm-current-prefix-arg
						(format "*shell<%s>*"
							(prefix-numeric-value helm-current-prefix-arg)))))
			(term-mode
				(ansi-term (getenv "SHELL")
					(when helm-current-prefix-arg
						(format "*ansi-term<%s>*"
							(prefix-numeric-value helm-current-prefix-arg))))
				(term-line-mode))))
	; Maybe wait for startup.
	(when-let (
		((memq major-mode '(shell-mode term-mode)))
		(process (get-buffer-process (current-buffer)))
	)
		; For shell-mode on windows it usually takes around 0.15 secs to startup.
		(accept-process-output process 0.5))
	(unless
		; Return non-nil when a process is running inside `shell-mode' buffer.
		(cl-case major-mode
			(eshell-mode (get-buffer-process (current-buffer)))
			(shell-mode
				(save-excursion
					(comint-goto-process-mark)
					(or
						(not comint-last-prompt)
						(/= (point) (cdr comint-last-prompt)))))
			(term-mode
				(save-excursion
					(goto-char (term-process-mark))
					(not (looking-back "\\$ " (- (point) 2))))))
		(if (eq major-mode 'eshell-mode)
			(progn
				(eshell/cd helm-ff-default-directory)
				(eshell-reset))
			(goto-char (point-max))
			(when (eq helm-ff-preferred-shell-mode 'shell-mode)
				(comint-delete-input))
			(insert
				"cd "
				(shell-quote-argument
					(or
						(file-remote-p helm-ff-default-directory 'localname)
						helm-ff-default-directory)))
			(cl-case helm-ff-preferred-shell-mode
				(shell-mode (comint-send-input))
				(term-mode (term-char-mode) (term-send-input))))))

(defun helm-ff-touch-files (_candidate) "The touch files action for helm-ff."
	(when-let (
		(failures
			(cl-loop
				with split =
					(cl-loop
						for f in (helm-files-marked-candidates)
						for spt =
							(unless helm-current-prefix-arg
								(cons (file-name-directory f) (split-string f ", ?")))
						if spt
							append
								(let ((dir (car spt)))
									(mapcar
										(lambda (ff) (expand-file-name ff dir))
										(cdr spt)))
						else collect f)
				with default-directory = helm-ff-default-directory
				for f in split
				for file = (or (file-remote-p f 'localname) f)
				when
					(>
						(process-file
							"touch" nil nil nil "-d"
							; Timestamp
							(helm-comp-read
								"Timestamp: "
								(cl-loop
									for f in split
									for date =
										(when-let ((attributes (file-attributes f)))
											(format-time-string
												"%Y-%m-%d %H:%M:%S"
												(file-attribute-modification-time
													attributes)))
									when date
										collect
											(cons
												(format "%s: %s" (helm-basename f) date)
												date))
								:default (format-time-string "%Y-%m-%d %H:%M:%S"))
							file)
						0)
					collect f))
	)
		(message "Failed to touch %s files:\n%s"
			(length failures)
			(mapconcat (lambda (f) (format "- %s\n" f)) failures))))

(defun helm-ff-serial-rename-action (fn)
"Call FN with each non-directory marked file modified to end with a number
as first arg and existing directory ending with slash chosen by user
as the second arg.

FN will usually be `rename-file'.

Extension can be an empty string to reuse original file's extension.

Example:
marked files - (\"C:/foo.el\" \"C:/foo/bar.c\" \"C:/\")
user chosen name - \"asd\"
user chosen starting number - 1
user chosen extension - \"ext\"
user chosen directory - \"D:/qwe/\"
FN - `rename-file'
result:
	\"C:/foo.el\" renamed to \"D:/qwe/asd1.ext\"
	\"C:/foo/bar.c\" renamed to \"D:/qwe/asd2.ext\"
	\"C:/\" ignored because it is a directory.
If user chosen extension would be \"\", then files would not have
their extension changed, e.g. \"C:/foo.el\" -> \"D:/qwe/asd1.el\"."
	(let* (
		; Remove directories selected by error.
		(candidates
			(cl-delete-if
				(lambda (f) (string-suffix-p "/" f))
				(helm-files-marked-candidates t)))
		(first_cand (car candidates))
		(name
			(read-string "New name: "
				(replace-regexp-in-string "[0-9]+\\'" "" (file-name-base first_cand))))
		(start (read-number "Start at number: " 1))
		(extension (read-string "Extension (empty string to use original extension): "))
		(dir
			; This will end with a slash.
			(helm-read-file-name "Serial rename to directory: "
				:input helm-ff-default-directory
				:test (lambda (file) (string-suffix-p "/" file))
				:type 'dir))
		done
	)
		(unless (file-exists-p dir) (make-directory dir t))
		; Change extension to nil or to one beginning with ".".
		(cond
			((string= extension "") (setq extension nil))
			((not (string-prefix-p "." extension))
				(setq extension (concat "." extension))))
		(helm-with-display-candidates
			(map_modify_list #'car (helm-files-highlight candidates))
			(when
				(y-or-n-p
					(format "Rename %d file%s to %s like this?\n%s -> %s%d%s"
						(length candidates)
						(if (length= candidates 1) "" "s")
						dir
						(file-name-nondirectory first_cand)
						name
						start
						(or extension (file-name-extension first_cand t))))
				; Rename start at number start - prefixname1.jpg.
				; extension is the file extension to use.
				; In empty prompt reuse the original extension of file.
				(dolist (cand candidates)
					(funcall fn
						cand
						(concat
							dir
							name
							(number-to-string start)
							(or extension (file-name-extension cand t))))
					(++ start))
				(setq done t)
				(message nil)))
		(if done
			(helm-ff-1 dir)
			(message "Operation aborted."))))

(defun helm-ff-serial-rename (_candidate)
"Serial rename marked files to some directory.
One prefix to copy instead of rename,
two prefixes to symlink instead of rename."
	(helm-ff-serial-rename-action
		(cond
			((eq helm-current-prefix-arg '(16)) #'make-symbolic-link)
			((eq helm-current-prefix-arg '(4)) #'copy-file)
			(t #'rename-file))))

(defvar helm-ff-query-replace-history-from nil)
(defvar helm-ff-query-replace-history-to nil)
(defun helm-ff-query-replace-filenames (_candidate)
"Query replace on filenames of marked filenames.
This doesn't replace inside the files, only modify filenames."
	(let ((candidates (helm-files-marked-candidates t)))
		(helm-with-display-candidates
			(map_modify_list #'car (helm-files-highlight candidates))
			(catch 'user-answered-q
				(let* (
					(regex
						(read-string "Replace regex on filename(s): "
							nil
							'helm-ff-query-replace-history-from
							(helm-basename (car candidates))))
					(rep
						(read-string (format "Replace regex \"%s\" with: " regex)
							nil 'helm-ff-query-replace-history-to))
					(query ?y)
					(count 0)
				)
					(dolist (old candidates)
						(let (
							(new
								; Setup a new name for OLD replacing part matching REGEX with REP.
								; COUNT is used for incrementing new name if needed.
								(let (subexp target)
									(setq regex
										(let (
											(prepare_str_with_regex
												(lambda (str &optional rep1 rep2)
													; Prepare STR when REGEXP is specified as substring e.g %:1:3
													; in this case substring from 1 to 3 in STR will be enclosed
													; with parenthesis to match this substring as a subexp e.g
													; %:1:3 on string "emacs" will be replaced by "e\\(ma\\)cs"
													; using subexp 1 like this:
													; (helm--replace-regexp-in-buffer-string
													; "e\\(ma\\)cs" "fo" "emacs" nil t 1)
													; => "efocs"
													;      ^^
													; Where "1" and "3" will be strings extracted with
													; match-string from regexp and refered respectively
													; in this function as REP1 and REP2.
													(let (
														(from (or (and rep1 (string-to-number rep1)) 0))
														(to (or (and rep2 (string-to-number rep2)) (length str)))
													)
														(concat
															(when (/= from 0)
																(regexp-quote (substring str 0 from)))
															"\\(" (regexp-quote (substring str from to)) "\\)"
															(when (/= to (length str))
																(regexp-quote (substring str to (length str))))))))
										)
											(save-match-data
												(cond
													((string= regex "%.")
														(setq subexp 1)
														(funcall prepare_str_with_regex
															(setq target (helm-basename old t))))
													((string= regex ".%")
														(setq subexp 1)
														(funcall prepare_str_with_regex
															(setq target (file-name-extension old))))
													((string= regex "%")
														(regexp-quote (setq target (helm-basename old))))
													((string-match "%:\\([0-9]+\\):\\([0-9]+\\)" regex)
														(setq subexp 1)
														(let (
															(beg (match-string 1 regex))
															(end (match-string 2 regex))
															(str (helm-basename old))
														)
															(setq target
																(substring
																	str
																	(string-to-number beg)
																	(string-to-number end)))
															(funcall prepare_str_with_regex str beg end)))
													(t regex)))))
									(concat
										(file-name-directory old)
										(let (
											(replacement
												(save-match-data
													(cond
														; Handle incremental replacement with \# in
														; search and replace feature in placeholder \@.
														((string-match
																"\\\\@/\\(.*\\)/\\(\\(.*\\)\\\\#\\)/"
																rep)
															(replace-regexp-in-string
																(match-string 1 rep)
																(concat
																	(match-string 3 rep)
																	(format "%03d" (1+ count)))
																target))
														; Incremental replacement before or after \@.
														((and
																(string-match "\\\\#" rep)
																(string-match "\\\\@" rep))
															(replace-regexp-in-string
																"\\\\#"
																(format "%03d" (1+ count))
																(replace-match target t t rep)))
														; Simple incremental replacement.
														((string-match "\\\\#" rep)
															(replace-match (format "%03d" (1+ count)) t t rep))
														; Substring replacement in placeholder.
														((string-match "\\\\@:\\([0-9]*\\):\\([0-9]*\\)" rep)
															(replace-match
																(substring
																	target
																	(string-to-number (match-string 1 rep))
																	(let ((matched_str (match-string 2 rep)))
																		(if (string= matched_str "")
																			(length target)
																			(string-to-number matched_str))))
																t t rep))
														; Search and replace in placeholder.
														; Doesn't handle incremental here.
														((string-match "\\\\@/\\(.*\\)/\\(.*\\)/" rep)
															(replace-match
																(replace-regexp-in-string
																	(match-string 1 rep)
																	(match-string 2 rep)
																	target
																	t)
																t t rep))
														; Simple replacement by placeholder.
														((string-match "\\\\@" rep)
															(replace-match target t t rep))
														; Replacement with upcase,
														; downcase or capitalized text.
														((string= rep "%u") #'upcase)
														((string= rep "%d") #'downcase)
														((string= rep "%c") #'capitalize)
														; Simple replacement with
														; whole replacement regexp.
														(t rep))))
											(str (helm-basename old))
										)
											; Replace REGEX by REP in string STR.
											;
											; Same as `replace-regexp-in-string' but handle properly REP as
											; function with SUBEXP specified.
											;
											; E.g.:
											;
											;     (helm--replace-regexp-in-buffer-string
											;      "e\\(m\\)acs" 'upcase "emacs" t nil 1)
											;     => "eMacs"
											;
											;     (replace-regexp-in-string
											;      "e\\(m\\)acs" 'upcase "emacs" t nil 1)
											;     => "eEMACSacs"
											;
											; Also START argument behaves as expected unlike
											; `replace-regexp-in-string'.
											;
											; E.g.:
											;
											;     (helm--replace-regexp-in-buffer-string "f" "r" "foofoo" t nil nil 3)
											;     => "fooroo"
											;
											;     (replace-regexp-in-string "f" "r" "foofoo" t nil nil 3)
											;     => "roo"
											;
											; Unlike `replace-regexp-in-string' this function is buffer-based
											; implemented i.e. replacement is computed inside a temp buffer, so
											; REGEXP should be used differently than with `replace-regexp-in-string'.
											(with-temp-buffer
												(insert str)
												(goto-char 1)
												(while (re-search-forward regex nil t)
													(replace-match
														(if (functionp replacement)
															(funcall replacement
																(if subexp
																	(match-string subexp)
																	str))
															replacement)
														t nil nil subexp))
												(buffer-string))))))
						)
							; If `regex' is not matched in `old'
							; `replace-regexp-in-string' will return `old' unmodified.
							(unless (string= old new)
								(when (file-exists-p new)
									(setq new
										(concat
											(file-name-sans-extension new)
											"("
											(number-to-string count)
											")"
											(file-name-extension new t))))
								(when (/= query ?!)
									(setq query
										(helm-read-answer
											(format "Rename \"%s\" to \"%s\"? [y,n,!,q]"
												(helm-basename old)
												(helm-basename new))
											"yn!q")))
								(when (= query ?q)
									(message "Operation aborted.")
									(throw 'user-answered-q nil))
								(when (/= query ?n)
									(rename-file old new)
									(++ count)))))
					(message "Renamed %d file%s." count (if (= count 1) "" "s"))))))
	; This fix the emacs bug where "Emacs-Lisp:" is sent
	; in minibuffer (not the echo area).
	(sit-for 0.1)
	(with-current-buffer (window-buffer (minibuffer-window))
		(delete-minibuffer-contents)))

(defun helm-ff-query-replace (_candidate &optional regex)
	(helm-buffers-query-replace-1
		regex
		(map_modify_list
			(lambda (f) (buffer-name (find-file-noselect f)))
			(helm-files-marked-candidates t))))

(defun helm-ff-query-replace-regex (_candidate) (helm-ff-query-replace nil 'regexp))

(defun helm-ff-delete-files (candidate) "Delete files default action."
	(funcall helm-ff-delete-files-function candidate))

(defun helm-ff-locate (candidate)
"Locate action function for `helm-ff'."
	(helm-locate-1
		helm-current-prefix-arg
		t
		(concat (helm-basename (helm-files-expand-and-save-to-history candidate)) " -b")))

(defun helm-ff-insert-as-org-link (candidate)
	(helm-files-expand-and-save-to-history candidate)
	(insert "[[" candidate "][]]")
	(forward-char -2))

(defvar lpr-printer-switch)
(defun helm-ff-print (_candidate)
"Print marked files.

You may to set in order variables `lpr-command',`lpr-switches'
and/or `printer-name', but with no settings Helm should detect
your printer(s) and print with the default `lpr' settings.

NOTE: DO NOT set the \"-P\" flag in `lpr-switches'.
If you really have to modify this, do it in `lpr-printer-switch'.

Same as `dired-do-print' but for Helm."
	(require 'lpr)
	(let* (
		(candidates (helm-files-marked-candidates t))
		(printer-name
			(if helm-ff-printer-list
				(helm-comp-read "Printer: " helm-ff-printer-list :must-match t)
				printer-name))
		(lpr-switches
			(if (and (stringp printer-name) (not (string= "" printer-name)))
				(cons (concat lpr-printer-switch " " printer-name) lpr-switches)
				lpr-switches))
		(command
			(read-string
				(format "Print %d file(s):\n%swith: "
					(length candidates)
					(mapconcat (lambda (f) (format "- %s\n" f)) candidates))
				(and
					lpr-command
					lpr-switches
					(mapconcat #'identity
						(cons
							lpr-command
							(if (stringp lpr-switches)
								(list lpr-switches)
								lpr-switches))
						" "))))
	)
		(if command
			(start-process-shell-command "helm-print" nil
				(concat command " " (mapconcat #'shell-quote-argument candidates " ")))
			(message "Error: Please verify your printer settings in Emacs."))))

(defun helm-ff-checksum (file)
"Calculate the checksum of FILE.
The checksum is copied.
Checksum is calculated with the md5sum, sha1sum, sha224sum,
sha256sum, sha384sum and sha512sum when available, otherwise the
Emacs function `secure-hash' is used but it is slow and may crash
Emacs and even the whole system as it eats all memory."
	(setq file (helm-files-expand-and-save-to-history file))
	(if (not (file-regular-p file))
		(message "\"%s\" is not a regular file." file)
		(let* (
			(algorithm_name
				(helm-comp-read
					"Algorithm: "
					'("md5" "sha1" "sha224" "sha256" "sha384" "sha512")
					:must-match t))
			(cmd (concat algorithm_name "sum"))
			(bn (helm-basename file))
			proc
		)
			(message "Calculating %s checksum for \"%s\"..." algorithm_name bn)
			(if (not (executable-find cmd))
				(async-start
					`(lambda ()
						(with-temp-buffer
							(insert-file-contents-literally ,file)
							(secure-hash (intern ,algorithm_name) (current-buffer))))
					`(lambda (sum)
						(clipboard-add sum ,helm-buffer)
						(message "Calculating %s checksum for \"%s\" done."
							,algorithm_name ,bn)))
				(setq proc (start-file-process cmd nil cmd "-b" file))
				(set-process-filter proc
					`(lambda (_process output) (clipboard-add output ,helm-buffer)))
				(set-process-sentinel proc
					`(lambda (_process event)
						(when (string= event "finished\n")
							(message "Calculating %s checksum for \"%s\" done."
								,algorithm_name ,bn))))))))

(defun helm-ff-mark-similar-files ()
"Mark all files similar to selection.
Files are considered similar if they have the same face and extension."
	(interactive)
	(with-helm-window
		(let* (
			(src (helm-get-current-source))
			(file (helm-get-selection 'withprop))
			(face (get-text-property 3 'face file))
			(ext (file-name-extension (get-text-property 0 'helm-real file)))
		)
			(helm-mark-some-toggle
				(lambda ()
					(let ((cand (helm-get-selection 'withprop)))
						(and
							(equal (get-text-property 3 'face cand) face)
							(equal
								ext
								(file-name-extension
									(get-text-property 0 'helm-real cand))))))))))


(defun helm-ff-up-one-level ()
"If helm-pattern ends with a slash and it seems to be a directory (not url),
go up one level, otherwise recall bind for keys that invoked this command.
Suitable for A-backspace or similar keys.
Safe when no source is current."
	(interactive)
	(cond
		((or (string= helm-pattern "") (< (point) (minibuffer-prompt-end))) nil)
		; Protect against situation when minibuffer prompt ends with "/".
		((= (point) (minibuffer-prompt-end))
			(delete-region (point) (progn (skip-chars-forward "^/") (point)))
			(funcall after_move_hook_fn current_column))
		; Windows drives.
		((and
				(eq system-type 'windows-nt)
				(string-match "\\`[A-Za-z]\\(:/?\\|:?\\)\\'" helm-pattern))
			; Up one level to Windows drive listing.
			(setq helm-ff--show-thumbnails nil)
			(let ((old_pattern helm-pattern))
				(helm-set-pattern "" t)
				(funcall after_move_hook_fn)
				(helm-force-update
					nil
					(when (= (length old_pattern) 3)
						(string ?^ (aref old_pattern 0) ?:)))))
		; If helm-pattern doesn't look like a filepath, recall normal keybinding.
		((or
				(string-match helm-ff-url-regexp helm-pattern)
				(helm-ff--invalid-tramp-name-p))
			; Current source's keymap is enabled through helm--minor-mode-map-cons,
			; so set it to default value temporarily.
			(let ((current_helm_map (cdr helm--minor-mode-map-cons)))
				(setcdr helm--minor-mode-map-cons helm-map)
				(setq this-command (key-binding (this-command-keys-vector) t))
				(call-interactively this-command)
				(setcdr helm--minor-mode-map-cons current_helm_map)))
		((eq (char-before) ?/)
			; Delete region back to next slash or minibuffer-prompt-end.
			; E.g. C:/asd/|zxc/ -> C:/|zxc/
			(let ((end (point)))
				(forward-char -1)
				(skip-chars-backward "^/" (minibuffer-prompt-end))
				(delete-region (point) end))
			(funcall after_move_hook_fn)
			(setq helm-pattern (minibuffer-contents-no-properties))
			; This is completely optional.
			(setf
				(helm-source-follow
					(buffer-local-value 'helm-default-source (get-buffer helm-buffer)))
				nil)
			(setq helm-ff--show-thumbnails
				(member helm-pattern helm-ff--thumbnailed-directories))
			(helm-force-update
				nil
				(when helm-ff-default-directory
					(let (
						(last_default_dir
							(file-name-nondirectory
								(substring-no-properties helm-ff-default-directory 0 -1)))
					)
						(unless (string= last_default_dir "")
							(helm-ff-regex-for-preselection last_default_dir))))))
		(t
			; Delete region between 2 slashes.
			(let ((end (save-excursion (skip-chars-forward "^/") (point))))
				(skip-chars-backward "^/" (minibuffer-prompt-end))
				(delete-region
					(point)
					; When surrounded by slashes, delete one of them.
					(if (and (eq (char-before) ?/) (eq (char-after end) ?/))
						(1+ end) end)))
			(funcall after_move_hook_fn)
			(setq helm-pattern (minibuffer-contents-no-properties))
			(helm-force-update))))

(defun helm-files-up-one-level ()
"Like `helm-ff-up-one-level' but for helm-source-files."
	(interactive)
	(cond
		((or (string= helm-pattern "") (< (point) (minibuffer-prompt-end))))
		; Protect against situation when minibuffer prompt ends with "/".
		((= (point) (minibuffer-prompt-end))
			(delete-region (point) (progn (skip-chars-forward "^/") (point)))
			(funcall after_move_hook_fn))
		((eq (char-before) ?/)
			; Actually go up one level.
			; Delete region back to previous slash or minibuffer-prompt-end.
			; E.g. C:/asd/|zxc/ -> C:/|zxc/
			(let ((end (point)))
				(forward-char -1)
				(skip-chars-backward "^/" (minibuffer-prompt-end))
				(delete-region (point) end))
			(funcall after_move_hook_fn))
		(t
			; Delete region between 2 slashes.
			(let ((end (save-excursion (skip-chars-forward "^/") (point))))
				(skip-chars-backward "^/" (minibuffer-prompt-end))
				(delete-region
					(point)
					; When surrounded by slashes, delete one of them.
					(if (and (eq (char-before) ?/) (eq (char-after end) ?/))
						(1+ end) end)))
			(funcall after_move_hook_fn))))


(defun helm-ff-marked-files-in-dired (_candidate)
"Open a dired buffer with only marked files.
With a prefix arg toggle dired buffer to wdired mode."
	(let ((marked (helm-marked-candidates t)))
		(unless (string-match helm-ff-url-regexp (car marked))
			(map_modify_list #'helm-files-expand-and-save-to-history marked)
			(dired (cons helm-ff-default-directory marked))
			(dired-goto-file (car marked))
			(when (or helm-current-prefix-arg current-prefix-arg)
				(call-interactively #'wdired-change-to-wdired-mode)))))

(defun helm-ff-wfnames (_candidate) "Edit marked fnames with `Wfnames' package."
	(wfnames-setup-buffer
		(helm-files-marked-candidates t)
		#'switch-to-buffer
		(buffer-live-p (get-buffer wfnames-buffer))))

(defun helm-ff-edit-marked-files (candidate)
"Edit marked files with `helm-ff-edit-marked-files-fn'."
	(funcall helm-ff-edit-marked-files-fn candidate))

(defun helm-ff--tramp-hostnames ()
"Get a list of hosts for tramp method found in `helm-pattern'."
	(and
		(fboundp 'tramp-get-completion-function)
		(string-match helm-ff-tramp-file-name-regexp helm-pattern)
		(helm-fast-remove-dups
			(let* (
				(mh-method
					(save-match-data
						(with-temp-buffer
							(insert helm-pattern)
							(when
								(re-search-backward
									(concat
										"\\(|\\)\\("
										(mapconcat
											#'identity helm-ff--tramp-methods "\\|")
										"\\):")
									nil
									t)
								(cons
									(buffer-substring-no-properties
										(pos-bol) (match-beginning 2))
									(buffer-substring-no-properties
										(match-beginning 2) (match-end 2)))))))
				(method (or (cdr mh-method) (match-string 1 helm-pattern)))
			)
				(mapcan
					(lambda (fn_and_host_list)
						(cl-loop
							for e in
								(funcall (car fn_and_host_list)
									(nth 1 fn_and_host_list))
							for host = (nth 1 e)
							for cand =
								(and
									; On emacs-27 host may be ("root" t) in sudo method.
									(stringp host)
									(not (member host helm-ff--tramp-methods))
									(helm-ff-filter-candidate-one-by-one
										(concat
											(or (car mh-method) "/")
											method
											":"
											host)))
							when cand collect cand))
					(tramp-get-completion-function method))))))

(defun helm-ff-cleanup ()
	(helm-ff-image-clean-cache)
	(clrhash helm-ff--list-directory-cache)
	(setq
		helm-ff--show-thumbnails nil
		helm-ff--thumbnailed-directories nil)
	; Add 'helm-ff-default-directory' to 'helm-ff-history'.
	(and
		helm-ff-default-directory
		(file-directory-p helm-ff-default-directory)
		(add-to-history 'helm-ff-history helm-ff-default-directory)))

(defun helm-ff--invalid-tramp-name-p (&optional pattern)
"Return non-nil when PATTERN is an invalid tramp filename.
pattern defaults to helm-pattern."
	(unless pattern (setq pattern helm-pattern))
	(or
		(not (helm-ff-set-pattern pattern))
		; Tramp methods completion.
		(string-match helm-ff-tramp-method-regexp pattern)))

(defun helm-ff--tramp-postfixed-p (str)
"Return non-nil when tramp path STR is complete."
	; E.g.:
	; (helm-ff--tramp-postfixed-p "/ssh:foo")
	; => nil
	; (helm-ff--tramp-postfixed-p "/ssh:foo:")
	; => 10
	; (helm-ff--tramp-postfixed-p "/ssh:foo|sudo:")
	; => nil
	; (helm-ff--tramp-postfixed-p "/ssh:foo|sudo::")
	; => 16
	(let (result)
		(with-temp-buffer
			(insert str)
			(goto-char 1)
			(while (search-forward ":" nil t)
				(setq result
					(when
						(save-excursion
							(forward-char -1)
							(or
								(looking-back "[/|]" (1- (point)))
								(looking-back
									(mapconcat
										(lambda (m) (format "[/|]%s" m))
										helm-ff--tramp-methods
										"\\|")
									(pos-bol))))
						(point)))))
		result))

(defun helm-ff--tramp-multihops-p (name)
	(cl-loop
		for m in helm-ff--tramp-methods
		thereis (string-match (concat "\\`/" m ":.*|") name)))

(defun helm-ff-complete-tramp-methods ()
"Completion on tramp methods in a nested helm session."
	(interactive)
	(helm-set-pattern
		(let (input)
			(concat
				(with-temp-buffer
					(insert helm-pattern)
					(let ((end (point)) beg)
						(when (re-search-backward "[/|]" nil t)
							(setq beg (1+ (point)))
							(when (/= beg end)
								(setq input (buffer-substring beg end))
								(delete-region beg end))
							(buffer-string))))
				(helm-comp-read
					"Tramp methods: "
					(mapcar
						(lambda (c) (propertize c 'face 'helm-ff-file))
						helm-ff--tramp-methods)
					:input input
					:default input
					:allow-nest t
					:must-match t)
				":"))))

(defun helm-ff-set-pattern (pattern)
"Maybe transform pattern.
Expand file name if it seems reasonable, handle tramp names."
	(if (string= pattern "")
		""
		(let (
			; Returns the position of last ":" entered.
			(postfixed (helm-ff--tramp-postfixed-p pattern))
			(reg "\\`/\\([^[/:]+\\|[^/]+]\\):.*:")
			cur-method
		)
			(when (string-prefix-p "/-:" pattern)
				(setq pattern
					(concat
						"/"
						(when (boundp 'tramp-default-method) tramp-default-method)
						(substring-no-properties pattern 2))))
			(cond
				((string-match helm-ff-url-regexp pattern) pattern)
				((string-search "../" pattern) (expand-file-name pattern))
				; Optional, could be just a keybind.
				((string-suffix-p "./" pattern) (expand-file-name default-directory))
				; I added these, fine for Windows, but probably some platforms
				; allow files ending with a dot.
				((or
						(string-search "./" pattern)
						(string-prefix-p "~/" pattern))
					(expand-file-name pattern))
				((string-match helm-ff-tramp-method-regexp pattern) pattern)
				; Match "/method:maybe_hostname:~"
				((and
						postfixed
						(string-match (concat reg "~") pattern)
						(setq cur-method (match-string 1 pattern))
						(member cur-method helm-ff--tramp-methods))
					(and
						(fboundp 'tramp-make-tramp-file-name)
						(fboundp 'tramp-dissect-file-name)
						(replace-match
							(expand-file-name
								(tramp-make-tramp-file-name
									(tramp-dissect-file-name (match-string 0 pattern))))
							nil t pattern)))
				; Match "/method:maybe_hostname:"
				((and
						postfixed
						(string-match reg pattern)
						(setq cur-method (match-string 1 pattern))
						(member cur-method helm-ff--tramp-methods))
					(and
						(fboundp 'tramp-make-tramp-file-name)
						(fboundp 'tramp-dissect-file-name)
						(replace-match
							(tramp-make-tramp-file-name
								(tramp-dissect-file-name (match-string 0 pattern)))
							nil t pattern)))
				; Match "/hostname:"
				((and
						postfixed
						(string-match helm-ff-tramp-file-name-regexp pattern)
						(setq cur-method (match-string 1 pattern))
						(not (member cur-method helm-ff--tramp-methods)))
					(and
						(fboundp 'tramp-make-tramp-file-name)
						(fboundp 'tramp-dissect-file-name)
						(replace-match
							(tramp-make-tramp-file-name
								(tramp-dissect-file-name (match-string 0 pattern)))
							nil t pattern)))
				; Match "/method:" in this case don't try to connect.
				((or
						postfixed
						(not (string-match helm-ff-tramp-file-name-regexp pattern))
						(not (member (match-string 1 pattern) helm-ff--tramp-methods)))
					; Return PATTERN unchanged.
					pattern)))))

(defun helm-ff-minibuffer-setup-hook ()
"Update minibuffer contents to correct value of 'helm-pattern' at helm start.
Only for rare situation in 'helm-ff-get-candidates'."
	(delete-minibuffer-contents)
	(insert helm-pattern)
	(funcall after_move_hook_fn)
	(remove-hook 'helm-minibuffer-set-up-hook #'helm-ff-minibuffer-setup-hook)
	nil)

(defun helm-ff-get-candidates (&optional must_match)
	(let ((pattern (helm-ff-set-pattern helm-pattern)))
		(if (not pattern)
			(progn
				(setf (helm-source-ff-mode-line-base helm-current-source) "Hostnames")
				(helm-ff--tramp-hostnames)) ; Hostnames completion.
			; This unless protects against moving cursor to the end of pattern when user
			; is typing inside pattern, like C:/ProgramFiles/name_wih|t_typo.
			(unless (string= helm-pattern pattern)
				; 'helm-ff-get-candidates' is only called as :candidates function,
				; so helm-pattern is let-bound for helm matching.
				(setq helm-pattern pattern)
				; This wants to set second level value, but actually sets toplevel value...
				; Not ideal.
				(set 'helm-pattern pattern)
				(if-let ((window (active-minibuffer-window)))
					(with-selected-window window
						(delete-minibuffer-contents)
						(insert pattern)
						(funcall after_move_hook_fn))
					; If we still haven't entered minibuffer (in first helm-update),
					; then minibuffer isn't set up yet, but there is no way of overriding
					; :input value of helm call, so right after we enter minibuffer,
					; delete its contents and insert correct value of helm-pattern.
					(add-hook 'helm-minibuffer-set-up-hook #'helm-ff-minibuffer-setup-hook)))
			(cond
				((string-search "/" pattern)
					; This part is a mess because I don't even know what tramp is lol
					; and also what these urls are here for.
					(let (
						(tramp-verbose helm-tramp-verbose) ; No tramp message when 0.
						(valid-basedir
							(or
								(and
									(string-suffix-p ":" pattern)
									(helm-ff--tramp-postfixed-p pattern))
								; Check if base directory of PATTERN is valid.
								(when-let ((dir (file-name-directory pattern)))
									; If base dir is invalid, that means user is starting
									; to write a non-existing path.
									(file-exists-p dir))))
						basedir
					)
						(when valid-basedir
							(setq basedir (or (file-name-directory pattern) default-directory))
							(setq helm-ff-default-directory
								; If pattern is an url default-directory has to be nil.
								(unless (string-match helm-ff-url-regexp pattern)
									basedir))
							(and
								(string-suffix-p ":" pattern)
								(file-remote-p basedir nil t)
								(setq helm-pattern basedir)))
						(setf (helm-source-ff-mode-line-base helm-current-source)
							helm-ff-default-directory)
						(cond
							; Tramp methods.
							((string-match helm-ff-tramp-method-regexp pattern)
								(delq nil
									(mapcar
										(lambda (method)
											(helm-ff-filter-candidate-one-by-one
												(concat "/:" method)))
										helm-ff--tramp-methods)))
							((string-match helm-ff-url-regexp pattern)
								(cons
									(helm-ff-prepend-icon
										(propertize pattern 'face 'helm-ff-url)
										(all-the-icons-octicon
											(if (string-prefix-p "mailto:" pattern)
												"mail" "link-external")))
									pattern))
							(valid-basedir
								(setq helm-pattern
									; Probably more efficient file-name-nondirectory.
									(substring-no-properties pattern (length basedir)))
								(if
									(or
										must_match
										(string= helm-pattern "")
										; If exact pattern exists and it will be skipped by
										; helm-ff-directory-files, include it forcefully.
										; We don't want to show it as a new file or don't show it at all,
										; also this would prevent creating files matching boring regex,
										; so that solves many problems.
										(and
											(file-exists-p pattern)
											(not
												(and
													(helm-source-files-base-skip-boring helm-current-source)
													(string-match helm-files-boring-regex helm-pattern)))))
									(helm-ff-directory-files basedir)
									(cons
										(helm-ff-filter-candidate-one-by-one pattern t)
										(helm-ff-directory-files basedir))))
							; Non existing path - leave visible only parts that don't exist,
							; e.g. everything after last existing dir.
							(t
								(when-let ((dir (helm-files-first-existing-dir pattern)))
									(setq helm-ff-default-directory dir)
									(setf (helm-source-ff-mode-line-base helm-current-source) dir)
									(setq helm-pattern
										(substring-no-properties pattern (length dir)))
									(list
										(cons
											(helm-ff-prepend-icon-new-file
												(if (string-suffix-p "/" pattern)
													(propertize helm-pattern
														'face '(helm-ff-nofile helm-ff-directory))
													(let (
														(pat
															(propertize helm-pattern
																'face '(helm-ff-nofile helm-ff-file)))
													)
														(helm-files-add-extension-face pat)
														(helm-files-add-basename-face pat)
														pat)))
											pattern)))))))
				; Return drives on Windows.
				((eq system-type 'windows-nt)
					(setf (helm-source-ff-mode-line-base helm-current-source) "Drives")
					(let (
						(long_name_max_length
							(cl-loop
								for drive_info in w32-drive-info-list
								maximize (length (w32-drive-info-volume-name drive_info))))
					)
						(mapcar
							(lambda (drive_info)
								(let* (
									(short_name (string (w32-drive-info-char drive_info) ?:))
									(long_name (w32-drive-info-volume-name drive_info))
									(display
										(concat
											(propertize short_name 'face 'helm-ff-directory)
											"  "
											(propertize long_name 'face 'helm-completions-detailed)
											(get_space_string (- long_name_max_length (length long_name)))
											"   "
											(propertize_no_copy
												(let (
													(size
														(helm-file-human-size
															(w32-drive-info-size drive_info)))
													(free_space
														(helm-file-human-size
															(w32-drive-info-free-space drive_info)))
													(percent
														(number-to-string
															(round
																(*
																	100
																	(/
																		(float (w32-drive-info-free-space drive_info))
																		(w32-drive-info-size drive_info))))))
												)
													(concat
														"Size: "
														(get_space_string (- 7 (length size)))
														size
														"   Free: "
														(get_space_string (- 7 (length free_space)))
														free_space
														", "
														(get_space_string (- 3 (length percent)))
														percent
														"%"))
												'face 'helm-ff-size)))
								)
									(cons
										(propertize_no_copy display
											'match-part
												(list
													; Include one space dividing short and long name.
													(cons 0 (1+ (length short_name)))
													(cons
														(+ (length short_name) 3)
														(+ (length short_name) 3 (length long_name)))))
										(concat short_name "/"))))
							w32-drive-info-list)))
				(t (setf (helm-source-ff-mode-line-base helm-current-source) nil))))))

(defun helm-list-dir-lisp (directory)
"List DIRECTORY using `file-name-all-completions'.
Add a `helm-ff-dir' property on each fname ending with \"/\",
and helm-ff-file on others.
'directory' must end with a slash."
	; NOTE: `file-name-all-completions' and `directory-files' and most
	; tramp file handlers don't handle cntrl characters in fnames, so
	; the displayed files will be plain wrong in this case, even worst
	; the filenames will be splitted in two or more filenames.
	(cl-loop
		for file in (file-name-all-completions "" directory)
		unless (member file '("./" "../"))
			collect
				(propertize_no_copy (expand-file-name file directory)
					(if (string-suffix-p "/" file) 'helm-ff-dir 'helm-ff-file) t)))

(defun helm-list-dir-adb (directory)
"List DIRECTORY.
This is used for tramp adb backend.
Add a `helm-ff-dir' property on each fname ending with \"/\",
helm-ff-sym with a value of truename for symlinks and helm-ff-file on others.
'directory' must end with a slash."
	(mapcar
		(lambda (f)
			(let* (
				(split (split-string f " -> " t))
				(fname (car split))
				; Actually idk if this is a truename or target...
				; And this might cause problems.
				(truename (nth 1 split))
				(val t)
				prop
			)
				(cond
					((string-suffix-p "/" fname) (setq prop 'helm-ff-dir))
					(truename
						; Strip "@" suffix.
						(setq fname (substring-no-properties fname 0 -1))
						(setq prop 'helm-ff-sym)
						; Truename may be a dir or a file, no way to tell now,
						; would need to call file-directory-p.
						(setq val truename))
					(t (setq prop 'helm-ff-file)))
				(propertize_no_copy (expand-file-name fname directory) prop val)))
		(with-temp-buffer
			(insert-directory (concat directory "*") "-1F" t)
			(split-string
				(buffer-substring-no-properties (point-min) (point-max))
				"\n" t))))

; This is to fix issue with file-notify.el no more following symlinks
; on emacs-29 (regression) with inotify backend at least. Also it
; seems inotify is not configured to follow symlinks on other systems
; (MacOS) so this should fix as well this issue on such systems see bug#2542.
; Store here associations of (truename . symlink) when opening a
; symlinked directory, then add the watch to the truename; When the
; watcher ring on the truename remove the symlinked directory from cache.
(defvar helm-ff--list-directory-links nil)

; Maybe this full cache clears for helm-ff that I added make this somewhat useless,
; but they are necessary now, that is until directory helm-ff--list-directory-cache
; will be split into cache for raw files and cache for processed, propertized files.
; But it's better to rewrite this in C than to complicate by adding caches.
(defvar helm-ff--file-notify-watchers (make-hash-table :test 'equal)
"File-notify watchers for `helm-ff' are stored here.")

(defun helm-ff-directory-files (directory)
"List contents of DIRECTORY.
Never return the dotted filenames \\='.' and \\='..'.
Returned directories always end with slash.
DIRECTORY must be expanded and end with a slash."
	(let (
		(truename
			; Remove final slash.
			(let ((dfn (substring-no-properties directory 0 -1)))
				(when (file-symlink-p dfn) (file-truename dfn))))
	)
		(when truename
			(cl-pushnew (cons truename directory) helm-ff--list-directory-links
				:test #'equal))
		(or
			(gethash directory helm-ff--list-directory-cache)
			(let* (
				(remote (file-remote-p directory 'method))
				(files
					; List directory.
					; If directory is remote use helm-list-dir-lisp or helm-list-dir-adb,
					; otherwise use `directory-files'.
					(let (
						(sort_fn
							(cl-case (helm-source-ff-sort-method helm-current-source)
								(newest #'file-newer-than-file-p) ; Newest first.
								(size ; Biggest first.
									(lambda (f1 f2)
										(>
											(file-attribute-size (file-attributes f1))
											(file-attribute-size (file-attributes f2)))))
								(ext ; helm-sort-alpha for extensions.
									(lambda (f1 f2)
										(helm-sort-alpha
											(or (file-name-extension f1) "")
											(or (file-name-extension f2) ""))))))
					)
						(condition-case err
							(let (
								(files
									; Always perform basic alphabetical sorting.
									(sort
										(cond
											((not remote)
												(helm-files-directory-files
													directory
													t
													directory-files-no-dot-files-regexp
													t))
											((string= remote "adb")
												(helm-list-dir-adb directory))
											(t (helm-list-dir-lisp directory)))
										#'helm-sort-alpha))
							)
								(when (helm-source-ff-dirs-first helm-current-source)
									(setq sort_fn
										`(lambda (f1 f2)
											(let ((is_f1_dir (string-suffix-p "/" f1)))
												(if (eq is_f1_dir (string-suffix-p "/" f2))
													,(when sort_fn (list sort_fn 'f1 'f2))
													is_f1_dir)))))
								(when sort_fn (setq files (sort files sort_fn)))
								(when (helm-source-ff-reverse helm-current-source)
									(setq files (nreverse files)))
								files)
							; Handle file-error from here for Windows
							; because predicates like `file-readable-p' and friends
							; seem broken on emacs for Windows systems (always returns t).
							; This should never be called on GNU/Linux/Unix
							; as the error is properly intercepted in
							; `helm-ff-get-candidates' by `file-readable-p'.
							(file-error
								; Prefix error message with @@@@ for safety
								; (some files may match file-error See bug#2400)
								(list
									(format "@@@@%s:%s"
										(car err)
										(mapconcat #'identity (cdr err) " ")))))))
				watcher
			)
				(prog1
					(puthash
						directory
						(delq nil (mapcar #'helm-ff-filter-candidate-one-by-one files))
						helm-ff--list-directory-cache)
					; Put an inotify watcher to check directory modifications.
					(unless
						(or
							(not helm-ff-use-notify)
							(member remote helm-ff-inotify-unsupported-methods)
							(and
								(setq watcher
									(gethash directory helm-ff--file-notify-watchers))
								; [1] If watcher is invalid enter
								; next and delete it.
								(file-notify-valid-p watcher)))
						(when watcher
							; If watcher is still in cache and we are here,
							; that's mean test [1] above fails and watcher
							; is invalid, so delete it.
							(file-notify-rm-watch watcher)
							(remhash directory helm-ff--file-notify-watchers))
						; Keep adding DIRECTORY to helm-ff--file-notify-watchers but
						; watch on the truename and not the symlink as before bug#2542.
						(puthash
							directory
							(let ((to-watch (or truename directory)))
								(file-notify-add-watch
									to-watch
									'(change attribute-change)
									; Return a callback for `file-notify-add-watch'.
									`(lambda (event)
										; `attribute-changed' means permissions have changed,
										; not file modifications like file changes, visit
										; etc... AFAIU the desc for this is `changed' and for
										; our use case we don't care of this. Elements of
										; `helm-ff--list-directory-links' are of the form
										; (truename . visited-symlink-directory)
										(when
											(memq
												(nth 1 event)
												'(created deleted renamed attribute-changed))
											(let ((target ,to-watch))
												; Watched directory is the truename which is
												; not in the cache, so remove its associated
												; directory (the symlink) from the cache.
												(when-let (
													(symlink
														(assoc ,to-watch
															helm-ff--list-directory-links))
												)
													(setq target (cdr symlink))
													(setq helm-ff--list-directory-links
														(delete symlink helm-ff--list-directory-links)))
												; When TARGET is modified remove it from cache.
												(remhash target helm-ff--list-directory-cache))))))
							helm-ff--file-notify-watchers)))))))

(when (boundp 'tramp-cleanup-connection-hook)
	(add-hook 'tramp-cleanup-connection-hook
		(fn_symbol "helm-ff-tramp-cleanup-hook"
			; Remove remote directories related to VEC in helm-ff* caches.
			; Remove as well all related file-notify watchers.
			; This is meant to run in `tramp-cleanup-connection-hook'.
			(lambda (vec)
				(cl-loop
					for key being the hash-keys in helm-ff--list-directory-cache
					do
					(when (equal (file-remote-p key 'method) (tramp-file-name-method vec))
						(remhash key helm-ff--list-directory-cache)))
				(cl-loop
					for key being the hash-keys in helm-ff--file-notify-watchers
					do
					(when (equal (file-remote-p key 'method) (tramp-file-name-method vec))
						(file-notify-rm-watch (gethash key helm-ff--file-notify-watchers))
						(remhash key helm-ff--file-notify-watchers)))))))

(defun helm-ff-valid-symlink-p (file)
"Returns the truename of FILE if it exists."
	; `file-truename' send error on cyclic symlinks.
	(and
		(setq file (file-truename file))
		(file-exists-p file)
		file))

(defun helm-ff-properties-persistent ()
"Show properties in a tooltip or message without quitting helm.
Show recursive size of directories with prefix arg."
	(interactive)
	(let ((candidate (helm-files-expand-and-save-to-history (helm-get-selection))))
		(if-let ((file_attributes (file-attributes candidate t)))
			(let (
				(type (pop file_attributes))
				(links (pop file_attributes))
				(uid (pop file_attributes))
				(gid (pop file_attributes))
				(access-time
					(propertize_no_copy
						(format-time-string "%d.%m.%Y  %H:%M" (pop file_attributes))
						'face 'helm-ff-access-time))
				(modification-time
					(propertize_no_copy
						(format-time-string "%d.%m.%Y  %H:%M" (pop file_attributes))
						'face 'helm-ff-modification-time))
				(creation-time
					(propertize_no_copy
						(format-time-string "%d.%m.%Y  %H:%M" (car file_attributes))
						'face 'helm-ff-creation-time))
				(mode (nth 2 file_attributes))
				(tooltip-hide-delay (or helm-tooltip-hide-delay tooltip-hide-delay))
			)
				(cl-destructuring-bind
					(mode-type user group other octal)
					(helm-split-mode-file-attributes mode)
					(if tooltip-mode
						(tooltip-show
							(concat
								(let ((display (car (car (helm-files-highlight (list candidate))))))
									(setq display
										(let ((range (car (get-text-property 0 'match-part display))))
											(substring display (car range) (cdr range))))
									(with-temp-buffer
										(insert display)
										(when (= (char-before) ?/) (forward-char -1))
										(skip-chars-backward "^/")
										(buffer-substring (point) (point-max))))
								; Default major mode to open file.
								(when-let (
									(mode (cdr (assoc candidate auto-mode-alist #'string-match)))
								)
									(concat "\nDefault major mode: " (symbol-name mode)))
								"\nType: "
								(cond
									((stringp type) ; Symlink.
										(if-let ((truename (helm-ff-valid-symlink-p candidate)))
											(let ((is_dir (file-directory-p truename)))
												(concat
													(when is_dir "directory ")
													"symlink, "
													mode-type
													"\nTrue name: "
													truename
													"\nSize"
													(if is_dir
														(concat
															" ("
															(unless current-prefix-arg "non ")
															"recursive): "
															(let (
																(size_info
																	(helm-directory-size
																		truename current-prefix-arg))
															)
																(concat
																	(helm-file-human-size (aref size_info 0))
																	" ("
																	(number-to-string (aref size_info 1))
																	" files, "
																	(number-to-string (aref size_info 2))
																	" dirs)")))
														(concat
															":"
															(helm-file-human-size
																(file-attribute-size
																	(file-attributes truename)))))))
											(concat "invalid symlink, " mode-type)))
									(type ; Directory.
										(concat
											"directory, "
											mode-type
											"\nSize ("
											(unless current-prefix-arg "non ")
											"recursive): "
											(let (
												(size_info
													(helm-directory-size
														candidate current-prefix-arg))
											)
												(concat
													(helm-file-human-size (aref size_info 0))
													" ("
													(number-to-string (aref size_info 1))
													" files, "
													(number-to-string (aref size_info 2))
													" dirs)"))))
									(t (concat "file, " mode-type))) ; Normal file.
								(format "\nAccessed: %s\n" access-time)
								(format "Modified: %s\n" modification-time)
								(format "Created: %s\n" creation-time)
								(format "Owner: %s, %s\n" uid user)
								(format "Group: %s, %s\n" gid group)
								(format "Others: %s\n" other)
								(format "Numerical: %s\n" octal)
								(when-let (
									(default_program
										(helm-get-default-program-for-file candidate))
								)
									(concat "Default program: " default_program "\n"))))
						(message "%s %s %s:%s"
							(helm-split-mode-file-attributes mode t)
							(number-to-string links)
							uid
							gid))))
			(message "Couldn't access file attributes."))))

(cl-defun helm-ff-file-to-display (file &optional (attributes 'default))
"Return (display_string . file_type) where display_string is an aligned icon
(3 chars) + 'file' with added face(s) and file_type is a symbol, one of:
nofile, symlink, directory, file.
If file_type is symlink, then propertized file has a 'truename
property added, or it doesn't if it is an invalid symlink.
Use (get-text-property 3 'face display_string) to check details of file type,
such as directory symlink, invalid symlink, executable, etc."
	; Don't call file-attributes twice.
	(when (eq attributes 'default) (setq attributes (file-attributes file)))
	(let (type icon display)
		(if attributes
			(let ((attribute_type (file-attribute-type attributes)))
				(cond
					((stringp attribute_type)
						(setq type 'symlink)
						(if-let ((truename (helm-ff-valid-symlink-p file)))
							(progn
								(if (file-directory-p truename)
									(setq
										icon
											(all-the-icons-octicon
												"file-symlink-directory")
										display
											(propertize file
												'face 'helm-ff-directory-symlink))
									(setq
										icon
											(all-the-icons-icon-for-file
												(file-name-nondirectory truename))
										display (propertize file 'face 'helm-ff-symlink))
									(helm-files-add-basename-face display))
								(propertize_no_copy display 'truename truename))
							(setq
								icon (all-the-icons-material "error")
								display (propertize file 'face 'helm-ff-invalid-symlink))))
					(attribute_type
						(setq type 'directory)
						(setq icon
							(helm-files-icon-for-dir
								(file-name-nondirectory
									(substring-no-properties file 0 -1))))
						(setq display (propertize file 'face 'helm-ff-directory)))
					(t
						(setq type 'file)
						(let ((basename (file-name-nondirectory file)))
							(setq icon (all-the-icons-icon-for-file basename))
							(cond
								((backup-file-name-p file)
									(setq display (propertize file 'face 'helm-ff-backup-file)))
								((auto-save-file-name-p basename)
									(setq display (propertize file 'face 'helm-ff-auto-save-file)))
								((helm-files-lock-file-p file)
									(setq display (propertize file 'face 'helm-ff-lock-file))
									(helm-files-add-extension-face display))
								(t
									(setq display
										(propertize file
											'face
											(let ((modes (file-attribute-modes attributes)))
												(cond
													; A character device file.
													((find_in_vector_= "cp" (aref modes 0))
														'helm-ff-pipe)
													; A socket file.
													((= ?s (aref modes 0)) 'helm-ff-socket)
													; An executable file.
													((= ?x (aref modes 3)) 'helm-ff-exe)
													; An executable file with suid.
													((= ?s (aref modes 3)) 'helm-ff-suid)
													; A file.
													(t 'helm-ff-file)))))
									(helm-files-add-extension-face display)))
							(helm-files-add-basename-face display basename)))))
			(setq type 'nofile)
			(if (string-suffix-p "/" file)
				(setq
					display (propertize file 'face '(helm-ff-nofile helm-ff-directory))
					icon (all-the-icons-material "create_new_folder"))
				(setq
					display (propertize file 'face '(helm-ff-nofile helm-ff-file))
					icon (all-the-icons-material "note_add"))
				(helm-files-add-extension-face display)
				(helm-files-add-basename-face display)))
		(cons (concat (all-the-icons-align icon) display) type)))

(defun helm-ff-properties (_candidate)
"Show extensive properties of marked files in a buffer."
	(fit-window-to-buffer
		(select-window
			(display-buffer
				(with-current-buffer (get-buffer-create "*helm-file-properties*")
					(erase-buffer)
					(fundamental-mode)
					(dolist (file (helm-files-marked-candidates t))
						(let (
							(append_stuff
								(lambda (&optional truename is_url)
									(concat
										; Default major mode to open file.
										; This might be wrong, idk how find-file or some other
										; function determines what mode to actually use
										; when opening file through symlink, but I'd guess
										; it uses truename for that, not symlink name.
										(when-let (
											(mode
												(cdr
													(assoc (or truename file)
														auto-mode-alist #'string-match)))
										)
											(concat "\nDefault major mode: " (symbol-name mode)))
										(when-let (
											(default_program
												(helm-get-default-program-for-file file))
										)
											(concat "\nDefault program: " default_program))
										; Probably should extend this unless to major mode and default program...
										(unless is_url
											(let ((basename (helm-files-basename-with-slash file)))
												(when (string-match helm-files-boring-regex basename)
													(concat
														"\nMatches helm's boring regex (matched \""
														(match-string 0 basename)
														"\").")))))))
						)
							(if (string-match helm-ff-url-regexp file)
								(insert
									(all-the-icons-align
										(all-the-icons-octicon
											(if (string-prefix-p "mailto:" file)
												"mail" "link-external")))
									(propertize file 'face 'helm-ff-url)
									"\nType: url"
									(funcall append_stuff nil t))
								; At this point 'file' should theoretically be an existing
								; file, but I guess it actually might be some tramp crap...
								(if-let ((attributes (file-attributes file t)))
									(cl-destructuring-bind (
										attribute_type links uid gid access-time modification-time
										creation-time size modes _ inode-number device-number
									)
										attributes
										(setq access-time
											(propertize_no_copy
												(format-time-string "%d.%m.%Y %H:%M" access-time)
												'face 'helm-ff-access-time))
										(setq modification-time
											(propertize_no_copy
												(format-time-string "%d.%m.%Y %H:%M" modification-time)
												'face 'helm-ff-modification-time))
										(setq creation-time
											(propertize_no_copy
												(format-time-string "%d.%m.%Y %H:%M" creation-time)
												'face 'helm-ff-creation-time))
										(setq size (helm-file-human-size size))
										(let (
											(type (helm-ff-file-to-display file attributes))
											display faces truename
										)
											(setq display (car type))
											(setq type (cdr type))
											(setq faces (get-text-property 3 'face display))
											; Prepare to use for memq.
											(unless (listp faces) (setq faces (list faces)))
											(insert display ?\n)
											(cl-destructuring-bind
												(mode-type user group other octal)
												(helm-split-mode-file-attributes modes)
												(insert
													(concat
														(let ((abbrev (abbreviate-file-name file)))
															(unless (string= file abbrev)
																(concat "Abbreviated as: " abbrev "\n")))
														"Type: "
														(cond
															((eq type 'symlink)
																(if (setq truename (get-text-property 3 'truename display))
																	(let (
																		(is_symlink_dir (memq 'helm-ff-directory-symlink faces))
																		(truename_attributes (file-attributes truename))
																	)
																		(concat
																			"symlink to "
																			(if (file-symlink-p attribute_type)
																				(concat
																					"symlink, finally to "
																					(if is_symlink_dir "directory" "file")
																					", "
																					mode-type
																					"\nDirect target: "
																					(car (helm-ff-file-to-display attribute_type)))
																				(concat
																					(if is_symlink_dir "directory" "file")
																					", "
																					mode-type))
																			"\nFinal target: "
																			(car
																				(helm-ff-file-to-display
																					truename truename_attributes))
																			"\nSize:\n\tsymlink - "
																			size
																			"\n\t"
																			(if is_symlink_dir
																				(concat
																					"final target:\n\t\tdirectory file - "
																					(helm-file-human-size
																						(file-attribute-size
																							truename_attributes))
																					"\n\t\tdirectory contents non recursive - "
																					(let (
																						(size_info
																							(helm-directory-size truename nil))
																					)
																						(concat
																							(helm-file-human-size (aref size_info 0))
																							" ("
																							(number-to-string (aref size_info 1))
																							" files, "
																							(number-to-string (aref size_info 2))
																							" dirs)"))
																					"\n\t\tdirectory contents recursive - "
																					(let (
																						(size_info
																							(helm-directory-size truename t))
																					)
																						(concat
																							(helm-file-human-size (aref size_info 0))
																							" ("
																							(number-to-string (aref size_info 1))
																							" files, "
																							(number-to-string (aref size_info 2))
																							" dirs)")))
																				(concat
																					"final target - "
																					(helm-file-human-size
																						(file-attribute-size
																							truename_attributes))))))
																	(concat
																		"invalid symlink, "
																		mode-type
																		"\nDirect target: "
																		(car (helm-ff-file-to-display attribute_type))
																		"\nSize: "
																		size)))
															((eq type 'directory)
																(concat
																	"directory, "
																	mode-type
																	"\nSize:\n\tdirectory file - "
																	size
																	"\n\tdirectory contents non recursive - "
																	(let ((size_info (helm-directory-size file nil)))
																		(concat
																			(helm-file-human-size (aref size_info 0))
																			" ("
																			(number-to-string (aref size_info 1))
																			" files, "
																			(number-to-string (aref size_info 2))
																			" dirs)"))
																	"\n\tdirectory contents recursive - "
																	(let ((size_info (helm-directory-size file t)))
																		(concat
																			(helm-file-human-size (aref size_info 0))
																			" ("
																			(number-to-string (aref size_info 1))
																			" files, "
																			(number-to-string (aref size_info 2))
																			" dirs)"))))
															(t ; Other existing file.
																(concat
																	(cond
																		((memq 'helm-ff-backup-file faces) "backup file")
																		((memq 'helm-ff-auto-save-file faces) "auto save file")
																		((memq 'helm-ff-lock-file faces) "lock file")
																		((memq 'helm-ff-pipe faces) "pipe")
																		((memq 'helm-ff-socket faces) "socket")
																		((memq 'helm-ff-exe faces) "executable")
																		((memq 'helm-ff-suid faces) "suid file")
																		(t "file"))
																	", "
																	mode-type
																	"\nSize: "
																	size)))
														"\nAccessed: "
														access-time
														"\nModified: "
														modification-time
														"\nCreated:  "
														creation-time
														"\nOwner"
														(if (integerp uid)
															(concat
																" id (name not found): "
																(number-to-string uid))
															(concat ": " uid))
														", "
														user
														"\nGroup"
														(if (integerp uid)
															(concat
																" id (name not found): "
																(number-to-string gid))
															(concat ": " gid))
														", "
														group
														"\nOthers: "
														other
														"\nNumerical: "
														octal
														"\nNumber of links: "
														(number-to-string links)
														"\nInode number: "
														(number-to-string inode-number)
														"\nDevice id: "
														; Integer or cons.
														(format "%S" device-number)
														(when-let ((user (file-locked-p file)))
															(concat
																"\nLocked by "
																(if (eq user t)
																	(concat (user-login-name) " (you)")
																	user)
																"."))
														(funcall append_stuff truename))))))
									(insert
										(concat
											(car (helm-ff-file-to-display file nil))
											(let ((abbrev (abbreviate-file-name file)))
												(unless (string= file abbrev)
													(concat "\nAbbreviated as: " abbrev)))
											"\nType: non-existing "
											(if (string-suffix-p "/" file) "directory" "file")
											(funcall append_stuff))))))
						(insert "\n\n"))
					(delete-char -1)
					(goto-char (point-min))
					(setq buffer-read-only t)
					(current-buffer))))))

(defun helm-ff-filter-candidate-one-by-one (file &optional skip-boring-check)
"Transform file in a cons cell like (DISPLAY . REAL).
DISPLAY is shown as basename of FILE and REAL as full path of FILE.
If SKIP-BORING-CHECK is non-nil don't filter boring files.
Return nil only if boring check returns true."
	(let ((basename (helm-basename file)))
		(when
			(or
				skip-boring-check
				(not
					(or
						; Maybe skip boring files.
						(and
							(helm-source-files-base-skip-boring helm-current-source)
							(string-match helm-files-boring-regex
								(if (string-suffix-p "/" file)
									(concat basename "/")
									basename)))
						; Maybe skip ".gitignore" files.
						(and
							helm-ff-skip-git-ignored-files
							(not (file-remote-p file))
							(= (call-process "git" nil nil nil "check-ignore" "-q" file) 0)))))
			(cons
				(let* (
					(tramp-invalid-fname
						; Extract hostname from an incomplete tramp file name.
						; Return nil on valid file name remote or not.

						; Check first if whole file is remote (file-remote-p is inefficient
						; in this case) otherwise we are matching e.g. /home/you/ssh:foo/
						; which is not a remote name.
						; FIXME this will not work with a directory or a file named like
						; "ssh:foo" and located at root (/) but it seems there is no real
						; solution apart disabling tramp-mode when a file/dir located at /
						; is matching helm-ff-tramp-file-name-regexp; This would prevent usage
						; of tramp if one have such a directory at / (who would want to
						; have such a dir at / ???) See emacs-bug#31489.
						(when (string-match helm-ff-tramp-file-name-regexp file)
							(let ((split (split-string basename ":" t)))
								(and
									(car split)
									(member (car split) helm-ff--tramp-methods)
									(string= (string-replace basename "" file) "/")
									(car (last split))))))
					(disp (or tramp-invalid-fname basename))
					(handle_locked
						(lambda ()
							(when (file-locked-p file)
								(setq disp
									(concat
										disp
										(get_space_string
											; Now size is up to 86 column.
											(max 2 (- 88 (length disp))))
										(all-the-icons-material "lock_outline"))))
							disp))
					(prepend_default_icon
						(lambda ()
							(setq disp
								(helm-ff-prepend-icon
									disp (all-the-icons-icon-for-file basename)))))
					(set_face_and_extension
						(lambda (str face)
							(propertize_no_copy str 'face face)
							(helm-files-add-extension-face str)))
					(handle_dir
						(lambda ()
							(propertize_no_copy disp 'face 'helm-ff-directory)
							(setq disp
								(helm-ff-prepend-icon
									disp (helm-files-icon-for-dir basename)))))
					(handle_symlink
						(lambda (truename is_dir)
							(setq disp
								(let (
									(truename_to_display
										(copy-sequence (abbreviate-file-name truename)))
									icon face
								)
									(if is_dir
										(progn
											(setq face 'helm-ff-directory-symlink)
											(propertize_no_copy truename_to_display
												'face 'helm-ff-directory)
											(setq icon
												(all-the-icons-octicon
													"file-symlink-directory")))
										(setq face 'helm-ff-symlink)
										(funcall set_face_and_extension
											truename_to_display 'helm-ff-file)
										(setq icon
											(all-the-icons-icon-for-file
												(file-name-nondirectory truename_to_display))))
									(propertize_no_copy disp 'face face)
									(helm-ff-prepend-icon
										(concat disp " -> " truename_to_display) icon)))))
					(handle_tramp_method
						(lambda (&optional handle_multihops_fn)
							; For now just give them default icon.
							; Should be replaced by something better.
							(setq disp (match-string 1 file))
							(unless
								(and handle_multihops_fn (not (funcall handle_multihops_fn)))
								(setq file (concat "/:" disp))
								(setq disp (concat "/" disp)))
							(propertize_no_copy disp 'face 'helm-ff-tramp-method)
							(helm-ff-prepend-icon disp
								(apply (car allTheIcons::DEFAULT_FILE_ICON)
									(cdr allTheIcons::DEFAULT_FILE_ICON)))))
				)
					; Handle tramp files with minimal highlighting.
					(if
						(or
							(string-match helm-ff-tramp-file-name-regexp helm-pattern)
							(helm-file-on-mounted-network-p helm-pattern))
						(cond
							((backup-file-name-p disp)
								(propertize_no_copy disp 'face 'helm-ff-backup-file)
								(funcall prepend_default_icon)
								(funcall handle_locked))
							((auto-save-file-name-p disp)
								(propertize_no_copy disp 'face 'helm-ff-auto-save-file)
								(funcall prepend_default_icon)
								(funcall handle_locked))
							((helm-files-lock-file-p file)
								(funcall set_face_and_extension disp 'helm-ff-lock-file)
								(funcall prepend_default_icon)
								(funcall handle_locked))
							((get-text-property 0 'helm-ff-dir file)
								(funcall handle_dir)
								(funcall handle_locked))
							((get-text-property 0 'helm-ff-sym file)
								(let ((truename (get-text-property 0 'helm-ff-sym file)))
									(funcall handle_symlink
										truename (file-directory-p truename)))
								(funcall handle_locked))
							((get-text-property 0 'helm-ff-file file)
								(funcall set_face_and_extension disp 'helm-ff-file)
								(funcall prepend_default_icon)
								(funcall handle_locked))
							; Tramp method.
							((string-match helm-ff-tramp-method-regexp file)
								(funcall handle_tramp_method
									(lambda ()
										(when (helm-ff--tramp-multihops-p helm-pattern)
											(setq file
												(concat
													(match-string 0 helm-pattern) ":" disp))
											t))))
							(t ; Non existing file.
								(funcall set_face_and_extension
									disp '(helm-ff-nofile helm-ff-file))
								(if tramp-invalid-fname
									(funcall prepend_default_icon)
									(helm-ff-prepend-icon-new-file disp))))
						; Highlight local files showing everything: symlinks, exe, dirs, ...
						(let* (
							(get_file_attributes
								(lambda (str)
									(condition-case err (file-attributes str)
										(file-error
											; Possible error not happening during listing
											; but when calling file-attributes see error
											; with sshfs bug#2405
											(message "%s:%s" (car err) (cdr err))
											nil))))
							(attributes (funcall get_file_attributes file))
							(type (file-attribute-type attributes))
							(append_attributes
								; If 'attributes' is non-nil, append 2 columns to 'disp':
								; modification date and size.
								(lambda (&optional no_size mod_time_override)
									(when attributes
										(setq disp
											(concat
												disp
												(get_space_string (max 1 (- 60 (length disp))))
												(propertize_no_copy
													(format-time-string "%d.%m.%Y  %H:%M"
														(or
															mod_time_override
															(file-attribute-modification-time
																attributes)))
													'face 'helm-ff-modification-time)))
										(unless no_size
											(setq disp
												(let (
													(size
														(helm-file-human-size
															(file-attribute-size attributes)))
												)
													(concat
														disp
														(get_space_string (- 10 (length size)))
														size)))))
									disp))
						)
							(cond
								((stringp type) ; Symlink.
									(if-let ((truename (helm-ff-valid-symlink-p file)))
										(let ((is_dir (file-directory-p truename)))
											(funcall handle_symlink truename is_dir)
											; Use size of a true file, not symlink,
											; because symlinks always have size of 0.0B.
											(let (
												(mod_time
													(file-attribute-modification-time
														attributes))
												(true_attibutes
													(funcall get_file_attributes truename))
												no_size
											)
												(if (not true_attibutes)
													; If for some reason true attributes are
													; not accessible, just don't show size.
													(setq no_size t)
													(setq attributes true_attibutes)
													(setq no_size is_dir))
												(funcall append_attributes no_size mod_time)))
										(propertize_no_copy disp 'face 'helm-ff-invalid-symlink)
										(setq disp
											(helm-ff-prepend-icon disp
												(all-the-icons-material "error"))))
									(funcall handle_locked))
								(type ; Directory.
									(funcall handle_dir)
									(funcall append_attributes t)
									(funcall handle_locked))
								((backup-file-name-p basename)
									(propertize_no_copy disp 'face 'helm-ff-backup-file)
									(funcall prepend_default_icon)
									(funcall append_attributes)
									(funcall handle_locked))
								((auto-save-file-name-p basename)
									(propertize_no_copy disp 'face 'helm-ff-auto-save-file)
									(funcall prepend_default_icon)
									(funcall append_attributes)
									(funcall handle_locked))
								((helm-files-lock-file-p file)
									(funcall set_face_and_extension disp 'helm-ff-lock-file)
									(funcall prepend_default_icon)
									(funcall append_attributes)
									(funcall handle_locked))
								(attributes ; Some other existing file.
									(funcall set_face_and_extension disp
										(let ((modes (file-attribute-modes attributes)))
											(cond
												; A character device file.
												((find_in_vector_= "cp" (aref modes 0))
													'helm-ff-pipe)
												; A socket file.
												((= ?s (aref modes 0)) 'helm-ff-socket)
												; An executable file.
												((= ?x (aref modes 3)) 'helm-ff-exe)
												; An executable file with suid.
												((= ?s (aref modes 3)) 'helm-ff-suid)
												; A file.
												(t 'helm-ff-file))))
									(funcall prepend_default_icon)
									(funcall append_attributes)
									(funcall handle_locked))
								; Tramp method.
								((string-match helm-ff-tramp-method-regexp file)
									; At this point no need to handle multi hops syntax
									; which is considered remote and handled above.
									(funcall handle_tramp_method))
								(t ; Non-existing file.
									(funcall set_face_and_extension
										disp '(helm-ff-nofile helm-ff-file))
									(helm-ff-prepend-icon-new-file disp))))))
				file))))

; Trashing files
; This whole section is unused because it's useless on Windows.

(defun helm-ff-trash-action (fn name_1 name_2 &optional trashed_files)
"Execute a trash action FN on marked files.

Arg name_1 and name_2 is a list of strings to pass to messages,
like \"restore\" \"restoring\"."
	(let (
		(marked (helm-files-marked-candidates))
		errors aborted
	)
		(helm-with-display-candidates
			(if trashed_files
				(cl-loop
					for f in marked
					for assoc = (assoc (helm-basename f) trashed_files)
					when assoc
						collect
							(concat
								(truncate-string-to-width (car assoc) 40 nil nil t)
								" -> "
								(truncate-string-to-width
									(file-name-directory (cdr assoc)) 40 nil nil t)))
				(map_modify_list #'car (helm-files-highlight marked)))
			(if
				(y-or-n-p
					(format "%s %d files from trash?" (capitalize name_1) (length marked)))
				(progn
					(message "%s files from trash..." (capitalize name_2))
					(dolist (f marked)
						(condition-case err
							(funcall fn f)
							(error (push (format "%s" (nth 1 err)) errors)))))
				(message "%s files from trash aborted." (capitalize name_2))
				(setq aborted t)))
		; Handle errors from outside the helm-with-display-candidates block,
		; otherwise warning is never displayed.
		(if (not errors)
			(unless aborted
				(message "%s %d files from trash done." (capitalize name_2) (length marked)))
			(display-warning 'helm
				(concat
					(format-time-string "%d.%m.%Y  %H:%M:%S")
					(format "Failed to %s %d/%d files from trash\n"
						name_1 (length errors) (length marked))
					(mapconcat (lambda (er) (concat er "\n")) errors))
				:error "*helm-restore-warnings*")
			(message "%s files from trash aborted." (capitalize name_2)))))

(defun helm-files-trashinfo (file)
"Return trashinfo file of 'file' or something."
	(concat
		(if (equal (file-remote-p file 'method) "ftp")
			; expand-file-name is not working as expected with ftp fnames (emacs bug).
			(or
				(let (
					(split (split-string (expand-file-name file) "/" t))
					(iter
						(lambda ()
							(when (cdr split)
								(mapconcat
									(lambda (i) (if (string= i "/") i (concat i "/")))
									(setq split (nbutlast split))))))
				)
					(and
						split
						(not (string-match "\\`\\(~\\|[[:alpha:]]:\\)" (car split)))
						(setq split (cons "/" split)))
					(helm-iter-next iter)
					(helm-iter-next iter))
				(expand-file-name "/"))
			; This version comes from Bug#2004 (UNC paths) and should fix it.
			; It works with local files and remote files as well but not with ftp.
			(let ((file_ file))
				(dotimes (_ 2)
					(or
						(string= file_ "/")
						(string= (file-remote-p file_ 'localname) "/")
						(setq file_ (expand-file-name (concat file_ "/../")))))
				file_))
		"info/"
		; Idk, I didn't write this.
		(cond
			((string-suffix-p "/" file)
				(file-name-nondirectory (substring-no-properties file 0 -1)))
			((string-suffix-p "trashinfo" file)
				(substring-no-properties (file-name-nondirectory file)
					0 (- (length "trashinfo"))))
			(t (file-name-nondirectory file)))
		".trashinfo"))

(defun helm-ff-trash-rm (_candidate)
"Delete marked-files from a Trash directory.

The Trash directory should be a directory compliant with
<http://freedesktop.org/wiki/Specifications/trash-spec> and each
file should have its \\='*.trashinfo' correspondent file in
Trash/info directory."
	(unless (fboundp 'system-move-file-to-trash)
		(helm-ff-trash-action
			(lambda (file)
				(let ((info-file (helm-files-trashinfo file)))
					(cl-assert (file-exists-p file) nil
						(format "No such file or directory `%s'" file))
					(cl-assert (file-exists-p info-file) nil
						(format "No such file or directory `%s'" info-file))
					(if (file-directory-p file)
						(delete-directory file t)
						(delete-file file))
					(delete-file info-file)))
			"delete" "deleting")))

(defun helm-restore-file-from-trash (_candidate)
"Restore marked-files from a Trash directory.

The Trash directory should be a directory compliant with
<http://freedesktop.org/wiki/Specifications/trash-spec> and each
file should have its \\='*.trashinfo' corresponding file in
Trash/info directory."
	(unless (fboundp 'system-move-file-to-trash)
		(let* (
			(default-directory helm-ff-default-directory)
			(trashed-files (helm-ff-trash-list helm-ff-default-directory))
		)
			(helm-ff-trash-action
				; Restore FILE from a trash directory.
				; TRASHED-FILES is an alist of (fname_in_trash . dest) obtained
				; with `helm-ff-trash-list'.
				(lambda (file)
					; Emacs trash duplicate files with a unique name + .trashinfo in
					; the filename which is wrong, only files in info directory should
					; end with .trashinfo, so fix the filename before looking for dest name.
					(let* (
						(fname (replace-regexp-in-string "\\.trashinfo\\'" "" file))
						(basename (helm-basename fname))
						(info-file (helm-files-trashinfo fname))
						(dest-file (assoc-default basename trashed-files))
					)
						(cl-assert (not (file-exists-p dest-file)) nil
							(format "File `%s' already exists" dest-file))
						(cl-assert dest-file nil "No such file in trash")
						(message "Restoring %s to %s..."
							(helm-basename file) (file-name-directory dest-file))
						(rename-file file dest-file)
						(message "Restoring %s to %s done"
							(helm-basename file) (file-name-directory dest-file))
						(delete-file info-file)))
				"restore" "restoring" trashed-files))))

(defun helm-ff-trash-list (trash-dir)
"Return an alist of trashed files basename and dest name.
Assume the trash system in use is freedesktop compatible, see
<http://freedesktop.org/wiki/Specifications/trash-spec>
This function is intended to be used from a trash directory i.e. it
use `helm-ff-default-directory', but it may be used elsewhere by
specifying the trash directory with TRASH-DIR arg."
	; Files owned by root are trashed in /root/.local/share/Trash.
	; Files owned by user and trashed by root are trashed in
	; /home/.Trash.
	; Files owned by user and trashed by user are trashed in
	; ~/.local/share/Trash.
	(mapcar
		(lambda (f)
			(cons
				(helm-basename (replace-regexp-in-string "\\.trashinfo\\'" "" f))
				(with-temp-buffer
					(save-excursion (insert-file-contents f))
					(when (re-search-forward "^path=" nil t)
						(let (
							(path
								(helm-url-unhex-string
									(buffer-substring-no-properties
										(point) (pos-eol))))
						)
							(if (string-prefix-p "/" path)
								path ; path is absolute
								; When path is relative, assume the
								; trash directory is located at
								; /home/.Trash and path is the
								; relative name of file from /home.
								(expand-file-name path "/home")))))))
		(directory-files
			(expand-file-name
				; helm-ff-default-directory is actually the
				; trash directory.
				"info"
				(file-name-directory
					(directory-file-name trash-dir)))
			t
			directory-files-no-dot-files-regexp)))

(defun helm-ff-mail-attach-files (_candidate)
"Run `mml-attach-file' on marked candidates."
	(require 'mml)
	(let (
		(flist (helm-files-marked-candidates t))
		(dest-buf (when (derived-mode-p 'message-mode 'mail-mode) (current-buffer)))
		bufs
	)
		(unless dest-buf
			(setq bufs
				(cl-loop
					for b in (buffer-list)
					when (with-current-buffer b (derived-mode-p 'message-mode 'mail-mode))
						collect (buffer-name b)))
			(if (and bufs (y-or-n-p "Attach files to existing mail composition buffer?"))
				(setq dest-buf
					(if (cdr bufs)
						(helm-comp-read "Attach to buffer: " bufs :must-match t)
						(car bufs)))
				(compose-mail)
				(setq dest-buf (current-buffer))))
		(switch-to-buffer dest-buf)
		(save-restriction
			(widen)
			(save-excursion
				(goto-char (point-max))
				(dolist (f flist)
					(mml-attach-file f
						(or
							(mm-default-file-encoding f)
							"application/octet-stream")))))))

(defvar image-dired-display-image-buffer)
(defun helm-ff-image-rotate (file angle) "Rotate current image at ANGLE degrees."
	(setq file (helm-files-expand-and-save-to-history file))
	(if
		(not
			(and
				(file-exists-p file)
				(string-match (image-file-name-regexp) file)))
		(message "Can't rotate non image file.")
		(setq file (file-truename file)) ; For symlinked images.
		(let ((default-directory (file-name-directory file)))
			(if
				(not
					(and
						helm-ff-image-rotate-program
						(executable-find helm-ff-image-rotate-program)))
				; When rotation fails fallback to `image-rotate' with no
				; transformation of file.
				(with-selected-window (helm-persistent-action-display-window)
					(image-rotate angle))
				(apply #'process-file helm-ff-image-rotate-program nil nil nil
					(append
						helm-ff-image-rotate-switch
						(list
							; Convert ANGLE to a suitable value for exiftran.
							(if (equal helm-ff-image-rotate-program "exiftran")
								(cl-case angle
									(90  "-9") ; 90 clockwise
									(270 "-2")) ; 270 clockwise == -90
								(number-to-string angle))
							(file-name-nondirectory file))))
				(helm-ff-image-display-native file)))))

(defun helm-ff-image-rotate-left (&optional candidate)
"Rotate image file CANDIDATE left.
This can affect file CANDIDATE."
	(interactive)
	(helm-ff-image-rotate (or candidate (helm-get-selection)) 270))

(defun helm-ff-image-rotate-right (&optional candidate)
"Rotate image file CANDIDATE right.
This can affect file CANDIDATE."
	(interactive)
	(helm-ff-image-rotate (or candidate (helm-get-selection)) 90))

(defun helm-ff-resize-image-1 (arg)
	; `image-decrease-size' and `image-increase-size' are not usable
	; because they run directly `image--change-size' in a timer without
	; taking care of the selected-window.
	(with-selected-window (helm-persistent-action-display-window)
		(image--change-size arg)))

(defun helm-ff-image-increase-size () (interactive) (helm-ff-resize-image-1 1.2))
(defun helm-ff-image-decrease-size () (interactive) (helm-ff-resize-image-1 0.8))

(defvar helm-ff-image-native-buffer "*image-native-display*")

(defvar helm-ff-sound-file-extensions '("wav" "au"))

(defun helm-files-persistent-action (candidate)
"Find file CANDIDATE or kill its buffer if it is visible.
Never kill `helm-current-buffer'.
Never kill modified buffer."
	(setq candidate (expand-file-name candidate))
	(let ((is_dir (file-directory-p candidate)))
		; Make sure candidate ends with a slash if it is a directory.
		(when is_dir (setq candidate (file-name-as-directory candidate)))
		; Maybe this shouldn't be here.
		(helm-files-save-to-file-name-history candidate)
		(let* (
			(buffer
				(if is_dir
					; Different search for dired buffers.
					(cl-loop
						for dir_and_buffer in dired-buffers
						when
							(and
								(string= candidate (car dir_and_buffer))
								(buffer-live-p (cdr dir_and_buffer)))
							return (cdr dir_and_buffer))
					(get-file-buffer candidate)))
			(window (get-buffer-window buffer))
			; Prevent tramp from asking yes-or-no-p for
			; `tramp-allow-unsafe-temporary-files'.
			auto-save-default
		)
			(cond
				((not (and buffer window)) (find-file candidate))
				((eq buffer (get-buffer helm-current-buffer))
					(message "Can't kill `helm-current-buffer' without quitting session."))
				; Should ask for: don't kill, kill, kill and save.
				((buffer-modified-p buffer) (message "Can't kill modified buffer."))
				(t
					(if
						(and
							helm-persistent-action-display-window
							(window-dedicated-p (next-window window 1)))
						(delete-window helm-persistent-action-display-window)
						(set-window-buffer window helm-current-buffer))
					(kill-buffer buffer))))))

(defun helm-ff-persistent-action-if (candidate)
"Choose persistent action depending on selection.
If a prefix arg is given or `helm-follow-mode' is on, then open file."
	(setq candidate (copy-sequence candidate))
	(set-text-properties 0 (length candidate) nil candidate)
	(let ((follow (helm-source-follow helm-current-source)))
		(cond
			((and
					follow
					(or
						(not
							(if helm-ff-ignore-following-on-directory
								(file-exists-p candidate)
								(file-regular-p candidate)))
						(and
							(not (string-match (image-file-name-regexp) candidate))
							(member (file-name-extension candidate)
								helm-ff-follow-blacklist-file-exts))))
				(helm-follow-mode -1)
				(message "Can't follow this kind of file.")
				nil)
			; Tramp methods completion.
			((string-match helm-ff-tramp-method-regexp candidate)
				(let ((method (match-string 1 candidate)))
					`(lambda (_candidate)
						(helm-set-pattern
							(if (helm-ff--tramp-multihops-p ,candidate)
								(concat (match-string 0 ,candidate) ,method ":")
								(concat "/" ,method ":"))))))
			((and
					(helm-ff--invalid-tramp-name-p)
					(string-match helm-ff-tramp-file-name-regexp candidate))
				`(lambda (_candidate)
					(helm-set-pattern
						; First hit insert hostname and
						; second hit insert ":" and expand.
						,(if (string= candidate helm-pattern)
							(concat candidate ":")
							candidate))))
			((file-directory-p candidate)
				; A directory - open it.
				`(lambda (_candidate)
					(let (
						(new-dir (file-name-as-directory (expand-file-name ,candidate)))
					)
						(and
							; If this is a symlink to directory and prefix arg is non-nil,
							; expand it to its truename.
							current-prefix-arg
							(file-symlink-p ,candidate)
							(setq new-dir (file-truename new-dir)))
						(setq helm-ff--show-thumbnails
							(member new-dir helm-ff--thumbnailed-directories))
						(helm-set-pattern new-dir))))
			; A symlink file, expand to it's true name. (first hit)
			((and (file-symlink-p candidate) (not current-prefix-arg) (not follow))
				`(lambda (_candidate)
					(helm-set-pattern (file-truename ,candidate))))
			((member (file-name-extension candidate) helm-ff-sound-file-extensions)
				#'play-sound-file)
			; An image file and it is the second hit on C-j, display it.
			((string-match (image-file-name-regexp) candidate)
				; Old helm-ff--display-or-kill-image-native.
				`(lambda (_candidate)
					; Display images in `helm-ff-image-native-buffer'.
					(if-let (
						(buffer (get-buffer helm-ff-image-native-buffer))
						((file-equal-p (buffer-file-name buffer) ,candidate))
						; Allow redisplaying `helm-ff-image-native-buffer' when
						; it already exists and display same image as candidate.
						((get-buffer-window buffer 'visible))
					)
						(progn
							(set-window-buffer
								helm-persistent-action-display-window
								helm-current-buffer)
							(kill-buffer buffer))
						(helm-ff-image-display-native ,candidate))))
			; Allow browsing archive on avfs fs.
			; Assume volume is already mounted with mountavfs.
			((when-let* (
				helm-ff-avfs-directory
				(dir (file-name-directory candidate))
				((string-match
					(regexp-quote (expand-file-name helm-ff-avfs-directory))
					dir))
			)
					; Whether CANDIDATE is a compressed file or not.
					(member (file-name-extension candidate)
						helm-ff-file-compressed-list))
				`(lambda (_candidate) (helm-set-pattern (concat ,candidate "#/"))))
			(t #'helm-files-persistent-action))))

; Native image display (with image-mode).

(defvar helm-ff-image-cache nil)

(defun helm-ff-image-clean-cache ()
	(mapc #'clear-image-cache helm-ff-image-cache)
	(setq helm-ff-image-cache nil))

(defun helm-ff-image-display-native (candidate)
	(setq candidate (helm-files-expand-and-save-to-history candidate))
	(when (get-buffer helm-ff-image-native-buffer)
		(kill-buffer helm-ff-image-native-buffer))
	; Avoid hight memory consumption see
	; https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-11/msg00879.html.
	(when (length> helm-ff-image-cache (* helm-ff-image-cache-max-len 2))
		; Only keep the last `helm-ff-image-cache-max-len' images in cache.
		(dolist (img (butlast helm-ff-image-cache (1+ helm-ff-image-cache-max-len)))
			(clear-image-cache img)
			(setq helm-ff-image-cache (delete img helm-ff-image-cache))))
	(cl-letf* (
		((symbol-function 'message) #'ignore)
		(buf (find-file-noselect candidate t))
	)
		; When going back reuse the cached images.
		(unless (member candidate helm-ff-image-cache)
			(setq helm-ff-image-cache
				(append helm-ff-image-cache (list (expand-file-name candidate)))))
		(with-current-buffer buf (rename-buffer helm-ff-image-native-buffer))
		(display-buffer buf)))

; Slideshow

(defvar helm-ff--slideshow-iterator nil)
(defvar helm-ff--slideshow-sequence nil)
(defvar helm-ff--slideshow-in-pause nil)
(defvar helm-ff-slideshow-helper
	"Type `\\[helm-ff-slideshow-pause-or-restart]' to %s, \
`\\[helm-ff-slideshow-next]' for next, `\\[helm-ff-slideshow-previous]' for previous, \
`\\[helm-ff-slideshow-quit]' to quit")

(defconst helm-slideshow-mode-map
	(let ((map (make-sparse-keymap)))
		(set-keymap-parent map image-mode-map)
		(define-key map [?\s] #'helm-ff-slideshow-pause-or-restart)
		(define-key map [escape] #'helm-ff-slideshow-quit)
		(define-key map [right] #'helm-ff-slideshow-next)
		(define-key map [left] #'helm-ff-slideshow-previous)
		map))

(define-derived-mode helm-slideshow-mode image-mode "helm-image-mode"
"Mode to display images from helm-ff.

\\{helm-slideshow-mode-map}")

(defun helm-ff-slideshow-set-mode-line (counter-string state)
	(setq mode-line-format
		(concat
			" "
			counter-string
			(substitute-command-keys
				(format helm-ff-slideshow-helper state)))))

(defun helm-ff-start-slideshow-on-marked (_candidate)
"Start a slideshow on marked files."
	(let ((marked (helm-files-marked-candidates t)))
		(if (not (cdr marked))
			(message "Can't start a slideshow on a single file.")
			(setq helm-ff--slideshow-sequence marked)
			(setq helm-ff--slideshow-iterator (helm-iter-circular marked))
			(helm-ff-image-display-native (helm-iter-next helm-ff--slideshow-iterator))
			(delete-other-windows (get-buffer-window helm-ff-image-native-buffer))
			(cl-letf (((symbol-function 'message) #'ignore))
				(helm-slideshow-mode))
			(helm-ff-slideshow-set-mode-line
				(format "(1/%d) " (length marked)) "pause")
			(helm-ff-slideshow-loop))))

(defun helm-ff-slideshow-state ()
	(format "(%d/%d) "
		(1+ (cl-position (buffer-file-name) helm-ff--slideshow-sequence :test #'equal))
		(length helm-ff--slideshow-sequence)))

(defun helm-ff-slideshow-sequence-from-current (&optional reverse)
"Reorganize helm-ff--slideshow-sequence from buffer file name.
Examples:
    (helm-reorganize-sequence-from-elm \\='(a b c d e f g h i j k l) \\='e)
    => (f g h i j k l a b c d e)
    (helm-reorganize-sequence-from-elm \\='(a b c d e f g h i j k l) \\='e t)
    => (d c b a l k j i h g f e)."
	(if reverse
		(let (
			(new (reverse helm-ff--slideshow-sequence))
			(pos (1+ (index_in_list_string= new (buffer-file-name))))
		)
			(append (nthcdr pos new) (ntake pos new)))
		(let (
			(pos
				(1+
					(index_in_list_string=
						helm-ff--slideshow-sequence (buffer-file-name))))
		)
			(append
				(nthcdr pos helm-ff--slideshow-sequence)
				(take pos helm-ff--slideshow-sequence)))))

(defun helm-ff-slideshow-loop (&optional restart)
	(while (sit-for helm-ff-slideshow-delay)
		(helm-ff-image-display-native (helm-iter-next helm-ff--slideshow-iterator))
		(delete-other-windows (get-buffer-window helm-ff-image-native-buffer))
		(cl-letf (((symbol-function 'message) #'ignore))
			(helm-slideshow-mode))
		(when restart
			(message "Slideshow started.")
			(sit-for 1)
			(message nil))
		(helm-ff-slideshow-set-mode-line (helm-ff-slideshow-state) "pause")))

(defun helm-ff-slideshow-pause-or-restart () (interactive)
	(setq helm-ff--slideshow-in-pause (not helm-ff--slideshow-in-pause))
	(if helm-ff--slideshow-in-pause
		(helm-ff-slideshow-set-mode-line nil "restart")
		(message "Slideshow restarting...")
		(setq helm-ff--slideshow-iterator
			(helm-iter-circular (helm-ff-slideshow-sequence-from-current)))
		(helm-ff-slideshow-loop t)))

(defun helm-ff-slideshow--next-or-previous (arg)
	(setq helm-ff--slideshow-in-pause t)
	(setq helm-ff--slideshow-iterator nil)
	(helm-ff-image-display-native
		(car (helm-ff-slideshow-sequence-from-current arg)))
	(delete-other-windows (get-buffer-window helm-ff-image-native-buffer))
	(cl-letf (((symbol-function 'message) #'ignore))
		(helm-slideshow-mode))
	(message "%s"
		(concat
			(helm-ff-slideshow-state)
			(substitute-command-keys (format helm-ff-slideshow-helper "restart")))))

(defun helm-ff-slideshow-next () (interactive)
	(helm-ff-slideshow--next-or-previous nil))

(defun helm-ff-slideshow-previous () (interactive)
	(helm-ff-slideshow--next-or-previous t))

(defun helm-ff-slideshow-quit () (interactive)
	(setq helm-ff--slideshow-iterator nil)
	(setq helm-ff--slideshow-in-pause nil)
	(helm-ff-image-clean-cache)
	(quit-window))

; Thumbnails view

(defvar helm-ff-image-dired-thumbnails-cache (make-hash-table :test 'equal)
"Store associations of image_file/thumbnail_file.")

; Run this crap to test
;(let* (
;	(file "path_to_image")
;	(thumb_file (image-dired-thumb-name file))
;)
;	(image-dired-create-thumb file thumb_file))

(defun helm-ff-maybe-show-thumbnails (candidates)
	(if
		(or
			(not helm-ff--show-thumbnails)
			(file-remote-p helm-ff-default-directory))
		candidates
		(cl-pushnew helm-ff-default-directory helm-ff--thumbnailed-directories
			:test #'equal)
		(mapcar
			(lambda (cand)
				(let (
					(disp (car cand))
					(img (cdr cand))
				)
					(when-let (
						(imgtype
							(let ((ext (file-name-extension img)))
								(cond
									((string= ext "png") 'png)
									((or (string= ext "jpg") (string= ext "jpeg"))
										'jpeg))))
					)
						(setq disp (copy-sequence disp))
						; Remove 'display prop from icon and spaces.
						(remove-text-properties 0 3 '(display) disp)
						; Replace icon with image.
						(put-text-property 1 2
							'display
							(list
								'image
								:type
									(if
										(memq image-dired-thumbnail-storage
											'(standard standard-large))
										'png
										imgtype)
								:margin 5
								:file
									(plist-get
										(cdr
											; Same as `image-dired-get-thumbnail-image' but
											; cache thumbnails for further use.
											(progn
												(unless (string-match (image-file-name-regexp) img)
													(error "%s is not a valid image file" img))
												(let* (
													(thumb-file
														(or
															(gethash
																img
																helm-ff-image-dired-thumbnails-cache)
															(puthash
																img
																(image-dired-thumb-name img)
																helm-ff-image-dired-thumbnails-cache)))
													(thumb-attr (file-attributes thumb-file))
												)
													(when
														(or
															(not thumb-attr)
															(time-less-p
																(file-attribute-modification-time
																	thumb-attr)
																(file-attribute-modification-time
																	(file-attributes img))))
														(image-dired-create-thumb img thumb-file))
													; TODO It's all broken.
													; Ok it turns out it calls by default
													; C:/windows/system32/convert.exe,
													; thats why there are random unhelpful messages.
													; Very epic.
													(create-image thumb-file))))
										:file))
							disp))
					(cons disp img)))
			candidates)))

(defun helm-ff-thumbnails-toggle () (interactive)
	(if (and helm-ff-default-directory (file-remote-p helm-ff-default-directory))
		(message "Thumbnails not supported on remote files.")
		(setq helm-ff--show-thumbnails (not helm-ff--show-thumbnails))
		(if helm-ff--show-thumbnails
			(message "Thumbnails enabled.")
			(message "Thumbnails disabled.")
			(setq helm-ff--thumbnailed-directories
				(delete helm-ff-default-directory helm-ff--thumbnailed-directories)))
		(helm-force-update)))

; Unused.
(defun helm-ff-cleanup-image-dired-dir-and-cache ()
"Cleanup `image-dired-dir' directory.
Delete all thumb files that are no more associated with an existing
image file in `helm-ff-image-dired-thumbnails-cache'."
	(interactive)
	(cl-loop
		for key being the hash-keys in helm-ff-image-dired-thumbnails-cache
			using (hash-value val)
		unless (file-exists-p key) do
			(progn
				(message "Deleting \"%s\"." val)
				(delete-file val)
				(remhash key helm-ff-image-dired-thumbnails-cache))))

(defun helm-insert-file-name-completion-at-point (_candidate)
"Insert file name completion at point.

If there is filename at point, delete it (kind of, it's wrong now, should be fixed).

First marked candidate is treated specially if there is filename at point
and prefix arg is nil - guess from filename at point if filename should be
absolute, abbreviated or relative.

The rest of candidates is handled like this:
default - absolute
1 prefix - abbreviated
2 prefixes - relative
3 prefixes - basename (with slash)."
	(let ((marked (helm-files-marked-candidates t)))
		(with-current-buffer helm-current-buffer
			(if buffer-read-only
				(message "Buffer \"%s\" is read-only." (buffer-name))
				(let (
					(escape_fn
						(if (memq major-mode helm-modes-using-escaped-strings)
							#'shell-quote-argument #'identity))
					(handle_normally
						(lambda (file)
							(cond
								((equal helm-current-prefix-arg '(4))
									(abbreviate-file-name file))
								((equal helm-current-prefix-arg '(16))
									(file-relative-name file))
								((equal helm-current-prefix-arg '(64))
									(helm-files-basename-with-slash file))
								(t file))))
				)
					(insert
						(funcall escape_fn
							(let ((file (car marked)))
								(set-text-properties 0 (length file) nil file)
								(or
									(when-let (
										(guess (helm-ffap-guesser))
										((stringp guess))
										((not (string= guess "")))
										; This is crap - it won't work if caret is inside filename.
										(beg (save-excursion (search-backward guess (pos-bol) t)))
									)
										(delete-region beg (point))
										(and
											(not helm-current-prefix-arg)
											(or
												(string-match "\\`\\([A-Za-z]:\\|~?\\)/" guess)
												(file-exists-p file))
											(cond
												; If guess is absolute.
												((or
														(string-prefix-p (getenv "HOME") guess)
														(string-match-p
															"\\`\\([A-Za-z]:\\)?/"
															guess))
													file)
												((string= (match-string 0 guess) "~/")
													(abbreviate-file-name file))
												(t (file-relative-name file)))))
									(funcall handle_normally file))))
						(mapconcat
							(lambda (file)
								(set-text-properties 0 (length file) nil file)
								(concat
									" "
									(funcall escape_fn (funcall handle_normally file))))
							(cdr marked))))))))

(defun helm-ff-1 (fname &optional preselect)
"Find FNAME filename with PRESELECT filename preselected.

Use it for non-interactive calls of `helm-ff'."
	; Resolve FNAME now outside of helm.
	; FIXME When `helm-ff-1' is used directly from lisp and FNAME
	; is an abbreviated path, for some reason `helm-update' is called
	; many times before resolving the abbreviated path (Bug#1939)
	; so be sure to pass a full path to helm-ff-1.
	(unless (string-match helm-ff-url-regexp fname)
		(setq fname (expand-file-name (substitute-in-file-name fname))))
	(let* (
		(tap (thing-at-point 'filename))
		(def (and tap (or (file-remote-p tap) (expand-file-name tap))))
		; Ensure not being prompted for password each time we
		; navigate to a directory.
		(password-cache t)
	)
		(setf (helm-source-follow helm-ff-source) nil)
		(helm
			:sources (list helm-ff-source)
			:input fname
			:preselect preselect
			:default def
			:prompt "Find files or url: "
			:helm-default-keymap helm-ff-default-keymap
			:helm-case-fold-search helm-file-name-case-fold-search
			:resume "*helm-ff*"))
	(helm-ff-cleanup)
	(setf (helm-source-resume helm-ff-source)
		`(lambda () (setq helm-ff-default-directory ,helm-ff-default-directory)))
	(setq helm-ff-default-directory nil))

(defun helm-ffap-guesser ()
"Same as `ffap-guesser' but without gopher and machine support."
	; Avoid "Stack overflow in regexp matcher" error
	; in evil `ffap-guesser' by removing crap `ffap-gopher-at-point'
	; (bug fixed in emacs-26 http://debbugs.gnu.org/cgi/bugreport.cgi?bug=25391).
	; `ffap-machine-at-point' have been removed too as it was anyway
	; disabled with `ffap-machine-p-known' bound to 'reject.
	; `ffap-file-at-point' can be neutralized with
	; `helm-ff-guess-ffap-filenames' and `ffap-url-at-point' with
	; `helm-ff-guess-ffap-urls'
	; Note also that `ffap-url-unwrap-remote' can override these
	; variables.
	(let (
		(ffap-alist (and helm-ff-guess-ffap-filenames ffap-alist))
		(ffap-url-regexp helm--url-regexp)
	)
		(if (eq major-mode 'dired-mode)
			(if-let (
				(beg (save-excursion (dired-move-to-filename)))
				(end (save-excursion (dired-move-to-end-of-filename t)))
				(the_wrath_of_man
					(member (buffer-substring-no-properties beg end) '("." "..")))
			)
				(concat
					(file-name-as-directory (expand-file-name dired-directory))
					(car the_wrath_of_man))
				(dired-get-filename 'no-dir t))
			(let (
				(ffap
					(if-let* (
						helm-ff-guess-ffap-urls
						(url (ffap-url-at-point))
						((setq url (ffap-fixup-url url)))
						((string-match ffap-url-regexp url))
					)
						url
						(ffap-file-at-point)))
			)
				; Workaround emacs bugs:
				; When the region is active and a file is detected
				; `ffap-string-at-point' returns the region prefixed with
				; "/", e.g. at a beginning of a patch (first bug) and make
				; `file-remote-p' returning an error (second bug), so in such
				; case returns the region itself instead of the region
				; corrupted by ffap.
				(if (and mark-active ffap)
					(buffer-substring-no-properties (region-beginning) (region-end))
					ffap)))))

; Delete and trash files

(defun helm-file-buffers (filename)
"Return a list of buffer names corresponding to FILENAME."
	(setq filename (expand-file-name filename))
	(cl-loop
		for buf in (buffer-list)
		for bfn = (buffer-file-name buf)
		when (equal filename bfn) collect (buffer-name buf)))

(defun helm-ff-should-trash ()
"Return non-nil if should delete by moving to trash."
	(if delete-by-moving-to-trash
		(not (or helm-current-prefix-arg current-prefix-arg))
		(or helm-current-prefix-arg current-prefix-arg)))

(defun helm-files-trashed-files (should_trash)
"If 'should_trash' is non-nil, call helm-ff-trash-list with a trash directory."
	(and
		should_trash
		(not (fboundp 'system-move-file-to-trash))
		; Try to find a trash directory.
		; Return the "files" subdirectory of trash directory.
		; When `helm-trash-default-directory' is set use it as trash directory.
		(let (
			(trash-files-dir
				(expand-file-name "files"
					(or
						helm-trash-default-directory
						(expand-file-name "Trash"
							(or (getenv "XDG_DATA_HOME") "~/.local/share")))))
		)
			; Just return nil if the Trash directory is not yet created.
			; It will be created later by `delete-directory'.
			(when (file-exists-p trash-files-dir)
				(helm-ff-trash-list trash-files-dir)))))

(defun helm-ff-file-already-trashed (file trash-alist)
"Return FILE when it is already in trash.

TRASH-ALIST should be an alist as what `helm-ff-trash-list' returns."
	(cl-loop
		for f in trash-alist
		thereis (file-equal-p file (cdr f))))

(defun helm-delete-marked-files (&optional _candidate dont_kill_helm_current_buffer)
"Delete marked files.
With prefix arg meaning of `delete-by-moving-to-trash' is the opposite.
Return non-nil if something was deleted/trashed."
	(let* (
		(files (helm-files-marked-candidates t))
		(files_length (length files))
		(delete-by-moving-to-trash (helm-ff-should-trash))
	)
		(helm-with-display-candidates
			(map_modify_list #'car (helm-files-highlight files))
			(if
				(y-or-n-p
					(format "%s %d file%s?"
						(if delete-by-moving-to-trash "Trash" "Delete")
						files_length
						(if (> files_length 1) "s" "")))
				(let (
					(trashed_files (helm-files-trashed-files delete-by-moving-to-trash))
					(deleted_count 0)
					(delete_recursively_all
						(or helm-ff-allow-recursive-deletes delete-by-moving-to-trash))
					delete_buffers
				)
					(catch 'break
						(dolist (file files)
							(when
								(cond
									; If file is already trashed.
									((and
											delete-by-moving-to-trash
											(helm-ff-file-already-trashed file trashed_files))
										(message "\"%s\" is already trashed." file)
										(sit-for 1.2)
										nil)
									; If not a directory.
									((not (eq (file-attribute-type (file-attributes file)) t))
										(delete-file file delete-by-moving-to-trash)
										(++ deleted_count))
									(delete_recursively_all
										(delete-directory file t delete-by-moving-to-trash)
										(++ deleted_count))
									; If directory is not empty.
									((directory-files file nil directory-files-no-dot-files-regexp t 1)
										(cl-case
											(helm-read-answer
												(format "Recursively delete \"%s\"? [y,n,!,q]"
													(abbreviate-file-name file))
												"yn!q")
											(?y
												(delete-directory file t delete-by-moving-to-trash)
												t)
											(?!
												(delete-directory file t delete-by-moving-to-trash)
												(setq delete_recursively_all t))
											(?q
												(message "Aborted.")
												(sleep-for 1)
												(throw 'break nil))))
									(t
										(delete-directory file nil delete-by-moving-to-trash)
										(++ deleted_count)))
								; File was just deleted, so maybe delete its buffers too.
								(dolist (buffer (helm-file-buffers file))
									(setq buffer (get-buffer buffer))
									(and
										; For persistent action.
										(not
											(and
												dont_kill_helm_current_buffer
												(eq buffer helm-current-buffer)))
										(cl-case delete_buffers
											(all t)
											(none nil)
											(t
												(cl-case
													(helm-read-answer
														(format "Kill buffer \"%s\" too? [y,n,!,q]" buffer)
														"yn!q")
													(?y t)
													(?! (setq delete_buffers 'all))
													(?n nil)
													; This could be throw 'break too.
													(t (setq delete_buffers 'none)))))
										(kill-buffer buffer))))))
					(if (= deleted_count 0)
						(progn
							(message "No deletions performed.")
							nil)
						(message "%s %d file%s."
							(if delete-by-moving-to-trash "Trashed" "Deleted")
							deleted_count
							(if (> deleted_count 1) "s" ""))
						t))
				(message "No deletions performed.")
				nil))))

; Delete files async

(defconst helm-ff-delete-log-file
	(locate-user-emacs-file "helm_delete_file.log")
"The file used to communicate with Emacs child when deleting files async.")

(defconst helm-ff-delete-async-mode-line t
"Used in global-mode-string by helm-ff-delete-async stuff to recognize
mode-line construct. Always t.")

(defun helm-ff-delete-async-clear-mode-line ()
	(assq-delete-all 'helm-ff-delete-async-mode-line global-mode-string)
	(force-mode-line-update t)
	nil)

; For interactive use.
(defun helm-delete-async-kill-process ()
"Kill async process created by helm delete files async."
	(interactive)
	(let* (
		(processes (dired-async-processes))
		(proc (car (last processes)))
	)
		(when proc (delete-process proc))
		(unless (cdr processes) (helm-ff-delete-async-clear-mode-line))))

(defun helm-delete-marked-files-async (_candidate)
"Same as `helm-delete-marked-files' but async.

When a prefix arg is given, meaning of `delete-by-moving-to-trash'
is the opposite.

This function is not using `helm-delete-file' and BTW not asking
user for recursive deletion of directory, be warned that
directories are always deleted with no warnings."
	(let* (
		(files (helm-files-marked-candidates t))
		(trash (helm-ff-should-trash))
		buffers
		already-trashed
	)
		(let ((trash-alist (helm-files-trashed-files trash)))
			(dolist (f files)
				(when-let ((buf (helm-file-buffers f)))
					(setq buffers (nconc buf buffers)))
				(when-let ((trashed (helm-ff-file-already-trashed f trash-alist)))
					(push trashed already-trashed))))
		(helm-with-display-candidates
			(map_modify_list #'car (helm-files-highlight files))
			(if
				(not
					(y-or-n-p
						(format "%s %d file(s)?"
							(if trash "Trash" "Delete") (length files))))
				(message "No deletions performed.")
				(async-start
					`(lambda ()
						; `delete-by-moving-to-trash' have to be set globally,
						; using the TRASH argument of delete-file or
						; delete-directory is not enough.
						(setq delete-by-moving-to-trash ,trash)
						(let ((result 0))
							(dolist (file ',files)
								(condition-case err
									(cond
										; If file is already trashed.
										((and
												,trash
												(cl-loop
													for f in ',already-trashed
													thereis (file-equal-p f file)))
											(error (format "\"%s\" is already trashed" file)))
										; If file is a directory.
										((eq (file-attribute-type (file-attributes file)) t)
											(delete-directory file t ,trash)
											(++ result))
										(t
											(delete-file file ,trash)
											(++ result)))
									(error
										(with-temp-file ,helm-ff-delete-log-file
											(insert
												(format-time-string "%x:%H:%M:%S\n")
												(format "%s:%s\n"
													(car err)
													(mapconcat #'identity (cdr err) " ")))))))
							result))
					`(lambda (result)
						(helm-ff-delete-async-clear-mode-line)
						(when (file-exists-p helm-ff-delete-log-file)
							(display-warning 'helm
								(with-temp-buffer
									(insert-file-contents helm-ff-delete-log-file)
									(buffer-string))
								:error "*helm-delete-files*")
							(fit-window-to-buffer
								(get-buffer-window "*helm-delete-files*"))
							(delete-file helm-ff-delete-log-file))
						(let (dont-ask)
							(catch 'break
								(dolist (elm ',buffers)
									(if dont-ask
										(kill-buffer elm)
										(cl-case
											(helm-read-answer
												(format
													"Kill buffer `%s', too? [y,n,!,q]"
													elm)
												"yn!q")
											(?y (kill-buffer elm))
											(?n nil)
											(?! (kill-buffer elm) (setq dont-ask t))
											(t (throw 'break nil)))))))
						(run-with-timer 0.1 nil
							; Notify end of async operation in mode-line.
							(lambda ()
								(message nil)
								(let (
									(mode-line-format
										,(concat
											" "
											(propertize_no_copy
												(format "%s (%s/%s) file(s) async done."
													(if trash "Trashing" "Deleting")
													result
													(length files))
												'face 'helm-delete-async-message)))
								)
									(force-mode-line-update)
									(sit-for 3))
								(force-mode-line-update)))))
				; FIXME: Handle jobs like in dired-async, needs first to allow
				; naming properly processes in async, they are actually all named
				; emacs and running `async-batch-invoke', so if one copy a file and
				; delete another file at the same time it may clash.
				(unless (assq 'helm-ff-delete-async-mode-line global-mode-string)
					(nconc
						global-mode-string
						(cons 'helm-ff-delete-async-mode-line nil)))
				(setcar
					(cdr (assq 'helm-ff-delete-async-mode-line global-mode-string))
					(concat
						"  "
						(propertize_no_copy
							(concat
								(if trash "Trashing" "Deleting")
								" file(s) async...")
							'face 'helm-delete-async-message)))
				(force-mode-line-update t)))))


(defun helm-ff-action (_candidate)
"Default action of helm-ff.
If there is are multiple candidates -
	open them (with `find-file-noselect'), with 2 prefix args don't
	show them, with other prefix arg value call `helm-window-show-buffers'
	to show them, there prefix arg may be used to alter window configuration.
If there is one candidate - find or create it, with prefix arg don't show it."
	(let (
		(marked (helm-files-marked-candidates t))
		(ffap-newfile-prompt helm-ff-newfile-prompt-p)
		find-file-wildcards
		(make_dir
			(lambda (dir is_candidate_dir)
				(if-let ((existing_path (helm-files-first-existing-dir dir)))
					(let (
						(first_dir_to_create_file_name
							(substring-no-properties dir
								0
								(string-search "/" dir (length existing_path))))
					)
						(cond
							((file-exists-p first_dir_to_create_file_name)
								(message
									"Unable to create directory \"%s\": file \"%s\" exists."
									dir first_dir_to_create_file_name)
								nil)
							; 'dir' is possible to create.
							; I prefer just 't' now, because, in helm-ff at least,
							; one need to confirm helm-new candidates anyway,
							; so don't ask for yet another confirmation.
							(t
;							((or
;									(not confirm-nonexistent-file-or-buffer)
;									(y-or-n-p
;										(format "Create directory \"%s\"?"
;											(abbreviate-file-name dir))))
								(make-directory dir t)
								t)))
					(message "Unable to create %s \"%s\": invalid path."
						(if is_candidate_dir "directory" "file") candidate)
					nil)))
		(find_file
			(lambda (file)
				(if helm-current-prefix-arg
					(find-file-noselect file)
					(find-file file))))
		candidate
	)
		(setq candidate (car marked))
		(cond
			((cdr marked)
				(if (equal helm-current-prefix-arg '(16))
					(mapc #'find-file-noselect marked)
					; Prefix arg is used by 'helm-window-show-buffers'.
					(helm-window-show-buffers (mapcar #'find-file-noselect marked))))
			((string-match helm--url-regexp candidate) (find-file-at-point candidate))
			((file-exists-p candidate) (funcall find_file candidate))
			((string-suffix-p "/" candidate)
				(when (funcall make_dir candidate t)
					(add-to-history 'helm-ff-history candidate)
					(unless helm-current-prefix-arg
						(setq helm-ff-default-directory candidate)
						; This could be dired instead.
						(helm-ff-1 candidate))))
			(t
				(if-let ((dir (file-name-directory candidate)))
					(cond
						; If base dir exists.
						((file-exists-p dir) (funcall find_file candidate))
						((funcall make_dir dir nil)
							; Hmm, will this ask for confirmation if
							; confirm-nonexistent-file-or-buffer is non-nil?
							; If yes, it should be changed, because user already
							; confirmed it here.
							(funcall find_file candidate)))
					(message "Unable to create file \"%s\": invalid base directory."
						candidate))))))

(defun helm-ff-multi-copy (_candidate)
"Copy the car of marked candidates to the remaining marked candidates.

The car of marked should be a regular file and the rest of marked (cdr) should
be directories."
	(let (file dirs)
		(let ((marked (helm-files-marked-candidates t)))
			(setq file (car marked))
			(setq dirs (cdr marked)))
		(cond
			((not (file-regular-p file))
				(message "Error: \"%s\" is not a regular file." file))
			((not dirs)
				(message "Error: No destination specified for file \"%s\"." file))
			((when-let (
				(non_dir (find_in_list dirs (lambda (dir) (not (file-directory-p dir)))))
			)
					(message "Error: Destinations must be directories (\"%s\" is not)."
						non_dir)
					t))
			(t
				(let ((skipped 0) (basename (file-name-nondirectory file)))
					(setq dirs
						(cl-delete-if
							(lambda (dir)
								(unless (file-accessible-directory-p dir)
									(++ skipped)
									t))
							dirs))
					(catch 'break
						(let (yes-for-all)
							(setq dirs
								(cl-delete-if
									(lambda (dir)
										(and
											(not yes-for-all)
											(file-exists-p (expand-file-name basename dir))
											(cl-case
												(helm-read-answer
													(format
														"File \"%s\" already exists, overwrite? [y,n,!,q]"
														(expand-file-name basename dir))
													"yn!q")
												(?y nil)
												(?n (++ skipped) t)
												(?! (setq yes-for-all t) nil)
												(t
													(message "Operation aborted.")
													(throw 'break nil)))))
									dirs)))
						(when dirs
							(helm-with-display-candidates
								(map_modify_list #'car (helm-files-highlight dirs))
								(if (y-or-n-p (format "Copy \"%s\" to directories?" basename))
									(let ((copies 0))
										(dolist (dir dirs)
											(condition-case nil
												(progn
													(copy-file file dir t)
													(++ copies))
												(file-error (++ skipped))))
										(message
											"%d %s of \"%s\" done%s."
											copies
											(if (> copies 1) "copies" "copy")
											basename
											(if (> skipped 0)
												(format ", %d file%s skipped"
													skipped
													(if (> skipped 1) "s" ""))
												"")))
									(message "Operation aborted.")))))))))
	nil)

(defun helm-file-on-mounted-network-p (file)
"Return non-nil when FILE is part of a mounted remote directory.
This function is checking `helm-mounted-network-directories' list."
	(cl-loop
		for dir in helm-mounted-network-directories
		thereis (file-in-directory-p file dir)))

; filecache

(require 'filecache)

(defun helm-ff-cache-add-file (_candidate)
	(mapc #'file-cache-add-file (helm-files-marked-candidates t)))

(defun helm-ff-cache-remove-file (_candidate)
"Remove marked files from `file-cache-alist'."
	(dolist (file (helm-files-marked-candidates t))
		(let* (
			(entry (assoc (helm-basename file) file-cache-alist))
			(new-entry (remove (file-name-directory file) entry))
		)
			(when (length= entry 1) (setq new-entry nil))
			(setq file-cache-alist (cons new-entry (delete entry file-cache-alist))))))

(defconst helm-file-cache-source
	(helm-source-files-make nil
		:name "File cache"
		:candidates
			(lambda ()
				(mapcan
					(lambda (item)
						(let ((base (car item)))
							(mapcar
								(lambda (dir) (concat dir base))
								(cdr item))))
					file-cache-alist))
		:action
			(append
				helm-files-actions
				(list
					(cons
						"Remove marked files from file cache"
						#'helm-ff-cache-remove-file)))))

(defun helm-file-cache () "helm for filecache." (interactive)
	(helm
		:sources (list helm-file-cache-source)
		:helm-default-keymap helm-files-default-keymap
		:helm-case-fold-search helm-file-name-case-fold-search))

; Files in files (crap right now)

; Old comment when every file had its own source:
;
; Temporary helm sources don't make much sense -
; can't have persistent settings like skipping boring or not, etc.
; Also mostly useless separation - can't mark across sources,
; different mode-line, keymaps, everything.
; Helm should support inserting separating lines, like now are source-header-lines,
; so multiple files could be easily distinguished, or maybe do it like helm-grep -
; add prefix to every match. The second soulution would nicely "support" pattern
; matching, because then headers would be a pain - better to sort candidates
; with helm-generic-sort rather than keep them separated in files (I think).
; So I think the reasonable solution for now would be to make a single
; (defconst) source that would be like helm-grep, and initial sorting
; would "separate" candidates into different files, but after pattern matching
; it would change. Also instead of C-S-down (helm-goto-next-source), there would
; be a binding for jumping to next file.

(defvar helm-files-in-files-file-count)
(defvar helm-files-in-files-marked-candidates)

(defconst helm-files-in-files-source
	(helm-source-files-make nil
		:name
			(lambda ()
				(concat
					"Files in files (from "
					(number-to-string helm-files-in-files-file-count)
					" file"
					(when (/= helm-files-in-files-file-count 1) "s")
					")"))
		:candidates
			(lambda ()
				(setq helm-files-in-files-file-count 0)
				(with-temp-buffer
					(mapcan
						(lambda (file)
							(when (file-regular-p file)
								(++ helm-files-in-files-file-count)
								(erase-buffer)
								(insert-file-contents file)
								(goto-char (point-min))
								; This is what searches for files, it's not great.
								; TODO Also like this comment above describes, this should
								; prepend but not match "basename:line:", like in
								; helm-grep, so it would need to change
								; helm-files-candidate-transformer to adjust icons and
								; every 'match-part property, and also align this in
								; columns, and also add binding to keymap to jump to
								; next/prev file. Also an action to jump to file and line.
								(cl-loop
									with candidate = nil
									with dir = (file-name-directory file)
									until (eobp)
									for line =
										(buffer-substring-no-properties
											(point) (goto-char (pos-eol)))
									unless (string= line "")
										if
											(file-exists-p
												(setq candidate
													; Try expanding from file's directory.
													; This could be more sophisticated,
													; e.g. trying current's project root
													; dir and more.
													(expand-file-name line dir)))
											collect candidate
										else
											nconc
												(cl-loop
													for possible_file in (split-string line ",;" t " ")
													when
														(file-exists-p
															(setq possible_file
																(expand-file-name possible_file dir)))
														collect possible_file)
									do (forward-char 1))))
						helm-files-in-files-marked-candidates)))))

(defun helm-ff-in-file (_candidate)
"Helm action for listing filenames found in marked files."
	; Save marked candidates here because when :candidates function will run
	; helm-buffer will be empty and also then it's possible to use helm-resume
	; and helm-force-update correctly.
	(setq helm-files-in-files-marked-candidates (helm-marked-candidates))
	(helm
		:sources (list helm-files-in-files-source)
		:quit-if-no-candidate (lambda () (message "No files found."))
		:helm-default-keymap helm-files-default-keymap
		:helm-case-fold-search helm-file-name-case-fold-search))

; File name history and helm-ff-history.

(defun helm-files-save-to-file-name-history (file)
"Save 'file' to `file-name-history'.

Most of actions and keybinds in helm-files should use this.

Ehh, it's a mess... For now there are more than 60 uses of this function
in this file, maybe it would be better to use some hook that adds marked
candidates if they appear to be files."
	(add-to-history 'file-name-history (abbreviate-file-name file)))

(defun helm-files-expand-and-save-to-history (file)
	(setq file (expand-file-name file))
	(helm-files-save-to-file-name-history file)
	file)

(defun helm-files-marked-candidates (&optional with-wildcard)
	(map_modify_list
		#'helm-files-expand-and-save-to-history
		(helm-marked-candidates with-wildcard)))


(cl-defstruct
	(helm-source-files-history
		(:copier nil)
		(:constructor helm-source-files-history--make)
		(:include helm-source-files
			(mode-line #'helm-files-history-mode-line)
			; I think it's better to show here everything by default.
			(skip-boring nil)))
	skip-deleted)

(helm-source-define-constructor "helm-source-files-history" "helm-source-files" nil
	(lambda ()
		(setf (helm-source-candidates source)
			`(lambda ()
				(helm-files-history-skip-deleted
					(,(helm-source-candidates source))
					helm-current-source)))))

(defun helm-files-history-mode-line ()
	(concat
		(helm-files-mode-line)
		", "
		(if (helm-source-files-history-skip-deleted helm-current-source)
			"skip" "show")
		" deleted  "))

(defun helm-files-history-skip-deleted (candidates source)
	(if (helm-source-files-history-skip-deleted source)
		(cl-remove-if
			(lambda (cand)
				(not
					(or
						; Don't call file-exists-p if file is remote.
						(file-remote-p (cdr cand))
						(file-exists-p (cdr cand)))))
			candidates)
		candidates))

(defun helm-files-history-skip-deleted-toggle () (interactive)
	(helm-files-toggle-base
		(lambda (source)
			(setf (helm-source-files-history-skip-deleted source)
				(not (helm-source-files-history-skip-deleted source)))
			(puthash
				source
				(helm-files-history-skip-deleted (helm-source-files-cache source) source)
				helm-candidate-cache))))

(let (
	(default_keymap (make-sparse-keymap))
	keymap
	(nested_action
		(lambda (candidate)
			(helm-set-pattern (helm-files-expand-and-save-to-history candidate))))
)
	(define-key default_keymap [?\C-s ?d] #'helm-files-history-skip-deleted-toggle)

	(setq keymap (copy-sequence default_keymap))
	(set-keymap-parent keymap helm-files-keymap)

	(set-keymap-parent default_keymap helm-files-default-keymap)

	; For file-name-history and helm-ff-history:
	;	create sources (nested and non nested versions)
	;	create command (one, with one 'if' choosing which version of source to use)
	(dolist (
		history_symbol_prefix_name_source_name
		(list
			(list 'file-name-history "helm-file-name-history" "File name history")
			(list 'helm-ff-history "helm-ff-history" "Helm find files history"))
	)
		(let (
			(history_symbol (pop history_symbol_prefix_name_source_name))
			(prefix_name (car history_symbol_prefix_name_source_name))
			(source_name (nth 1 history_symbol_prefix_name_source_name))
		)
			; Create sources.
			(let* (
				(source
					(helm-source-files-history-make nil
						:name source_name
						:candidates `(lambda () ,history_symbol)
						:keymap
							(let ((map (make-sparse-keymap)))
								(set-keymap-parent map keymap)
								map)))
				(nested_source (copy-sequence source))
			)
				; Adjust nested source.
				(setf (helm-source-name nested_source) (concat source_name " nested"))
				(setf (helm-source-keymap nested_source)
					(let ((map (make-sparse-keymap)))
						(set-keymap-parent map default_keymap)
						map))
				(setf (helm-source-action nested_source) nested_action)
				(setf (helm-source-action-transformer nested_source) nil)

				; Add delete bindings.
				(dolist (source (list source nested_source))
					(helm-add-delete-binds
						(helm-source-keymap source)
						`(lambda (cand cache)
							; History variable.
							(setq ,history_symbol (delete cand ,history_symbol))
							; helm-source-files's cache.
							(setf (helm-source-files-cache ',source)
								(rassq-delete-all
									cand (helm-source-files-cache ',source)))
							; Helm's general cache.
							(rassq-delete-all cand cache))
						t))

				; Create sources' symbols.
				(let (
					(source_symbol (intern (concat prefix_name "-source")))
					(nested_source_symbol (intern (concat prefix_name "-nested-source")))
				)
					(set source_symbol source)
					(set nested_source_symbol nested_source)

					; Create command.
					(fset (intern prefix_name)
						`(lambda ()
							,(format
"`%s' helm source.
Can be used with limited functionality in `helm-ff'."
								(symbol-name history_symbol))
							(interactive)
							(if helm-alive-p
								; Nested.
								(helm
									:sources (list ,nested_source_symbol)
									:allow-nest t
									:helm-default-keymap ',default_keymap
									:helm-case-fold-search helm-file-name-case-fold-search)
								; Normal.
								(helm
									:sources (list ,source_symbol)
									:helm-default-keymap ',default_keymap
									:helm-case-fold-search helm-file-name-case-fold-search)))))))))

(defvar helm-ff-history-display-function nil)
(defvar helm-ff-history-last-frame-parameters nil)

(setf (helm-source-cleanup helm-ff-history-nested-source)
	(lambda ()
		(setq helm-ff-history-display-function helm-display-function)
		(setq helm-ff-history-last-frame-parameters helm--last-frame-parameters)))

; Browse project
; Need dependencies:
; <https://github.com/emacs-helm/helm-ls-git>
; <https://github.com/emacs-helm/helm-ls-hg>
; Only hg and git are supported for now.

(defun helm-browse-project-rg (_candidate)
	(let ((default-directory (helm-default-directory)))
		(helm-files-save-to-file-name-history default-directory)
		(helm-grep-rg)))

; name will be set before every helm call.
(defconst helm-browse-project-source
	(helm-source-files-make nil
		:candidates
			(lambda ()
				(split-string
					(with-temp-buffer
						(call-process helm-fd-exe nil t nil
							"--hidden" "--color=never" "--path-separator=/")
						; Last line is empty.
						(buffer-substring-no-properties (point-min) (1- (point-max))))
					"\n"))
		:keymap
			(let ((map (make-sparse-keymap)))
				(set-keymap-parent map helm-files-keymap)
				(define-key map [?\A-G]
					(helm-make-action-command #'helm-browse-project-rg))
				map)
		:action
			(helm-append-at-nth
				helm-files-actions
				(list (cons "Grep project with RG" #'helm-browse-project-rg))
				7)
		:candidate-number-limit 9999))

; History

(defvar helm-browse-project-history nil
"History variable for `helm-browse-project'.")

(defconst helm-browse-project-history-source
	(helm-source-history-make 'helm-browse-project-history nil
		:name "Project history"
		:action
			(lambda (candidate)
				(let ((default-directory candidate))
					(helm-browse-project helm-current-prefix-arg)))))

(defun helm-browse-project-history ()
"Jump to project already visisted with `helm-browse-project'."
	(interactive)
	(helm
		:sources (list helm-browse-project-history-source)
		:helm-case-fold-search helm-file-name-case-fold-search))

; Browse buffers in project

; name will be set before every helm call.
(defconst helm-browse-project-buffers-source
	(helm-source-buffers-make nil
		:candidates
			(lambda ()
				(cl-loop
					for buffer_name in (helm-buffers-list)
					for buffer = (get-buffer buffer_name)
					for buffer_file_name = (buffer-file-name buffer)
					for default_dir = (buffer-local-value 'default-directory buffer)
					when
						(cond
							(buffer_file_name
								(file-in-directory-p buffer_file_name default-directory))
							((not (file-remote-p default_dir))
								(file-in-directory-p default_dir default-directory)))
						collect buffer_name))))

; Main function

(defun helm-browse-project (arg &optional default)
"helm to browse current project.
Browse files and see status of project with its VCS.
Only HG and GIT are supported for now.
With prefix arg browse buffers, not files.

Needed dependencies for VCS:
<https://github.com/emacs-helm/helm-ls-git>
<https://github.com/emacs-helm/helm-ls-hg>."
	(interactive "P")
	(if arg
		(let ((default-directory (helm-current-directory)))
			; Browse buffers.
			(helm-files-save-to-file-name-history default-directory)
			(setf (helm-source-name helm-browse-project-buffers-source)
				(concat
					"Buffers in project ("
					(abbreviate-file-name default-directory)
					")"))
			(helm
				:sources (list helm-browse-project-buffers-source)
				; Yes this regex works for helm-buffers too.
				:preselect (helm-ff-regex-for-preselection (buffer-name))
				:default (buffer-name)
				:helm-default-keymap helm-buffers-default-keymap))
		; Browse files.
		(let (
			root_dir
			(push-to-hist
				(lambda ()
					(helm-files-save-to-file-name-history root_dir)
					(add-to-history 'helm-browse-project-history root_dir)))
		)
			(cond
				((setq root_dir
						(and
							(require 'helm-ls-git nil t)
							(fboundp 'helm-ls-git-root-dir)
							(helm-ls-git-root-dir)))
					(funcall push-to-hist)
					(helm-ls-git))
				((setq root_dir
						(and
							(require 'helm-ls-hg nil t)
							(fboundp 'helm-hg-root)
							(helm-hg-root)))
					(funcall push-to-hist)
					(helm-hg-find-files-in-project))
				(t
					(setq root_dir (helm-current-directory))
					(funcall push-to-hist)
					(setf (helm-source-name helm-browse-project-source)
						(concat "Browse project (" (abbreviate-file-name root_dir) ")"))
					(let ((default-directory root_dir) preselect)
						(when default
							(setq default (file-name-nondirectory default))
							(setq preselect (helm-ff-regex-for-preselection default)))
						(helm
							:sources (list helm-browse-project-source)
							:default default
							:preselect preselect
							:helm-default-keymap helm-files-default-keymap
							:helm-case-fold-search helm-file-name-case-fold-search)))))))

; Actions calling helm and main interactive functions.

(defun helm-ff-browse-project (candidate)
"Browse project in current directory.
See `helm-browse-project'."
	(with-helm-default-directory helm-ff-default-directory
		(helm-browse-project helm-current-prefix-arg candidate)))

(defun helm-ff-gid (_candidate)
	(with-helm-default-directory helm-ff-default-directory
		(helm-files-save-to-file-name-history default-directory)
		(helm-gid)))

(defun helm-ff-find-command (_candidate) "Run `helm-find' from `helm-ff'."
	(helm-files-save-to-file-name-history
		(or helm-ff-default-directory default-directory))
	(helm-find-1 helm-ff-default-directory))


(defun helm-ff (arg)
"helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-ff-1' instead.
This is the starting point for nearly all actions you can do on files."
	(interactive "P")
	(let* (
		; Disable tramp-archive which is kicking in unexpectedly.
		tramp-archive-enabled
		(hist (and arg helm-ff-history (helm-ff-history)))
		(smart-input
			(or
				hist
				; Try to guess initial input.
				(unless (eq major-mode 'image-mode)
					(let ((guesser (helm-ffap-guesser)))
						(when (equal guesser "") (setq guesser nil))
						(let* (
							(file-at-pt
								(if
									(and
										helm-ff-allow-non-existing-file-at-point
										guesser
										(not (string-match ffap-url-regexp guesser)))
									; Keep the ability of jumping to numbered lines even
									; when allowing non existing filenames at point.
									(when-let ((filename (thing-at-point 'filename)))
										(replace-regexp-in-string ":[0-9]+\\'" "" filename))
									guesser))
							(thing-at-pt (thing-at-point 'filename))
							(not_remp
								(not
									(or
										(and file-at-pt (file-remote-p file-at-pt))
										(and thing-at-pt (file-remote-p thing-at-pt)))))
							(urlp
								(and
									file-at-pt
									(string-match helm--url-regexp file-at-pt)))
						)
							(cond
								; String at point is an hyperlink.
								; Try to find link to an url in text-property at point.
								((let ((he (get-text-property (point) 'help-echo)))
										; Org link.
										(when (and (stringp he) (string-match "^LINK: " he))
											(setq he (replace-match "" t t he)))
										(cl-loop
											for i in
												(list
													he
													(when-let ((ov (overlays-at (point))))
														(overlay-get (car ov) 'help-echo))
													(get-text-property (point) 'w3m-href-anchor)
													(get-text-property (point) 'nt-link))
											thereis
												(and
													(stringp i)
													(string-match helm--url-regexp i)
													i))))
								; A regular file.
								((and
										file-at-pt
										(not (string= file-at-pt ""))
										not_remp
										(or
											(file-exists-p file-at-pt)
											helm-ff-allow-non-existing-file-at-point)
										(not urlp)
										thing-at-pt
										(not (string= thing-at-pt ""))
										(file-exists-p
											(file-name-directory
												(expand-file-name thing-at-pt
													(helm-current-directory)))))
									(if (member (helm-basename file-at-pt) '("." ".."))
										file-at-pt
										(expand-file-name file-at-pt)))
								; Possibly an url or email.
								(urlp (helm-html-decode-entities-string file-at-pt))
								((and
										file-at-pt
										not_remp
										(or
											helm-ff-allow-non-existing-file-at-point
											(file-exists-p file-at-pt)))
									(expand-file-name file-at-pt))))))))
		(default-input (expand-file-name (helm-current-directory)))
		(input
			(cond
				((and (null hist) helm-ff-ignore-thing-at-point) default-input)
				((and (eq major-mode 'org-agenda-mode) org-directory (not smart-input))
					(file-name-as-directory (expand-file-name org-directory)))
				((and (eq major-mode 'dired-mode) smart-input)
					(file-name-directory smart-input))
				((and (not (string= smart-input "")) smart-input))
				(t default-input)))
	)
		; Continue using the same display function as history which used
		; probably itself the same display function as inner HFF call,
		; i.e. if history was using frame use a frame otherwise use a window.
		(and
			hist
			helm-ff-history-display-function
			(helm-set-local-variable
				'helm-display-function helm-ff-history-display-function
				'helm--last-frame-parameters helm-ff-history-last-frame-parameters))
		(set-text-properties 0 (length input) nil input)
		(setq current-prefix-arg nil)
		(helm-ff-1
			input
			(when-let (
				((not helm-ff-no-preselect))
				(preselect
					(or
						hist
						(when-let (
							(file
								(or
									(and (not (file-directory-p input)) input)
									(buffer-file-name)
									(and (eq major-mode 'dired-mode) smart-input)))
						)
							(helm-basename file))))
			)
				(helm-ff-regex-for-preselection preselect)))))

(provide 'helm-files)
