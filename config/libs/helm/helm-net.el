; -*- lexical-binding:nil -*-

(require 'url)
(require 'xml)
(require 'browse-url)

(defconst helm-google-suggest-default-browser-function nil
"The browse url function you prefer to use with Google suggest.
When nil, use the first browser function available
See `helm-browse-url-default-browser-alist'.")

(defconst helm-home-url "https://www.google.com"
"Default url to use as home url.")

(defconst helm-surfraw-default-browser-function nil
"The browse url function you prefer to use with surfraw.
When nil, fallback to `browse-url-browser-function'.")

(defconst helm-google-suggest-url
	"https://encrypted.google.com/complete/search?output=toolbar&q=%s"
"URL used for looking up Google suggestions.
This is a format string, don't forget the `%s'.")

(defconst helm-google-suggest-search-url
	"https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q=%s"
"URL used for Google searching.
This is a format string, don't forget the `%s'.")

(defconst helm-net-prefer-curl nil
"When non-nil use CURL external program to fetch data.
Otherwise `url-retrieve-synchronously' is used.")

(defconst helm-surfraw-duckduckgo-url "https://duckduckgo.com/lite/?q=%s&kp=1"
"The Duckduckgo url.
This is a format string, don't forget the `%s'.
If you have personal settings saved on duckduckgo you should have
a personal url, see your settings on duckduckgo.")

(defconst helm-search-suggest-action-wikipedia-url
	"https://en.wikipedia.org/wiki/Special:Search?search=%s"
"The Wikipedia search url.
This is a format string, don't forget the `%s'.")

(defconst helm-search-suggest-action-youtube-url
	"https://www.youtube.com/results?aq=f&search_query=%s"
"The Youtube search url.
This is a format string, don't forget the `%s'.")

(defconst helm-search-suggest-action-imdb-url
	"http://www.imdb.com/find?s=all&q=%s"
"The IMDb search url.
This is a format string, don't forget the `%s'.")

(defconst helm-search-suggest-action-google-maps-url
	"https://maps.google.com/maps?f=q&source=s_q&q=%s"
"The Google Maps search url.
This is a format string, don't forget the `%s'.")

(defconst helm-search-suggest-action-google-news-url
	"https://www.google.com/search?safe=off&prmd=nvlifd&source=lnms&tbs=nws:1&q=%s"
"The Google News search url.
This is a format string, don't forget the `%s'.")

(defconst helm-browse-url-firefox-new-window "--new-tab"
"Allow choosing to browse url in new window or new tab.
Can be \"--new-tab\" (default), \"--new-window\" or \"--private-window\".")

(defconst helm-net-curl-options '("-s" "-L")
"Arguments list passed to curl when using `helm-net-prefer-curl'.")

; Additional actions for search suggestions

(defvar helm-net-curl-log-file (expand-file-name "helm-curl.log" user-emacs-directory))
(defun helm-search-suggest-perform-additional-action (url query)
"Perform the search via URL using QUERY as input."
	(browse-url (format url (url-hexify-string query))))

; Google Suggestions

(defun helm-google-suggest-set-candidates (&optional request-prefix)
"Set candidates with result and number of Google results found."
	(let (
		(suggestions
			; Fetch suggestions for INPUT from XML buffer.
			(let* (
				(input
					(if request-prefix
						(concat request-prefix " " helm-pattern)
						helm-pattern))
				(request (format helm-google-suggest-url (url-hexify-string input)))
				(suggest_parser
					(lambda ()
						(mapcar
							(lambda (i) (cdr (car (car (cdr (assq 'suggestion i))))))
							(xml-get-children
								(car (xml-parse-region (point-min) (point-max)))
								'CompleteSuggestion))))
			)
				(if helm-net-prefer-curl
					(with-temp-buffer
						(apply #'call-process
							"curl"
							nil
							(list t helm-net-curl-log-file)
							nil
							request
							helm-net-curl-options)
						(funcall suggest_parser))
					(with-current-buffer (url-retrieve-synchronously request)
						(funcall suggest_parser)))))
	)
		(if (member helm-pattern suggestions)
			suggestions
			; if there is no suggestion exactly matching the input then
			; prepend a Search on Google item to the list
			(append
				suggestions
				(list
					(cons
						(format "Search for '%s' on Google" helm-pattern)
						helm-pattern))))))

(defconst helm-google-suggest-source
	(helm-source-sync-make nil
		:name "Google Suggest"
		:candidates #'helm-google-suggest-set-candidates
		:sort nil ; As long as helm-google-suggest-set-candidates adds this last cand.
		:action
			(list
				(cons
					"Google Search"
					; Default action to jump to a Google suggested candidate.
					(lambda (candidate)
						(funcall
							(or
								helm-google-suggest-default-browser-function
								#'helm-browse-url)
							(format helm-google-suggest-search-url
								(url-hexify-string candidate)))))
				(cons
					"Wikipedia"
					(lambda (candidate)
						(helm-search-suggest-perform-additional-action
							helm-search-suggest-action-wikipedia-url
							candidate)))
				(cons
					"Youtube"
					(lambda (candidate)
						(helm-search-suggest-perform-additional-action
							helm-search-suggest-action-youtube-url
							candidate)))
				(cons
					"IMDb"
					(lambda (candidate)
						(helm-search-suggest-perform-additional-action
							helm-search-suggest-action-imdb-url
							candidate)))
				(cons
					"Google Maps"
					(lambda (candidate)
						(helm-search-suggest-perform-additional-action
							helm-search-suggest-action-google-maps-url
							candidate)))
				(cons
					"Google News"
					(lambda (candidate)
						(helm-search-suggest-perform-additional-action
							helm-search-suggest-action-google-news-url
							candidate))))
		:volatile t
		:requires-pattern 3))

(defun helm-google-suggest-emacs-lisp ()
"Try to emacs lisp complete with Google suggestions."
	(helm-google-suggest-set-candidates "emacs lisp"))

(defun helm-google-suggest () (interactive)
	(helm :sources (list helm-google-suggest-source)))

; Web browser functions.

; If default setting of `w3m-command' is not
; what you want and you modify it, you will have to reeval
; also `helm-browse-url-default-browser-alist'.

(defvar helm-browse-url-chromium-program "chromium-browser")
(defvar helm-browse-url-uzbl-program "uzbl-browser")
(defvar helm-browse-url-nyxt-program "nyxt")
(defvar helm-browse-url-conkeror-program "conkeror")
(defvar helm-browse-url-opera-program "opera")
(defvar helm-browse-url-w3m-program
	(or (and (boundp 'w3m-command) w3m-command) (executable-find "w3m")))
(defvar helm-browse-url-default-browser-alist
	'(
		(helm-browse-url-w3m-program . w3m-browse-url)
		(browse-url-firefox-program . browse-url-firefox)
		(helm-browse-url-chromium-program . helm-browse-url-chromium)
		(helm-browse-url-conkeror-program . helm-browse-url-conkeror)
		(helm-browse-url-opera-program . helm-browse-url-opera)
		(helm-browse-url-uzbl-program . helm-browse-url-uzbl)
		(helm-browse-url-nyxt-program . helm-browse-url-nyxt)
		(browse-url-kde-program . browse-url-kde)
		(browse-url-gnome-moz-program . browse-url-gnome-moz)
		(browse-url-mozilla-program . browse-url-mozilla)
		(browse-url-galeon-program . browse-url-galeon)
		(browse-url-netscape-program . browse-url-netscape)
		(browse-url-xterm-program . browse-url-text-xterm)
		("emacs" . eww-browse-url)
	)
"Alist of (browse_url_variable . function) to try to find a suitable url browser.")

(defun helm-generic-browser (url name &rest args) "Browse URL with NAME browser."
	(message "Starting %s..." name)
	(make-process
		:name (concat name " " url)
		:command (nconc (list name) args (list url))
		:sentinel
			(lambda (process event)
				(when (string= event "finished\n")
					(message "%s process %s."
						process (string-replace "\n" " " event))))))

(defun helm-browse-url-firefox (url &optional _)
"Same as `browse-url-firefox' but detach from Emacs.

So when you quit Emacs you can keep your Firefox session open and
not be prompted to kill the Firefox process.

NOTE: Probably not supported on some systems (e.g. Windows)."
	(interactive (list (read-string "URL: " (browse-url-url-at-point)) nil))
	(setq url (browse-url-encode-url url))
	(let ((process-environment (browse-url-process-environment)))
		(call-process-shell-command
			(format "(%s %s %s &)"
				browse-url-firefox-program
				helm-browse-url-firefox-new-window
				(shell-quote-argument url)))))

(defun helm-browse-url-opera (url &optional _)
"Browse URL with Opera browser and detach from Emacs.

So when you quit Emacs you can keep your Opera session open and
not be prompted to kill the Opera process.

NOTE: Probably not supported on some systems (e.g. Windows)."
	(interactive (list (read-string "URL: " (browse-url-url-at-point)) nil))
	(setq url (browse-url-encode-url url))
	(let ((process-environment (browse-url-process-environment)))
		(call-process-shell-command
			(format "(%s %s &)"
				helm-browse-url-opera-program (shell-quote-argument url)))))

(defun helm-browse-url-chromium (url &optional _)
"Browse URL with Google Chrome browser."
	(interactive "sURL: ")
	(helm-generic-browser url helm-browse-url-chromium-program))

(defun helm-browse-url-uzbl (url &optional _)
"Browse URL with uzbl browser."
	(interactive "sURL: ")
	(helm-generic-browser url helm-browse-url-uzbl-program "-u"))

(defun helm-browse-url-conkeror (url &optional _)
"Browse URL with conkeror browser."
	(interactive "sURL: ")
	(helm-generic-browser url helm-browse-url-conkeror-program))

(defun helm-browse-url-nyxt (url &optional _)
"Browse URL with nyxt browser."
	(interactive "sURL: ")
	(helm-generic-browser url helm-browse-url-nyxt-program))

(defun helm-browse-url-default-browser (url &rest args)
"Find the first available browser and ask it to load URL."
	(let (
		(default-browser-fn
			(cl-loop
				for (var . fn) in helm-browse-url-default-browser-alist
				for exe =
					(if (stringp var)
						var
						(and (boundp var) (symbol-value var)))
				thereis (and exe (executable-find exe) (fboundp fn) fn)))
	)
		(if default-browser-fn
			(apply default-browser-fn url args)
			(error "No usable browser found"))))

(defun helm-browse-url (url &rest args)
"Default command to browse URL."
	(if browse-url-browser-function
		(browse-url url args)
		(helm-browse-url-default-browser url args)))

; Surfraw

; Need external program surfraw.
; <http://surfraw.alioth.debian.org/>

; Internal
(defvar helm-surfraw-engines-history nil)
(defvar helm-surfraw-input-history nil)
(defvar helm-surfraw--elvi-cache nil)

(defun helm-build-elvi-list ()
"Return list of all engines and descriptions handled by surfraw."
	(or
		helm-surfraw--elvi-cache
		(setq helm-surfraw--elvi-cache
			(cdr
				(with-temp-buffer
					(call-process "surfraw" nil t nil "-elvi")
					(split-string (buffer-string) "\n"))))))

(defun helm-surfraw (pattern engine)
"helm to search PATTERN with search ENGINE."
	(interactive
		(list
			(let (
				(default
					(if mark-active
						(buffer-substring-no-properties (region-beginning) (region-end))
						(thing-at-point 'symbol)))
			)
				(read-string
					(if default
						(format "Search for (default %s): " default)
						"Search for: ")
					nil
					'helm-surfraw-input-history
					default))
			(helm-comp-read "Engine: " (helm-build-elvi-list)
				:must-match t
				:name "Surfraw Search Engines"
				:history 'helm-surfraw-engines-history)))
	(let* (
		(engine-nodesc (car (split-string engine)))
		(url
			(if (string= engine-nodesc "duckduckgo")
				; "sr duckduckgo -p foo" is broken, workaround.
				(format helm-surfraw-duckduckgo-url (url-hexify-string pattern))
				(with-temp-buffer
					(apply #'call-process
						"surfraw"
						nil
						t
						nil
						(nconc (list engine-nodesc "-p") (split-string pattern)))
					(string-replace "\n" "" (buffer-string)))))
		(browse-url-browser-function
			(or helm-surfraw-default-browser-function browse-url-browser-function))
	)
		(if (string= engine-nodesc "W")
			(helm-browse-url helm-home-url)
			(helm-browse-url url))))

(provide 'helm-net)
