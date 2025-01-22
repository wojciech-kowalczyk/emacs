; -*- lexical-binding:nil -*-

; company-mode completion backend for BBDB in message-mode.

(defconst company::bbdb::mode_list '(message-mode)
"List with major modes in which `company-bbdb' may complete.")


(declare-function bbdb-record-get-field "bbdb")
(declare-function bbdb-records "bbdb")
(declare-function bbdb-dwim-mail "bbdb-com")
(declare-function bbdb-search "bbdb-com")

(defun company-bbdb (command &optional arg)
"`company-mode' completion backend for BBDB."
(interactive '(interactive))
	(cl-case command
		(interactive (company-begin-backend 'company-bbdb))
		(prefix
			(and
				(memq major-mode company::bbdb::mode_list)
				(featurep 'bbdb-com)
				(let ((case-fold-search t))
					(looking-back
						"^\\([^ :]*-\\)?\\(To\\|B?Cc\\|From\\):.*? *\\([^,;]*\\)"
						(line-beginning-position)))
				(match-string-no-properties 3)))
		(candidates
			(mapcan
				(lambda (record)
					(mapcar
						(lambda (mail) (bbdb-dwim-mail record mail))
						(bbdb-record-get-field record 'mail)))
				(bbdb-search (bbdb-records) arg nil arg)))
		(sorted t)
		(no-cache t)))

(provide 'company-bbdb)
