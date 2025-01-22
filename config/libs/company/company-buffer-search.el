; -*- lexical-binding:nil -*-

; Now this is unusable, as search takes too long and there is no way to run that on a different thread
; in elisp, so it will need to be done in C.

; company-mode backend simply searching open buffers for symbols after each couple of minutes.

(defconst company::bufferSearch::other_buffers t
"Determines whether `company-buffer-search' should search other buffers.
If nil, don't search in other buffers.
If t, search all other buffers, except the ignored ones.
If 'sameMode, search buffers with the same major mode.")

(defun company::bufferSearch::should_ignore_buffer (buffer)
"Return non-nil if buffer should be ignored.
This function and company::bufferSearch::other_buffers are used for filtering buffers,
though only this function can filter out current buffer.
This is all very easy to change."
	(= ?\s (aref (buffer-name buffer) 0)))

(defconst company::bufferSearch::refresh_time (* 60 3)
"Time in seconds after which buffers should be searched if they have changed.
This search is done on a separate thread.")

(defconst company::bufferSearch::char_regex "\\w\\|\\s_" ; Word or symbol constituents.
"Regex matching the characters `company-buffer-search' looks for.
Must be a string.")

(defconst company::bufferSearch::ignore_case 'keepPrefix
"Non-nil to ignore case when collecting completion candidates.
When it's `keepPrefix', the text before point will remain unchanged after
candidate is inserted, even some of its characters have different case.")

(defconst company::bufferSearch::min_length 4
"The minimum length for the completion candidate to be included.")

(defconst company::bufferSearch::ignore_invisible nil "Non-nil to skip invisible text.")


(defvar company::bufferSearch::buffer_cache (make-hash-table :test 'eq)
"Hash table mapping buffers to conses (buffer_chars_modified_tick . candidate_list).
Refresh when buffer-chars-modified-tick is different.")

(defvar company::bufferSearch::candidate_cache nil
"List with candidates.
Refreshed after every company::bufferSearch::refresh_time seconds.")

(defvar company::bufferSearch::timer nil
"Timer refreshing company::bufferSearch::buffer_cache and company::bufferSearch::candidate_cache
after every company::bufferSearch::refresh_time seconds.")

;(defun company::bufferSearch::define_searching_function
;(function_name_string time_limit match_min_length should_ignore_comments_and_strings
;should_ignore_invisible omit_buffer_regex_or_fn)
;"Use parameters to define more optimized function to search buffers.
;All parameters can be quoted forms (except function_name_string)."
;	(let ((collect_match_form
;			`(let ((match (match-string-no-properties 0)))
;				(and
;					(>= (length match) ,match_min_length)
;					,@(if should_ignore_invisible
;						'((not (invisible-p (match-beginning 0)))))
;					(puthash match t match_hash_table))))
;		  (search_forward_form
;			(if time_limit
;				`(let ((time_limit_check_i 0)) ; Counter to check time limit every 25 found matches.
;					(while
;						(and
;							(or
;								(/= 25 time_limit_check_i)
;								(progn
;									(setq time_limit_check_i 0)
;									(<= (float-time (time-subtract nil start_time)) ,time_limit)))
;							(not (input-pending-p))
;							(re-search-forward regex nil t))
;						(funcall collect_match_or_skip_comment_or_string nil)
;						(++ time_limit_check_i)))
;				'(while (and (not (input-pending-p)) (re-search-forward regex nil t))
;					(funcall collect_match_or_skip_comment_or_string nil)))))
;		(fset (intern function_name_string)
;			`(lambda (regex &optional modes_to_check)
;"modes_to_check should be:
;- a list of modes,
;- nil to search only current buffer
;- t to search all buffers (not omitted by omit_buffer_regex_or_fn)."
;				(let ((match_hash_table (make-hash-table :test 'equal))
;					  (collect_match_or_skip_comment_or_string
;						(lambda (search_bound) ; Dynamic binding: match_hash_table.
;							,(if should_ignore_comments_and_strings
;								(list #'if '(save-match-data (not (inside_code)))
;									'(re-search-forward "\\s>\\|\\s!\\|\\s\"" search_bound t)
;									collect_match_form)
;								collect_match_form)))
;					  ,@(if time_limit '((start_time (current-time)))))
;					; Search before caret.
;					(save-excursion
;						(cl-loop
;							; Limit for re-search-forward for current 10000 chars section.
;							for end_position = (point)
;							while (and (not (input-pending-p)) (> end_position (point-min)))
;							do
;							; Check if we're out of time after searching through max of 10000 chars.
;							(ignore-errors (forward-char -10000))
;							(save-excursion
;								; Before, we used backward search, but it matches non-greedily, and
;								; that forced us to use the "beginning/end of word" anchors in
;								; company-buffer-search->candidates->lambda->call_to_company::bufferSearch::search->first_arg.
;								; It's also about 2x slower.
;								(while (and (not (input-pending-p)) (re-search-forward regex end_position t))
;									(funcall collect_match_or_skip_comment_or_string end_position)))
;							,@(if time_limit ; Maybe add time limit check.
;								(list 'while (list #'<= '(float-time (time-subtract nil start_time)) time_limit)))))
;					; Search after caret.
;					(save-excursion ,search_forward_form)
;					; Search in other buffers.
;					(if modes_to_check
;						(cl-loop
;							with current_buffer = (current-buffer)
;							for buffer in (buffer-list)
;							until (input-pending-p)
;							unless (or
;									(eq buffer current_buffer)
;									,(if (stringp omit_buffer_regex_or_fn)
;										(list #'string-match-p omit_buffer_regex_or_fn '(buffer-name buffer))
;										(list #'funcall omit_buffer_regex_or_fn 'buffer)))
;								do (with-current-buffer buffer
;									(if (or (eq t modes_to_check) (apply #'derived-mode-p modes_to_check))
;										; Search forward from buffer start.
;										(save-excursion (goto-char (point-min)) ,search_forward_form)))
;							; Maybe add time limit check.
;							,@(if time_limit
;								(list 'while (list #'<= '(float-time (time-subtract nil start_time)) time_limit)))))
;					match_hash_table)))))
;
;(defun company::bufferSearch::define_searching_function_cache
;(function_name_string time_limit match_min_length should_ignore_comments_and_strings
;should_ignore_invisible omit_buffer_regex_or_fn)
;"Use parameters to define more optimized function to search buffers.
;All parameters can be quoted forms (except function_name_string).
;Use cache."
;	(let ((collect_match_form
;			`(let ((match (match-string-no-properties 0)))
;				(and
;					(>= (length match) ,match_min_length)
;					,@(if should_ignore_invisible
;						'((not (invisible-p (match-beginning 0)))))
;					(push match buffer_match_list))))
;		  (search_forward_and_save_form
;			`(progn
;				(setq match_list
;					(append
;						match_list
;						(if (or
;								(save-excursion
;									,(if time_limit
;										`(cl-loop
;											with time_limit_check_i = 0 ; Counter to check time limit every 25 found matches.
;											while (and
;													(not (input-pending-p))
;													(or
;														(/= 25 time_limit_check_i)
;														(progn
;															(setq time_limit_check_i 0)
;															(<= (float-time (time-subtract nil start_time)) ,time_limit))))
;											; If search reached end of buffer, then return t to save results in cache.
;											unless (re-search-forward regex nil t)
;												return t
;											do
;											(funcall collect_match_or_skip_comment_or_string nil)
;											(++ time_limit_check_i))
;										'(cl-loop
;											until (input-pending-p)
;											; If search reached end of buffer, then return t to save results in cache.
;											unless (re-search-forward regex nil t)
;												return t
;											do (funcall collect_match_or_skip_comment_or_string nil))))
;								(>= (* 2 (length buffer_match_list)) (length (cdr (gethash (current-buffer) cache_hash_table)))))
;						; Update cache and return buffer's new list of matches.
;						(progn
;							(puthash (current-buffer) (cons (buffer-chars-modified-tick) buffer_match_list) cache_hash_table)
;							buffer_match_list)
;						; Return buffer's old list of matches.
;						(cdr (gethash (current-buffer) cache_hash_table)))))
;				(setq buffer_match_list nil))))
;
;		(fset (intern function_name_string)
;			`(lambda (regex cache_hash_table &optional modes_to_check)
;"modes_to_check should be:
;- a list of modes,
;- nil to search only current buffer
;- t to search all buffers (not omitted by omit_buffer_regex_or_fn).
;Update cache in changed and fully (or mostly) searched buffers,
;in unchanged buffers get theirs cached candidates."
;				(let ((collect_match_or_skip_comment_or_string
;						(lambda (search_bound) ; Dynamic binding: match_list.
;							,(if should_ignore_comments_and_strings
;								`(if (save-match-data (not (inside_code)))
;									(re-search-forward "\\s>\\|\\s!\\|\\s\"" search_bound t)
;									,collect_match_form)
;								collect_match_form)))
;					  buffer_match_list
;					  match_list
;					  ,@(if time_limit '((start_time (current-time)))))
;					; Search before caret.
;					(save-excursion
;						(cl-loop
;							; Limit for re-search-forward for current 10000 chars section.
;							for end_position = (point)
;							while (and (not (input-pending-p)) (> end_position (point-min)))
;							do
;							; Check if we're out of time after searching through max of 10000 chars.
;							(ignore-errors (forward-char -10000))
;							(save-excursion
;								; Before, we used backward search, but it matches non-greedily, and
;								; that forced us to use the "beginning/end of word" anchors in
;								; company-buffer-search->candidates->lambda->call_to_company::bufferSearch::search->first_arg.
;								; It's also about 2x slower.
;								(while (and (not (input-pending-p)) (re-search-forward regex end_position t))
;									(funcall collect_match_or_skip_comment_or_string end_position)))
;							,@(if time_limit ; Maybe add time limit check.
;								`(while (<= (float-time (time-subtract nil start_time)) ,time_limit)))))
;					; Search after caret.
;					,search_forward_and_save_form
;					; Search in other buffers.
;					(if modes_to_check
;						(cl-loop
;							with current_buffer = (current-buffer)
;							for buffer in (buffer-list)
;							until (input-pending-p)
;							unless (or
;									(eq buffer current_buffer)
;									,(if (stringp omit_buffer_regex_or_fn)
;										`(string-match-p ,omit_buffer_regex_or_fn (buffer-name buffer))
;										`(funcall ,omit_buffer_regex_or_fn buffer)))
;								do
;								; If buffer hadn't been modified till last successful search, return it's cached candidates.
;								(if (eq (buffer-chars-modified-tick buffer) (car (gethash buffer cache_hash_table)))
;									(setq match_list (append match_list (cdr (gethash buffer cache_hash_table))))
;									(with-current-buffer buffer
;										(if (or (eq t modes_to_check) (apply #'derived-mode-p modes_to_check))
;											; Search forward from buffer start.
;											(save-excursion (goto-char (point-min)) ,search_forward_and_save_form))))
;							; Maybe add time limit check.
;							,@(if time_limit
;								`(while (<= (float-time (time-subtract nil start_time)) ,time_limit)))))
;					match_list)))))

(defun company::bufferSearch::search
(regex cache_hash_table modes_to_check match_min_length should_ignore_comments_and_strings
should_ignore_invisible should_omit_buffer_fn)
"Update cache in modified buffers and return list of all candidates according to parameters.
modes_to_check should be:
- a list of modes,
- nil to search only current buffer
- t to search all buffers (not omitted by omit_buffer_regex_or_fn).
This function can take many seconds to complete so it should always run on a separate thread."
	(let* ((search_current_buffer_and_update_cache
			(lambda ()
				(if (eq (buffer-chars-modified-tick) (car (gethash (current-buffer) cache_hash_table)))
					(cdr (gethash (current-buffer) cache_hash_table))
					(let (buffer_match_list)
						(save-excursion
							(goto-char (point-min))
							(while (re-search-forward regex nil t)
								(if (and should_ignore_comments_and_strings (save-match-data (not (inside_code))))
									(re-search-forward "\\s>\\|\\s!\\|\\s\"" nil t)
									(let ((match (match-string-no-properties 0)))
										(and
											(>= (length match) match_min_length)
											(not (and should_ignore_invisible (invisible-p (match-beginning 0))))
											(push match buffer_match_list))))))
						; Delete duplicates and count matches in buffer_match_list.
						; This operation will change buffer_match_list to be alist (unique_match_string match_count).
						(let ((buffer_match_list_i buffer_match_list) match match_count)
							(while buffer_match_list_i
								(setq match (car buffer_match_list_i)
									  match_count 1)
								; Delete and count duplicates for match.
								; It turns out that cl-delete doesn't actually always delete -_-
								; so this setcdr is necessary.
								(setcdr buffer_match_list_i
									(cl-delete-if
										(lambda (match_i)
											(if (string= match match_i) ; <- case-sensitive
												(++ match_count)))
										(cdr buffer_match_list_i)))
								; Save match and it's number of occurrences (1 = there was no dups).
								(setcar buffer_match_list_i (list match match_count))
								; Go to next match if it exists.
								(setq buffer_match_list_i (cdr buffer_match_list_i))))
						; Update cache and return buffer's new list of matches.
						(puthash (current-buffer) (cons (buffer-chars-modified-tick) buffer_match_list) cache_hash_table)
						buffer_match_list))))
		   (match_list
			(unless (funcall should_omit_buffer_fn (current-buffer))
				; Append current buffer name to every candidate from current buffer.
				; Create new list because it is still a list from buffer cache.
				(let ((current_buffer_name (buffer-name)))
					(mapcar
						(lambda (match) (append match (list current_buffer_name)))
						(funcall search_current_buffer_and_update_cache))))))

		(if modes_to_check
			(let ((current_buffer (current-buffer)))
				(dolist (buffer (buffer-list))
					; This function will be executed on a separate thread, so (I guess) it's possible that this buffer
					; will be dead when we will get to it, so that's why there is (buffer-live-p buffer) check.
					(if (and
							(buffer-live-p buffer)
							(not (eq buffer current_buffer))
							(not (funcall should_omit_buffer_fn buffer)))
						(with-current-buffer buffer
							(if (or (eq t modes_to_check) (apply #'derived-mode-p modes_to_check))
								(let ((buffer_match_list (funcall search_current_buffer_and_update_cache))
									  (buffer_name (buffer-name)))
									; Go through buffer_match_list and find duplicates in match_list.
									; If such duplicate exist, modify match's entry in match_list,
									; by increasing count of it's occurences and adding buffer name.
									; If it doesn't exist, then just add it to global match_list,
									; with buffer name appended.
									(dolist (buffer_match buffer_match_list)
										(let* ((buffer_match_string (pop buffer_match))
											   (global_match
												(cl-find-if
													(lambda (global_match)
														(string= buffer_match_string (car global_match)))
													match_list)))
											; If match was found in buffer's match list,
											; then add it's number of occurrences to the global counter in match_list.
											; Also append buffer's name to this match in match_list.
											(if global_match
												(progn
													(setq global_match (cdr global_match))
													(setcar global_match (+ (car global_match) (car buffer_match)))
													(nconc global_match (list buffer_name)))
												(setq match_list
													(cons
														(list buffer_match_string (car buffer_match) buffer_name)
														match_list))))))))))))
		match_list))

(fset (intern "company-buffer-search")
	`(lambda (command &optional arg)
	"`company-mode' backend simply searching open buffers for symbols after each couple of minutes."
	(interactive '(interactive))
		(cl-case command
			(interactive (company-begin-backend 'company-buffer-search))
			(prefix
				; Old version for discarding mid-word: (unless (looking-at company::bufferSearch::char_regex).
				; Emacs can't do greedy backward-search.
				(company-grab-line
					,(concat "\\(?:^\\| \\)[^ ]*?\\(\\(?:" company::bufferSearch::char_regex "\\)*\\)")
					1))
			(candidates
				(let ((filtered_candidate_list
						(let ((completion-ignore-case company::bufferSearch::ignore_case))
							(all-completions arg company::bufferSearch::candidate_cache))))
					(if (and (eq company::bufferSearch::ignore_case 'keepPrefix) (> (length arg) 0))
						(company::substitute_prefix arg filtered_candidate_list)
						filtered_candidate_list)))
			(kind 'text)
			; no-cache t, only because if company::bufferSearch::ignore_case = 'keepPrefix,
			; then company::substitute_prefix must be called after every prefix change.
			; This is kind of slow but probably still the best option.
			(no-cache t)
			(ignore-case company::bufferSearch::ignore_case))))

; Initialization

;(add-hook 'emacs-startup-hook
;	(fn_symbol "company::bufferSearch::initialize"
;		(lambda ()
;			(setq company::bufferSearch::timer
;				(run-with-timer nil company::bufferSearch::refresh_time
;					(lambda ()
;						(let ((non-essential t))
;							(setq company::bufferSearch::candidate_cache
;								(company::bufferSearch::search
;									(concat "\\(?:" company::bufferSearch::char_regex "\\)+")
;									company::bufferSearch::buffer_cache
;									(if (eq 'sameMode company::bufferSearch::other_buffers)
;										(list major-mode)
;										company::bufferSearch::other_buffers)
;									company::bufferSearch::min_length
;									nil
;									company::bufferSearch::ignore_invisible
;									#'company::bufferSearch::should_ignore_buffer)))))))))

(provide 'company-buffer-search)
