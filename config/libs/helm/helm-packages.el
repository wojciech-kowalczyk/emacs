; -*- lexical-binding:nil -*-

(require 'package)

(defun helm-packages--action (msg fn)
	(let ((mkd (helm-marked-candidates)))
		(helm-with-display-candidates
			(mapcar #'symbol-name mkd)
			(when (y-or-n-p (concat msg " " (number-to-string (length mkd)) " packages?"))
				(mapc fn mkd)))))

(defun helm-packages-upgrade (_candidate)
	(helm-packages--action "Upgrade" #'package-upgrade))

(defun helm-packages-reinstall (_candidate)
	(helm-packages--action "Reinstall" #'package-reinstall))

(defun helm-packages-uninstall (_candidate)
"Unlike `helm-packages-delete' this will refuse to delete packages when they are
needed by others packages as dependencies."
	(helm-packages--action "Uninstall"
		(lambda (package) (package-delete (package-get-descriptor package)))))

(defun helm-packages-delete (_candidate)
"Unlike `helm-packages-uninstall' this delete packages even when they are needed
as dependencies."
	(helm-packages--action "Delete"
		(lambda (package) (package-delete (package-get-descriptor package) 'force))))

(defun helm-packages-recompile (_candidate)
	(helm-packages--action "Recompile" #'package-recompile))

(defun helm-packages-install (_candidate)
	(helm-packages--action "Install" #'package-install))

(defun helm-packages-describe (candidate) (describe-package (intern-soft candidate)))

(defun helm-packages-visit-homepage (candidate)
"Helm action for visiting package CANDIDATE home page."
	(let* (
		(id (package-get-descriptor (intern-soft candidate)))
		(name (package-desc-name id))
		(extras (package-desc-extras id))
		(url (and (listp extras) (cdr-safe (assq :url extras))))
	)
		(if (stringp url)
			(browse-url url)
			(message "Package %s has no homepage."
				(propertize (symbol-name name) 'face 'font-lock-keyword-face)))))

(defun helm-packages-isolate (_candidate)
"Start a new Emacs with only marked packages loaded."
	(let (
		(pkg-names (helm-marked-candidates))
		(isolate
			(if (fboundp 'package-isolate)
				#'package-isolate
				(lambda (packages)
					(make-process
						:name (concat "package-isolate-" (mapconcat #'identity packages "_"))
						:command
							(list
								(expand-file-name invocation-name invocation-directory)
								"-Q"
								"--debug-init"
								(format "--eval=%S"
									`(progn
										(require 'package)
										(setq package-load-list
											',(nconc
												(mapcar
													(lambda (p) (list (intern p) t))
													packages)
												(mapcar (lambda (p) (list p t))
													(mapcan
														(lambda (p)
															(package--dependencies (intern p)))
														packages))))
										(package-initialize))))))))
	)
		(helm-with-display-candidates
			pkg-names
			(when (y-or-n-p "Start a new Emacs with only package(s)?")
				(funcall isolate pkg-names)))))

(defun helm-packages-candidate-transformer (candidates)
	(let ((max_length (helm-get-max-length candidates)))
		(map_modify_list
			(lambda (c)
				(cons
					(let* (
						(sym (intern-soft c))
						(archive (assq sym package-archive-contents))
						(id (package-get-descriptor sym))
						(provider (when archive (package-desc-archive (nth 1 archive))))
						status version description
					)
						(when id
							(setq status (package-desc-status id))
							(setq description (package-desc-summary id))
							(when description
								(setq description
									(propertize description 'face 'font-lock-warning-face)))
							(setq version
								(mapconcat #'number-to-string (package-desc-version id) ".")))
						(concat
							; Package name.
							(propertize c
								'face
								(if (equal status "dependency")
									'font-lock-type-face
									'font-lock-keyword-face))
							; Separator.
							(get_space_string (1+ (- max_length (length c))))
							; Package status.
							(when status
								(if (string= status "dependency")
									(propertize status 'face 'bold-italic)
									status))
							; Separator.
							(get_space_string (- 11 (length status)))
							; Package provider.
							provider
							; Separator.
							(get_space_string (- 11 (length provider)))
							; Package version.
							version
							; Separator.
							(get_space_string (- 21 (length version)))
							; Package description.
							description))
					c))
			candidates)))

(defun helm-packages-simple-candidate-transformer (candidates)
	(map_modify_list
		(lambda (c) (propertize c 'face 'font-lock-keyword-face))
		candidates))

(defconst helm-packages-sources
	(map_modify_list
		(lambda (ct_name_action_candidates_vector)
			(helm-source-sync-make nil
				:name (aref ct_name_action_candidates_vector 1)
				:candidates
					`(lambda ()
						(sort
							(map_modify_list
								#'symbol-name
								(,(aref ct_name_action_candidates_vector 3)))
							#'helm-sort-alpha))
				:action (aref ct_name_action_candidates_vector 2)
				:update #'package-refresh-contents
				:candidate-transformer (aref ct_name_action_candidates_vector 0)))
		(list
			(vector
				#'helm-packages-simple-candidate-transformer
				"Availables for upgrade"
				'(("Upgrade package(s)" . helm-packages-upgrade))
				#'package--updateable-packages)
			(vector
				#'helm-packages-simple-candidate-transformer
				"Packages to delete"
				'(("Delete package(s)" . helm-packages-delete))
				#'package--removable-packages)
			(vector
				#'helm-packages-candidate-transformer
				"Installed packages"
				'(
					("Describe package" . helm-packages-describe)
					("Visit homepage" . helm-packages-visit-homepage)
					("Reinstall package(s)" . helm-packages-reinstall)
					("Recompile package(s)" . helm-packages-recompile)
					("Uninstall package(s)" . helm-packages-uninstall)
					("Isolate package(s)" . helm-packages-isolate)
				)
				(lambda () (mapcar #'car package-alist)))
			(vector
				#'helm-packages-candidate-transformer
				"Available external packages"
				'(
					("Describe package" . helm-packages-describe)
					("Visit homepage" . helm-packages-visit-homepage)
					("Install packages(s)" . helm-packages-install)
				)
				(lambda ()
					(cl-loop
						for p in package-archive-contents
						for sym = (car p)
						for id = (package-get-descriptor sym)
						unless
							(and
								id
								(or
									(member
										(package-desc-status id)
										'("installed" "dependency" "source"))
									(assoc sym package--builtins)))
							collect sym)))
			(vector
				#'helm-packages-candidate-transformer
				"Available built-in packages"
				'(
					("Describe package" . helm-packages-describe)
					("Visit homepage" . helm-packages-visit-homepage)
					("Install packages(s)" . helm-packages-install)
				)
				(lambda ()
					(cl-loop
						for p in package--builtins
						; Show only builtins that are available as
						; well on (m)elpa. Other builtins don't
						; have a package-descriptor, the format is
						; (sym . [version reqs summary]).
						when (package-desc-p (package-get-descriptor (car p)))
							collect (car p)))))))

(defun helm-packages (&optional arg)
"Helm interface to manage packages.

With a prefix arg ARG refresh package list. Or helm-force-update in helm.

When installing or upgrading ensure to refresh the package list
to avoid errors with outdated packages no more availables."
	(interactive "P")
	(package-initialize)
	(when arg (package-refresh-contents))
	(helm :sources helm-packages-sources))

(provide 'helm-packages)
