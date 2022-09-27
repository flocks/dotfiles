
(defun ft-yarn--list-yarn-scripts (file)
  "List all script defined in package.json FILE."
  (mapcar #'car (alist-get 'scripts (json-read-file file))))


(defun ft-yarn--is-sub-package ()
  (let ((root-folder (locate-dominating-file (buffer-file-name) ".git"))
	(package-folder (locate-dominating-file (buffer-file-name) "package.json")))
    (and package-folder (not (eq root-folder package-folder)))))


(defun ft-yarn--get-all-sub-packages (dir)
  (let ((default-directory dir))
    (cons "all" (split-string (shell-command-to-string "ls")))))

(defun ft-yarn--search-buffer-by-name (name)
  (seq-filter (lambda (x) (string-equal name (buffer-name x))) (buffer-list)))

(defun ft-yarn--run-command (prefix &optional command)
  "Wrapper function that calls the command on the correct default-directory
- in the closest folder with package.json without prefix
- prompt for a packages to select when prefix is non nil."
  (let* ((root-directory (projectile-compilation-dir))
	 (compilation-scroll-output t)
	 (default-directory
	   (if prefix (ft-yarn--prompt-packages)
	     (if (buffer-file-name)
		 (locate-dominating-file (buffer-file-name) "package.json")
	       root-directory)))
	 (cmd (or command
		  (completing-read "Script: "
				   (ft-yarn--list-yarn-scripts (concat default-directory "/package.json")))))
	 (compilation-buffer-name (format "*compilation* -- %s" cmd))
	 (compilation-buffer-name-function (lambda (x) compilation-buffer-name)))
    (if (ft-yarn--search-buffer-by-name compilation-buffer-name)
	(switch-to-buffer compilation-buffer-name)
      (compile (concat "yarn run " cmd)))))


(ft-yarn--search-buffer-by-name "*compilation* -- dev")

(defun ft-yarn--prompt-packages ()
  (interactive)
  (let ((choice (completing-read
		 "Prompt:"
		 (ft-yarn--get-all-sub-packages
		  (format "%spackages" (projectile-compilation-dir))))))
    (if (string-equal choice "all")
	(projectile-compilation-dir)
      (format "%spackages/%s/" (projectile-compilation-dir) choice))))


(defun ft-yarn-run-lint (prefix)
  (interactive "P")
  (ft-yarn--run-command prefix "eslint --cache . --format=unix"))

(defun ft-yarn-run-flow (prefix)
  (interactive "P")
  (ft-yarn--run-command prefix "flow"))

(defun ft-yarn-run-prettier (prefix)
  (interactive "P")
  (ft-yarn--run-command prefix "prettier"))

(defun ft-yarn-run-build (prefix)
  (interactive "P")
  (ft-yarn--run-command prefix "build"))

(defun ft-yarn-run-script (prefix)
  (interactive "P")
  (ft-yarn--run-command prefix))



(global-set-key (kbd "C-c p y p") 'ft-yarn-run-prettier)
(global-set-key (kbd "C-c p y l") 'ft-yarn-run-lint)
(global-set-key (kbd "C-c p y f") 'ft-yarn-run-flow)
;; (global-set-key (kbd "C-c p y f") 'flow-minor-status)
(global-set-key (kbd "C-c p y s") 'ft-yarn-run-script)
(global-set-key (kbd "C-c p y b") 'ft-yarn-run-build)


(provide 'ft-yarn)
