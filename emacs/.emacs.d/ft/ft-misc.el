
(defun ft-start-front-project--sentinel (process event)
  (when (eq 0 (process-exit-status process))
	(switch-to-buffer (find-file (process-name process)))))

(defun ft-start-front-project (name)
  (interactive "MName: ")
  (let ((buff-name "tsx-starter") 
		(dir (format "%s/%s" (getenv "HOME") name)))
	(message "Cloning starter project....")
	(make-process
	 :name dir
	 :buffer buff-name
	 :sentinel #'ft-start-front-project--sentinel
	 :command `("git" "clone" "git@github.com:flocks/tsx-starter.git" ,dir ))))

(defun ft-start-lib-project (name)
  (interactive "MName: ")
  (let ((buff-name "ts-lib-starter") 
		(dir (format "%s/%s" (getenv "HOME") name)))
	(message "Cloning starter project....")
	(make-process
	 :name dir
	 :buffer buff-name
	 :sentinel #'ft-start-front-project--sentinel
	 :command `("git" "clone" "git@github.com:flocks/ts-lib-starter.git" ,dir ))))

(defun ft-last-screenshots ()
  (interactive)
  (find-file-other-window "~/screenshots")
  (revert-buffer)
  (goto-char (point-max))
  (forward-line -1))

(global-set-key (kbd "M-^") 'project-dired)

(defun ft-copy-project-file-name ()
  ;; inside /home/flocks/project/src/app.js will copy src/app
  (interactive)
  (let* ((root (file-truename (locate-dominating-file default-directory ".git")))
		(filename (file-name-sans-extension (buffer-file-name)))
		(copy (file-relative-name filename root)))
	(kill-new copy)
	(message "Copied %s" copy)))

(global-set-key (kbd "C-x p y") 'ft-copy-project-file-name)

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'ft-misc)
