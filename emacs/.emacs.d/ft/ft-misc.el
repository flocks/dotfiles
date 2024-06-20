
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

(defun ft-copy-project-file-name (file-name)
  ;; inside /home/flocks/project/src/app.js will copy app
  (interactive "s")
  (let* ((root (file-truename (locate-dominating-file default-directory ".git")))
		 (filename (file-name-sans-extension file-name))
		(copy (file-relative-name filename (format "%s/src" root))))
	(kill-new copy)
	(message "Copied %s" copy)))


(global-set-key (kbd "C-x p y") (lambda () (interactive)
								  (ft-copy-project-file-name buffer-file-name)))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; TODO conflict with vim s/foo/bar command
(define-abbrev minibuffer-mode-abbrev-table "s" "rg --vimgrep")
;; (add-hook 'minibuffer-mode-hook 'abbrev-mode)

(defun ft-lorem ()
  (interactive)
  (insert "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"))

(provide 'ft-misc)

