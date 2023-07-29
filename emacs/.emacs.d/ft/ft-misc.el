
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

;; make this work for other project
;; detect package builder (pnpm,yarn/npm)
;; find `ci' script
;; append --loglevel silent to prettier task


(defun ft/front-ci ()
  (interactive)
  (let ((default-directory "~/ledger/ledger-vault-front")
		(compilation-read-command nil)
		(compile-command "yarn lint && yarn prettier:check --loglevel silent && yarn flow && yarn typecheck" ))
	(call-interactively #'project-compile)))

(use-package minibar
  :straight '(:type git :repo  "https://codeberg.org/akib/emacs-minibar.git")
  :config
  (setq minibar-group-left '((lambda ()
							   (if (boundp 'notmuch-indicator-string)
								   notmuch-indicator-string
								 ""))))
  (setq minibar-group-middle '((lambda ()
								 (if (boundp 'erc-modified-channels-object)
									 erc-modified-channels-object
								   ""))))
  (minibar-mode))


(provide 'ft-misc)


