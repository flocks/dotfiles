
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

(provide 'ft-misc)


