(defun ft-add-trailing-slash (dir)
  (if (string-suffix-p "/" dir)
	  dir
    (concat dir "/")))

(defun ft-get-minibuffer-content ()
  (interactive)
  (when-let ((new-content (ft-minibuffer--get-new-path (minibuffer-contents))))
	(delete-minibuffer-contents)
	(insert-and-inherit new-content)))

(defun ft-minibuffer--get-new-path (current-path)
  (let ((project
		 (locate-dominating-file current-path "package.json")))
	(if (string= (ft-add-trailing-slash current-path) project)
		(locate-dominating-file current-path ".git")
	  (or project (locate-dominating-file current-path ".git") "~"))))

(define-key vertico-map (kbd "M-u") 'ft-get-minibuffer-content)



