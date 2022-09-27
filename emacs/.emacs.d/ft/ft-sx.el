(defun ft-search-stack (term)
  (interactive "MSearch: ")
  (let* ((keywords (split-string term))
		 (tag (car keywords))
		 (query (string-join (cdr keywords) " "))
		 (url (format "https://api.stackexchange.com/2.3/search?tagged=%s&order=desc&sort=activity&intitle=%s&site=stackoverflow" tag query)))
	(message "%s" url)
	(url-retrieve
	 url
	 (lambda (status)
	   (goto-char (point-min))
	   (re-search-forward "^$")
	   (delete-region (point) (point-min))
	   (let* ((json (json-parse-buffer :object-type 'alist))
			  (items (append (alist-get 'items json) nil)))
		 (erase-buffer)
		 (dolist (item (append items nil))
		   (insert-button (alist-get 'title item)
						  'action (lambda (x) (browse-url (button-get x 'url)))
						  'url (alist-get 'link item))
		   (newline))
		 (goto-char (point-min))
		 (special-mode))
	   (switch-to-buffer-other-window (current-buffer))))))


(global-set-key (kbd "C-c C-h") 'ft-search-stack)



(provide 'ft-sx)
