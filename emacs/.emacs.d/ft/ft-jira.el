(defun ft-jira--get-basic-auth-token ()
  (let ((token (funcall
                (plist-get (nth 0 (auth-source-search :host "ledgerhq.atlassian.net" :user "florent.teissier@ledger.fr")) :secret))))
    (format "Basic %s"
            (shell-command-to-string
             (format "echo -n florent.teissier@ledger.fr:%s | base64 -w0" token)))))


(defun ft-jira--extract-json ()
  (goto-char (point-min))
  (re-search-forward "^$")
  (delete-region (point) (point-min))
  (json-parse-buffer :object-type 'alist))

(defun ft-jira-issue-summary (issue)
  "Fetch summary of issue"
  (interactive "sIssue: ")
  (let* ((url (format "https://ledgerhq.atlassian.net/rest/api/2/issue/%s" issue))
		 (url-request-extra-headers `(("Content-Type" . "application/json")
									  ("Authorization" . ,(ft-jira--get-basic-auth-token)))))
	(url-retrieve url
				  (lambda (status)
					(let ((json (ft-jira--extract-json)))
					  (message "%s" (alist-get 'summary (alist-get 'fields json))))))))


(defun ft-jira-issue-summary-at-point (beg end)
  (interactive "r")
  (unless (use-region-p)
	(user-error "Nothing selected"))
  (let* ((issue (buffer-substring-no-properties beg end))
		 (browse-url (format "https://ledgerhq.atlassian.net/browse/%s" issue)))
	(ft-jira-issue-summary issue)
	(kill-new browse-url)))

(global-set-key (kbd "C-c j") 'ft-jira-issue-summary-at-point)



(provide 'ft-jira)
;;; ftjira.el ends here


