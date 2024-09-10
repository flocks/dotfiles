(require 'ft-utils)

(defun ft-jira--get-basic-auth-token ()
  (let* ((host "ledgerhq.atlassian.net")
		 (user "florent.teissier@ledger.fr")
		 (token (ft-extract-auth-token host user))
		 (command (format "echo -n florent.teissier@ledger.fr:%s | base64 -w0" token)))
    (format "Basic %s" (shell-command-to-string command))))


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
					(let* ((json (ft-jira--extract-json))
                           (fields (alist-get 'fields json))
                           (summary (alist-get 'summary fields))
                           (description (alist-get 'description fields)))
					  (message "%s\n %s" summary (if (eq description :null) "" description)))))))

(defun ft-jira-issue-summary-at-point ()
  (interactive)
  (let* ((issue (thing-at-point 'word t))
		 (browse-url (format "https://ledgerhq.atlassian.net/browse/%s" issue)))
	(ft-jira-issue-summary issue)
	(kill-new browse-url)))

(global-set-key (kbd "C-c j") 'ft-jira-issue-summary-at-point)



(provide 'ft-jira)
;;; ftjira.el ends here


