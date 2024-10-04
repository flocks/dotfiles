(require 'ft-utils)

(defun ft-jira--get-basic-auth-token ()
  (let* ((host "ledgerhq.atlassian.net")
		 (user "florent.teissier@ledger.fr")
		 (token (ft-extract-auth-token host user))
		 (command (format "echo -n florent.teissier@ledger.fr:%s | base64 -w0" token)))
    (format "Basic %s" (shell-command-to-string command))))

(defun ft-jira--extract-json ()
  "extract the JSON from url-retrieve result buffer"
  (goto-char (point-min))
  (re-search-forward "^$")
  (delete-region (point) (point-min))
  (json-parse-buffer :object-type 'alist))

(defun ft-jira--get-headers ()
  "Build headers object for url-retrieve"
  `(("Content-Type" . "application/json")
	("Authorization" . ,(ft-jira--get-basic-auth-token))))

(defun ft-jira--format-issue (issue)
  "From a jira ISSUE object format the issue for completion"
  (let ((fields (alist-get 'fields issue)))
	(format "%s: %s %s"
			(alist-get 'key issue)
			(alist-get 'summary fields)
			(alist-get 'name (alist-get 'status fields)))))

(defun ft-jira--select-issue (issues)
  "Diplay prompt with all ISSUES, copy id of selected issue and propose to open in browser"
  (let* ((choice (completing-read "Issue: " (mapcar 'ft-jira--format-issue issues)))
		 (id (car (split-string choice ":"))))
	(kill-new id)
	(when (yes-or-no-p "Open in browser?")
	  (eww-browse-with-external-browser (format "https://ledgerhq.atlassian.net/browse/%s" id)))))

(defvar ft-jira--issues-cache nil
  "used to cache jira issues")

(defun ft-jira-my-issues (prefix)
  "Fetch all my jira issues and run ft-jira--select-issue

Takes the data from the cache if it's populated.
When PREFIX non nil, will force a cache refresh."
  (interactive "P")
  (if (or prefix (not ft-jira--issues-cache))
	  (ft-jira--get-my-issues
	   (lambda (issues)
		 (setq ft-jira--issues-cache issues)
		 (ft-jira--select-issue issues)))
	(ft-jira--select-issue ft-jira--issues-cache)))


(defun ft-jira--get-my-issues (callback &optional limit)
  "Get all my issues and call CALLBACK with them"
  (let* ((jql (url-hexify-string "assignee = currentUser() order by updated DESC"))
		 (limit (or limit 20))
		 (url (format "https://ledgerhq.atlassian.net/rest/api/2/search?jql=%s&maxResults=%s" jql limit))
		 (url-request-extra-headers (ft-jira--get-headers)))
	(url-retrieve url
				  (lambda (status _callback)
					(let* ((json (ft-jira--extract-json))
						   (issues (append (alist-get 'issues json) nil)))
					  (funcall _callback issues)))
				  ;; CBARGS
				  (list callback))))



(defun ft-jira-issue-print-summary (issue)
  "Fetch summary of one jira ISSUE, ISSUE is the key and not the id in jira world"
  (interactive "sIssue: ")
  (let* ((url (format "https://ledgerhq.atlassian.net/rest/api/2/issue/%s" issue))
		 (url-request-extra-headers (ft-jira--get-headers)))
	(url-retrieve url
				  (lambda (status)
					(let* ((json (ft-jira--extract-json))
						   (fields (alist-get 'fields json))
						   (summary (alist-get 'summary fields))
						   (description (alist-get 'description fields)))
					  (message "%s\n %s" summary (if (eq description :null) "" description)))))))

(defun ft-jira-issue-summary-at-point ()
  "Grab the word at point and try to fetch the jira issue summary

provided thing-at-point is a valid jira KEY"
  (interactive)
  (let* ((issue (thing-at-point 'word t))
		 (browse-url (format "https://ledgerhq.atlassian.net/browse/%s" issue)))
	(ft-jira-issue-print-summary issue)
	(kill-new browse-url)))

(global-set-key (kbd "C-c j") 'ft-jira-issue-summary-at-point)
(global-set-key (kbd "C-c J") 'ft-jira-my-issues)



(provide 'ft-jira)
;;; ftjira.el ends here


