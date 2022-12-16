;;; Code:
(require 'request)
(require 'thingatpt)

(defgroup jira () "Variables related to Jira package."
  :group 'convenience
  :link '(url-link "https://github.com/focks/jira.el"))


(defcustom jira-folder nil
  "Directory to store the jira files into."
  :group 'jira
  :type 'directory)

(defcustom jira-organization-name nil
  "Directory to store the jira files into."
  :group 'jira
  :type 'string)

(defcustom jira-watched-boards nil
  "Directory to store the jira files into."
  :group 'jira
  :type '(string))

;; ;; VSD-1286
;; ;; VSD1286
;; ;; VFE-2399 test
;; ;; to be able to use (thing-at-point 'jira-ticket)
;; (defvar thing-at-point-jira-ticket-chars "[a-zA-Z]+-[0-9]+")
;; (define-thing-chars jira-ticket "[a-z]+-[0-9]+")


(defun ft-jira-open-issue (&optional arg)
  "Open buffer with ISSUE information"
  (interactive "P")
  (let* ((issue (or (and arg (read-string "Issue: ")) (thing-at-point 'word)))
		 (url (format "https://ledgerhq.atlassian.net/rest/api/2/issue/%s" issue))
		 (url-request-extra-headers `(("Content-Type" . "application/json")
									  ("Authorization" . ,(ft-jira--get-basic-auth-token)))))
	(message "%s" issue)
	(url-retrieve url
				  (lambda (status)
					(goto-char (point-min))
					(re-search-forward "^$")
					(delete-region (point) (point-min))
					(let ((buffer-content (ft-jira--parse-issue
										   (json-parse-buffer :object-type 'alist))))
					  (erase-buffer)
					  (goto-char (point-min))
					  (insert buffer-content))
					(jiraissue-mode)
					;; (setq-local ft-jira-current-issue issue)
					(switch-to-buffer-other-window (current-buffer))))))

(defun ft-jira--parse-issue (json)
  (format "%s: %s by %s \n\n\nDescription:\n\n %s"
		  (alist-get 'key json)
		  (alist-get 'summary (alist-get 'fields json))
		  (alist-get 'emailAddress (alist-get 'creator (alist-get 'fields json)))
		  (alist-get 'description (alist-get 'fields json))))


(defvar jiraissue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") (lambda ()
									   (interactive)
									   (eww-browse-with-external-browser
										(format "https://ledgerhq.atlassian.net/browse/%s" ft-jira-current-issue))
									   ()))
    map))

(define-derived-mode jiraissue-mode
  text-mode "Jiraissue"
  "Major mode for viewing jira issue."
  (setq-local ft-jira-current-issue nil))


(defun jira--get-base-url ()
  "Compute base API url."
  (concat "https://" jira-organization-name ".atlassian.net"))


(defun ft-jira--get-basic-auth-token ()
  (let ((token (funcall
				(plist-get (nth 0
								(auth-source-search :host "ledgerhq.atlassian.net"
													:user "florent.teissier@ledger.fr"))
						   :secret))))
	(format "Basic %s"
			(shell-command-to-string
			 (format "echo -n florent.teissier@ledger.fr:%s | base64 -w0" token)))))

(setq jira-organization-name "ledgerhq")
(setq jira-folder "~/.emacs.d/ft/")

(defun ft-jira--extract-json ()
  (goto-char (point-min))
  (re-search-forward "^$")
  (delete-region (point) (point-min))
  (json-parse-buffer :object-type 'alist))

(provide 'ft-jira)
;;; ftjira.el ends here


