(require 'request)

;; TODO
;; make it more generic to handle commit links as well with url /repos/<ORG>/REPOT/commits/<commit-sha>

(defun ft-view-diff--get-github-token ()
  (funcall
   (plist-get
	(nth 0
		 (auth-source-search :host "api.github.com" :user "flocks^github-review"))
	:secret)))


(defun ft-view-diff-from-github-url (url)
  "Take URL and display the diff it points to into a Diff buffer"
  (let* ((api-url (ft-view-diff--build-full-api-url url))
	 (buffer-name (format "*Diff-github* %s" url)))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (ft-view-diff--get-diff-content api-url))
      (goto-char (point-min))
      (diff-mode))
    (switch-to-buffer buffer-name)
	(delete-other-windows)))

(defun ft-view-diff--find-diff-link ()
  "Find .diff url in the current buffer"
  (save-excursion
    (goto-char (point-min))
    (search-forward ".diff")
    (thing-at-point 'url)))

(defun ft-view-diff--build-full-api-url (url)
  "From URL construct github API url"
  (let ((token (ft-view-diff--get-github-token)))
    (format "https://%s:x-oauth-basic@%s" token (ft-view-diff--extract-org-repo-pr url))))

(defun ft-view-diff--extract-org-repo-pr (url)
  "Turn github url into API url"
  (let* ((suffix (cadr (split-string url "https://github.com")))
	 (items (cdr (split-string suffix "/")))
	 (org (nth 0 items))
	 (repo (nth 1 items))
	 (pr (car (split-string (nth 3 items) ".diff"))))
    (format "api.github.com/repos/%s/%s/pulls/%s" org repo pr)))

(defun ft-view-diff--get-diff-content (url)
  "Fetch diff content of URL."
  (let (result)
    (request url
      :sync t
      :headers '(("Accept" . "application/vnd.github.v3.diff"))
      :success (cl-function
		(lambda (&key data &allow-other-keys)
		  (setq result data))))
    result))

(defun ft-view-diff ()
  "Take .diff url in the buffer, fetch the diff and display it in a
Diff buffer"
  (interactive)
  (let ((url (ft-view-diff--find-diff-link)))
    (when url
      (ft-view-diff-from-github-url url))))

(evil-define-key 'normal notmuch-show-mode-map (kbd "C-c C-g") 'ft-view-diff)
