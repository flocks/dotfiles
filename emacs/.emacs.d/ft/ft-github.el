(use-package json
  :straight t)

(use-package dashub
  :straight (dashub :type git :host github :repo "flocks/dashub")
  :config
  (require 'dashub-evil)
  (setq dashub--favorite-repos '("LedgerHQ/ledger-vault-api"
								 "LedgerHQ/ledger-vault-front"
								 "LedgerHQ/minivault"
								 "LedgerHQ/vault-remote"
								 "LedgerHQ/vault-ts"))

  (custom-set-variables '(dashub--notify-delay 60))
  (global-set-key (kbd "C-c G") 'dashub))

(use-package code-review
  :after (dashub)
  :straight t
  :config
  (defun dashub-review-pr (notif)
	(let* ((url (plist-get notif :url))
		   (type (plist-get notif :type)))
	  (cond ((string= "PullRequest" type)
			 (code-review-start (dashub--get-pull-request-url url)))
			t (message (format "%s not supported" type))))
	)
  (setq code-review-auth-login-marker 'forge)
  (setq dashub--action #'dashub-review-pr)

  (remove-hook 'magit-status-sections-hook 'forge-insert-pullreqs)
  (remove-hook 'magit-status-sections-hook 'forge-insert-issues))

(provide 'ft-github)
