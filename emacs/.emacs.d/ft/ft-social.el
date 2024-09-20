(use-package mastodon
  :straight t
  :config
  (setq mastodon-instance-url "https://social.linux.pizza")
  (setq mastodon-active-user "flocks@social.linux.pizza")

  (defun ft-mastodon-dwim ()
	(interactive)
	(call-interactively #'mastodon-tl--thread))

  (evil-define-key 'normal mastodon-mode-map (kbd "o") 'ft-mastodon-dwim)
  (evil-define-key 'normal mastodon-mode-map (kbd "u") 'mastodon-url-lookup)
  (evil-define-key 'normal mastodon-mode-map (kbd "C-c f") 'mastodon-tl--follow-user)
  (evil-define-key 'normal mastodon-mode-map (kbd "C-c r") 'mastodon-toot--reply)
  (evil-define-key 'normal mastodon-mode-map (kbd "C-c h") 'mastodon-tl--get-home-timeline)

  (defun ft-mastodon ()
	(interactive)
	(mastodon)
	(mastodon-tl--get-home-timeline))

  (global-set-key (kbd "C-c M") 'ft-mastodon))


(setq erc-join-buffer 'window-noselect)
(setq erc-track-switch-direction 'importance)
(setq erc-send-whitespace-lines t)


(defun ft-irc ()
  (interactive)
  (let ((password
		 (funcall
		  (plist-get (nth 0
						  (auth-source-search :host "irc.libera.chat"
											  :user "flocks"))
					 :secret))))
	(erc-tls :server "134.122.90.60" :port "5000" :nick "flocks" :password password)))

(use-package elpher
  :straight t
  :config
  (defun ft-elpher-page-url ()
	(let* ((address (elpher-page-address elpher-current-page))
		   (url (elpher-address-to-url address)))
	  url))

  (defun ft-share-elpher-web ()
	(interactive)
	(unless (string= major-mode "elpher-mode")
	  (user-error "Not inside elpher"))
	(let* ((url (ft-elpher-page-url))
		   (without-protocol (cadr (split-string url "gemini://")))
		   (web-url (format "https://portal.mozz.us/gemini/%s" without-protocol)))
	  (kill-new web-url)
	  (message "%s" web-url))))

(use-package bongo
  :straight t
  :config
  (evil-define-key 'normal bongo-mode-map (kbd "RET") 'bongo-play)
  ;; (define-key 'normal bongo-mode-map (kbd "o") 'bongo-insert-file)
  (evil-define-key 'normal bongo-mode-map (kbd "o") 'bongo-insert-file))

(defun ft-share-region (begin end)
  "Take region and upload the text to 0x0.st or equivalent"
  (interactive "r")
  (when (not (use-region-p))
	(user-error "No region selected"))
  (let* ((hostname "https://envs.sh")
		 (text (buffer-substring-no-properties begin end))
		 (file-text (make-temp-file "" nil ".txt" text))
		 (upload-command (format "curl -s -F 'file=@%s' %s" file-text hostname))
		 (url (shell-command-to-string upload-command)))
	(kill-new url)
	(delete-file file-text)
	(message "%s" url)))

(defun ft-share-file (file)
  "Take marked file and upload it to 0x0.st. Prompt for file if no marked file"
  (interactive (list
				(let ((files-marked (dired-get-marked-files)))
				  (if (= (length files-marked) 1)
					  (car files-marked)
					(read-file-name "File to upload: ")))))
  (let* ((hostname "https://envs.sh")
		 (upload-command (format "curl -s -F'file=@%s' %s" (expand-file-name file) hostname))
		 (url (shell-command-to-string upload-command)))
	(kill-new url)
	(message "%s" url)))

(provide 'ft-social)
