(use-package mastodon
  :straight t
  :config
  (setq mastodon-instance-url "https://social.linux.pizza")
  (setq mastodon-active-user "flocks@social.linux.pizza")

  (defun ft-mastodon-dwim ()
	(interactive)
	;; TODO write other stuff if needed
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


(defun ft-irc ()
  (interactive)
  (let ((password
		 (funcall
		  (plist-get (nth 0
						  (auth-source-search :host "irc.libera.chat"
											  :user "flocks"))
					 :secret))))
	(erc-tls :server "134.122.90.60" :port "5000" :nick "flocks" :password password)))


(use-package sx
  :straight t
  :config
  (defun ft-search-stack ()
	(interactive)
	(call-interactively 'sx-search))

   (global-set-key (kbd "C-c C-h") 'ft-search-stack)
   (evil-define-key 'normal sx-question-list-mode-map (kbd "RET") 'sx-display))

(use-package elpher
  :straight t)


(provide 'ft-social)
