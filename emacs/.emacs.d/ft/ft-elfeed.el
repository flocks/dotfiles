(use-package elfeed
  :bind ("C-c e" . elfeed)
  :straight t
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format '("%F %R" 16 :left))

  (defun ft-elfeed--get-link ()
	(let ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region))))
	  (elfeed-entry-link entry)))


  (defun ft-elfeed-copy-link ()
	(interactive)
	(let ((link (ft-elfeed--get-link)))
	  (message "Copied %s" link)
	  (kill-new link)))

  (defun ft-elfeed-browse-entry-eww ()
	(interactive)
	(eww (ft-elfeed--get-link)))

  (defun ft-elfeed-mpv ()
	(interactive)
	(let ((command (format "mpv '%s'" (ft-elfeed--get-link))))
	  (message "launching mpv....")
	  (async-shell-command command "*mpv-elfeed*")))

  (let ((map elfeed-show-mode-map))
	(evil-define-key 'normal map (kbd "&") 'ft-elfeed-browse-entry-eww))

  (let ((map elfeed-search-mode-map))
	(evil-define-key 'normal map (kbd "&") 'ft-elfeed-browse-entry-eww))

  (let ((map elfeed-search-mode-map))
	(evil-define-key 'normal map (kbd "C-c C-v") 'ft-elfeed-mpv))

  (let ((map elfeed-show-mode-map))
	(evil-define-key 'normal map (kbd "C-c C-v") 'ft-elfeed-mpv))

  (let ((map elfeed-search-mode-map))
	(evil-define-key 'normal map (kbd "C-c C-y") 'ft-elfeed-copy-link))

  (let ((map elfeed-show-mode-map))
	(evil-define-key 'normal map (kbd "C-c C-y") 'ft-elfeed-copy-link))

  (load-file "~/dotfiles/emacs/.emacs.d/feeds.el.gpg"))

(use-package elfeed-notifier
  :straight (elfeed-notifier :type git :host github :repo "flocks/elfeed-notifier")
  :config
  (elfeed-notifier-mode))

(provide 'ft-elfeed)
