(setq erc-join-buffer 'window-noselect)
(setq erc-track-switch-direction 'importance)
(setq erc-send-whitespace-lines t)


(defun ft-irc ()
  (interactive)
  (erc-tls :server "irc.libera.chat" :nick "flocks")
  (erc-tls :server "irc.dgtlgrove.com" :nick "flocks"))

(setq erc-autojoin-channels-alist
      '(("Libera.chat" "#emacs" "#linux" "#brillance" "#emacs-social")
        ("irc.dgtlgrove.com" "#public")))

(defun ft/erc-after-connect (server nick)
  "Run command after erc conncetion"
  (let ((password (funcall (plist-get (nth 0 (auth-source-search :host "irc.libera.chat" :user "flocks")) :secret))))
	(erc-message "PRIVMSG" (format "NickServ IDENTIFY %s" password))))

(add-hook 'erc-after-connect #'ft/erc-after-connect)
;; (add-hook 'erc-after-connect 'erc-autojoin-channels)

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
  "Take region and upload the text to my unxr server"
  (interactive "r")
  (when (not (use-region-p))
	(user-error "No region selected"))
  (let* ((text (buffer-substring-no-properties begin end))
		 (file-text (make-temp-file "" nil ".txt" text))
		 (upload-command (format "scp %s flocks@unxr:/srv/data" file-text))
		 (url (format "https://files.unxr.net/%s" (file-name-nondirectory file-text))))
	(set-file-modes file-text #o644)
	(shell-command-to-string upload-command)
	(kill-new url)
	(delete-file file-text)
	(message "%s" url)))

(defun ft-share-file (file)
  "Take marked file and upload it my server. Prompt for file if no marked file"
  (interactive (list
				(let ((files-marked (dired-get-marked-files)))
				  (if (= (length files-marked) 1)
					  (car files-marked)
					(read-file-name "File to upload: ")))))
  (let* ((upload-command (format "scp %s flocks@unxr:/srv/data" (expand-file-name file)))
		 (url (format "https://files.unxr.net/%s" (file-name-nondirectory (expand-file-name file)))))
	(shell-command-to-string upload-command)
	(kill-new url)
	(message "%s" url)))


(provide 'ft-social)

