(defvar vault-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `vault2-mode'.")

(setq vault--api-url "https://remote.minivault.ledger-sbx.com/api")

(define-derived-mode vault-mode tabulated-list-mode "Vault-Remote"
  "Special mode for vault remote instances"
  ;; (kill-all-local-variables)
  (setq mode-name "Vault")
  (setq major-mode 'vault-mode)
  (use-local-map vault-mode-map)
  (setq tabulated-list-format [("Name" 30 t)
							   ("Owner" 30 t)
							   ("CID" 10 t)
							   ("Status" 10 t '(:right-align t))])

  (setq tabulated-list-entries (vault--get-tabulated-entries))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (hl-line-mode 1)
  (run-mode-hooks 'vault-mode-hook))


(defun vault--create-buffer (name)
  "Utility function to pop to buffer or create it.

NAME is the buffer name."
  (unless (get-buffer name)
    (get-buffer-create name))
  (pop-to-buffer-same-window name))

(defun vault ()
  (interactive)
  (vault--create-buffer "*vault*")
  (vault-mode))


(defun ft-parse-retrieve-buffer (&optional no-parse)
  (goto-char (point-min))
  (re-search-forward "^$")
  (delete-region (point) (point-min))
  (if no-parse
	  (buffer-string)
	(json-parse-buffer :object-type 'alist)))


(defun vault--get-instances ()
  (with-current-buffer
	  (url-retrieve-synchronously (format "%s/instances" vault--api-url) t)
	(ft-parse-retrieve-buffer)))

(defun vault--get-presets ()
  (with-current-buffer
	  (url-retrieve-synchronously (format "%s/presets" vault--api-url) t)
	(ft-parse-retrieve-buffer)))

(defun vault--list-instances ()
  (let ((json (append (vault--get-instances) nil)))
	(mapcar (lambda (x) (alist-get 'name x)) json)))

(defun vault--get-tabulated-entries ()
  (let ((json (append (vault--get-instances) nil))
		(result nil))
	(dolist (instance json result)
	  (push
	   `(,(alist-get 'name instance)
		 ,(vector (vault--propertize-name (alist-get 'name instance))
				  (vault--propertize-owner (alist-get 'owner instance))
				  (vault--propertize-cid (number-to-string (alist-get 'cid instance)))
				  (vault--propertize-status (alist-get 'status instance)))) result ))
	result))

(defun vault--propertize-status (status)
  (cond ((string= status "HEALTHY") (propertize status 'face '(:foreground "green")))
		((string= status "BUSY") (propertize status 'face '(:foreground "orange")))
		(t (propertize status 'face '(:foreground "red")))))


(defun vault--propertize-cid (owner)
  (propertize owner 'face '(:foreground "grey")))

(defun vault--propertize-owner (owner)
  (propertize owner 'face 'italic))

(defun vault--propertize-name (name)
  (propertize name 'face '(:foreground "blue"  :underline t )))

(defun vault-copy-url (instance &optional prefix)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))
		 current-prefix-arg))
  (let ((url (if prefix (format "https://remote.minivault.ledger-sbx.com/instances/%s" instance)
				(format "https://%s.minivault.ledger-sbx.com" instance))))
	(kill-new url)
	(message "Copy %s" url)))

(defun vault-fetch (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (let* ((salt (read-string "Salt: "))
		 (endpoint (read-string "Endpoint: "))
		 (url (format "https://%s.minivault.ledger-sbx.com" instance))
		 (device (read-number "Device" 4))
		 (command (format "ledger-vault fetch %s --salt %s --device %s --minivaultURL %s"
						  endpoint salt device url)))
	(vault--cli-fetch command)))

(defun vault-browse (instance &optional prefix)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))
		 current-prefix-arg))
  (let* ((url (if prefix (format "https://remote.minivault.ledger-sbx.com/instances/%s" instance)
				(format "https://%s.minivault.ledger-sbx.com" instance))))
	(eww-browse-with-external-browser url)))

(defun vault-bake (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (let* ((preset (completing-read "Preset: " (vault--list-bake-preset)))
		(salt (read-string "Salt: "))
		(minivault-url (format "https://%s.minivault.ledger-sbx.com" instance))
		(command (format "ledger-vault bake --preset %s --minivaultURL %s --salt %s --noisechannel"
			 preset minivault-url salt)))
	(message "%s\n" command)
	(async-shell-command command (format "*vault-bake-%s*" instance))))

(defun vault-token (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (let* ((device (read-number "Device: "))
		 (salt (read-string "Salt: "))
		 (minivault-url (format "https://%s.minivault.ledger-sbx.com" instance)))
	(kill-new (shell-command-to-string (format "ledger-vault getAuthToken --device %s  --salt %s --minivaultURL %s"
									  device salt minivault-url)))))

(defun vault--list-bake-preset ()
  (let ((default-directory "~/ledger/vault-js/packages/cli/src/presets"))
    (mapcar
     (lambda (preset) (string-trim-right preset ".json"))
     (split-string (shell-command-to-string "ls")))))

(defun vault-deploy ()
  (interactive)
  (let* ((presets (mapcar (lambda (x) (alist-get 'id x)) (vault--get-presets)))
		 (preset (completing-read "Preset: " presets)))
	(vault--deploy-preset preset)))

(defun vault--deploy-preset (preset)
  (let* ((owner "Florent")
		 (name (format "%s-%s" "flo" preset)))
	(async-shell-command (format "ledger-vault deploy --name %s --owner %s --preset %s"
								 name owner preset)
						 "*vault-deploy*")
	(run-at-time 2 nil 'vault)))


(defun vault-proxy (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (let ((buff-name "*vault-proxy*")
		(url (format "https://%s.minivault.ledger-sbx.com" instance) ))
	(when-let ((process (get-buffer-process (get-buffer buff-name))))
	  (kill-process process))
	(make-process
	 :name buff-name
	 :buffer buff-name
	 :command `("ledger-vault" "proxy" ,url))
	(switch-to-buffer-other-window buff-name)
	(goto-char (point-max))))

(defun vault-destroy (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (let* ((url-request-method "DELETE")
		(confirm (yes-or-no-p (format "Delete %s instance?" instance))))
	(when confirm
	  (url-retrieve (format "https://remote.minivault.ledger-sbx.com/api/instances/%s" instance) (lambda (_)
																								   (run-at-time 3 nil 'vault))))))

(defun vault-view-config (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (url-retrieve (format "%s/instances/%s" vault--api-url instance)
				(lambda (status)
				  (goto-char (point-min))
				  (re-search-forward "^$")
				  (delete-region (point) (point-min))
				  (delete-line)
				  (let* ((json (json-parse-buffer :object-type 'alist))
						 (name (alist-get 'name json))
						 (buff-name (format "*vault* - %s" name)))
					(json-pretty-print-buffer)
					(goto-char (point-min))
					(when (get-buffer buff-name)
					  (kill-buffer buff-name))
					(rename-buffer buff-name))
				  (json-mode)
				  (switch-to-buffer-other-window (current-buffer)))))


(let ((map vault-mode-map))
  (evil-define-key 'motion map
	(kbd "RET") #'vault-view-config
	(kbd "+") #'vault-deploy
	(kbd "C-c b") #'vault-bake
	(kbd "C-c f") #'vault-fetch
	(kbd "C-c y") #'vault-copy-url
	(kbd "C-c d") #'vault-destroy
	(kbd "C-c p") #'vault-proxy
	(kbd "C-c t") #'vault-token
	(kbd "&") #'vault-browse))

(let ((map vault-mode-map))
  (evil-define-key 'normal map
	(kbd "&") #'vault-browse))

(global-set-key (kbd "C-c v") 'vault)



(provide 'ft-vault2)

(defun vault--cli-fetch-sentinel (process event)
  (when (eq 0 (process-exit-status process))
	(with-current-buffer (process-buffer process)
	  (json-mode)
	  (json-pretty-print-buffer)
	  (goto-char (point-min)))))

(defun vault--cli-fetch (cmd)
  (let ((buff-name (format "*vault* %s" cmd)))
	(when (get-buffer buff-name)
	  (kill-buffer buff-name))
	
	(make-process
	 :name buff-name
	 :buffer buff-name
	 :command (split-string cmd)
	 :sentinel #'vault--cli-fetch-sentinel)
	(switch-to-buffer-other-window buff-name)))

(defface vault-buffer-comment
  '((t (:inherit font-lock-comment-face)))
  "Face used in instance buffer"
  :group 'vault-faces)

(defun vault--create-instance-buffer (instance)
  ;; (interactive
  ;;  (list (or (tabulated-list-get-id)
  ;; 			 (completing-read "Instance: " (vault--list-instances)))))

  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " '("florent")))))
  (message "%s" instance)
  (let* ((buff (get-buffer-create (format "*vault* %s" instance))))
	(with-current-buffer buff
	  (let ((inhibit-read-only t))
		(erase-buffer))
	  

	  (let ((fields '(("Instance" . "https://localhosttagueule")
					  ("Device" . "4")
					  ("Salt" . "salt"))))
		(dolist (field fields)
		  (insert (propertize (format "%s: " (car field)) 'face 'vault-buffer-comment))
		  (insert (propertize (cdr field) 'face 'vault-buffer-comment))
		  (insert "\n"))
		(insert (propertize "=============" 'face 'vault-buffer-comment))
		(goto-char (point-min))
		(search-forward "=")
		(end-of-line)
		;; (add-text-properties 1 (point) ')
		))

	(switch-to-buffer buff))
  )


(defun vault--buffer-get-device ()
  (save-excursion
	(goto-char (point-min))
	(search-forward "Device: ")
	(thing-at-point 'word)))

(defun vault--buffer-get-url ()
  (save-excursion
	(goto-char (point-min))
	(search-forward "Instance: ")
	(thing-at-point 'word)))


  
 ;; (let ((url (if prefix (format "https://remote.minivault.ledger-sbx.com/instances/%s" instance)
 ;; 				(format "https://%s.minivault.ledger-sbx.com" instance))))
 ;; 	(kill-new url)
 ;; 	(message "Copy %s" url)) 
