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
				  (vault--propertize-status (alist-get 'status instance)))) result ))
	result))

(defun vault--propertize-status (status)
  (cond ((string= status "HEALTHY") (propertize status 'face '(:foreground "green")))
		((string= status "BUSY") (propertize status 'face '(:foreground "orange")))
		(t (propertize status 'face '(:foreground "red")))))


(defun vault--propertize-owner (owner)
  (propertize owner 'face 'font-lock-comment-face))

(defun vault--propertize-name (name)
  name)

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
  (let ((default-directory "~/ledger/vault-ts/apps/cli/src/presets"))
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
(define-minor-mode vault-recipe-mode
  "Vault recipe mode"
  :keymap (make-sparse-keymap))

(defun ft-vault-recipe-bake ()
  (interactive)
  (let ((buff-manifest (current-buffer)) 
		(file (make-temp-file "vault-bake")))
	(with-current-buffer (find-file-noselect file)
	  (erase-buffer)
	  (insert-buffer buff-manifest)
	  (write-file file))

	(async-shell-command (format "ledger-vault bake %s --minivaultURL %s --salt %s"
								 file vault--url vault--salt)))
  )

(define-key vault-recipe-mode-map (kbd "C-c C-c") 'ft-vault-recipe-bake)

(defun vault-recipe (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (let* ((salt (read-string "Salt: ")) 
		 (buff-name (format "*vault-recipe* %s" instance))
		 (url (format "https://%s.minivault.ledger-sbx.com" instance) )
		 (buffer (get-buffer-create buff-name)))
	(when-let ((process (get-buffer-process buffer)))
	  (kill-process process))
	(with-current-buffer buff-name
	  (erase-buffer)
	  (json-mode)
	  (setq-local vault--salt salt)
	  (setq-local vault--url url)
	  (switch-to-buffer-other-window (current-buffer)))
	(make-process
	 :name buff-name
	 :buffer buff-name
	 :sentinel #'vault--recipe-sentinel
	 :command `("ledger-vault" "recipe" "--minivaultURL" ,url "--salt" ,salt))
	))


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
	(kbd "C-c r") #'vault-recipe
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

(defun vault--recipe-sentinel (process event)
  (when (eq 0 (process-exit-status process))
	(with-current-buffer (process-buffer process)
	  (json-pretty-print-buffer)
	  (vault-recipe-mode)
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

