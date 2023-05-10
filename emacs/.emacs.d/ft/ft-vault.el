(defvar vault-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `vault2-mode'.")

;; TODO use this
(defvar vault-remote-base-url "minivault.ledger-sbx.com")

(setq vault--api-url "https://remote.minivault.ledger-sbx.com/api")

(defun vault-read-salt ()
  (or (read-string "Salt: ") "\"\""))

(define-derived-mode vault-mode tabulated-list-mode "Vault-Remote"
  "Special mode for vault remote instances"
  ;; (kill-all-local-variables)
  (setq mode-name "Vault")
  (setq major-mode 'vault-mode)
  (use-local-map vault-mode-map)
  (setq tabulated-list-format [("Name" 30 t)
							   ("Owner" 30 t)
							   ("Status" 10 t '(:right-align t))])

  (setq tabulated-list-entries (vault--set-tabulated-entries vault-instances))
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
	(url-retrieve-synchronously (format "https://remote.%s/api/instances" vault-remote-base-url) t)
	(ft-parse-retrieve-buffer)))

(defun vault--get-presets ()
  (with-current-buffer
	  (url-retrieve-synchronously (format "https://remote.%s/api/presets" vault-remote-base-url) t)
	(ft-parse-retrieve-buffer)))

(defun vault--list-instances ()
  (let ((json (append (vault--get-instances) nil)))
	(mapcar (lambda (x) (alist-get 'name x)) json)))

(defun vault--set-tabulated-entries (instances-json)
  (let ((json (append instances-json nil))
		(result nil))
	(dolist (instance json result)
	  (push
	   `(,(alist-get 'name instance)
		 ,(vector (vault--propertize-name (alist-get 'name instance))
				  (vault--propertize-owner (alist-get 'owner instance))
				  (vault--propertize-status (alist-get 'status instance)))) result ))
	result))

(defun vault--get-instances-async ()
  (url-retrieve (format "https://remote.%s/api/instances" vault-remote-base-url)
				(lambda (status)
				  (setq vault-instances (ft-parse-retrieve-buffer))
				  (with-current-buffer "*vault*"
					(vault-mode)))))

(defun vault-2 ()
  (interactive)
  (setq vault-instances nil)
  (vault--create-buffer "*vault*")
  (vault-mode)
  (vault--get-instances-async))

(defun vault-draw ()

  )

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
  (let ((url (if prefix (format "https://remote.%s/instances/%s" vault-remote-base-url instance)
				(format "https://%s.%s" instance vault-remote-base-url))))
	(kill-new url)
	(message "Copy %s" url)))


(defun vault-kube (namespace)
  "Set the namespace."
  (interactive
   (list (let ((instance (or (tabulated-list-get-id)
							 (completing-read "Instance: " (vault--list-instances)))))
		   (format "minivault-%s-sbx" instance))))
  (let* ((kubel--buffer (get-buffer (kubel--buffer-name)))
         (last-default-directory (when kubel--buffer
                                   (with-current-buffer kubel--buffer default-directory))))
    (when kubel--buffer (kill-buffer kubel--buffer))
    (setq kubel-namespace namespace)
    (kubel--add-namespace-to-history namespace)
    (kubel last-default-directory))
  )

(defun vault-fetch (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (let* ((salt (vault-read-salt))
		 (endpoint (read-string "Endpoint: "))
		 (url (format "https://%s.%s" instance vault-remote-base-url))
		 (device (read-number "Device" 4))
		 (command (format "ledger-vault fetch %s --salt %s --device %s --minivaultURL %s"
						  endpoint salt device url)))
	(vault--cli-fetch command)))

(defun vault-change-host ()
  (interactive)
  (setq vault-remote-base-url (completing-read "Host:"
											   '("minivault.ledger-sbx.com"
												 "minivault.ledger-stg.com")))
  (vault-2))

(defun vault-browse (instance &optional prefix)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))
		 current-prefix-arg))
  (let* ((url (if prefix (format "https://remote.%s/instances/%s" vault-remote-base-url instance)
				(format "https://%s.%s" instance vault-remote-base-url))))
	(eww-browse-with-external-browser url)))

;; (defun vault-bake (instance)
;;   (interactive
;;    (list (or (tabulated-list-get-id)
;; 			 (completing-read "Instance: " (vault--list-instances)))))
;;   (let* ((preset (completing-read "Preset: " (vault--list-bake-preset)))
;; 		 (salt (vault-read-salt))
;; 		 (salt-param (if (not (string= salt "")) (format "--salt %s" salt) ""))
;; 		 (minivault-url (format "https://%s.%s" instance vault-remote-base-url))
;; 		 (command (format "ledger-vault bake --manifest %s --minivaultURL %s %s"
;; 						  preset minivault-url salt-param))
;; 		 (compilation-buffer-name-function (lambda (mode) (format "*vault-bake-%s*" instance))))
;; 	(compile command)))

(defun vault-bake (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (let* ((default-directory "~/ledger/vault-ts/apps/cli/src/manifests/") 
		 (manifest (read-file-name "Manifest"))
		 (salt (vault-read-salt))
		 (salt-param (if (not (string= salt "")) (format "--salt %s" salt) ""))
		 (minivault-url (format "https://%s.%s" instance vault-remote-base-url))
		 (command (format "ledger-vault bake %s --minivaultURL %s %s --noiseChannelV2"
						  manifest minivault-url salt-param))
		 (compilation-buffer-name-function (lambda (mode) (format "*vault-bake-%s*" instance))))
	(compile command)))

(defun vault-bake-file (instance)
  (interactive
   (list (completing-read "Instance: " (vault--list-instances))))
  (unless (eq major-mode 'json-mode)
	(user-error "Not a JSON file"))
  (let* ((manifest (buffer-file-name))
		 (salt (vault-read-salt))
		 (salt-param (if (not (string= salt "")) (format "--salt %s" salt) ""))
		 (minivault-url (format "https://%s.%s" instance vault-remote-base-url))
		 (command (format "ledger-vault bake %s --minivaultURL %s %s"
						  manifest minivault-url salt-param))
		 (compilation-buffer-name-function (lambda (mode) (format "*vault-bake-%s*" instance))))
	(compile command)))


(defun vault-token (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (let* ((device (read-number "Device: "))
		 (salt (vault-read-salt))
		 (minivault-url (format "https://%s.%s" instance vault-remote-base-url)))
	(kill-new (shell-command-to-string (format "ledger-vault getAuthToken --device %s  --salt %s --minivaultURL %s"
									  device salt minivault-url)))))

(defun vault--list-bake-preset ()
  (let ((default-directory "~/ledger/vault-ts/apps/cli/bin/manifests"))
    (mapcar
     (lambda (preset) (string-trim-right preset ".json"))
     (split-string (shell-command-to-string "ls")))))

(defun vault-deploy (prefix)
  (interactive "P")
  (let* ((presets (mapcar (lambda (x) (alist-get 'id x)) (vault--get-presets)))
		 (preset-or-file (if prefix (read-file-name "File: ") (completing-read "Preset: " presets))))
	(if prefix (vault--deploy-file preset-or-file) (vault--deploy-preset preset-or-file))))

(defun vault--deploy-file (file)
  (let* ((owner "Florent")
		 (name (read-string "Name: " "flo-")))
	(async-shell-command (format "ledger-vault deploy --name %s --owner %s --values \"$(cat %s |yq)\""
								 name owner file)
						 "*vault-deploy*")
	(run-at-time 2 nil 'vault)))

(defun vault--deploy-preset (preset)
  (let* ((owner "Florent")
		 (name (replace-regexp-in-string "\\." "" (format "%s-%s" "flo" preset))))
	(async-shell-command (format "ledger-vault deploy --name %s --owner %s --preset %s"
								 name owner preset)
						 "*vault-deploy*")
	(run-at-time 2 nil 'vault)))


(defun vault-proxy (instance)
  (interactive
   (list (or (tabulated-list-get-id)
			 (completing-read "Instance: " (vault--list-instances)))))
  (let ((buff-name "*vault-proxy*")
		(url (format "https://%s.%s" instance vault-remote-base-url) ))
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
	  (url-retrieve (format "https://remote.%s/api/instances/%s" vault-remote-base-url instance) (lambda (_)
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
  (let* ((salt (vault-read-salt)) 
		 (buff-name (format "*vault-recipe* %s" instance))
		 (url (format "https://%s.%s" instance vault-remote-base-url) )
		 (buffer (get-buffer-create buff-name)))
	(when-let ((process (get-buffer-process buffer)))
	  (kill-process process))
	(with-current-buffer buff-name
	  (erase-buffer)
	  (json-ts-mode)
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
  (url-retrieve (format "https://remote.%s/api/instances/%s" vault-remote-base-url instance)
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
				  (json-ts-mode)
				  (switch-to-buffer-other-window (current-buffer)))))


(let ((map vault-mode-map))
  (evil-define-key 'motion map
	(kbd "RET") #'vault-view-config
	(kbd "+") #'vault-deploy
	(kbd "C-c b") #'vault-bake
	(kbd "C-c h") #'vault-change-host
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

(global-set-key (kbd "C-c v") 'vault-2)

(defun vault--cli-fetch-sentinel (process event)
  (when (eq 0 (process-exit-status process))
	(with-current-buffer (process-buffer process)
	  (json-ts-mode)
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


(provide 'ft-vault)

