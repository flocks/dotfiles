(require 'transient)
(require 'docker)
(require 'request)

;;; Code:
(defvar ft-vault-minivault-dir "~/ledger/minivault")
(defvar ft-vault-remote-url "https://remote.minivault.ledger-sbx.com")

(defun ft-vault--list-bake-preset ()
  (let ((default-directory "~/ledger/vault-js/packages/cli/src/presets"))
    (mapcar
     (lambda (preset) (string-trim-right preset ".json"))
     (split-string (shell-command-to-string "ls")))))

(defun ft-vault-bake (args)
  (interactive (list (transient-args transient-current-command)))
  (let ((preset (completing-read "Preset: " (ft-vault--list-bake-preset)))
	(options (string-join args " ")))
    (compile (format "ledger-vault bake --preset %s %s" preset options))))

(defun ft-vault-get (args)
  (interactive (list (transient-args transient-current-command)))
  (let* ((entity (completing-read "Entity: " '(accounts groups users entities)))
	 (options (string-join args " "))
	 (command (format "ledger-vault get %s %s --format json" entity options))
	 (output (shell-command-to-string command)))
    (let ((buf (get-buffer-create command)))
	  (with-current-buffer buf
		(message "%s" command)
		(erase-buffer)
		(json-mode)
		(insert output)
		(json-pretty-print-buffer)
		(goto-char (point-min)))
      (switch-to-buffer-other-window buf))))

(defun ft-vault-read-gate (prompt initial-input history)
  (replace-regexp-in-string
   "remote"
   (magit-completing-read prompt (ft-vault-get-vm-instances))
   ft-vault-remote-url))

(transient-define-prefix ft-vault ()
  "Transient for vault."
  ["CLI Options"
   ("-g" "Gate" "--minivaultURL " ft-vault-read-gate)
   ("-s" "Salt" "--salt " read-string)
   ("-d" "device" "--device " read-string)
   ]
  ["ledger-vault (CLI)"
   ("f" "fetch"     ft-vault-fetch)
   ("t" "getAuthToken"     ft-vault-get-auth-token)
   ("b" "bake"     ft-vault-bake)
   ("v" "version"     ft-vault-get-versions)
   ("g" "get"     ft-vault-get)]
  ["Minivault"
   ("r" "reset"     ft-vault-minivault-reset)
   ("k" "kubernetes"     kubel)
   ("p" "postgres"     ft-vault-psql-connect)])


(defvar ft-vault-sql--databases '("dbi:Pg:dbname=gate_minivault;host=localhost"
			    "dbi:Pg:dbname=hsm_driver;host=localhost"))

(defun ft-vault-get-pod-by-name (name)
  (replace-regexp-in-string "\n$" ""
							(shell-command-to-string
							 (format "kubectl get pods | sed 1d | awk '{print $1}' | grep %s" name))))

(defun ft-vault-forward-postgres-port ()
  (interactive)
  (let* ((postgres-pod (ft-vault-get-pod-by-name "postgres"))
		 (command (format "kubectl port-forward %s 5432:5432" postgres-pod)))
	(async-shell-command command)))

(defun ft-vault-psql-connect ()
  (interactive)
  (let* ((db (completing-read "Database: " ft-vault-sql--databases))
	 (conn (edbi:start)))
    (edbi:connect conn (list db "vault" "vault"))
    (edbi:dbview-open conn)))

(defun ft-vault--get-gate-params-from-args (args)
  "Utils function to extract --gate from transient ARGS"
  (let ((default "https://localhost:8443/gate/minivault"))
	(if args
		(or
		 (cadr (member "--minivaultURL" (split-string (car args))))
		 default)
	  default)))

(defun ft-vault-create-json-buffer (json buffer-name)
  "Take JSON string and BUFFER-NAME and create a buffer
with json-mode to display it."
  (let ((buf (get-buffer-create buffer-name)))
	(with-current-buffer buf
	  (erase-buffer)
	  (json-mode)
	  (insert json)
	  (json-pretty-print-buffer)
	  (goto-char (point-min)))
	(switch-to-buffer-other-window buf)))


(defun ft-vault-get-versions (args)
  "Call /_versions endpoint"
  (interactive
   (list (transient-args transient-current-command)))
  (let* ((result)
		 (gate (ft-vault--get-gate-params-from-args args))
		 (url (format "%s/gate/minivault/_version" gate)))
	(message "%s" url)
	(request url
	  :sync t
	  :headers '(("Content-Type" . "application/json"))
	  :success (cl-function
				(lambda (&key data &allow-other-keys)
				  (setq result data))))
	(ft-vault-create-json-buffer result (format "*json* %s" url))))

(defun ft-vault-minivault-upgrade ()
  (interactive)
  (let ((default-directory ft-vault-minivault-dir))
    (compile "helm upgrade minivault ."))
  )

(defun ft-vault-get-auth-token (args)
  (interactive (list (transient-args transient-current-command)))
  (let* ((options (string-join args " "))
	 (command (format "ledger-vault getAuthToken  %s" options))
	 (token (shell-command-to-string command)))
    (message "%s" command)
    (kill-new token)
    (message "%s" token)))


(defun ft-vault-fetch (args)
  (interactive (list (transient-args transient-current-command)))
  (let* ((endpoint (read-string "Endpoint: "))
	 (options (string-join args " "))
	 (command (format "ledger-vault fetch \"%s\" %s" endpoint options))
	 (output (shell-command-to-string command))
	 (buf (get-buffer-create command)))
    (with-current-buffer buf
      (erase-buffer)
      (json-mode)
      (insert output)
      (json-pretty-print-buffer)
      (goto-char (point-min)))
    (switch-to-buffer-other-window buf)))

(defun ft-vault--get-all-values-files ()
  (let* ((default-directory "~/ledger/minivault")
	 (files (mapcar
		 (lambda (path)
		   (substring path 2 (length path)))
		 (split-string (shell-command-to-string "find . -name \"values*.yaml\""))))
	 (choice (completing-read "Values: " files)))
    choice))

(defun ft-vault-minivault-reset ()
  (interactive)
  (let ((default-directory "~/ledger/minivault") 
		(values (ft-vault--get-all-values-files)))
    (async-shell-command
     (format "helm uninstall minivault || true ; helm install minivault . --set hsm.compartmentID=697 -f %s"
			 values))))

(defun ft-vault-get-vm-instances ()
  (let (result)
	(request (concat ft-vault-remote-url "/api/instances")
	  :sync t
	  :parser 'json-read
	  :headers '(("Content-Type" . "application/json"))
	  :success (cl-function
				(lambda (&key data &allow-other-keys)
				  (setq result data))))
	(mapcar (lambda (x) (alist-get 'name x)) result )))

(global-set-key (kbd "C-c v") 'ft-vault)
(provide 'ft-vault)

