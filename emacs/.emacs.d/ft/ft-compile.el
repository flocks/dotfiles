(let ((regex-alist
	   '(
		 ("ts-build". "^\s?+\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")
		 ("vitest" . "^ ❯ \s?+\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")
		 ("turbo" ."\\([^\s]+\\):\\([0-9]+\\):\\([0-9]+\\)")
		 ("cc" . "^\\([^\s]+\\):\\([0-9]+\\):\\([0-9]+\\)")
		 ("flowtype" . "- \\([^\s]+\\):\\([0-9]+\\):\\([0-9]+\\)")
		 ("prettier" . "\\[error] \\(.*\\): SyntaxError: Unexpected token (\\([0-9]+\\):\\([0-9]+\\)")
		 ("typecheck" . "^\s?+\\(src.*\\):\\([0-9]+\\):\\([0-9]+\\)" )
		 ("haskell" . "\\([a-zA-Z]+.hs\\):\\([0-9]+\\):\\([0-9]+\\)")
		 )))
  (dolist (regex regex-alist)
	(add-to-list 'compilation-error-regexp-alist (intern (car regex)))
	(add-to-list 'compilation-error-regexp-alist-alist
				 `(,(intern (car regex))
				  ,(cdr regex)
				  1 2 3))))

(setq project-compilation-buffer-name-function 'project-prefixed-buffer-name)
(setq compilation-ask-about-save nil)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(defun ft-kill-command ()
  "Takes the command and compilation directory and execute command in $TERMINAL"
  (interactive)
  (kill-new compile-command)
  (message "Copy command to clipboard"))

(let ((map compilation-mode-map))
  (evil-define-key 'normal map
    (kbd "Y") 'ft-kill-command)
  (evil-define-key 'normal map
	(kbd "R") (lambda ()
				(interactive)
				(recompile t)))
  )

;; TODO makes this more generic to  use also  .git
(defun ft-get-project-root ()
  (locate-dominating-file (or (buffer-file-name) default-directory) "package.json"))

(defun ft-get-git-root ()
  (locate-dominating-file (or (buffer-file-name) default-directory) ".git"))



;; make emacs consider project with package.json as full project
;; useful for monorepo!
(setq project-vc-extra-root-markers '("package.json"))

(defun ft-compile-run (prefix)
  "Wrapper to run compile command.
Prompt for a directory target if it's not dired or if we have a
prefix arg"
  (interactive "P")
  (let* ((is-dired (eq 'dired-mode major-mode))
		 (prompt-dir prefix))
	(let ((default-directory
		   (or (and prompt-dir (read-directory-name "Dir: "))
			   (ft-get-project-root)
			   default-directory)))
	  (let ((current-prefix-arg nil))
		(call-interactively #'compile)))))


(defun ft/project-custom-ci ()
  (interactive)
  (let ((current (file-name-nondirectory (directory-file-name (ft-get-git-root))))
		(projects '(("ledger-vault-front" .
					 "yarn lint --format unix && yarn prettier:check --loglevel silent && yarn flow && yarn typecheck")
					("turboph" .
					 "stack build")
					("hchess" .
					 "cabal build")
					("gof" .
					 "go test")
					("concage" .
					 "go run main.go")
					("vault-ts" .
					 "pnpm run ci")
					("vault-remote" .
					 "yarn lint && yarn prettier:check --loglevel silent && yarn typecheck && yarn validate-hooks && yarn test --coverage --coverageThreshold '{ \"global\": { \"branches\": 100, \"functions\": 100, \"lines\": 100, \"statements\": 100 } }'"))))
	(if-let ((project (assoc current projects)))
		(let ((compilation-read-command nil)
			  (compile-command (cdr project)))
		  (call-interactively #'project-compile))
	  (call-interactively #'project-compile))))

(global-set-key (kbd "M-*") 'ft/project-custom-ci)

(global-set-key (kbd "M-&") 'compile)

;; idea being able to filter output of commands
;; match the command, hide output until another command is run (by detecting prompt $)


(provide 'ft-compile)
