(let ((regex-alist
	   '(
		 ;; ("ts-build". "^\s?+\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")
		 ;; ("vitest" . "^ ❯ \s?+\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)")
		 ;; ("turbo" ."\\([^\s]+\\):\\([0-9]+\\):\\([0-9]+\\)")
		 ;; ("cc" . "^\\([^\s]+\\):\\([0-9]+\\):\\([0-9]+\\)")
		 ;; ("flowtype" . "- \\([^\s]+\\):\\([0-9]+\\):\\([0-9]+\\)")
		 ;; ("prettier" . "\\[error] \\(.*\\): SyntaxError: Unexpected token (\\([0-9]+\\):\\([0-9]+\\)")
		 ;; ("typecheck" . "^\s?+\\(src.*\\):\\([0-9]+\\):\\([0-9]+\\)" )
		 ;; ("haskell" . "\\([a-zA-Z]+.hs\\):\\([0-9]+\\):\\([0-9]+\\)")
		 )))
  (dolist (regex regex-alist)
	(add-to-list 'compilation-error-regexp-alist (intern (car regex)))
	(add-to-list 'compilation-error-regexp-alist-alist
				 `(,(intern (car regex))
				   ,(cdr regex)
				   1 2 3))))

(setq project-compilation-buffer-name-function 'project-prefixed-buffer-name)
(setq compilation-ask-about-save nil)
(setq shell-command-switch "-c")
(setq compilation-max-output-line-length 300)

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
	(kbd "J") 'next-error-no-select
	(kbd "K") 'previous-error-no-select
	(kbd "D") 'ft-compilation-switch-directory
	(kbd "R") (lambda ()
				(interactive)
				(recompile t))))

(defun ft-compilation-switch-directory (dir)
  (interactive "D")
  (setq compilation-directory dir)
  (setq default-directory dir)
  (recompile t))

;; TODO makes this more generic to  use also  .git
(defun ft-get-project-root ()
  (locate-dominating-file (or (buffer-file-name) default-directory) "package.json"))

(defun ft-get-git-root ()
  (locate-dominating-file (or (buffer-file-name) default-directory) ".git"))

;; make emacs consider project with package.json as full project
;; useful for monorepo!
(setq project-vc-extra-root-markers '("package.json"))


(global-set-key (kbd "M-!") 'async-shell-command)
;; (define-key dired-mode-map (kbd "M-!") 'async-shell-command)
(defun my-compile ()
  "Run `compile` with an empty prompt."
  (interactive)
  (let ((compile-command ""))
    (call-interactively 'compile)))
(global-set-key (kbd "C-c c") 'my-compile)

(defcustom project-exe-name "main"
  "Executable name for the current project."
  :type '(choice string (const nil)))

(defcustom makeprg "make"
  "Command to compile current project."
  :type '(choice string (const nil)))

(defun ft-run-project-exe ()
  (interactive)
  (async-shell-command (format "%s" project-exe-name)))

(defun ft-debug-project-exe (begin end)
  (interactive "r")
  (async-shell-command (format "gf2 %s" project-exe-name)))

(defun ft-compile-project ()
  (interactive)
  (let ((compile-command makeprg)
		(compilation-read-command nil))
	(call-interactively 'project-compile)))

(define-key c-mode-map (kbd "C-c C-r") #'ft-run-project-exe)
(define-key c-mode-map (kbd "C-c C-d") #'ft-debug-project-exe)
(define-key c-mode-map (kbd "C-c C-c") #'ft-compile-project)

(provide 'ft-compile)
