(add-to-list 'compilation-error-regexp-alist
             'ts-build)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(ts-build
   "^\s?+\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)"
   1 2 3))

(add-to-list 'compilation-error-regexp-alist
             'vitest)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(vitest
   "^ ❯ \s?+\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)"

   1 2 3))

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

(global-set-key (kbd "M-*") 'ft-compile-run)

(provide 'ft-compile)
