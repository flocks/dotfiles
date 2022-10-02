(add-to-list 'compilation-error-regexp-alist
             'flowtype)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(flowtype
   "Error -+ \\(.*\\)\:\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

(add-to-list 'compilation-error-regexp-alist
             'eslint-non-unix)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(eslint-non-unix
   "\\(.*\\)\n  \\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

(add-to-list 'compilation-error-regexp-alist
             'ts-build)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(ts-build
   "\\(.*\\)\(\\([0-9]+\\),\\([0-9]+\\)" 1 2 3))

(defun ft-build-compilation-buffer (filename &optional wildcards)
  (interactive
   (find-file-read-args "Find file: " (confirm-nonexistent-file-or-buffer)))
  (let ((default-directory filename)
	(command (read-shell-command "Command: ")))
    (compile command)
    (with-current-buffer "*compilation*"
      (rename-buffer (format "*compil* - %s - %s" filename command)))))

(global-set-key (kbd "s-b") 'ft-build-compilation-buffer)

(setq project-compilation-buffer-name-function 'project-prefixed-buffer-name)
(setq compilation-ask-about-save nil)

(defun ft-recompile-visible-window ()
  "Run re-compile on visible compilation window"
  (interactive)
  (dolist (buff (mapcar #'window-buffer (window-list)))
    (with-current-buffer buff
      (when (eq major-mode 'compilation-mode)
	(recompile)))))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)



(defun ft-compile-change-directory-packages ()
  "Change lerna package to compile on.
[TODO] to improve to handle all cases/projects"
  (interactive)
  (let* ((root-folder (locate-dominating-file default-directory ".git"))
	 (default-directory (format "%s/packages" root-folder))
	 (packages (cons "root" (split-string (shell-command-to-string "ls"))) )
	 (package (completing-read "Packages: " packages))
	 (dir (if (string-equal package "root")
		  "~/ledger/vault-js"
		(format "~/ledger/vault-js/packages/%s" package))))
    (setq compilation-directory dir)
    (message "%s" compilation-directory)
    (recompile)))

(defun ft-compile-change-directory (dir)
  "Change directory of current compilation buffer and
recompile."
  (interactive "Ddir")
  (setq compilation-directory dir)
  (recompile))

(defun ft-compile-change-command ()
  "Update command of current compilation buffer."
  (interactive)
  (recompile t))

(defun ft-compile-change-buffer-name (buff-name)
  "Save compilation buffer by giving it a name."
  (interactive
   (list (read-string "Buffer name: "
		      (format "*compilation* - %s "
			      (car compilation-arguments)))))
  (rename-buffer buff-name))


;; TODO have a variant to only list of current project
;; TODO list in completion and in ibuffer only when prefix-arg
(defun ft-compile-list-other-buffers ()
  "Display all compilation buffer in ibuffer"
  (interactive)
  (ibuffer t "*compilations buffers*" '((mode . compilation-mode))))

(defun ft-compile-list-other-buffers-completion ()
  (interactive)
  (let* ((buffers (seq-filter
		  (lambda (x)
		    (string-match "*compilation*" x))
		  (mapcar 'buffer-name (buffer-list))))
	(buffer (completing-read "Compilation buffers: " buffers)))
    (switch-to-buffer buffer)))


(defun ft-compile-new-compile (dir)
  "Start a new compilation from the current one"
  (interactive "Ddir")
  (with-temp-buffer
    (let ((command (compilation-read-command ""))
	  (default-directory dir))
      (compile command))))

(defun ft-kill-command ()
  "Takes the command and compilation directory and execute command in $TERMINAL"
  (interactive)
  (kill-new compile-command)
  (message "Copy command to clipboard"))

(let ((map compilation-mode-map))
  (evil-define-key 'normal map
    (kbd "d") 'ft-compile-change-directory)
  (evil-define-key 'normal map
    (kbd "D") 'ft-compile-change-directory-packages)
  (evil-define-key 'normal map
    (kbd "c") 'ft-compile-change-command)
  (evil-define-key 'normal map
    (kbd "Y") 'ft-kill-command)

  (evil-define-key 'normal map
    (kbd "L") 'ft-compile-list-other-buffers-completion)
  (evil-define-key 'normal map
    (kbd "+") 'ft-compile-new-compile)
  (evil-define-key 'normal map
    (kbd "s") 'ft-compile-change-buffer-name))



;; TODO check if it's part of project
(defun ft-compile--get-visible-compilation-buffers ()
  (let (buffers)
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
	(when (eq major-mode 'compilation-mode)
	  (push (window-buffer window) buffers))
	(message "%s" major-mode)))
    buffers))

(defun ft-project-compile-dwim ()
  "Run project-compile but rather run re-compile if a
*compilation* buffer is visible.

If more than 1 compilation buffer is visible, prompt
to choose the target.
"
  (interactive)
  (let ((buffers (ft-compile--get-visible-compilation-buffers)))
    (cond ((eq 0 (length buffers)) (project-compile))
	  ((eq 1 (length buffers)) (with-current-buffer (car buffers)
				     (recompile)))
	  (t (let ((choice (completing-read "Buffer: " (mapcar #'buffer-name buffers))))
	       (with-current-buffer choice
		 (recompile))))
	  ))
  )

(global-set-key (kbd "s-C") 'ft-project-compile-dwim)
(global-set-key (kbd "s-u") 'ft-project-compile-dwim)
(global-set-key (kbd "M-*") 'project-compile)
(global-set-key (kbd "C-c r") 'recompile)

(provide 'ft-compile)
