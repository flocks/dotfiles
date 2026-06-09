
(defun ft-last-screenshots ()
  (interactive)
  (find-file-other-window "~/screenshots")
  (revert-buffer)
  (goto-char (point-max))
  (forward-line -1))

(global-set-key (kbd "M-^") 'project-dired)

(defun ft-copy-project-file-name (file-name)
  ;; inside /home/flocks/project/src/app.js will copy app
  (interactive "s")
  (let* ((root (file-truename (locate-dominating-file default-directory ".git")))
		 (filename (file-name-sans-extension file-name))
		 (copy (file-relative-name filename (format "%s/src" root))))
	(kill-new copy)
	(message "Copied %s" copy)))


(global-set-key (kbd "C-x p y") (lambda () (interactive)
								  (ft-copy-project-file-name buffer-file-name)))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'minibuffer-setup-hook 'yas-minor-mode)

(defun my-dabbrev-minibuffer-setup ()
  "Enable dabbrev in minibuffer."
  (local-set-key (kbd "M-/") 'dabbrev-expand)
  (local-set-key (kbd "C-M-/") 'dabbrev-completion))

(add-hook 'minibuffer-setup-hook 'my-dabbrev-minibuffer-setup)

(defun ft-reload-dir-locals ()
  (interactive)
  (dolist (buffer (project-buffers (project-current)))
	(with-current-buffer buffer
	  (normal-mode))))

(defun ft-quick-replace ()
  (interactive)
  (let ((word (thing-at-point 'word t)))
	(evil-ex (format "%%s/\\<%s\\>/" word))))

(global-set-key (kbd "C-c r") 'ft-quick-replace)

(setq-default mode-line-remote
			  '(:eval
				(when (tramp-tramp-file-p (or (buffer-file-name) (dired-current-directory)))
				  (propertize " TRAMP " 'face '(:background "red" :foreground "white")))))

(provide 'ft-misc)

