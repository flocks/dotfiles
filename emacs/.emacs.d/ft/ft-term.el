(use-package vterm
  :straight t
  :config
  (setq vterm-min-window-width 500)

  (defun project-vterm ()
	(interactive)
	(defvar vterm-buffer-name)
	(let* ((default-directory (project-root     (project-current t)))
		   (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
		   (vterm-buffer (get-buffer vterm-buffer-name)))
	  (if (and vterm-buffer (not current-prefix-arg))
		  (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
		(vterm))))

  (defun ft/auto-insert-vterm-focus ()
	(when (member major-mode '(erc-mode vterm-mode eshell-mode))
	  (evil-insert-state)))

  (add-hook 'buffer-list-update-hook #'ft/auto-insert-vterm-focus)
  (global-set-key (kbd "C-x p t") 'project-vterm))

;;(setq eshell-destroy-buffer-when-process-dies t)

(evil-set-initial-state 'term-mode 'emacs)
(provide 'ft-term)
