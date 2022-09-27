;; (defun ft-term-dwim ()
;;   "Dwim wrapper to open a terminal.

;; If inside a project, run projectile-run-eshell, otherwise run eshell.
;; Spawn a new instance if current buffer is a eshell"
;;   (interactive)
;;   (let ((inside-project-p (ft--term-is-inside-project))
;; 	(current-buffer-eshell-p (eq major-mode 'eshell-mode)))
;;     (setq current-prefix-arg (if current-buffer-eshell-p '(4)))
;;     (if inside-project-p
;; 	(call-interactively 'projectile-run-eshell)
;;       (call-interactively 'eshell))))

;; (defun ft-next-major-mode-projectile-buffer ()
;;   "Switch to project buffer of same mode of current buffer."
;;   (interactive)
;;   (let* ((mode major-mode)
;; 	 (buffers (cdr (projectile-project-buffers)))
;; 	 (next (dolist (buff buffers)
;; 		 (with-current-buffer buff
;; 		   (when (eq major-mode mode)
;; 		     buff)))))
;;     (when next
;;       (switch-to-buffer next))))

;; (defun ft-term-dwim-other-window ()
;;   (interactive)
;;   (if (> (length (window-list)) 1)
;;       (split-window-vertically)
;;     (split-window-horizontally))
;;   (other-window 1)
;;   (ft-term-dwim))

;; ;; (global-set-key (kbd "s-e") 'ft-term-dwim)
;; (global-set-key (kbd "s-SPC") 'ft-next-major-mode-projectile-buffer)



;; ;; TODO does not work in buffer that are not files (ex magit buffer if called from other place)
;; (defun ft--term-is-inside-project ()
;;   (projectile-project-root default-directory))


(use-package vterm
  :straight t
  :config
  (setq vterm-min-window-width 500)
  (defun ft/auto-insert-vterm-focus ()
	(when (member major-mode '(erc-mode vterm-mode eshell-mode))
	  (evil-insert-state)))

  (add-hook 'buffer-list-update-hook #'ft/auto-insert-vterm-focus))

;;(setq eshell-destroy-buffer-when-process-dies t)

(evil-set-initial-state 'term-mode 'emacs)
(provide 'ft-term)
