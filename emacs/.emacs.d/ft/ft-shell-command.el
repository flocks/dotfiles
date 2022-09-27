;; (defun ft-async-shell-command-in-directory (dir)
;;   (interactive "Ddirectory: \n")
;;   (let ((default-directory dir))
;; 	(call-interactively 'async-shell-command)))

;; (evil-define-key 'motion global-map (kbd "M-&") 'ft-async-shell-command-in-directory)

;; (use-package shelldon
;;   :straight t
;;   :config
;;   (global-set-key (kbd "M-&") 'shelldon))

;; (provide 'ft-shell-command)
;; (define-key selectrum-minibuffer-map (kbd "C-x C-x") 'ft-goto-root)



;; (defun ft-goto-root ()
;;   (interactive)
;;   (let* ((filename (thing-at-point 'filename))
;; 		(root (locate-dominating-file filename ".git")))
;; 	  (beginning-of-line)
;; 	  (kill-line)
;; 	  (insert root)
;; 	  (end-of-line)))

;; (define-key selectrum-minibuffer-map (kbd "C-x C-x") 'ft-goto-root)


(setq shell-command-prompt-show-cwd t)

(provide 'ft-shell-command)

