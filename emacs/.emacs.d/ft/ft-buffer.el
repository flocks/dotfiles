(setq ibuffer-expert t)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(global-set-key (kbd "C-x C-b") 'ibuffer)

(let ((map ctl-x-x-map))              
  (define-key map "e" #'eval-buffer))

(defun ft-ibuffer-buffers-diff ()
  "Run diff-buffers directly with 2 marked buffers in ibuffer."
  (interactive)
  (let ((buffers (ibuffer-get-marked-buffers)))
	(unless (eq 2 (length buffers))
	  (user-error "I need 2 buffers to be marked"))
	(apply 'diff-buffers buffers)))
 
(define-key ibuffer-mode-map (kbd "C-c C-d") 'ft-ibuffer-buffers-diff)

(use-package ibuffer-vc
  :straight t
  :config
  (require 'uniquify)
  ;; I prefer to manually toggle the filter rather than adding a hook
  (define-key ibuffer-mode-map (kbd "C-c C-p") 'ibuffer-vc-set-filter-groups-by-vc-root))

(use-package scratch
  :straight t
  :config
  (defun ft-new-buffer (prefix)
	(interactive "P")
	(let ((current-prefix-arg (if prefix nil 4)))
	  (call-interactively #'scratch))) 

  (global-set-key (kbd "C-c n") 'ft-new-buffer))

(setq display-buffer-alist
	  `(;; no window
		("\\*kubel stderr\\*"
		 (display-buffer-no-window))
		("\\**vault-deploy*"
		 (display-buffer-no-window))
		("\\**mpv-elfeed*"
		 (display-buffer-no-window))
		("\\**shelldon.*"
		 (display-buffer-reuse-window display-buffer-pop-up-window))
		))





(provide 'ft-buffer)
