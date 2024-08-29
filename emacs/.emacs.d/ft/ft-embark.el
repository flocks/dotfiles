(recentf-mode 1)
(use-package consult
  :straight t
  :config
  (global-set-key (kbd "C-;") 'consult-buffer)
  (setq consult-async-min-input 2)
  (setq consult-find-args
		"find . -not ( -wholename */.* -prune ) -not ( -wholename *node_modules* -prune )")

  (global-set-key (kbd "C-c M-i") 'consult-imenu)
  (global-set-key (kbd "C-c M-f") 'consult-focus-lines)
  (global-set-key (kbd "C-c M-k") 'consult-keep-lines)
  (global-set-key (kbd "C-c M-t") 'consult-theme)
  (define-key evil-normal-state-map (kbd "C-p") 'consult-find))

(use-package vertico
  :straight t
  :config
  (vertico-mode)
  (define-key vertico-map (kbd "M-r") 'consult-history)
  (define-key minibuffer-mode-map (kbd "M-r") 'consult-history)
  (define-key minibuffer-local-shell-command-map (kbd "M-r") 'consult-history))


(use-package embark
  :straight t
  :bind
  (("C-=" . embark-act)      
   ("C-c C-o" . embark-export))       
)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult-dir
  :straight t
  :bind (:map vertico-map
         ("C-;" . consult-dir)
         ("C-x C-f" . consult-dir-jump-file)
		 )
  :config
  (setq consult-dir-shadow-filenames nil))

(use-package monobuffer
  :straight (monobuffer :type git :host github :repo "flocks/monobuffer.el")
  :config
  (define-key vertico-map (kbd "M-u") 'monobuffer)
  (define-key vertico-map (kbd "M-U") 'monobuffer-root))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(provide 'ft-embark)
