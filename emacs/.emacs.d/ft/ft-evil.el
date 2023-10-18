(use-package evil
  :straight t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it

  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-keybinding nil)

  :config ;; tweak evil after loading it
  (add-hook 'view-mode-hook 'evil-local-mode)
  (add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?- "w")))
  (add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
  (evil-mode))

(use-package evil-commentary
  :after evil
  :straight t
  :init (evil-commentary-mode))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

(provide 'ft-evil)
