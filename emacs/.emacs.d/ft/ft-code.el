(require 'evil)
(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq js-import-style "absolute")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package magit
  :straight t
  :config
  (setq magit-save-repository-buffers nil)
  (setq transient-default-level 5)
  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  ;; (setq magit-display-buffer-function 'magit-display-buffer-traditional)
  (setq magit-refresh-status-buffer nil)
  (setq magit-status-headers-hook '(magit-insert-head-branch-header))
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-file-dispatch))

;; useful to turn camelCase to snakeCase etc
(use-package string-inflection
  :straight t)

(use-package magit-scripts
  :straight (magit-scripts :type git :host github :repo "flocks/magit-scripts"))

;; (use-package typescript-mode
;;   :straight t
;;   :config
;;   (define-derived-mode typescript-react-mode typescript-mode
;;     "Typescript JSX")
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-react-mode))
;;   (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

(use-package flymake-eslint
  :straight t
  :config
  (add-hook 'web-mode-hook (lambda ()
							;; it seems we need to wait for eglot
							(run-with-timer 10 nil 'flymake-eslint-enable)))
  (add-hook 'js-ts-mode-hook (lambda ()
							;; it seems we need to wait for eglot
							(run-with-timer 10 nil 'flymake-eslint-enable))))


 (use-package eldoc-box
   :straight t
   :config
   (global-set-key (kbd "C-c C-k") #'eldoc-box-help-at-point))

(setq eldoc-echo-area-use-multiline-p nil)
(setq eglot-events-buffer-size 0
	  ;; eglot-ignored-server-capabilities '(:hoverProvider
	  ;; 									  :documentHighlightProvider)
	  eglot-autoshutdown t)


(use-package add-node-modules-path
    :straight t
  :init
  (add-hook 'web-mode-hook 'add-node-modules-path t))

(use-package msp
  :straight (:repo "https://github.com/flocks/msp.git")
  :config
  (setq msp-config-file '(".prettierrc" ".prettierrc.js" ".prettierrc.json" "prettier.config.cjs"))
  (global-set-key (kbd "C-c C-f") 'msp-prettify))

(use-package haskell-mode
  :straight t)

(use-package go-mode
  :straight t)

(use-package eglot
  :config
  ;; I don't like the small lag the first time I open a file that
  ;; that starts the server
  (setq eglot-sync-connect nil)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'js-ts-mode-hook 'eglot-ensure)
  (add-hook 'js-mode-hook 'eglot-ensure)
  (add-hook 'web-mode-hook 'eglot-ensure)

  (setq eglot-server-programs '((html-mode . ("tailwindcss-language-server" "--stdio"))
								(tsx-ts-mode . ("typescript-language-server" "--stdio" "--log-level" "4"))
								;; (typescript-ts-mode . ,(eglot-alternatives '("typescript-language-server" "--stdio" "--log-level" "4")))
								(typescript-ts-mode . ("typescript-language-server" "--stdio" "--log-level" "4"))
								(rust-ts-mode . ("rust-analyzer"))
								(c-mode . ("clangd-12"))
								;; (web-mode . ("npx" "--no-install" "flow" "lsp"))
								(go-mode . ("gopls"))
								(go-ts-mode . ("gopls"))
								;; (js-ts-mode . ("npx" "--no-install" "flow" "lsp"))
								;; (js-mode . ("npx" "--no-install" "flow" "lsp"))
								))

  (define-key eglot-mode-map (kbd "C-c A") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename))

(use-package restclient
  :straight t)


(use-package rainbow-delimiters
    :straight t)

(use-package evil-cleverparens
    :straight t
    :init
	(defun ft-insert-current-defun-call ()
	  "Take current defun and insert a new line calling defun"
	  (interactive)
	  (let ((name (lisp-current-defun-name)))
		(end-of-defun)
		(insert (format "(%s )" name))
		(backward-char 1)
		(evil-insert 0)))

	(let ((map emacs-lisp-mode-map))
	  (define-key map (kbd "C-c C-e") 'ft-insert-current-defun-call)
	  (define-key map (kbd "C-c C-c") 'eval-defun))

    (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
    (add-hook 'clojure-mode-hook 'evil-cleverparens-mode)
    (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode))


(use-package eros
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'eros-mode))

(use-package yafolding
  :straight t
  :config
  (add-hook 'json-ts-mode-hook 'yafolding-mode))

(progn
  (evil-define-key 'normal prog-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (evil-define-key 'normal prog-mode-map (kbd "M-n") 'flymake-goto-next-error))

(use-package git-link
  :straight t
  :config
  (setq git-link-default-branch "main"))

(use-package nodejs-repl
  :straight t
  :config
  (global-set-key (kbd "C-c C-n") 'nodejs-repl)
  (defun dp/nodejs-repl-remove-broken-filter ()
	(remove-hook 'comint-output-filter-functions 'nodejs-repl--delete-prompt t))
  (add-hook 'nodejs-repl-mode-hook #'dp/nodejs-repl-remove-broken-filter))

;; ocaml repl
(use-package utop
  :straight t)

(provide 'ft-code)
