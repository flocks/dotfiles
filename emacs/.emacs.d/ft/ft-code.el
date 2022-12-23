(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq js-import-style "absolute")


(require 'treesit)
(setq treesit-extra-load-path '("~/tree-sitter-module/dist"))

;; use json-ts-mode from tree-sitter instead of default json-mode
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

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
  ("M-g" . magit-status)
  ("C-x g" . magit-status)
  ("C-c g" . magit-file-dispatch))

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
  (add-hook 'js-mode-hook (lambda ()
							;; it seems we need to wait for eglot
							(run-with-timer 10 nil 'flymake-eslint-enable))))


(use-package add-node-modules-path
  :straight t
  :init
  (add-hook 'web-mode-hook 'add-node-modules-path t))

(use-package prettier
  :straight t
    :init
    (add-hook 'typescript-ts-mode-hook 'prettier-mode)
    (add-hook 'js-mode-hook 'prettier-mode)
    (add-hook 'json-ts-mode-hook 'prettier-mode)
    (add-hook 'web-mode-hook 'prettier-mode)
  )

(use-package haskell-mode
  :straight t)

(use-package go-mode
  :straight t)

(use-package eglot
  :straight t
  :config
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'js-ts-mode-hook 'eglot-ensure)
  (add-hook 'js-mode-hook 'eglot-ensure)
  (add-hook 'web-mode-hook 'eglot-ensure)

  (setq eglot-server-programs '((typescript-ts-mode . ("typescript-language-server" "--stdio"))
								(web-mode . ("npx" "--no-install" "flow" "lsp"))
								(js-ts-mode . ("npx" "--no-install" "flow" "lsp"))
								(js-mode . ("npx" "--no-install" "flow" "lsp"))))

  (define-key eglot-mode-map (kbd "C-c A") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename))

(use-package restclient
  :straight t)

(use-package eldoc-box
  :straight t
  :config
  (add-hook 'eglot-connect-hook #'eldoc-box-hover-at-point-mode t))

(use-package edbi
  :straight t
  :config
  (define-key ctbl:table-mode-map (kbd "C-c C-o") 'edbi:dbview-query-result-quicklook-command))

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

(evil-define-key 'normal prog-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(evil-define-key 'normal prog-mode-map (kbd "M-n") 'flymake-goto-next-error)

(use-package git-link
  :straight t
  :config
  (setq git-link-default-branch "main"))

;; realud is a frontend wrapper to run debugger (gdb, bashdb..etc..)
(use-package realgud
  :straight t)

(use-package multiple-cursors
  :straight t)
(use-package nodejs-repl
  :straight t
  :config
  (global-set-key (kbd "C-c C-n") 'nodejs-repl))


(provide 'ft-code)
