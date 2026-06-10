(require 'evil)


;; C code
(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq c-basic-offset 2)))
; use // for comment in C
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

(use-package transient :straight t)

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

(use-package typescript-mode
  :straight t
  :config
  (define-derived-mode typescript-react-mode typescript-mode
    "Typescript JSX")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-react-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))


(setq eldoc-echo-area-use-multiline-p nil)
(setq eglot-events-buffer-size 0
	  ;; eglot-ignored-server-capabilities '(:hoverProvider
	  ;; 									  :documentHighlightProvider)
	  eglot-autoshutdown t)


(use-package eglot
  :config
  ;; I don't like the small lag the first time I open a file that
  ;; that starts the server
  (setq eglot-sync-connect nil)
  (add-hook 'typescript-mode-hook 'eglot-ensure)

  (setq eglot-server-programs '((html-mode . ("tailwindcss-language-server" "--stdio"))))

  (define-key eglot-mode-map (kbd "C-c A") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename))

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

(use-package markdown-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(provide 'ft-code)
