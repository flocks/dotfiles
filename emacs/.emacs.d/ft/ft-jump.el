(use-package hydra
  :straight t)

(defhydra hydra-jump (evil-motion-state-map "C-e")
  "Jump"
  ("f" (lambda () (interactive) (find-file "~/ledger/ledger-vault-front")) :exit t)
  ("v" (lambda () (interactive) (find-file "~/ledger/vault-ts")) :exit t)
  ("g" (lambda () (interactive) (find-file "~/ledger/ledger-vault-api")) :exit t)
  ("0" (lambda () (interactive) (find-file "~/0xparser")) :exit t)
  ("r" (lambda () (interactive) (find-file "~/ledger/vault-remote")) :exit t)
  ("R" (lambda () (interactive) (find-file "~/ledger/revault")) :exit t)
  ("m" (lambda () (interactive) (find-file "~/ledger/minivault")) :exit t)
  ("M" (lambda () (interactive) (find-file "~/ledger/vault-mobile-app")) :exit t)
  ("w" (lambda () (interactive) (find-file "~/ledger/wallet-connect-live-app")) :exit t)
  ("D" (lambda () (interactive) (find-file "~/Downloads")) :exit t)
  ("p" (lambda () (interactive) (find-file "~/riverodds")) :exit t)
  ("s" (lambda () (interactive) (find-file "~/screenshots")) :exit t)
  ("S" (lambda () (interactive) (find-file "~/sandbox/main.ts")) :exit t)
  ("a" (lambda () (interactive) (find-file "~/ledger/vault-apdu-connector")) :exit t)
  ("d" (lambda () (interactive) (find-file "~/dotfiles")) :exit t)
  ("h" (lambda () (interactive) (find-file "~")) :exit t)
  ("l" (lambda () (interactive) (find-file "~/ledger")) :exit t)
  ("L" (lambda () (interactive) (find-file "~/ledger/ledger-live")) :exit t)
  )

(defun ft-diff-file-dwim ()
  (interactive)
  (if (buffer-modified-p)
      (diff-buffer-with-file (current-buffer))
    (magit-diff-buffer-file)))

(defun ft-open-lisp-file ()
  (interactive)
  (find-file (read-file-name "Config file: " "~/.emacs.d/ft/")))

(defun visit-*Messages* ()
  "Visit buffer `*Messages* in another window"
  (interactive)
  (switch-to-buffer-other-window (messages-buffer))
  (goto-char (point-max)))

(use-package dumb-jump
  :straight t
  :config
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'ivy))

(defun ft/edit-config-file ()
  "Open settings.org file"
  (interactive)
  (find-file "~/dotfiles/emacs/.emacs.d/settings.el"))

(defun ft-fuzzy-find-file ()
  "Use 'project-find-file' when inside a project,
fallback to counsel-fzf otherwise."
  (interactive)
  (let ((func (or
			   (and (project-current) 'project-find-file)
			   'consult-find)))
	(call-interactively func)))

;; open directly elisp manual
(global-set-key (kbd "C-h l") (lambda ()
								(interactive)
								(info "Elisp")))

(with-eval-after-load 'ft-evil
  (define-key evil-normal-state-map (kbd "C-p") 'ft-fuzzy-find-file))

(global-set-key (kbd "C-=") 'ft/edit-config-file)
(global-set-key (kbd "C-h e") 'visit-*Messages*)
(global-set-key (kbd "C-c L") 'ft-open-lisp-file)

(provide 'ft-jump)
