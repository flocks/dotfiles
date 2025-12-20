(use-package hydra
  :straight t)

(defhydra hydra-jump (evil-motion-state-map "C-e")
  "Jump"
  ("0" (lambda () (interactive) (find-file "~/0xparser")) :exit t)
  ("A" (lambda () (interactive) (find-file "~/ledger/revault/packages/api")) :exit t)
  ("D" (lambda () (interactive) (find-file "~/Downloads")) :exit t)
  ("L" (lambda () (interactive) (find-file "~/ledger/ledger-live")) :exit t)
  ("M" (lambda () (interactive) (find-file "~/ledger/revault/packages/mobile")) :exit t)
  ("R" (lambda () (interactive) (find-file "~/ledger/revault")) :exit t)
  ("S" (lambda () (interactive) (find-file "~/sandbox/main.ts")) :exit t)
  ("U" (lambda () (interactive) (find-file "~/ledger/revault/packages/ui")) :exit t)
  ("W" (lambda () (interactive) (find-file "~/ledger/revault/packages/web")) :exit t)
  ("a" (lambda () (interactive) (find-file "~/ledger/vault-apdu-connector")) :exit t)
  ("c" (lambda () (interactive) (find-file "~/chessblind")) :exit t)
  ("d" (lambda () (interactive) (find-file "~/dotfiles")) :exit t)
  ("e" (lambda () (interactive) (find-file "~/ledger/vault-e2e-tests")) :exit t)
  ("f" (lambda () (interactive) (find-file "~/ledger/ledger-vault-front")) :exit t)
  ("g" (lambda () (interactive) (find-file "~/ledger/ledger-vault-api")) :exit t)
  ("h" (lambda () (interactive) (find-file "~")) :exit t)
  ("l" (lambda () (interactive) (find-file "~/ledger")) :exit t)
  ("m" (lambda () (interactive) (find-file "~/ledger/les-multisig")) :exit t)
  ("p" (lambda () (interactive) (find-file "~/riverodds")) :exit t)
  ("r" (lambda () (interactive) (find-file "~/ledger/vault-remote")) :exit t)
  ("s" (lambda () (interactive) (find-file "~/screenshots")) :exit t)
  ("v" (lambda () (interactive) (find-file "~/ledger/vault-ts")) :exit t)
  )

(defun ft-diff-file-dwim ()
  (interactive)
  (if (buffer-modified-p)
      (diff-buffer-with-file (current-buffer))
    (magit-diff-buffer-file)))

(defun ft-open-lisp-file ()
  (interactive)
  (find-file (read-file-name "Config file: " "~/.emacs.d/ft/")))

(global-set-key (kbd "C-c U") 'project-dired)

;; (use-package dumb-jump
;;   :straight t
;;   :config
;;   (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
;;   (setq dumb-jump-selector 'ivy))

;; open directly elisp manual
(global-set-key (kbd "C-h l") (lambda ()
								(interactive)
								(info "Elisp")))

(global-set-key (kbd "C-c L") 'ft-open-lisp-file)

(provide 'ft-jump)
