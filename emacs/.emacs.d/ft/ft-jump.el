
(use-package transjump
  :straight (transjump :type git :host github :repo "flocks/transjump")
  :config
  (evil-define-key '(motion insert normal visual) global-map (kbd "C-e") 'transjump)
  (setq transjump-folders
		'((:name  "front" :key  "f" :path  "~/ledger/ledger-vault-front")
		  (:name  "vjs" :key  "v" :path  "~/ledger/vault-ts")
		  (:name  "e2e" :key  "e" :path  "~/ledger/vault-e2e-tests")
		  (:name  "minivault" :key  "m" :path  "~/ledger/minivault")
		  (:name  "downloads" :key  "d" :path  "~/dotfiles")
		  (:name  "gate" :key  "g" :path  "~/ledger/ledger-vault-api")
		  (:name  "vault-automation" :key  "a" :path  "~/ledger/vault-automation")
		  (:name  "emacs packages" :key  "p" :path  "~/emacs-packages")
		  (:name  "Notes" :key  "N" :path  "~/denotes")
		  (:name  "Notes" :key  "D" :path  "~/Downloads")
		  (:name  "settings" :key  "s" :path  "~/.emacs.d")
		  (:name  "home" :key  "h" :path  "~")
		  (:name  "ledger" :key  "l" :path  "~/ledger")
		  (:name  "vault-remote" :key  "r" :path  "~/ledger/vault-remote")
		  (:name  "ft" :key  "F"  :path  "~/.emacs.d/ft")
		  (:name  "bin" :key  "b" :path  "~/bin")
		  (:name  "blog" :key  "B" :path  "~/florent.link"))))

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
(global-set-key (kbd "C-c =") 'ft-diff-file-dwim )
(global-set-key (kbd "C-h e") 'visit-*Messages*)
(global-set-key (kbd "C-c L") 'ft-open-lisp-file)

(provide 'ft-jump)
