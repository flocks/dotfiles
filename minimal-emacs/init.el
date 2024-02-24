(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(repeat-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(savehist-mode 1)
(electric-pair-mode 1)
(ido-mode)

;;;; package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; dired
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(setq dired-dwim-target t)
(setq dired-listing-switches "-al --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; compilation mode
(defun ft-compilation-switch-directory (dir)
  (interactive "D")
  (setq compilation-directory dir)
  (setq default-directory dir)
  (recompile))

(require 'compile)
(let ((map compilation-mode-map))
  (define-key map (kbd "D") 'ft-compilation-switch-directory)
  (define-key map (kbd "R") (lambda () (interactive)
			      (recompile t))))

(global-set-key (kbd "M-&") 'compile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; magit
(use-package magit
  :ensure t)

;; ediiting
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

