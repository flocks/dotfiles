(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;; package setup
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))



(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(repeat-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-frame-font "Berkeley Mono 12" nil t)
(savehist-mode 1)
(electric-pair-mode 1)

;; look and feel
(setq display-line-numbers 'relative) 
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq modus-themes-slanted-constructs t)
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-common-palette-overrides
      '((fg-line-number-inactive "gray50")
	(fg-line-number-active red-cooler)
	(bg-line-number-inactive unspecified)
	(fringe unspecified)
	(bg-line-number-active unspecified)
	(bg-mode-line-active unspecified)
	(fg-mode-line-active fg-main)
	(border-mode-line-active blue-intense)
	(border-mode-line-inactive "#696969")
	(bg-mode-line-inactive unspecified)))

(defun ft-load-theme ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (call-interactively 'load-theme))


;; dired
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(setq dired-dwim-target t)
(setq dired-listing-switches "-al --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; compilation mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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
(setq project-compilation-buffer-name-function 'project-prefixed-buffer-name)
(setq compilation-ask-about-save nil)
(setq shell-command-switch "-ic")

(global-set-key (kbd "M-&") 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completions
;; vertico make vertical completion menu
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; marginalia: annotation in minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; ordeless matching style completion
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)))

;; embark: act on item
(use-package embark
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'embark-act))


;; magit
(use-package magit
  :defer t
  :ensure t)

;; ediiting
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-,") 'er/expand-region))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-.") 'mc/mark-next-like-this-word))


;; buffer
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)


;; compilation mode
(global-set-key (kbd "C-c c") 'compile)
