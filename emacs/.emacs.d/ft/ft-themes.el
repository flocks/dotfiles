(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)

(setq visible-bell nil ring-bell-function #'ignore)

; default font
(set-face-attribute 'default nil
		    :family "IBM Plex Mono"
		    :width 'normal
		    :height 120)

;; highlight/hide current line
(global-hl-line-mode 1)
(setq display-line-numbers-type 'relative)


(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package modus-themes
  :straight t
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-common-palette-overrides
		'((fg-line-number-active red-cooler)
		  (bg-line-number-inactive unspecified)
		  (fringe unspecified)
		  )
		)

  (modus-themes-toggle))

(use-package standard-themes :straight t
  :config
  (standard-themes-toggle))


(defun ft-load-theme ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (call-interactively 'load-theme))

(global-set-key (kbd "C-x F l") 'global-display-line-numbers-mode)
(global-set-key (kbd "C-x F t") 'standard-themes-toggle)


(provide 'ft-themes)
