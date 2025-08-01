(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)

(setq
 visible-bell nil
 ring-bell-function #'ignore)

; default font
(set-face-attribute 'default nil
		    :family "Berkeley Mono"
		    :width 'normal
		    :height 120)

;; highlight/hide current line
(global-hl-line-mode -1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers 'relative)

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

(use-package fontaine
  :straight t
  :config
  (setq fontaine-presets
		'((small
		   :default-height 100)
		  (regular
		   :default-height 125)
		  (medium
		   :default-height 135)
		  (big
		   :default-height 155)
		  (large
		   :default-height 180)
		  (chill
		   :default-height 200)
		  (t ; our shared fallback properties
		   :default-family "Berkeley Mono"
		   ))))

(global-set-key (kbd "C-x F l") 'global-display-line-numbers-mode)
(global-set-key (kbd "C-x F t") 'standard-themes-toggle)
(global-set-key (kbd "C-x F f") 'fontaine-set-preset)



(provide 'ft-themes)
