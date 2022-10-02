(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)

;; use tab-bar as a notif bar
(tab-bar-mode 1)
(setq tab-bar-format '(tab-bar-format-align-right tab-bar-format-global))

(setq visible-bell       nil
      ring-bell-function #'ignore)
; default font
(set-face-attribute 'default nil
		    ;; :family "Roboto Mono"
		    :family "VictorMono"
		    ;; :family "IBM Plex Mono"
		    ;; :family "Fira Code"
		    :weight 'SemiBold
		    ;; :slant 'italic
		    :width 'normal
		    :height 140)
;; highlight/hide current line
(global-hl-line-mode -1)

(use-package tab-bar-echo-area
  :straight t
  :config
  (tab-bar-echo-area-mode))

(use-package minions
  :straight t
  :config (minions-mode))

(use-package modus-themes
  :straight t
  :init
  ;; Add all your customizations prior to loading the themes
  (global-set-key (kbd "<XF86Favorites>") 'modus-themes-toggle)

  (setq modus-themes-mode-line '(borderless moody accented))
  (setq modus-themes-region '(no-extend))
  (setq modus-themes-prompts '(intense bold))
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  )

;; free distracting editing/reading
(use-package darkroom
  :straight t
  :config
  (defun ft-darkroom-setup ()
	"Kill other window and toggle darkroom-mode"
	(interactive)
	(delete-other-windows)
	(call-interactively 'darkroom-mode))
  (setq darkroom-text-scale-increase 1)
  (setq darkroom-margins 0.2)
  (global-set-key (kbd "<XF86Tools>") 'ft-darkroom-setup))

(use-package ef-themes
  :straight t)

(use-package pulsar
  :straight t
  :config
  (pulsar-global-mode)
  (global-set-key (kbd "C-x L") 'pulsar-pulse-line)
  (setq pulsar-pulse-functions
      '(recenter-top-bottom
        move-to-window-line-top-bottom
        reposition-window
        bookmark-jump
        other-window
        delete-window
        delete-other-windows
        forward-page
        backward-page
        scroll-up-command
        scroll-down-command
        windmove-right
        windmove-left
        windmove-up
        windmove-down
        windmove-swap-states-right
        windmove-swap-states-left
        windmove-swap-states-up
        windmove-swap-states-down
        tab-new
        tab-close
        tab-next
        )))

(defun ft-load-theme ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (call-interactively 'load-theme))

(provide 'ft-themes)
