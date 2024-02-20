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
		    ;; :family "VictorMono"
		    :family "Iosevka Comfy"
		    ;; :weight 'semibold
		    ;; :slant 'normal
		    :width 'normal
		    :height 120)
;; highlight/hide current line
(global-hl-line-mode -1)

(use-package tab-bar-echo-area
  :straight t
  :config
  (tab-bar-echo-area-mode))

(use-package gruber-darker-theme
  :straight t)


;; (use-package minions
;;   :straight t
;;   :config (minions-mode))

(use-package modus-themes
  :straight t
  :config
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

  (modus-themes-toggle))


(use-package standard-themes
  :straight t)

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
  (global-set-key (kbd "C-x F F") 'ft-darkroom-setup))

(use-package ef-themes
  :straight t)

(use-package gruvbox-theme
  :straight t)

(use-package doom-themes
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
        tab-next)))

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
		   :deeault-height 155)
		  (large
		   :default-height 180)
		  (t ; our shared fallback properties
		   :default-family "Iosevka Comfy"
		   ))))

(global-set-key (kbd "C-x F l") 'global-display-line-numbers-mode)
(global-set-key (kbd "C-x F t") 'modus-themes-toggle)
(global-set-key (kbd "C-x F f") 'fontaine-set-preset)


(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers 'relative)

(use-package rose-pine-emacs
  :straight (rose-pine-emacs :type git :host github :repo "thongpv87/rose-pine-emacs"))

;; (use-package minibar
;;   :straight '(:type git :repo  "https://codeberg.org/akib/emacs-minibar.git")
;;   :config
;;   (setq minibar-group-left '((lambda ()
;; 							   (if (boundp 'notmuch-indicator-string)
;; 								   notmuch-indicator-string
;; 								 ""))))
;;   (setq minibar-group-middle '((lambda ()
;; 								 (if (boundp 'erc-modified-channels-object)
;; 									 erc-modified-channels-object
;; 								   ""))))
;;   (minibar-mode))

(provide 'ft-themes)
