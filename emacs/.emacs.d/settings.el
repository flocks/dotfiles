(require 'package)
;; optimization
(setq gc-cons-threshold (* 1024 1024 100))
(add-to-list 'load-path
	     (expand-file-name (concat user-emacs-directory "ft")))

(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;; package setup
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; activate repeat-mode, very useful for tab-bar mode
(repeat-mode)

;; I always accicentally do this keystroke that is slow
(global-set-key (kbd "C-h h") nil)

;;; I don't like that is done auto
(setq auto-save-default nil)

;;; setup straight.el 
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq make-backup-files nil)
(setq create-lockfiles nil)

(use-package wgrep
  :straight t
  :config
  (setq wgrep-auto-save-buffer t))

(custom-set-variables
    '(tab-width 4))

(setq help-window-select t)

;; display scratch as startup
(setq initial-buffer-choice t)

;; save read-shell-command history
(savehist-mode 1)

;; faster to type y/n than yes/no
(defalias 'yes-or-no-p 'y-or-n-p)


(setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "backups"))))

(setq auto-save-file-name-transforms
  `((".*" "~/.emacs-saves/" t)))

(setq epa-pinentry-mode 'loopback)


(use-package ivy-xref
  :straight t
  :config
  (setf xref-show-definitions-function #'ivy-xref-show-defs))


(use-package yasnippet
  :straight t
  :config (yas-global-mode))


;;; org mode
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; 

;; auto insert closing parenthesis/bracket/quote..etc..
(electric-pair-mode 1)

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  :init
  (global-corfu-mode)
  :config
  (defun corfu-move-to-minibuffer ()
	(interactive)
	(let (completion-cycle-threshold completion-cycling)
	  (apply #'consult-completion-in-region completion-in-region--data)))
  (global-set-key (kbd "C-SPC") #'completion-at-point)
  (define-key corfu-map (kbd "C-c C-o") #'corfu-move-to-minibuffer))

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

(use-package key-chord
  :straight t
  :after evil
  :config (key-chord-mode)
  (key-chord-define evil-motion-state-map ",s"
		    (lambda (arg)
		      (interactive "P")
		      (let ((file "~/.emacs.d/settings.el"))
			(if arg (find-file-other-window file)
			  (find-file file)))))
  (key-chord-define evil-motion-state-map "ss"
		    (lambda ()
		      (interactive)
		      (split-window-vertically)
		      (other-window 1)))
  (key-chord-define evil-motion-state-map "vv"
		    (lambda ()
		      (interactive)
		      (split-window-horizontally)
		      (other-window 1))))

(use-package request
  :straight t)

(require 'ft-evil)
(require 'ft-themes)
(require 'ft-jump)
(require 'ft-vault)
(require 'ft-system)
(require 'ft-window)
(require 'ft-json)
(require 'ft-term)
(require 'ft-eww)
(require 'ft-compile)
(require 'ft-dired)
(require 'ft-embark)
(require 'ft-search)
(require 'ft-shell-command)
(require 'ft-elfeed)
(require 'ft-buffer)
(require 'ft-eshell)
(require 'ft-emms)
(require 'ft-code)
(require 'ft-github)
(require 'ft-monorepo)
(require 'ft-misc)
(require 'ft-mail)
(require 'ft-sx)

(use-package notmuch)

