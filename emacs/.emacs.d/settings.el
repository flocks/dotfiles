;; optimization
(require 'package)
;; we use straight
; (setq package-enable-at-startup nil)
(setq gc-cons-threshold (* 1024 1024 100))
(add-to-list 'load-path
             (expand-file-name (concat user-emacs-directory "ft")))

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/lisp")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(fido-vertical-mode 1)
(setq max-mini-window-height 5)

(indent-tabs-mode -1)

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

;;; I like to version control emacs bookmarks because I use different machines
;;; but I want to keep some privacy (espacially for mail/web/gemini bookmarks)
(setq bookmark-file (expand-file-name "~/dotfiles/emacs/bookmarks.gpg"))

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

;; (setq epa-pinentry-mode 'loopback)

(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(use-package yasnippet
  :straight t
  :config (yas-global-mode))

;;; org mode
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
;;(evil-define-key 'motion org-mode-map (kbd "RET") 'org-open-at-point)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C") 'org-capture)
;; 

;; auto insert closing parenthesis/bracket/quote..etc..
(electric-pair-mode 1)


;; this put clipboard into kill ring
(setq save-interprogram-paste-before-kill t)

(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-backends '((company-capf company-dabbrev-code)))
  (global-set-key (kbd "C-M-i") 'company-complete))

(require 'ft-evil)
(require 'ft-themes)
(require 'ft-jump)
;; (require 'ft-vault)
(require 'ft-system)
(require 'ft-window)
(require 'ft-json)
(require 'ft-eww)
(require 'ft-compile)
(require 'ft-dired)
(require 'ft-search)
(require 'ft-shell-command)
(require 'ft-elfeed)
(require 'ft-buffer)
(require 'ft-code)
(require 'ft-misc)
(require 'ft-mail)
(require 'ft-social)
;; (require 'ft-view-diff)
;; (require 'ft-jira)

(use-package notmuch)

