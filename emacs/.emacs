(load-file (concat user-emacs-directory "settings.el"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)
