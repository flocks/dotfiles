(load-file (concat user-emacs-directory "settings.el"))

(setq custom-file "~/dotfiles/emacs/.emacs.d/custom.el")
(load custom-file :noerror)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
