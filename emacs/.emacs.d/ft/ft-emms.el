(use-package emms
    :config 
    (emms-player-mpd-connect)
    (add-to-list 'emms-player-list 'emms-player-mpd)
	(setq emms-track-description-function
		  (lambda(track)
			(let ((file-name (alist-get 'name track)))
			  (file-name-base file-name))))

    (setq emms-player-mpd-music-directory "/home/flocks/music"))

(define-minor-mode emms-dired-mode
  "Emms dired mode"
  :keymap (make-sparse-keymap))

(evil-define-key 'normal 'emms-dired-mode (kbd "RET") 'emms-add-dired)

(defun ft-emms-setup ()
  "Close all window and set up 1 window with emms, and another with
music folder"
  (interactive)
  (delete-other-windows)
  (emms)
  (find-file-other-window "~/music")
  (emms-dired-mode))

(global-set-key (kbd "C-c M") 'ft-emms-setup)

(provide 'ft-emms)
