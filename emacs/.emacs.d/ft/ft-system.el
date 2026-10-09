;;; Code:

(use-package proced
  :straight t
  :config
  (setq proced-enable-color-flag t)
  (setq proced-goal-attribute nil))

(use-package nginx-mode
  :straight t)

(defun ft-sudo-this-file ()
  (interactive)
  (let* ((file (if (derived-mode-p 'dired-mode)
				   default-directory
				 (buffer-file-name)))
		 (file (and file (expand-file-name file))))
	(when file
	  (find-file (format "/sudo::%s" file)))))


(provide 'ft-system)
;;; ft-system ends here
