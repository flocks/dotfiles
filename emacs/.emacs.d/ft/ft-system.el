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
  (let ((file (buffer-file-name (current-buffer))))
	(find-file (format "/sudo::%s" file))))

(provide 'ft-system)
;;; ft-system ends here
