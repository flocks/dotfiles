;; find a way to perform rg on list of file, list of marked files

(use-package rg
  :straight t
  :config
  (rg-enable-menu)
  (rg-define-toggle "--context 3" (kbd "C-c c"))
  (setq rg-group-result nil) ;; much faster with large result
  )

(defun ft-run-occur (arg)
  "Run occur, with pre-selected term if no prefix arg"
  (interactive "P")
  (if arg
	  (call-interactively #'occur)
	(occur (thing-at-point 'word))))

(define-key prog-mode-map (kbd "C-c o") 'ft-run-occur)

(eval-after-load 'ft-dired
  (progn 
	(defun ft-my-consult-ripgrep ()
	  (interactive)
	  (if (and (eq major-mode 'dired-mode)
			   (ft-dired-is-mark-active))
		  (let* ((files (string-join (dired-get-marked-files) " "))
				 (consult-ripgrep-args
				  (format "%s %s"
						  (string-join (butlast (split-string consult-ripgrep-args)) " ")
						  files)))
			(call-interactively #'consult-ripgrep)) 
		(call-interactively #'consult-ripgrep)))

	(evil-define-key '(motion normal) global-map (kbd "C-f") 'ft-my-consult-ripgrep)))

(provide 'ft-search)
