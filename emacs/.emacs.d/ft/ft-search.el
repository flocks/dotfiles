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



(defun ft-grep-to-dired ()
  "Inside a consult--grep buffer, takes all files and produce a
dired buffer"
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(let ((files))
	  (while (not (eobp))
		(when (thing-at-point 'filename)
		  (when-let (file (get-text-property (point) 'consult--grep-file))
			(push file files)))
		(forward-line 1))
	  (dired (cons "Grep" (cl-remove-duplicates files :test 'string=))))))

(define-key grep-mode-map (kbd "C-c d") 'ft-grep-to-dired)

(provide 'ft-search)
