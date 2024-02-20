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

(defun ft--sanitize-grep-filename (filename)
  "Just clear filename from `:line:col' that is in the grep buffer"
  (car (split-string filename ":")))

(defun ft-grep-to-dired ()
  "Inside a consult--grep buffer, takes all files and produce a
dired buffer"
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(forward-line 2)
	(let ((files))
	  (while (not (eobp))
		(when-let (file (thing-at-point 'filename))
		  (push (ft--sanitize-grep-filename file) files))
		(forward-line 1))
	  (dired (cons "Grep" (cl-remove-duplicates files :test 'string=))))))

(defun rgl (dir regexp)
  (interactive "DFind-grep (directory): \nsFind-grep (grep regexp): ")
  (let ((default-directory dir))
	(dired (cons (format "*rgl* - %s" regexp) (process-lines "rg" "-l" regexp)))))

(define-key grep-mode-map (kbd "C-c d") 'ft-grep-to-dired)
(evil-define-key 'normal grep-mode-map (kbd "g r") 'embark-rerun-collect-or-export)

(provide 'ft-search)
