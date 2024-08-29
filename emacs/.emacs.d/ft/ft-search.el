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


(define-key grep-mode-map (kbd "C-c d") 'ft-grep-to-dired)
(evil-define-key 'normal grep-mode-map (kbd "g r") 'embark-rerun-collect-or-export)

(provide 'ft-search)
