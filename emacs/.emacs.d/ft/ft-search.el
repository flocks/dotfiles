(defun ft-search-thing (beg end)
  "Use rg to search for selected thing in root project

When simple prefix-arg passed it searches in current folder When double
prefix-arg passed it searches only in current file"

  (interactive "r")
  (let* ((search-term (if (use-region-p)
                          (buffer-substring-no-properties beg end)
                        (or (thing-at-point 'word t) (read-string "Search for: "))))
         (default-directory (if current-prefix-arg
                                default-directory
                              (locate-dominating-file "." ".git")))
         (file (or (and (equal current-prefix-arg '(16)) buffer-file-name) ""))
         (command (format "rg --vimgrep '%s' %s" search-term file)))
		(when search-term
			(compile command))))

(evil-global-set-key 'motion (kbd "C-c f") 'ft-search-thing)


(defun ft-grep-to-dired ()
  "Inside a consult--grep buffer, takes all files and produce a
dired buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 4)
    (let ((files))
      (while (not (eobp))
        (when-let* ((file-col-row (thing-at-point 'filename t))
										(file (car (split-string file-col-row ":"))))
					(when (file-exists-p file)
						(push file files)))
        (forward-line 1))
      (dired (cons "Grep" (cl-remove-duplicates files :test 'string=))))))


(define-key compilation-mode-map (kbd "C-c d") 'ft-grep-to-dired)
(evil-define-key 'normal grep-mode-map (kbd "g r") 'embark-rerun-collect-or-export)


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
