(require 'dired)

(use-package dired
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-al --group-directories-first")
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (require 'dired-x)
  (evil-define-key 'normal dired-mode-map (kbd "M-%") 'dired-do-query-replace-regexp)
  ;; I don't want repeat mode for dired-jump
  (setq dired-jump-map nil))

(use-package dired-subtree
  :straight t)

(defun ft-dired-do-compile-command (command &optional arg file-list)
  "Same as dired-do-async-shell-command but with compilation-mode"
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  
  (let ((compile-command (format "%s %s" command (s-join " " file-list)))
		(compilation-read-command nil))
	(call-interactively #'compile)))


(defun ft--ensure-dired-buffer ()
  (unless (eq major-mode 'dired-mode)
    (error "Not in a dired buffer")))

(defun ft-dired-mark-images (arg)
  "Mark all images in current dired buffer.
With a prefix argument, prompt for the regex."

  (interactive "P")
  (let ((regex-image
	 (or (and arg (read-regexp "Regex for images"))
	     ".png\\|.jpg\\|.svg")))
    (ft--ensure-dired-buffer)
    (dired-mark-files-regexp regex-image)))
    ;; (dired-toggle-marks)
    ;; (dired-do-kill-lines)))


(defun ft-dired-mark-today ()
  "Filter files created today, with a prefix argument change
the preset date (1hour/1week/1month)"
  (interactive)
  (let ((time-today (format-time-string "%b %d" (time-stamp))))
    (dired-mark-sexp '(string-match-p time-today time))))

(defun ft-dired-sort-date ()
  (interactive)
  (dired-sort-other "-alt")
  (goto-char (point-min)))

(defun ft-dired-insert-sub-directory ()
  (interactive)
  (dired-insert-subdir
   (completing-read "Dir: "
		    (split-string (shell-command-to-string "fd --type directory")))))


(defun ft-gen-id ()
  (substring (sha1 (format "%s" (float-time))) 0 5))

(defun ft-list-of-files-in-dired (beginning end)
  "Take files listed in the region and create a dired buffer with
the list"
  (interactive "r")
  (if (use-region-p)
	  (let* ((files (split-string (buffer-substring-no-properties beginning end)))
			 (filtered-files (seq-filter (lambda (file) (file-exists-p file)) files)))
		(when filtered-files
		  (let ((buff-name (concat "*Dired-find* " (ft-gen-id))))
			(dired (cons buff-name filtered-files)))))))

(let ((map dired-mode-map))
  (evil-define-key 'normal map (kbd "C-c C-d d") 'ft-dired-sort-date)
  (evil-define-key 'normal map (kbd "C-c C-d i") 'ft-dired-mark-images)
  (evil-define-key 'normal map (kbd "C-c C-d c") 'ft-dired-concat-images)
  (evil-define-key 'normal map (kbd "C-c C-d t") 'ft-dired-mark-today)
  (evil-define-key 'normal map (kbd "C-c C-d I") 'ft-dired-insert-sub-directory)
  (evil-define-key 'normal map (kbd "C-c C-d m") 'dired-mark-files-regexp)
  (evil-define-key 'normal map (kbd "&") 'ft-dired-do-compile-command)
  (evil-define-key 'normal map (kbd ";y") 'ft-dired-copy-project-filename-as-kill))

(defun ft-clone-dired-buffer ()
  "Build a new independant buffer with files present in current dired buffer.
So we can apply complex filter/mark without risking loosing it by revisiting
a dired buffer on the same directory"
  (interactive)
  (unless (equal major-mode 'dired-mode)
	(user-error "Not a dired buffer"))
  (let ((name (format "free dired: %s" default-directory)))
	(dired
	 (cons (format "%s/NEWD" default-directory)
		   (directory-files default-directory)))
	(rename-buffer name)))

(define-key dired-mode-map (kbd "C-x D") 'ft-clone-dired-buffer)
(global-set-key (kbd "C-x d") 'find-dired)

(defun ft-dired-is-mark-active ()
  "dired `dired-get-marked-files' returns file under cursor even
if it is not explicitly marked.

This function checks if at leat 1 mark has been explicitly set."

  ;; if `dired-get-marked-files' length is > 1 we are sure mark is
  ;; active, otherwise we need to check if the buffer contains a line
  ;; beginning with `dired-marker-char'
  (or (> (length (dired-get-marked-files)) 1)
      (save-excursion
	(goto-char (point-min))
	(while (and
		(not (looking-at-p (char-to-string dired-marker-char)))
		(not (eobp)))
	  (forward-line 1))
	(not (= (point) (point-max))))))

(defun ft-dired-concat-images (arg)
  "Use imagemagick to concat marked images.

With a prefix ARG, the command can be edited"
  (interactive "P")
  (let ((files (dired-get-marked-files t)))
	(unless (> (length files) 1)
	  (user-error "needs at least 2 images"))
	(let* ((output (read-string "Name: "))
		   (files-input (string-join (mapcar (lambda (x) (format "\"%s\"" x)) files) " "))
		   (default-command
			(format "convert -background white -border 100 -bordercolor white -append %s %s"
					files-input output))
		   (command (or (and arg (read-string "Command: " default-command))
						default-command)))
	  (shell-command command)
	  (revert-buffer)
	  (dired-jump nil output))))


(defun ft-dired-copy-project-filename-as-kill ()
  (interactive)
  (let* ((files (dired-get-marked-files))
		 (project-path (file-truename (locate-dominating-file default-directory ".git")))
		 (result (mapconcat
				  (lambda (file) (file-relative-name file project-path)) files " ")))
    (message "%s" result)
	(kill-new result)))

(defun ft-dired-last ()
  (interactive)
  (revert-buffer)
  (goto-char (point-max))
  (forward-line -1))


(defun ft-build-dired ()
  "Build a dired buffer with marked files.
We can reuse a dired buffer and it will append the marked files."
  (interactive)
  (unless (equal major-mode 'dired-mode)
	(user-error "Not a dired buffer"))
  (let ((files (dired-get-marked-files t))
		(buffer-name (read-buffer "Buffer name: " nil nil
								  (lambda (b)
									(with-current-buffer (car b)
									  (equal major-mode 'dired-mode))))))
	(if (get-buffer buffer-name)
		(let ((existing-files (with-current-buffer buffer-name (dired-utils-get-all-files))))
		  (kill-buffer buffer-name)
		  (dired (cons buffer-name (append existing-files files))))
	  (dired (cons buffer-name files)))))

(use-package dired-narrow
  :straight t
  :bind (:map dired-mode-map
              ("C-/" . dired-narrow)))

(provide 'ft-dired)
