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
  (setq dired-jump-map nil)
  )

;; (use-package dired-copy-paste
;;   :straight (dired-copy-paste :type git :host github :repo "jsilve24/dired-copy-paste")
;;   :config
;;   (evil-define-key 'normal dired-mode-map "d" 'dired-copy-paste-do-cut)
;;   (evil-define-key 'normal dired-mode-map "p" 'dired-copy-paste-do-paste)
;;   (evil-define-key 'normal dired-mode-map "y" 'dired-copy-paste-do-copy))

(use-package dired-subtree
  :straight t)

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


(defun ft-dired-mark-recent (number)
  "Mark n latest update files"
  (interactive "nLast number items: ")
  (ft--ensure-dired-buffer)
  (message "GOO"))

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

(defun ft-list-of-files-in-dired (beginning end)
  "Take files listed in the region and create a dired buffer with
the list"
  (interactive "r")
  (if (use-region-p)
	  (let* ((files (split-string (buffer-substring-no-properties beginning end)))
			 (filtered-files (seq-filter (lambda (file) (file-exists-p file)) files)))
		(when filtered-files
		  (dired (cons (read-string "Buffer name: ") filtered-files))))))

(let ((map dired-mode-map))
  (evil-define-key 'normal map (kbd "C-c C-p") 'dired-toggle-read-only) ;; to be consisent with rg.el/occur/grep
  (evil-define-key 'normal map (kbd "C-c C-d d") 'ft-dired-sort-date)
  (evil-define-key 'normal map (kbd "C-c C-d i") 'ft-dired-mark-images)
  (evil-define-key 'normal map (kbd "C-c C-d c") 'ft-dired-concat-images)
  (evil-define-key 'normal map (kbd "C-c C-d t") 'ft-dired-mark-today)
  (evil-define-key 'normal map (kbd "C-c C-d I") 'ft-dired-insert-sub-directory)
  (evil-define-key 'normal map (kbd "C-c C-d m") 'dired-mark-files-regexp)
  (evil-define-key 'normal map (kbd ";y") 'ft-dired-copy-project-filename-as-kill)
  (evil-define-key 'normal map (kbd "C-c C-d r") 'ft-dired-mark-recent))


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

(defun dired-do-eshell-command (command)
  "Run an Eshell command on the marked files."
  (interactive "sEshell command: ")
  (let ((files (dired-get-marked-files t)))
    (eshell-command
     (format "%s %s" command (mapconcat #'identity files " ")))))

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

(use-package dired-narrow
  :straight t
  :bind (:map dired-mode-map
              ("C-/" . dired-narrow)))

(provide 'ft-dired)
