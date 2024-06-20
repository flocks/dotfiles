
(defun ft-js-create-component-folder ()
  "Create folder named after the current filename,
and move this file to this folder renamed as index.{ext}"
  (interactive)
  (let* ((name (buffer-file-name))
	 (directory-name (file-name-base name))
	 (extension (file-name-extension (buffer-file-name)))
	 (new-file-name (concat directory-name "/index." extension)))
    (unless (or (file-exists-p directory-name) (eq name "index"))
      (when (yes-or-no-p (format "create file %s ?" new-file-name))
	(make-directory directory-name)
	(rename-file name new-file-name)
	(kill-current-buffer)
	(find-file new-file-name)))))


(defun ft-jump-directory (arg)
  (interactive "P")
  (let ((default-directory (or
			    (and arg "~")
			    default-directory)))
    (find-file
     (completing-read "Dir: "
		      (split-string (shell-command-to-string "fd --type directory"))))))

(defun ft-jump-direct-directory ()
  (interactive)
  (find-file
   (completing-read "Dir: "
		    (split-string (shell-command-to-string "fd --type directory --max-depth 1")))))

(global-set-key (kbd "C-c d") 'ft-jump-directory)
(global-set-key (kbd "C-c j") 'ft-jump-direct-directory)


(defun ft-dired-insert-sub-directory ()
  (interactive)
  (dired-insert-subdir
   (completing-read "Dir: "
		    (split-string (shell-command-to-string "fd --type directory ")))))



(defun ft-eval-defun-and-print ()
  "Eval top-level form and print result"
  (interactive)
  (save-excursion
    (end-of-defun)
    (eval-print-last-sexp)))

(define-key emacs-lisp-mode-map (kbd "C-c C-p") 'ft-eval-defun-and-print)

;;    idea define sort method for dired
;;    - type
;;    - alpha
;;    - date


(setq ft-cycle-buffer-last nil)
(defun ft-cycle-or-switch-buffer (arg)
  (interactive "P")
  (if arg
      (ivy-switch-buffer)
    (if (equal ft-cycle-buffer-last 'previous)
	(progn
	  (setq ft-cycle-buffer-last 'next)
	  (previous-buffer))
      (progn
	(setq ft-cycle-buffer-last 'previous)
	(next-buffer)))))

(global-set-key (kbd "C-;") 'ft-cycle-or-switch-buffer)


(defun async-shell-to-buffer (cmd)
  (interactive "sCall command: ")
  (let ((output-buffer (generate-new-buffer (format "*async:%s*" cmd)))
        (error-buffer  (generate-new-buffer (format "*error:%s*" cmd))))
    (async-shell-command cmd output-buffer error-buffer)))

(defun ft-dired-arbitrary-command (dir command)
  (interactive "Ddirectory:\nMcommand ")
  (let* ((default-directory dir)
	(output (shell-command-to-string command)))
    (dired  (cons (format "*Dired - %s *" command) (split-string output)))))


(setq shell-command-prompt-show-cwd t)
(setq enable-recursive-minibuffers t)


(defun ft-insert-current-defun-call ()
  "Take current defun and insert a new line calling defun"
  (interactive)
  (let ((name (lisp-current-defun-name)))
	(end-of-defun)
	(insert (format "(%s )" name))
	(backward-char 1)
	(evil-insert 0)))


(defun ft-change-dir-shell-command ()
  (interactive)
  (let* ((enable-recursive-minibuffers t)
		 (file-name (file-name-nondirectory (minibuffer-contents)))
		 (new-dir (consult-dir--pick))
		 (new-full-name (concat (file-name-as-directory new-dir)
								file-name)))
	(when new-dir
	  (let ((default-directory new-full-name)
			(previous-content (minibuffer-contents)))
		(minibuffer-quit-recursive-edit)
		(call-interactively #'shell-command)))))

(define-key minibuffer-local-map (kbd "C-x C-d") 'ft-change-dir-shell-command)



(defun ft-button ()
  (interactive)
  (let ((changes '(("variant=\"primary\"". "variant=\"color\"")
				   ("variant=\"outline\"". "variant=\"color\"\noutline\n")
				   ("variant=\"unstyled\"". "")
				   ("variant=\"outline_danger\"". "variant=\"error\"\noutline\n")
				   ("square". "iconButton")
				   ("variant=\"transparent\"". "variant=\"main\"\noutline\n")
				   ("iconOnRight". "iconPosition=\"right\"")
				   ("variant=\"link\"". "variant=\"shade\""))))
	(ignore-errors
	  (goto-char (point-min))
	  (while (and (re-search-forward "<Button") (not (eobp)))
		(re-search-backward "<")
		(mark-sexp)
		(narrow-to-region (region-beginning) (region-end))
		(dolist (change changes)
		  (while (re-search-forward (car change) nil t)
			(replace-match (cdr change))))
		(widen)
		(forward-word))
	  (deactivate-mark))))

(defun ft-secpr1 ()
  (interactive)
  (let* ((default-directory "~/secp256k1")
		 (result (shell-command-to-string "ts-node src/index.ts | head -n 1 | awk '{print $3}'")))
	(message "%s" result)
	(kill-new result)))

(defun ft-dired-buttons ()
  (interactive)
  (dolist (file (dired-get-marked-files))
	(with-current-buffer (find-file-noselect file)
	  (ft-button)
	  (save-buffer))))

(let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (call-interactively #'compile))

(defun ft-search-word ()
  (interactive)
  (let* ((search (thing-at-point 'word))
		(default-directory (project-root (project-current t)))
		(compilation-buffer-name-function (lambda (_) (format "grep %s" search)))
		(compile-command (format "rg --vimgrep %s" search)))
	(call-interactively #'compile)))


(global-set-key (kbd "M-*") 'ft-search-word)


(define-key minibuffer-mode-map (kbd "C-SPC") 'set-mark-command)
