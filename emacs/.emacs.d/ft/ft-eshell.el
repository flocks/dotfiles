(require 'eshell)
(require 'em-cmpl)

(use-package pcmpl-args
  :straight t)

(use-package eshell-git-prompt
  :straight t)

(use-package eshell
  :config
  (defun ft/eshell-previous-and-insert ()
	(interactive)
	(evil-insert-state)
	(eshell-previous-matching-input-from-input ""))

  (defun ft/eshell-prompt-history ()
	(interactive)
	(let ((history (ring-elements eshell-history-ring))
		  (content (eshell-get-old-input)))
	  (when-let ((new-content (completing-read "Command: " history nil nil content)))
		(eshell-kill-input)
		(insert new-content))))

  (let ((map eshell-mode-map))

	(evil-define-key 'insert map
	  (kbd "C-p") 'eshell-previous-matching-input-from-input)
	(evil-define-key 'normal map
	  (kbd "C-p") 'ft/eshell-previous-and-insert)
	(evil-define-key 'insert map
	  (kbd "C-a") 'eshell-bol)
	(evil-define-key 'insert map
	  (kbd "C-k") 'evil-collection-eshell-evil-delete-line)
	(evil-define-key 'insert map
	  (kbd "C-r") 'ft/eshell-prompt-history)
	)

  
  (setq
   eshell-scroll-to-bottom-on-input t
   eshell-hist-ignoredups t)

  (eshell-git-prompt-use-theme 'git-radar))

(defmacro prot-eshell-ffap (name doc &rest body)
  "Make `find-file-at-point' commands for Eshell.
NAME is how the function is called.  DOC is the function's
documentation string.  BODY is the set of arguments passed to the
`if' statement to be evaluated when a file at point is present."
  `(defun ,name ()
     ,doc
     (interactive)
     (if-let ((file (ffap-file-at-point)))
         ,@body
       (user-error "No file at point"))))



(defun ft-export-eshell-output ()
  (interactive)
  (let ((output (ft-eshell--command-output)))
	(with-current-buffer (get-buffer-create "*Eshell Output*")
	  (erase-buffer)
	  (goto-char (point-max))
	  (newline)
	  (insert output)
	  (switch-to-buffer-other-window (current-buffer)))))

(defun ft-export-eshell-output-compilation ()
  (interactive)
  (let ((output (ft-eshell--command-output)))
	(with-current-buffer (get-buffer-create "*Eshell Compilation Output*")
	  (let ((inhibit-read-only t))
		(erase-buffer)
		(compilation-mode)
		(goto-char (point-min))
		(insert output))
	  (switch-to-buffer (current-buffer)))))

(defun ft-yank-eshell-output ()
  (interactive)
  (let ((output (ft-eshell--command-output)))
	(kill-new output)
	(message "Output copied.")))

(defun ft-select-eshell-output ()
  (interactive)
  (goto-char (eshell-beginning-of-output))
  (set-mark (point))
  (goto-char (eshell-end-of-output)))

(prot-eshell-ffap
 ft-eshell-ffap-find-file
 "Run `find-file' for file at point (ordinary file or dir).
Recall that this will produce a `dired' buffer if the file is a
directory."
 (find-file file))

(defun ft-eshell-root-dir ()
  "Switch to the root directory of the present project."
  (interactive)
  (if-let ((root (or (vc-root-dir) (locate-dominating-file "." ".git"))))
      (prot-eshell--cd root)
    (user-error "Cannot find a project root here")))

(prot-eshell-ffap
 ft-eshell-ffap-dired-jump
 "Run `find-file' for file at point (ordinary file or dir).
Recall that this will produce a `dired' buffer if the file is a
directory."
 (dired (file-name-directory file)))



;; Copied on 2022-01-04 10:32 +0200 from Sean Whitton's `spw/eshell-cd'.
;; I had to change the symbol to use the prot-eshell prefix for lexical
;; binding.  Sean's dotfiles: <https://git.spwhitton.name/dotfiles>.
(defun prot-eshell--cd (dir)
  "Routine to cd into DIR."
  (delete-region eshell-last-output-end (point-max))
  (when (> eshell-last-output-end (point))
    (goto-char eshell-last-output-end))
  (insert-and-inherit "cd " (eshell-quote-argument dir))
  (eshell-send-input))

(prot-eshell-ffap
 ft-eshell-ffap-insert
 "Insert (cat) contents of file at point."
 (progn
   (goto-char (point-max))
   (insert (format "cat %s" file))
   (eshell-send-input)))

(defun ft-eshell--command-output ()
  "Capture last output."
  (let ((beg (save-excursion
               (goto-char (eshell-beginning-of-output))
               (goto-char (point-at-bol)))))
  (when (derived-mode-p 'eshell-mode)
    (buffer-substring-no-properties beg (eshell-end-of-output)))))

(defun ft-eshell-launcher ()
  (interactive)
  (if (project-current)
	  (call-interactively 'project-eshell)
	(call-interactively 'eshell)))

(global-set-key (kbd "M-`") 'ft-eshell-launcher)
(global-set-key (kbd "M-~") 'vterm)

(let ((map eshell-mode-map))
  (evil-collection-define-key 'normal 'map (kbd "C-p") nil)
  (define-key map (kbd "C-c Y") 'ft-yank-eshell-output)
  (define-key map (kbd "C-c A") 'ft-select-eshell-output)
  (define-key map (kbd "C-c C") 'ft-export-eshell-output-compilation)
  (define-key map (kbd "C-c C-f") 'ft-eshell-ffap-find-file)
  (define-key map (kbd "C-c C-j") #'ft-eshell-ffap-dired-jump)
  (define-key map (kbd "C-c C-r") #'ft-eshell-root-dir)
  (define-key map (kbd "C-c O") 'ft-export-eshell-output))

(let ((map eshell-cmpl-mode-map))
  (define-key map (kbd "C-c C-i") #'ft-eshell-ffap-insert))

(provide 'ft-eshell);

