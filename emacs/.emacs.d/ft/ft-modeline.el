
(defface ft-modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "")

(defun ft-modeline--buffer-name ()
  "Return `buffer-name' with spaces around it."
  (format " %s " (buffer-name)))

(defvar-local ft-modeline-buffer-name
	'(:eval
	  (if (and (buffer-file-name) (buffer-modified-p (current-buffer)))
		  (propertize (ft-modeline--buffer-name) 'face 'ft-modeline-indicator-red)
		(propertize (ft-modeline--buffer-name) 'face 'bold)))

  "Mode line construct to display the buffer name.")

(put 'ft-modeline-buffer-name 'risky-local-variable t)

(defun ft-modeline--mode ()
  "Return `major-mode' with spaces around it."
  (format " %s " major-mode))

(defvar-local ft-modeline-mode
    '(:eval
      (when (and (mode-line-window-selected-p) (buffer-file-name))
        (propertize (ft-modeline--mode) 'face 'italic)))

  "Mode line construct to display the major mode.")

(put 'ft-modeline-mode 'risky-local-variable t)

(defvar-local ft-modeline-misc
    '(:eval
      (when (mode-line-window-selected-p)
        (list erc-modified-channels-object notmuch-indicator-string dashub-alert-mode-line)))

  "Mode line construct to display the major mode.")

(put 'ft-modeline-misc 'risky-local-variable t)

(setq-default mode-line-format
			  '("%e" ft-modeline-buffer-name " " ft-modeline-mode " " ft-modeline-misc))

(provide 'ft-modeline)
