
(use-package eww
  :config
  (setq eww-history-limit 100)
  (setq browse-url-browser-function 'eww-browse-url)
  (setq shr-use-fonts nil)
  (setq shr-width 80)
  (setq shr-max-image-proportion 0.5)
  (setq eww-auto-rename-buffer 'url)

  (defun ft-eww--ensure-eww-buffer ()
	(unless (eq major-mode 'eww-mode)
	  (user-error "not in eww buffer")))

  (defun ft-eww--extact-all-links ()
	"Extract all link from current eww buffer."
	(interactive)
	(ft-eww--ensure-eww-buffer)
	(save-excursion
	  (goto-char (point-min))
	  (let ((links nil)
			(match (text-property-search-forward 'shr-url)))
		(while (and match (not (eobp)))
		  (setq match (text-property-search-forward 'shr-url))
		  (when (and match (stringp (prop-match-value match)))
			(push (format "%s @ %s"
						  (propertize (buffer-substring-no-properties (prop-match-beginning match) (prop-match-end match)) 'face 'bold)
						  (propertize (prop-match-value match) 'face 'link))
				  links)))
		links)))

  (defun ft-eww-jump ()
	"Jump to links"
	(interactive)
	(let* ((links (reverse (ft-eww--extact-all-links)))
		   (selection (completing-read "Links: " links)))
	  (goto-char (point-min))
	  (search-forward (s-trim (car (split-string selection "@"))))
	  (backward-word)
	  (call-interactively 'eww-follow-link)))

  (defun ft-eww-prompt (arg)
	(interactive "P")
	(let* ((history eww-prompt-history)
		   (pages (cl-remove-duplicates (append history nil) :test (lambda (x y) (equal x y))))
		   (initial-prompt (plist-get eww-data :url)))
	  (eww (completing-read "EWW: " pages nil nil initial-prompt) arg)))

  (global-set-key (kbd "C-\\") 'ft-eww-prompt)
  (evil-collection-define-key  'normal 'eww-mode-map (kbd "o") 'ft-eww-prompt)
  (evil-collection-define-key 'normal 'eww-mode-map "J" 'ft-eww-jump)
  (evil-collection-define-key 'normal 'eww-mode-map "gm" 'ft-eww-jump))

(use-package ewaser
  :straight (ewaser :type git :host github :repo "flocks/ewaser")
  :after (eww)
  :config
  (setq ewaser-rules-alist '(("\\**stackoverflow" .
							  ("#left-sidebar"
							   "#sidebar"
							   "#footer"
							   ".s-topbar--container"
							   ".post-taglist"
							   ".answers-subheader"
							   ".js-post-menu"
							   ".votecell"
							   ))))
  (add-hook 'eww-after-render-hook #'ewaser--hook))

(use-package shr-tag-pre-highlight
  :straight t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

;;; add ability to bookmark eww page into global bookmark
(defun ft-eww-bookmark-make-record ()
  "Make a bookmark record for the current eww buffer"
  `(,(plist-get eww-data :title)
    ((location . ,(eww-current-url))
     (handler . ft-eww-bookmark-handler)
     (defaults . (,(plist-get eww-data :title))))))

;; This function simply tells Emacs to use the custom function when using the
;; bookmarking system.
(defun ft-eww-set-bookmark-handler ()
  "This tells Emacs which function to use to create bookmarks."
  (interactive)
  (set (make-local-variable 'bookmark-make-record-function)
       #'ft-eww-bookmark-make-record))

(defun ft-eww-bookmark-handler (record)
  "Jump to an eww bookmarked location using EWW."
  (eww (bookmark-prop-get record 'location)))

;; Finally, add a hook to set the make-record-function
(add-hook 'eww-mode-hook 'ft-eww-set-bookmark-handler)

;;;;;;;;;;;;;;;;;;;;

(provide 'ft-eww)
