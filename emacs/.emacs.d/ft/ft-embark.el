(recentf-mode 1)
(use-package consult
  :straight t
  :config
  (global-set-key (kbd "C-;") 'consult-buffer)
  (setq consult-async-min-input 2)
  (setq consult-find-args
		"find . -not ( -wholename */.* -prune ) -not ( -wholename *node_modules* -prune )")

  (define-key evil-normal-state-map (kbd "C-p") 'consult-find))

(use-package vertico
  :straight t
  :config
  (vertico-mode)
  (define-key vertico-map (kbd "M-r") 'consult-history)
  (define-key minibuffer-mode-map (kbd "M-r") 'consult-history)
  (define-key minibuffer-local-shell-command-map (kbd "M-r") 'consult-history))


(use-package embark
  :straight t
  :bind
  (("C-=" . embark-act)      
   ("C-c C-o" . embark-export))       

  :init
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;                nil
  ;;                (window-parameters (mode-line-format . none))))
)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult-dir
  :straight t
  :bind (:map vertico-map
         ("C-;" . consult-dir)
         ("C-x C-f" . consult-dir-jump-file)
		 )
  :config
  (setq consult-dir-shadow-filenames nil))

(global-set-key (kbd "C-s") 'consult-line)

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)


(use-package monobuffer
  :straight (monobuffer :type git :host github :repo "flocks/monobuffer.el")
  :config
  (define-key vertico-map (kbd "M-u") 'monobuffer)
  (define-key vertico-map (kbd "M-U") 'monobuffer-root)
  )


(provide 'ft-embark)
