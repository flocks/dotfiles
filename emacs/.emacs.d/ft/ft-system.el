;;; Code:


(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package daemons
   :straight t)

(use-package disk-usage
   :straight t)

(use-package proced
   :straight t)

;; kubernetes
(use-package kubel
  :straight t
  :config
  (global-set-key (kbd "C-c k") 'kubel))

(use-package kubel-evil
  :straight t)

(use-package bluetooth
  :straight t)

(use-package xinput
  :straight (xinput :type git :host github :repo "flocks/xinput")
  :config
  (let ((map xinput-mode-map))
	(evil-define-key 'motion map (kbd "RET") #'xinput-view-device-props))
  (let ((map xinput-props-mode-map))
	(evil-define-key 'motion map (kbd "RET") #'xinput-set-prop))
  )

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(require 'tramp)
(defun sudo-edit-current-file ()
  (interactive)
  (let ((position (point)))
    (find-alternate-file
     (if (file-remote-p (buffer-file-name))
         (let ((vec (tramp-dissect-file-name (buffer-file-name))))
           (tramp-make-tramp-file-name
            "sudo"
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            (tramp-file-name-localname vec)))
       (concat "/sudo:root@localhost:" (buffer-file-name))))
    (goto-char position)))

(provide 'ft-system)
;;; ft-system ends here
