;;; Code:


(use-package docker
  :after (transient)
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

(provide 'ft-system)
;;; ft-system ends here
