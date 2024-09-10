
(defun ft-my-split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun ft-my-split-window-horizontally ()
  (interactive)
  (split-window-horizontally )
  (other-window 1))

(global-set-key "\C-x2" #'ft-my-split-window-vertically)
(global-set-key "\C-x3" #'ft-my-split-window-horizontally)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
;; same as C-x 4 4
(global-set-key (kbd "M-4") 'other-window-prefix)


(use-package hydra
  :straight t)

(use-package buffer-move
  :straight t
  :config
  (defhydra hydra-windows (global-map "C-c w")
	"window"

	("J" evil-window-increase-height)
	("K" evil-window-decrease-height)
	("L" evil-window-increase-width)
	("H" evil-window-decrease-width)

	("j" buf-move-down)
	("k" buf-move-up)
	("l" buf-move-right)
	("h" buf-move-left)))


(use-package winner
   :straight t
   :config
    (winner-mode)
    (global-set-key (kbd "s-p") 'winner-undo)
    (global-set-key (kbd "s-n") 'winner-redo))

(global-set-key (kbd "C-x RET") 'tear-off-window)

(provide 'ft-window)
