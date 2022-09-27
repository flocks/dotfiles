
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

(use-package winner
   :straight t
   :config
    (winner-mode)
    (global-set-key (kbd "s-p") 'winner-undo)
    (global-set-key (kbd "s-n") 'winner-redo))

(use-package hydra
  :straight t)

(use-package buffer-move
  :straight t)

(defhydra hydra-windows (global-map "C-c w")
  "window"

  ("J" evil-window-increase-height)
  ("K" evil-window-decrease-height)
  ("L" evil-window-increase-width)
  ("H" evil-window-decrease-width)

  ("j" buf-move-down)
  ("k" buf-move-up)
  ("l" buf-move-right)
  ("h" buf-move-left))

(use-package winner
   :straight t
   :config
    (winner-mode)
    (global-set-key (kbd "s-p") 'winner-undo)
    (global-set-key (kbd "s-n") 'winner-redo))


(defun ft-i3-move-down ()
  (condition-case nil
      (windmove-down)
    ((debug error)
     (shell-command "i3-msg focus down"))))

(defun ft-i3-move-up ()
  (condition-case nil
      (windmove-up)
    ((debug error)
     (shell-command "i3-msg focus up"))))

(defun ft-i3-move-right ()
  (condition-case nil
      (windmove-right)
    ((debug error)
     (shell-command "i3-msg focus right"))))

(defun ft-i3-move-left ()
  (condition-case nil
      (windmove-left)
    ((debug error)
     (shell-command "i3-msg focus left"))))


(global-set-key (kbd "s-J") 'evil-window-move-very-bottom)
(global-set-key (kbd "s-K") 'evil-window-move-very-top)

(global-set-key (kbd "s-C-h") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-j") 'enlarge-window)
(global-set-key (kbd "s-C-k") 'shrink-window)
(global-set-key (kbd "s-C-l") 'shrink-window-horizontally)
(global-set-key (kbd "C-x RET") 'tear-off-window)

(provide 'ft-window)
