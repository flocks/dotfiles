(setq shell-command-prompt-show-cwd t)
(setq enable-recursive-minibuffers t)

(use-package shelldon
  :straight (shelldon :type git
                      :host github
                      :repo "Overdr0ne/shelldon"
                      :branch "master"
                      :files ("shelldon.el"))
  :config
  (global-set-key (kbd "M-&") 'shelldon)
  (define-key shelldon-mode-map (kbd "C-c C-c") 'compilation-minor-mode)
  (define-key shelldon-minibuffer-local-command-map (kbd "C-;") 'shelldon-cd))


(provide 'ft-shell-command)

