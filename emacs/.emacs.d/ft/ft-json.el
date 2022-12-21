(use-package json-mode
  :straight t)

(defun ft-json-view (start end)
  "Format json inside selection into a another buffer"
  (interactive "r")
  (unless (region-active-p)
	(user-error "Need to select the json"))
  (let ((json (buffer-substring-no-properties start end))
	(buff (get-buffer-create (format "*json* - %s" (format-time-string "%s")))))
    (switch-to-buffer-other-window buff)
    (json-ts-mode)
    (insert json)
	(evil-join-whitespace (point-min) (point-max))
    (json-pretty-print-buffer)
	(goto-char (point-min))))

(global-set-key (kbd "C-c C-j") 'ft-json-view)


(use-package jjumper
  :straight (jjumper :type git :host github :repo "flocks/jjumper")
  :after (json)
  :config
  (define-key json-mode-map (kbd "C-c C-j") 'jjumper-jump-key)
  (define-key json-ts-mode-map (kbd "C-c C-j") 'jjumper-jump-key)

  (defun ft-insert-translation-key ()
	(interactive)
	(let ((key (with-current-buffer
				   (find-file-noselect "~/ledger/ledger-vault-front/locales/en/en.json")
				 (jjumper--prompt (jjumper--get-json-in-buffer)))))
	  (insert (format "\"%s\"" key)))))

(provide 'ft-json)




