;; idea ffap-dwim that handle relative path in project (like C-c p g of projectile)
(defun ft--filter-buffer-list-by-mode (mode buffers)
  "Filter the BUFFERS list by MODE."
  (seq-filter
   (lambda (buff)
     (equal mode (buffer-local-value 'major-mode (get-buffer buff))))
   buffers))


;; useful to switch between multiple vterm buffer on the same project
;; C-c p xv to launch terminal then (ft-next-mode-projectile-buffer)
(defun ft-next-mode-projectile-buffer ()
  "Switch to next project buffer with same mode as current buffer."
  (interactive)
  (let ((buffers (ft--filter-buffer-list-by-mode major-mode (projectile-project-buffers))))
    (message "%s" buffers)
    (when-let ((buffer (nth 1 buffers)))
      (bury-buffer)
      (switch-to-buffer buffer))))

(global-set-key (kbd "C-SPC") 'ft-next-mode-projectile-buffer)
(global-set-key (kbd "C-s-b") 'ft-next-mode-projectile-buffer)


(use-package dumb-jump
  :ensure t
  :config
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'ivy))

(use-package ivy-xref
  :ensure t
  :config
  (setf xref-show-definitions-function #'ivy-xref-show-defs))

(global-set-key (kbd "C-=") 'ft/edit-config-file)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-;") 'counsel-switch-buffer)

(global-set-key (kbd "C-'") 'bookmark-jump)
(global-set-key (kbd "M-o") 'projectile-find-file-dwim)

;; (define-key evil-motion-state-map (kbd "C-c f") `counsel-fzf)

(provide 'ft-navigation)


;; idea
;; projectile / ivy view mixed
;; list of project ("vaultjs" . "~/ledger/vault-js")
;; C-c C-p to choose a project
;; if project doesnt have a view, close all windows, open window vterm buffer and save view
;; otherwise swwitch to ivy view of the project
;; if project dosn't have a view but current buffer is part of project, just save the view
;; before switching to other view, save current view if needed

;; (let* ((projects
;;        '(("vaultjs" . "~/ledger/vault-js")
;; 	 ("vfront" . "~/ledger/ledger-vault-front")
;; 	 ("inte" . "~/.vault/vault-integration")
;; 	 ("gate" . "~/ledger/ledger-vault-api")))
;;        (names (mapc (lambda (x) (car x)) projects)))
;;   (completing-read "Projects: " names))


;; workviews-frequency-save-config: dwim|always|never
;; `always' save view config when switching to another view
;; `never'  just never save the view config when switching config, but
;;  allow   to save manually by switching to the same view 
;; `dwim'   save only when there are still at least one buffer belonging to the project unless `C-u' prefix argument


(defcustom workviews-projects 
  '((vjs   . "~/ledger/vault-js")
    (front . "~/ledger/ledger-vault-front")
    (gate .  "~/ledger/ledger-vault-api")
    (int .   "~/.vault/vault-integration"))
  "List of projects.

Stored as a `alist' where the key is the name of the project and the value is the path 
to that project. The key will be used to fill the `completing-read' prompt"
  :type '(alist :key-type (symbol :tag "Key")
                :value-type (string :tag "Value"))
  :group 'workviews)


(defvar workviews-current-view nil)


(defun workviews-switch-project ()
  (interactive)
  (let ((view-name (completing-read "Projects: " (workviews--get-projects-name))))
    (when (member view-name (workviews--get-projects-name))
      ;; (workviews--save)
      (setq workviews-current-view view-name)
      ;; TODO check also if view-config contain a buffer belonging to the project
      ;; --------> either open vterm of that project OR may be open a currently opened buffer from that project? 
      ;; TODO when current buffer is belonging to one the project, and no view is saved for that project
      ;; TODO save current config to
      (if (workviews--view-created-p view-name)
	  (ivy--switch-buffer-action view-name)
	(workview--create-project-view view-name)))))

(global-set-key (kbd "C-c C-p") 'workviews-switch-project)


(defun workview--get-view-config (view)
  (seq-find (lambda (v) (string-equal view (car v))) ivy-views))




(defun workviews--get-projects-name ()
  (mapcar #'car workviews-projects))

(defun workviews--view-created-p (view)
  "Whether VIEW exists or not."
  (seq-find (lambda (x) (string-equal (car x) view)) ivy-views))


(defun workviews--save ()
  "Save current view config into `workviews-current-view'"
  (let ((x (assoc workviews-current-view ivy-views))
	(view (ivy--get-view-config)))
    (if x
        (setcdr x (list view))
      (push (list workviews-current-view view) ivy-views))))


(defun workviews--get-project-path (project)
  "Find the project path in `workviews-projects'"
  (alist-get (intern project) workviews-projects))


(defun workview--create-project-view (view)
  "Create a new ivy view named VIEW with a project vterm buffer."
  (with-current-buffer (dired (workviews--get-project-path view))
    (projectile-run-vterm)
    (delete-other-windows)
    (push (list view (ivy--get-view-config)) ivy-views)))

(defun workview--create-empty-view (view)
  "Create a new ivy view named VIEW with just the scratch buffer."
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (push (list view (ivy--get-view-config)) ivy-views))

