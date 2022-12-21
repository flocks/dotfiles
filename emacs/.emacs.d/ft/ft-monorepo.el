(require 'consult-dir)
(defun ft-monorepo--get-root-directory ()
  (locate-dominating-file (or (buffer-file-name) default-directory) ".git"))


(defvar ft-monorepo-alist '(("~/ledger/vault-ts/" .
							 ("~/ledger/vault-ts/apps/cli/"
							  "~/ledger/vault-ts/packages/common"
							  "~/ledger/vault-ts/packages/ui"
							  "~/ledger/vault-ts/packages/tsconfig"
							  "~/ledger/vault-ts/packages/live-common-stub"
							  "~/ledger/vault-ts/packages/eslint-config-custom"
							  "~/ledger/vault-ts/apps/poc/"
							  ))))


(defun ft-get-subpackage ()
  (assoc (ft-monorepo--get-root-directory) ft-monorepo-alist))

(defvar consult--source-custom-files
  `(:name     "Custom"
    :narrow   ?c
    :category file
    :face     consult-file
 	:action   ,#'consult--file-action
    :history  file-name-history
    :items (lambda () (ft-get-subpackage)))
  "Recent file candidate source for `consult-buffer'.")

(defvar consult-dir--source-custom-files
  `(:name     "Custom"
    :narrow   ?c
    :category file
    :face     consult-file
    :history  file-name-history
    :items ,(lambda ()
			  (ft-get-subpackage)))
  "Recent file candidate source for `consult-buffer'.")

(add-to-list 'consult-buffer-sources 'consult--source-custom-files 'append)
(add-to-list 'consult-dir-sources 'consult-dir--source-custom-files 'append)


(defun ft-get-sub-folders ()
  (when-let ((default-directory (ft-monorepo--get-root-directory)))
	(mapcar (lambda (s)
			  (format "%s%s" default-directory s))
			(split-string (shell-command-to-string "fd --type d")))))

(defvar consult--source-folders-files
  `(:name     "Folders"
    :narrow   ?d
    :category file
    :face     consult-file
 	:action   ,#'consult--file-action
    :history  file-name-history
    :items (lambda () (ft-get-sub-folders)))
  "Recent file candidate source for `consult-buffer'.")

(defvar consult-dir--source-folders-files
  `(:name     "Folders"
    :narrow   ?d
    :category file
    :face     consult-file
 	:action   ,#'consult--file-action
    :history  file-name-history
    :items ,(lambda () (ft-get-sub-folders)))
  "Recent file candidate source for `consult-buffer'.")

(add-to-list 'consult-buffer-sources 'consult--source-folders-files 'append)
(add-to-list 'consult-dir-sources 'consult-dir--source-folders-files 'append)

(provide 'ft-monorepo)
