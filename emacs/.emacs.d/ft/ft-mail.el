(require 'notmuch)

(setq notmuch-show-logo nil
	  notmuch-column-control 1.0
	  notmuch-hello-auto-refresh t
	  notmuch-hello-recent-searches-max 20
	  notmuch-hello-thousands-separator ""
	  notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
	  notmuch-show-all-tags-list t)

;; (setq notmuch-address-command "notmuch-get-addresses")
;; (notmuch-address-setup)

(setq notmuch-address-command 'internal)
(setq notmuch-address-use-company nil)

(setq-default notmuch-search-oldest-first nil) ;; why it's not the default val lol
(setq notmuch-wash-signature-lines-max 100)
(setq notmuch-wash-wrap-lines-length 90)

;; (advice-add 'notmuch-show-insert-headerline :before (lambda (&rest r) (newline 2)))

(setq notmuch-tagging-keys
	  '(("u" notmuch-show-mark-read-tags "Mark read")
		("t" ("+todo" "-unread") "Todo")
		("f" ("+flagged") "Fav")))

(setq notmuch-saved-searches
	  `(( :name "inbox"
		  :query "tag:inbox"
		  :key ,(kbd "i"))
		( :name "unread (inbox)"
		  :query "tag:unread and tag:inbox"
		  :key ,(kbd "u"))
		( :name "unread all"
		  :query "tag:unread not tag:archived"
		  :key ,(kbd "U"))
		( :name "fav"
		  :query "tag:fav"
		  :key ,(kbd "f"))
		( :name "todo"
		  :query "tag:todo not tag:archived"
		  :key ,(kbd "t"))
		( :name "unread mailing lists"
		  :query "tag:list and tag:unread"
		  :key ,(kbd "M"))
		( :name "github"
		  :query "tag:github"
		  :key ,(kbd "g"))
		( :name "unread github"
		  :query "tag:github and tag:unread"
		  :key ,(kbd "G"))
		( :name "Sent"
		  :query "(from:teissierflorent@gmail.com)"
		  :key ,(kbd "s"))
		( :name "mailing lists"
		  :query "tag:list"
		  :key ,(kbd "m"))
		;; Emacs
		( :name "emacs-devel"
		  :query "(from:emacs-devel@gnu.org or to:emacs-devel@gnu.org) not tag:archived"
		  :key ,(kbd "e d"))
		( :name "emacs-bugs"
		  :query "'to:\"/*@debbugs.gnu.org*/\"' not tag:archived"
		  :key ,(kbd "e b"))
		( :name "emacs-humanities"
		  :query "(from:emacs-humanities@gnu.org or to:emacs-humanities@gnu.org) not tag:archived"
		  :key ,(kbd "e h"))))

(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "teissierflorent@gmail.com")
(setq user-full-name "Florent Teissier")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(global-set-key (kbd "C-c m")
				(lambda ()
				  (interactive)
				  (notmuch-jump-search)))


(use-package notmuch-bookmarks
  :straight t
  :config
  (notmuch-bookmarks-mode))

(defun ft-fetch-mail ()
  (interactive)
  (async-shell-command "mbsync -a && notmuch new"))

(evil-define-key 'normal notmuch-search-mode-map (kbd "R") 'ft-fetch-mail)

(provide 'ft-mail)
