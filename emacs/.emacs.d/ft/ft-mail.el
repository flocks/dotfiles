(require 'notmuch)

(setq notmuch-show-logo nil
	  notmuch-column-control 1.0
	  notmuch-hello-auto-refresh t
	  notmuch-hello-recent-searches-max 20
	  notmuch-hello-thousands-separator ""
	  notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
	  notmuch-show-all-tags-list t)

(setq-default notmuch-search-oldest-first nil) ;; why it's not the default val lol
(setq notmuch-wash-signature-lines-max 100)

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

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(use-package notmuch-indicator
  :straight t
  :init
  (setq notmuch-indicator-args
		'((:terms "tag:unread and tag:inbox" :label "📧")
		  (:terms "tag:unread and tag:github" :label "🖥️")
		  (:terms "tag:unread and tag:list" :label "L")))
  (notmuch-indicator-mode))

(global-set-key (kbd "C-c m")
				(lambda ()
				  (interactive)
				  (notmuch-jump-search)))
(provide 'ft-mail)
