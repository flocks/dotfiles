(require 'notmuch)

(setq notmuch-show-logo nil
	  notmuch-column-control 1.0
	  notmuch-hello-auto-refresh t
	  notmuch-hello-recent-searches-max 20
	  notmuch-hello-thousands-separator ""
	  notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
	  notmuch-show-all-tags-list t)

(setq-default notmuch-search-oldest-first nil) ;; why it's not the default val lol

(setq mail-user-agent 'message-user-agent)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(provide 'ft-mail)
