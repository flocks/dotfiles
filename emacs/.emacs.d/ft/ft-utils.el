(defun ft-extract-auth-token (host user)
  (funcall
   (plist-get (nth 0 (auth-source-search :host host :user user)) :secret)))


(provide 'ft-utils)
