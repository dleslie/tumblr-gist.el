(eval-when-compile (require 'gist))

(defvar tumblr-email nil)
(defvar tumblr-password nil)

(defun tumblr-region (begin end &optional private)
  "Post the current region as a new paste in a blog post"
  (interactive "r\nP")
  (gist-region begin end private 'tumblr-gist-callback))

(defun tumblr-gist-callback (status)
  (with-current-buffer (url-retrieve-synchronously (cadr status))
    (destructuring-bind (email . password) (tumblr-auth-info)
      (let* ((title (read-string "Post title: "))
             (data (replace-regexp-in-string 
                    "\\(<pre>\\|</pre>\\)" ""
                    (buffer-substring (point-min) (point-max))))
             (url-request-method "POST")
             (url-request-extra-headers '(("Content-Type" . "application/xml")))
             (url-request-data (gist-make-query-string
                                '(("email" . email)
                                ("password" . password)
                                ("title" . title)
                                ("format" . "html")
                                ("private" . "0")
                                ("body" . data)))))
        (kill-buffer (url-retrieve-synchronously "http://www.tumblr.com/api/write"))))))

(defun tumblr-auth-info ()
  "Returns the user's tumblr authorization information."
  (interactive)
  (when (not tumblr-email)
    (setq tumblr-email (read-string "Tumblr email: ")))
  (when (not tumblr-password)
    (setq tumblr-password (read-string "Tumblr password: ")))
  (cons tumblr-email tumblr-password))

(provide 'tumblr)
;;; tumblr.el ends here.
