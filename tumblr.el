(eval-when-compile (require 'gist))

(defvar tumblr-email nil)
(defvar tumblr-password nil)

(defun tumblr-region (begin end &optional private)
  "Post the current region as a new paste in a blog post"
  (interactive "r\nP")
  (gist-region begin end private 'tumblr-gist-callback))

(defun tumblr-gist-callback (status)
  (let* ((url-max-redirections 5)
         (url-request-method "GET")
         (gist-buffer (url-retrieve-synchronously (cadr status))))
    (with-current-buffer gist-buffer
      (let* ((content (buffer-substring (point-min) (point-max)))
             (fixed-content (replace-regexp-in-string 
                             "\\(<pre>\\|</pre>\\)" ""
                             content))
             (title (read-string "Post title: ")))
        (tumblr-post-data title content)))))

(defun tumblr-post-data (title data)
  (destructuring-bind (email . password) (tumblr-auth-info)
    (let ((url-max-redirections 5)
          (url-request-method "POST")
          (url-request-extra-headers '(("Content-Type" . "application/xml")))
          (url-request-data (gist-make-query-string
                             `(("email" . ,email)
                               ("password" . ,password)
                               ("title" . ,title)
                               ("format" . "html")
                               ("private" . "0")
                               ("body" . ,data)))))
      (url-retrieve "http://www.tumblr.com/api/write" 
                    (lambda (status) (kill-buffer (current-buffer)))))))

(defun tumblr-auth-info ()
  "Returns the user's tumblr authorization information."
  (interactive)
  (when (not tumblr-email)
    (setq tumblr-email (read-string "Tumblr email: ")))
  (when (not tumblr-password)
    (setq tumblr-password (read-passwd "Tumblr password: ")))
  (cons tumblr-email tumblr-password))

(provide 'tumblr)
;;; tumblr.el ends here.
