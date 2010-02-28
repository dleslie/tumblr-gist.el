(require 'gist)
(require 'tumblr)

(defun tumblr-gist-region (begin end &optional private)
  "Post the current region as a new paste in a blog post"
  (interactive "r\nP")
  (gist-region begin end private 'tumblr-gist-callback))

(defun tumblr-gist-callback (status)
  (let* ((url-max-redirections 5)
         (url-request-method "GET")
         (location (concat (cadr status) ".pibb"))
         (gist-buffer (url-retrieve-synchronously location)))
    (message "Fetched %s" location)
    (with-current-buffer gist-buffer
      (let* ((content (buffer-substring (point-min) (point-max)))
             (fixed-content (replace-regexp-in-string 
                             "\\(<pre>\\|</pre>\\)" ""
                             content))
             (title (read-string "Post title: "))
        (send-post title fixed-content)))))

(provide 'tumblr-gist)
;;; tumblr.el ends here.
