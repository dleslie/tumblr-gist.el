;; tumblr-gist.el --- Ties the Gist and Tumblr integration to allow syntax highlighted and revision controlled region pasting

;; Author: Daniel Leslie <www.google.com/profiles/dleslie>
;; Maintainer: Dan Leslie <dan@ironoxide.ca>
;; Contributors:
;; Version: 0.1
;; Created: 28 Feb, 2010
;; Keywords: gist git github paste pastie pastebin tumblr blog

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Requires gist.el and tumblr.el, available at:
;; http://github.com/defunkt/gist.el
;; http://www.emacswiki.org/emacs/Tumblr#toc7

;;; Code:

(require 'gist)
(require 'tumblr)
(require 'http-post-simple)

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
      (search-forward-regexp "\n\n")
      (delete-region (point-min) (point))
      (set-buffer-modified-p nil)
      (let* ((content (buffer-substring (point-min) (point-max)))
             (fixed-content (replace-regexp-in-string 
                             "\\(<pre>\\|</pre>\\)" ""
                             content))
             (title (read-string "Post title: "))
             (tags (read-string "Post tags: ")))
        (tumblr-gist-send-post title tags fixed-content)))))

(defun tumblr-gist-send-post (title tags body)
  "Slightly more detailed variant on the 'tumblr send-post function"
  (http-post-simple tumblr-post-url (list (cons 'email tumblr-email)
                                          (cons 'password tumblr-password)
                                          (cons 'type tumblr-default-type)
                                          (cons 'title title)
                                          (cons 'tags tags)
                                          (cons 'slug title)
                                          (cons 'private "0")
                                          (cons 'body body))
                    'utf-8))

(provide 'tumblr-gist)
;;; tumblr-gist.el ends here.
