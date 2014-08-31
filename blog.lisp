#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defvar *blog/info* "https://api.tumblr.com/v2/blog/~a.tumblr.com/info")
(defvar *blog/avatar* "https://api.tumblr.com/v2/blog/~a.tumblr.com/avatar/~a")
(defvar *blog/likes* "https://api.tumblr.com/v2/blog/~a.tumblr.com/likes")
(defvar *blog/followers* "https://api.tumblr.com/v2/blog/~a.tumblr.com/followers")

(defun blog/info (blog)
  (aget :blog (request (format NIL *blog/info* blog) :parameters `(("api_key" . ,south:*oauth-api-key*)))))

(defun blog/avatar (blog &key (size 64) fetch)
  (assert (find size '(16 24 30 40 48 64 96 128 512))
          () "Size must be one of (16 24 30 40 48 64 96 128 512)")
  (multiple-value-bind (image status headers uri) (request (format NIL *blog/avatar* blog size) :redirect (if fetch 1 NIL))
    (declare (ignore status))
    (if fetch
        (values image (puri:render-uri uri NIL))
        (aget :location headers))))

(defun blog/likes (blog &key (limit 20) (offset 0))
  (assert (<= 1 limit 20)
          () "Limit must be between 1 and 20 (inclusive).")
  (assert (<= 0 offset)
          () "Offset must be positive.")
  (let ((data (request (format NIL *blog/likes* blog) :parameters (cons `("api_key" . ,south:*oauth-api-key*) (prepare* limit offset)))))
    (values (aget :liked-posts data)
            (aget :liked-count data))))

(defun blog/followers (blog &key (limit 20) (offset 0))
  (assert (<= 1 limit 20)
          () "Limit must be between 1 and 20 (inclusive).")
  (assert (<= 0 offset)
          () "Offset must be positive.")
  (let ((data (request (format NIL *blog/followers* blog) :oauth T :parameters (prepare* limit offset))))
    (values (aget :users data)
            (aget :total-users data))))
