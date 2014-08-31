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

(defun blog/info (username)
  (request (format NIL *blog/info* username) :parameters `(("api_key" . ,south:*oauth-api-key*))))

(defun blog/avatar (username &key (size 64) fetch)
  (assert (find size '(16 24 30 40 48 64 96 128 512))
          () "Size must be one of (16 24 30 40 48 64 96 128 512)")
  (multiple-value-bind (image status headers uri) (request (format NIL *blog/avatar* username size) :redirect (if fetch 1 NIL))
    (declare (ignore status))
    (if fetch
        (values image (puri:render-uri uri NIL))
        (cdr (assoc :location headers)))))

(defun blog/likes (username &key (limit 20) (offset 0))
  (assert (<= 1 limit 20)
          () "Limit must be between 1 and 20 (inclusive).")
  (assert (<= 0 offset)
          () "Offset must be positive.")
  (request (format NIL *blog/likes* username) :parameters (cons `("api_key" . ,south:*oauth-api-key*) (prepare* limit offset))))

(defun blog/followers (username &key (limit 20) (offset 0))
  (assert (<= 1 limit 20)
          () "Limit must be between 1 and 20 (inclusive).")
  (assert (<= 0 offset)
          () "Offset must be positive.")
  (request (format NIL *blog/followers* username) :oauth T :parameters (prepare* limit offset)))
