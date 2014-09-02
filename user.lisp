#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defvar *user/info* "https://api.tumblr.com/v2/user/info")
(defvar *user/dashboard* "https://api.tumblr.com/v2/user/dashboard")
(defvar *user/likes* "https://api.tumblr.com/v2/user/likes")
(defvar *user/following* "https://api.tumblr.com/v2/user/following")
(defvar *user/follow* "https://api.tumblr.com/v2/user/follow")
(defvar *user/unfollow* "https://api.tumblr.com/v2/user/unfollow")
(defvar *user/like* "https://api.tumblr.com/v2/user/like")
(defvar *user/unlike* "https://api.tumblr.com/v2/user/unlike")

(defun user/info ()
  (aget :user (request *user/info* :oauth T)))

(defun user/dashboard (&key (limit 20) (offset 0) type since-id reblog-info notes-info)
  (assert (member type '(NIL :text :quote :link :answer :video :audio :photo :chat))
          () "Type has to be one of (NIL :text :quote :link :answer :video :audio :photo :chat)")
  (assert (<= 1 limit 20)
          () "Limit must be between 1 and 20 (inclusive).")
  (assert (<= 0 offset)
          () "Offset must be positive.")
  (assert (or (null since-id) (<= 0 since-id))
          () "Since-ID must be positive.")
  (if reblog-info (setf reblog-info T))
  (if notes-info (setf notes-info T))
  (aget :posts
        (request *user/dashboard* :oauth T :parameters (prepare* limit offset type since-id reblog-info notes-info))))

(defun user/likes (&key (limit 20) (offset 0))
  (assert (<= 1 limit 20)
          () "Limit must be between 1 and 20 (inclusive).")
  (assert (<= 0 offset)
          () "Offset must be positive.")
  (let ((data (request *user/likes* :oauth T :parameters (prepare* limit offset))))
    (values (aget :liked-posts data)
            (aget :liked-count data))))

(defun user/following (&key (limit 20) (offset 0))
  (assert (<= 1 limit 20)
          () "Limit must be between 1 and 20 (inclusive).")
  (assert (<= 0 offset)
          () "Offset must be positive.")
  (let ((data (request *user/following* :oauth T :parameters (prepare* limit offset))))
    (values (aget :blogs data)
            (aget :total-blogs data))))

(defun user/follow (blog)
  (or (request *user/follow* :method :POST :oauth T :parameters `(("url" . ,(format NIL "~a.tumblr.com" blog))))
      T))

(defun user/unfollow (blog)
  (or (request *user/unfollow* :method :POST :oauth T :parameters `(("url" . ,(format NIL "~a.tumblr.com" blog))))
      T))

(defun user/like (id reblog-key)
  (or (request *user/like* :method :POST :oauth T :parameters (prepare* id reblog-key))
      T))

(defun user/unlike (id reblog-key)
  (or (request *user/unlike* :method :POST :oauth T :parameters (prepare* id reblog-key))
      T))
