#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defvar *blog/posts* "https://api.tumblr.com/v2/blog/~a.tumblr.com/posts~@[/~a~]")
(defvar *blog/posts/queue* "https://api.tumblr.com/v2/blog/~a.tumblr.com/posts/queue")
(defvar *blog/posts/draft* "https://api.tumblr.com/v2/blog/~a.tumblr.com/posts/draft")
(defvar *blog/posts/submission* "https://api.tumblr.com/v2/blog/~a.tumblr.com/posts/submission")

(defun blog/posts (username &key type id tag (limit 20) (offset 0) reblog-info notes-info filter)
  (assert (member type '(NIL :text :quote :link :answer :video :audio :photo :chat))
          () "Type has to be one of (NIL :text :quote :link :answer :video :audio :photo :chat)")
  (assert (<= 1 limit 20)
          () "Limit must be between 1 and 20 (inclusive).")
  (assert (<= 0 offset)
          () "Offset must be positive.")
  (assert (member filter '(NIL :text :raw))
          () "Filter must be one of (NIL :text :raw)")
  (if reblog-info (setf reblog-info T))
  (if notes-info (setf notes-info T))
  (request (format NIL *blog/posts* username (when type (string-downcase type)))
           :parameters (cons `("api_key" . ,south:*oauth-api-key*)
                             (prepare* id tag limit offset reblog-info notes-info filter))))

(defun blog/posts/queue (username &key (limit 20) (offset 0) filter)
  (assert (<= 1 limit 20)
          () "Limit must be between 1 and 20 (inclusive).")
  (assert (<= 0 offset)
          () "Offset must be positive.")
  (assert (member filter '(NIL :text :raw))
          () "Filter must be one of (NIL :text :raw)")
  (request (format NIL *blog/posts/queue* username)
           :oauth T :parameters (prepare* limit offset filter)))

(defun blog/posts/draft (username &key (before-id 0) filter)
  (assert (<= 0 before-id)
          () "Before-ID must be positive.")
  (assert (member filter '(NIL :text :raw))
          () "Filter must be one of (NIL :text :raw)")
  (request (format NIL *blog/posts/draft* username)
           :oauth T :parameters (prepare* before-id filter)))

(defun blog/posts/submission (username &key (offset 0) filter)
  (assert (<= 0 offset)
          () "Offset must be positive.")
  (assert (member filter '(NIL :text :raw))
          () "Filter must be one of (NIL :text :raw)")
  (request (format NIL *blog/posts/submission* username)
           :oauth T :parameters (prepare* offset filter)))
