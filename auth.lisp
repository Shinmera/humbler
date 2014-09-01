#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defvar *user*)

(south:prepare
 :oauth/request-token "http://www.tumblr.com/oauth/request_token"
 :oauth/authorize "http://www.tumblr.com/oauth/authorize"
 :oauth/access-token "http://www.tumblr.com/oauth/access_token")

(defun init-user ()
  (setf *user* (myself)))

(setf south:*authentication-callback*
      #'(lambda (&rest a) (declare (ignore a)) (init-user)))

(defun login ()
  (south:initiate-authentication :method :server))

(defun logout ()
  (setf south:*oauth-access-token* NIL
        south:*oauth-access-secret* NIL))
