#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defvar *oauth/request-token* "http://www.tumblr.com/oauth/request_token")
(defvar *oauth/authorize* "http://www.tumblr.com/oauth/authorize")
(defvar *oauth/access-token* "http://www.tumblr.com/oauth/access_token")

(defvar *user* NIL
  "The central user object storing a reference to whoever is currently logged in.")

(south:prepare
 :oauth/request-token *oauth/request-token*
 :oauth/authorize *oauth/authorize*
 :oauth/access-token *oauth/access-token*)

(defun init-user ()
  "Initializes the *USER* object."
  (setf *user* (myself)))

(setf south:*authentication-callback*
      #'(lambda (&rest a) (declare (ignore a)) (init-user)))

(defun login ()
  "Performs a SERVER login with South."
  (south:initiate-authentication :method :server))

(defun logout ()
  "Resets South's access tokens, thus logging you out."
  (setf south:*oauth-access-token* NIL
        south:*oauth-access-secret* NIL))
