#|
 This file is a part of Humbler
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defparameter *oauth/request-token* "https://www.tumblr.com/oauth/request_token")
(defparameter *oauth/authorize* "https://www.tumblr.com/oauth/authorize")
(defparameter *oauth/access-token* "https://www.tumblr.com/oauth/access_token")

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
