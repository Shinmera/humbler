#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(south:prepare
 :oauth/request-token "http://www.tumblr.com/oauth/request_token"
 :oauth/authorize "http://www.tumblr.com/oauth/authorize"
 :oauth/access-token "http://www.tumblr.com/oauth/access_token")

(defun login (&optional callback-url)
  (south:initiate-authentication :method (or callback-url :server)))
