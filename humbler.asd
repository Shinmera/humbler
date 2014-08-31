#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.humbler.asdf
  (:use #:cl #:asdf))
(in-package :org.tymoonnext.humbler.asdf)

(defsystem humbler
  :name "Humbler"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A Tumblr API interface"
  :homepage "https://github.com/Shinmera/humbler"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "auth")
               (:file "blog")
               (:file "posts")
               (:file "post")
               (:file "user")
               (:file "tagged")
               (:file "documentation"))
  :depends-on (:south
               :yason
               :trivial-mimes
               :local-time))
