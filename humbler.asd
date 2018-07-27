#|
 This file is a part of Humbler
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem humbler
  :name "Humbler"
  :version "2.0.0"
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
               (:file "objects")
               (:file "object-bridge")
               (:file "generics")
               (:file "deferring")
               (:file "documentation"))
  :depends-on (:north-core
               :yason
               :trivial-mimes
               :local-time
               :closer-mop
               :cl-ppcre))
