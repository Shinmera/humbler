(defsystem humbler
  :name "Humbler"
  :version "2.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A Tumblr API interface"
  :homepage "https://Shinmera.github.io/humbler/"
  :bug-tracker "https://github.com/Shinmera/humbler/issues"
  :source-control (:git "https://github.com/Shinmera/humbler.git")
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
