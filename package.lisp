#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:humbler-api
  (:nicknames #:org.tymoonnext.humbler.api)
  ;; blog.lisp
  (:export
   #:blog/info
   #:blog/avatar
   #:blog/likes
   #:blog/followers)
  ;; post.lisp
  (:export
   #:blog/post-text
   #:blog/post-quote
   #:blog/post-link
   #:blog/post-chat
   #:blog/post-photo
   #:blog/post-audio
   #:blog/post-video
   #:blog/post/edit
   #:blog/post/reblog
   #:blog/post/delete)
  ;; posts.lisp
  (:export
   #:blog/posts
   #:blog/posts/queue
   #:blog/posts/draft
   #:blog/posts/submission)
  ;; tagged.lisp
  (:export
   #:tagged)
  ;; user.lisp
  (:export
   #:user/info
   #:user/dashboard
   #:user/likes
   #:user/following
   #:user/follow
   #:user/unfollow
   #:user/like
   #:user/unlike))

(defpackage #:humbler-objects
  (:nicknames #:org.tymoonnext.humbler.objects)
  ;;
  (:export)
  )

(defpackage #:humbler-extra
  (:nicknames #:org.tymoonnext.humbler.extra)
  ;; auth.lisp
  (:export
   #:login
   #:logout)
  ;; toolkit.lisp
  (:export
   #:to-keyword
   #:from-keyword
   #:raw-request
   #:request
   #:prepare
   #:prepare*
   #:get-unix-time))

(defpackage #:humbler
  (:nicknames #:org.tymoonnext.humbler)
  (:use #:cl
        #:humbler-api
        #:humbler-objects
        #:humbler-extra))

;; Export everything from sub-packages
(do-external-symbols (symb '#:HUMBLER-OBJECTS)
  (export symb '#:HUMBLER))
(do-external-symbols (symb '#:HUMBLER-API)
  (export symb '#:HUMBLER))
(do-external-symbols (symb '#:HUMBLER-EXTRA)
  (export symb '#:HUMBLER))
