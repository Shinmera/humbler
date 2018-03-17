#|
 This file is a part of Humbler
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
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
  ;; generics.lisp
  (:export
   #:blog=
   #:post=
   #:augment
   #:copy
   #:blog
   #:followers
   #:follow
   #:unfollow
   #:likes
   #:submissions
   #:drafts
   #:queue
   #:posts
   #:post
   #:save
   #:repost
   #:destroy
   #:reblog
   #:like
   #:unlike
   #:reply
   #:refresh
   #:dashboard
   #:following
   #:myself
   #:my-blogs
   #:my-followers
   #:my-likes
   #:my-submissions
   #:my-drafts
   #:my-queue
   #:my-posts
   #:tag)
  ;; object-bridge.lisp
  (:export
   #:make-photo
   #:make-photo-size
   #:make-dialogue
   #:make-video-player
   #:make-blog
   #:make-user
   #:make-post)
  ;; objects.lisp
  (:export
   #:blog
   #:name
   #:url
   #:title
   #:avatar
   #:description
   #:updated
   #:draft-count
   #:queue-count
   #:message-count
   #:post-count
   #:like-count
   #:share-likes
   #:admin
   #:facebook-opengraph-enabled
   #:twitter-send
   #:twitter-enabled
   #:can-send-fan-mail
   #:followed
   #:ask-p
   #:ask-anon
   #:ask-page-title
   #:nsfw-p
   
   #:user
   #:following-post
   #:default-post-format
   #:blogs
   
   #:post
   #:id
   #:post-type
   #:blog-name
   #:post-url
   #:timestamp
   #:date
   #:text-format
   #:reblog-key
   #:tags
   #:bookmarklet
   #:mobile
   #:source-url
   #:source-title
   #:liked
   #:state
   #:post-format
   #:tweet
   #:slug
   #:note-count
   
   #:text-post
   #:title
   #:body
   
   #:photo-post
   #:photos
   #:file
   #:caption
   #:width
   #:height
   
   #:quote-post
   #:text
   #:source
   
   #:link-post
   #:title
   #:url
   #:description
   
   #:chat-post
   #:title
   #:body
   #:dialogue
   
   #:audio-post
   #:caption
   #:file
   #:player
   #:play-count
   #:album-art
   #:artist
   #:album
   #:track-name
   #:track-number
   #:year
   
   #:video-post
   #:caption
   #:file
   #:players
   
   #:answer-post
   #:asking-name
   #:asking-url
   #:question
   #:answer
   
   #:photo
   #:caption
   #:sizes
   
   #:photo-size
   #:width
   #:height
   #:url
   
   #:dialogue
   #:label
   #:name
   #:phrase
   
   #:video-player
   #:width
   #:embed-code

   #:blog-p
   #:user-p
   #:post-p
   #:photo-p
   #:photo-size-p
   #:dialogue-p
   #:video-player-p
   #:video-post-p
   #:audio-post-p
   #:chat-post-p
   #:link-post-p
   #:quote-post-p
   #:photo-post-p
   #:text-post-p
   #:answer-post-p))

(defpackage #:humbler-extra
  (:nicknames #:org.tymoonnext.humbler.extra)
  ;; auth.lisp
  (:export
   #:*oauth/access-token*
   #:*oauth/authorize*
   #:*oauth/request-token*
   #:*user*
   #:login
   #:logout)
  ;; toolkit.lisp
  (:export
   #:*tumblr-datetime-format*
   #:to-keyword
   #:from-keyword
   #:raw-request
   #:request
   #:prepare
   #:prepare*
   #:get-unix-time
   #:format-tumblr-date
   #:parse-tumblr-date
   #:pageinate
   #:pageinate-id
   #:pageinate-time
   #:print-slots))

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
