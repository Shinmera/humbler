#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defmacro set-deferred (class readers &body fill-calls)
  (destructuring-bind (class &optional (var class)) (if (listp class) class (list class))
    `(progn
       ,@(loop for reader in readers
               collect `(defmethod ,reader :before ((,var ,class))
                          (unless (slot-boundp ,var ',reader)
                            (let ((reader ',reader))
                              ,@fill-calls)))))))

(defmacro set-undeferred (class readers)
  `(set-deferred ,class ,readers
     (error "Slot ~a not set and don't know how to retrieve it." reader)))

(set-deferred blog (title post-count name updated description
                          ask ask-anon like-count)
  (blog blog))

(set-deferred blog (avatar)
  (setf (slot-value blog '%avatar)
        (blog/avatar (name blog))))

(set-undeferred blog (url draft-count queue-count message-count
                          share-likes admin facebook-opengraph-enabled
                          twitter-send twitter-enabled can-send-fan-mail
                          followed as-page-title is-nsfw))

(set-deferred user (following-count default-post-format blogs)
  (if (blog= user *user*)
      (augment user (myself))
      (error "Slot ~a not set and don't know how to retrieve it." reader)))

(set-deferred post (post-type blog-name post-url timestamp date text-format
                              reblog-key tags bookmarklet mobile source-url
                              source-title liked state)
  (refresh post))

(set-deferred text-post (title body)
  (refresh text-post))

(set-deferred photo-post (photos caption width height)
  (refresh photo-post))

(set-deferred quote-post (text source)
  (refresh quote-post))

(set-deferred link-post (title url description)
  (refresh link-post))

(set-deferred chat-post (title body dialogue)
  (refresh chat-post))

(set-deferred audio-post (caption player play-count album-art artist album
                                  track-name track-number year)
  (refresh audio-post))

(set-deferred video-post (caption players)
  (refresh video-post))

(set-deferred answer-post (asking-name asking-url question answer)
  (refresh answer-post))

;; Defaulting to undeferred
(set-undeferred post (id title body photos caption width height source url
                         description dialogue player play-count album-art
                         artist album track-name track-number year players
                         askin-name asking-url question answer))
