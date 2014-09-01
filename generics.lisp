#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defgeneric blog= (a b))

(defgeneric post= (a b))

(defgeneric augment (target source))

(defgeneric blog (name))

(defgeneric followers (blog &key amount offset))

(defgeneric follow (blog))

(defgeneric unfollow (blog))

(defgeneric likes (blog &key amount offset))

(defgeneric submissions (blog &key amount offset))

(defgeneric drafts (blog &key amount offset))

(defgeneric queue (blog &key amount offset))

(defgeneric posts (blog &key type tag amount offset))

(defgeneric post (id))

(defgeneric save (post))

(defgeneric destroy (post))

(defgeneric reblog (post &key comment))

(defgeneric like (post))

(defgeneric unlike (post))

(defgeneric refresh (post))

(defgeneric dashboard (&key amount offset))

(defgeneric following (&key amount offset))

(defgeneric myself ())

(defgeneric my-followers (&key amount offset))

(defgeneric my-likes (&key amount offset))

(defgeneric my-submissions (&key amount offset))

(defgeneric my-drafts (&key amount offset))

(defgeneric my-queue (&key amount offset))

(defgeneric my-posts (&key type tag amount offset))
