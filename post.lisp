#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defvar *blog/post* "https://api.tumblr.com/v2/blog/~a.tumblr.com/post")
(defvar *blog/post/edit* "https://api.tumblr.com/v2/blog/~a.tumblr.com/post/edit")
(defvar *blog/post/reblog* "https://api.tumblr.com/v2/blog/~a.tumblr.com/post/reblog")
(defvar *blog/post/delete* "https://api.tumblr.com/v2/blog/~a.tumblr.com/post/delete")

(defun post-wrapper (uri params &key video photo audio)
  (aget
   :id
   (cond ((and (not video) (not photo) (not audio))
          (request
           uri :method :POST :oauth T
               :parameters params))
         ((or (stringp video) (stringp photo) (stringp audio))
          (request
           uri :method :POST :oauth T
               :parameters (cons (cond (video `("embed" . ,video))
                                       (audio `("external_url" . ,audio))
                                       (photo `("source" . ,photo)))
                                 params)))
         (T
          (data-request
           uri :parameters params
               :data-parameters (cond (video `(("data" . ,video)))
                                      (audio `(("data" . ,audio)))
                                      (photo (if (listp photo)
                                                 (loop for image in photo
                                                       for i from 0
                                                       collect `(,(format NIL "data[~a]" i) . (,image :content-type ,(mimes:mime-lookup image))))
                                                 `(("data" . (,photo :content-type ,(mimes:mime-lookup photo))))))))))))

(defun blog/post-text (username body &key title (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :mardk")
  (post-wrapper (format NIL *blog/post* username) (prepare* ("type" . "text") body title state tags tweet date format slug)))

(defun blog/post-quote (username quote &key source (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (post-wrapper (format NIL *blog/post* username) (prepare* ("type" . "quote") quote source state tags tweet date format slug)))

(defun blog/post-link (username url &key description title (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (post-wrapper (format NIL *blog/post* username) (prepare* ("type" . "link") url description title state tags tweet date format slug)))

(defun blog/post-chat (username conversation &key title (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (post-wrapper (format NIL *blog/post* username) (prepare* ("type" . "chat") conversation title state tags tweet date format slug)))

(defun blog/post-photo (username photo &key caption link (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (let ((params (prepare* ("type" . "photo") caption link state tags tweet date format slug)))
    (post-wrapper (format NIL *blog/post* username) params :photo photo)))

(defun blog/post-audio (username audio &key caption (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (let ((params (prepare* ("type" . "audio") caption state tags tweet date format slug)))
    (post-wrapper (format NIL *blog/post* username) params :audio audio)))

(defun blog/post-video (username video &key caption (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (let ((params (prepare* ("type" . "video") caption state tags tweet date format slug)))
    (post-wrapper (format NIL *blog/post* username) params :video video)))

(defun blog/post/edit (username id &key photo audio video body quote url conversation caption description
                                     source link title (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (let ((params (prepare* id body quote url conversation caption description source link title state tags tweet date format slug)))
    (post-wrapper (format NIL *blog/post/edit* username) params :photo photo :audio audio :video video)))

(defun blog/post/reblog (username id reblog-key &key comment)
  (post-wrapper (format NIL *blog/post/reblog* username) (prepare* id reblog-key comment)))

(defun blog/post/delete (username id)
  (post-wrapper (format NIL *blog/post/delete* username) (prepare* id)))
