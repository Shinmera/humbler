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

;; Evil macro, but it's only used in this controlled env, so I don't care to make it nice.
(defun %post-fun-header ()
  `(progn
     (assert (find state '(:published :draft :queue :private))
             () "State must be one of (:published :draft :queue :private)")
     (assert (find format '(:html :markdown))
             () "Format must be one of (:html :markdown)")
     (assert (or (stringp tweet) (eql tweet :off) (eql tweet NIL)))
     (if (listp tags) (setf tags (format NIL "~{~a~^,~}" tags)))
     (if (typep date 'local-time:timestamp) (setf date (format-tumblr-date date)))))

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

(defun blog/post-text (blog body &key title (state :published) tags tweet date (format :html) slug)
  (%post-fun-header)
  (post-wrapper (format NIL *blog/post* blog) (prepare* ("type" . "text") body title state tags tweet date format slug)))

(defun blog/post-quote (blog quote &key source (state :published) tags tweet date (format :html) slug)
  (%post-fun-header)
  (post-wrapper (format NIL *blog/post* blog) (prepare* ("type" . "quote") quote source state tags tweet date format slug)))

(defun blog/post-link (blog url &key description title (state :published) tags tweet date (format :html) slug)
  (%post-fun-header)
  (post-wrapper (format NIL *blog/post* blog) (prepare* ("type" . "link") url description title state tags tweet date format slug)))

(defun blog/post-chat (blog conversation &key title (state :published) tags tweet date (format :html) slug)
  (%post-fun-header)
  (post-wrapper (format NIL *blog/post* blog) (prepare* ("type" . "chat") conversation title state tags tweet date format slug)))

(defun blog/post-photo (blog photo &key caption link (state :published) tags tweet date (format :html) slug)
  (%post-fun-header)
  (let ((params (prepare* ("type" . "photo") caption link state tags tweet date format slug)))
    (post-wrapper (format NIL *blog/post* blog) params :photo photo)))

(defun blog/post-audio (blog audio &key caption (state :published) tags tweet date (format :html) slug)
  (%post-fun-header)
  (let ((params (prepare* ("type" . "audio") caption state tags tweet date format slug)))
    (post-wrapper (format NIL *blog/post* blog) params :audio audio)))

(defun blog/post-video (blog video &key caption (state :published) tags tweet date (format :html) slug)
  (%post-fun-header)
  (let ((params (prepare* ("type" . "video") caption state tags tweet date format slug)))
    (post-wrapper (format NIL *blog/post* blog) params :video video)))

(defun blog/post/edit (blog id &key photo audio video body quote url conversation caption description
                                     source link title (state :published) tags tweet date (format :html) slug)
  (%post-fun-header)
  (let ((params (prepare* id body quote url conversation caption description source link title state tags tweet date format slug)))
    (post-wrapper (format NIL *blog/post/edit* blog) params :photo photo :audio audio :video video)))

(defun blog/post/reblog (blog id reblog-key &key comment)
  (post-wrapper (format NIL *blog/post/reblog* blog) (prepare* id reblog-key comment)))

(defun blog/post/delete (blog id)
  (post-wrapper (format NIL *blog/post/delete* blog) (prepare* id)))
