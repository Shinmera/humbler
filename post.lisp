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

(defun blog/post-text (username body &key title (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :mardk")
  (request (format NIL *blog/post* username)
           :method :POST :oauth T :parameters (prepare* ("type" . "text") body title state tags tweet date format slug)))

(defun blog/post-quote (username quote &key source (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (request (format NIL *blog/post* username)
           :method :POST :oauth T :parameters (prepare* ("type" . "quote") quote source state tags tweet date format slug)))

(defun blog/post-link (username url &key description title (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (request (format NIL *blog/post* username)
           :method :POST :oauth T :parameters (prepare* ("type" . "link") url description title state tags tweet date format slug)))

(defun blog/post-chat (username conversation &key title (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (request (format NIL *blog/post* username)
           :method :POST :oauth T :parameters (prepare* ("type" . "chat") conversation title state tags tweet date format slug)))

;; bad macro but I don't care to gensym and once-only this, since I know the uses are safe.
(defmacro %photo-push (photo params)
  `(flet ((add (photo)
            (etypecase photo
              (string (push `("source[]" . ,photo) ,params))
              (pathname (push `("data[]" . (,photo)) ,params))
              (array (push `("data[]" . ,photo) ,params)))))
     (if (listp ,photo) (mapc #'add ,photo) (add ,photo))))

(defun blog/post-photo (username photo &key caption link (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (let ((params (prepare* ("type" . "photo") caption link state tags tweet date format slug)))
    (%photo-push photo params)
    (request (format NIL *blog/post* username)
             :method :POST :oauth T :parameters params)))

(defun %audio (audio)
  (etypecase audio
    (string `("external_url" . ,audio))
    (pathname `("data" . (,audio)))
    (array `("data" . ,audio))))

(defun blog/post-audio (username audio &key caption (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (let ((params (prepare* ("type" . "audio") caption state tags tweet date format slug)))
    (push (%audio audio) params)
    (request (format NIL *blog/post* username)
             :method :POST :oauth T :parameters params)))

(defun %video (video)
  (etypecase video
    (string `("embed" . ,video))
    (pathname `("data" . (,video)))
    (array `("data" . ,video))))

(defun blog/post-video (username video &key caption (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (let ((params (prepare* ("type" . "video") caption state tags tweet date format slug)))
    (push (%video video) params)
    (request (format NIL *blog/post* username)
             :method :POST :oauth T :parameters params)))

(defun blog/post/edit (username id &key photo audio video body quote url conversation caption description
                                     source link title (state :published) tags tweet date (format :html) slug)
  (assert (find state '(:published :draft :queue :private))
          () "State must be one of (:published :draft :queue :private)")
  (assert (find format '(:html :markdown))
          () "Format must be one of (:html :markdown)")
  (let ((params (prepare* id body quote url conversation caption description source link title state tags tweet date format slug)))
    (when photo (%photo-push photo params))
    (when video (push (%video video) params))
    (when audio (push (%audio audio) params))
    (request (format NIL *blog/post/edit* username)
             :method :POST :oauth T :parameters params)))

(defun blog/post/reblog (username id reblog-key &key comment)
  (request (format NIL *blog/post/reblog* username)
           :method :POST :oauth T :parameters (prepare* id reblog-key comment)))

(defun blog/post/delete (username id)
  (request (format NIL *blog/post/delete* username)
           :method :POST :oauth T :parameters (prepare* id)))
