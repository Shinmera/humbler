#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defclass blog ()
  ((name :initarg :name :reader name)
   (url :initarg :url :reader url)
   (title :initarg :title :reader title)
   (avatar :initarg :avatar :reader avatar)
   (description :initarg :description :reader description)
   (updated :initarg :updated :reader updated)
   (draft-count :initarg :draft-count :reader draft-count)
   (queue-count :initarg :queue-count :reader queue-count)
   (message-count :initarg :message-count :reader message-count)
   (post-count :initarg :post-count :reader post-count)
   (like-count :initarg :like-count :reader like-count)
   (share-likes :initarg :share-likes :reader share-likes)
   (admin :initarg :admin :reader admin)
   (facebook-opengraph-enabled :initarg :facebook-opengraph-enabled :reader facebook-opengraph-enabled)
   (twitter-send :initarg :twitter-send :reader twitter-send)
   (twitter-enabled :initarg :twitter-enabled :reader twitter-enabled)
   (can-send-fan-mail :initarg :can-send-fan-mail :reader can-send-fan-mail)
   (followed :initarg :followed :reader followed)
   (ask-p :initarg :ask-p :reader ask-p)
   (ask-anon :initarg :ask-anon :reader ask-anon)
   (ask-page-title :initarg :ask-page-title :reader ask-page-title)
   (nsfw-p :initarg :nsfw-p :reader nsfw-p)))

(defmethod print-object ((blog blog) stream)
  (print-unreadable-object (blog stream :type T :identity T)
    (format stream "~a" (name blog)))
  blog)

(defclass user (blog)
  ((following-count :initarg :following-count :reader following-count)
   (default-post-format :initarg :default-post-format :reader default-post-format)
   (blogs :initarg :blogs :reader blogs)))

(defclass post ()
  ((id :initarg :id :reader id)
   (post-type :initarg :post-type :reader post-type)
   (blog-name :initarg :blog-name :reader blog-name)
   (post-url :initarg :post-url :reader post-url)
   (timestamp :initarg :timestamp :reader timestamp)
   (date :initarg :date :accessor date)
   (text-format :initarg :text-format :accessor text-format)
   (reblog-key :initarg :reblog-key :reader reblog-key)
   (tags :initarg :tags :accessor tags)
   (bookmarklet :initarg :bookmarklet :reader bookmarklet)
   (mobile :initarg :mobile :reader mobile)
   (source-url :initarg :source-url :reader source-url)
   (source-title :initarg :source-title :reader source-title)
   (liked :initarg :liked :reader liked)
   (state :initarg :state :accessor state)
   ;; Posting-only props.
   (post-format :initarg :post-format :accessor post-format)
   (tweet :initarg :tweet :accessor tweet)
   (slug :initarg :slug :accessor slug)))

(defmethod print-object ((post post) stream)
  (print-unreadable-object (post stream :type T :identity T)
    (format stream "~a ~a" (blog-name post) (id post)))
  post)

(defclass text-post (post)
  ((title :initarg :title :accessor title)
   (body :initarg :body :accessor body)))

(defclass photo-post (post)
  ((photos :initarg :photos :accessor photos)
   (file :initarg :file :accessor file)
   (caption :initarg :caption :accessor caption)
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)))

(defclass quote-post (post)
  ((text :initarg :text :accessor text)
   (source :initarg :source :accessor source)))

(defclass link-post (post)
  ((title :initarg :title :accessor title)
   (url :initarg :url :accessor url)
   (description :initarg :description :accessor description)))

(defclass chat-post (post)
  ((title :initarg :title :accessor title)
   (body :initarg :body :accessor body)
   (dialogue :initarg :dialogue :reader dialogue)))

(defclass audio-post (post)
  ((caption :initarg :caption :accessor caption)
   (file :initarg :file :accessor file)
   (player :initarg :player :reader player)
   (play-count :initarg :play-count :reader play-count)
   (album-art :initarg :album-art :reader album-art)
   (artist :initarg :artist :reader artist)
   (album :initarg :album :reader album)
   (track-name :initarg :track-name :reader track-name)
   (track-number :initarg :track-number :reader track-number)
   (year :initarg :year :reader year)))

(defclass video-post (post)
  ((caption :initarg :caption :accessor caption)
   (file :initarg :file :accessor file)
   (players :initarg :players :reader players)))

(defclass answer-post (post)
  ((asking-name :initarg :asking-name :reader asking-name)
   (asking-url :initarg :asking-url :reader asking-url)
   (question :initarg :question :reader question)
   (answer :initarg :answer :accessor answer)))

(defclass photo ()
  ((caption :initarg :caption :accessor caption)
   (sizes :initarg :sizes :reader sizes)))

(defclass photo-size ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (url :initarg :url :reader url)))

(defmethod print-object ((photo-size photo-size) stream)
  (print-unreadable-object (photo-size stream :type T :identity T)
    (format stream "~ax~a (~a)" (width photo-size) (height photo-size) (url photo-size)))
  photo-size)

(defclass dialogue ()
  ((label :initarg :label :reader label)
   (name :initarg :name :reader name)
   (phrase :initarg :phrase :reader phrase)))

(defmethod print-object ((dialogue dialogue) stream)
  (print-unreadable-object (dialogue stream :type T :identity T)
    (format stream "~a ~s" (label dialogue) (phrase dialogue)))
  dialogue)

(defclass video-player ()
  ((width :initarg :width :reader width)
   (embed-code :initarg :embed-code :reader embed-code)))

(defmethod print-object ((video-player video-player) stream)
  (print-unreadable-object (video-player stream :type T :identity T)
    (format stream "~a" (width video-player)))
  video-player)

(defmacro define-predicates (&rest classes)
  `(progn
     ,@(loop for class in classes
             collect `(defun ,(intern (format NIL "~a-P" class)) (thing) (typep thing ',class)))))

(define-predicates
    blog user post photo photo-size dialogue video-player
  video-post audio-post chat-post link-post
  quote-post photo-post text-post answer-post)
