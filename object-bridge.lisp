#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defun saget (alist field)
  (aget field alist 'undefined))

(defun naget (alist field)
  (aget field alist))

(defun kaget (alist field)
  (let ((value (saget field alist)))
    (if (eq value 'undefined)
        'undefined
        (find-symbol (string-upcase value) "KEYWORD"))))

(defun taget (alist field)
  (let ((value (saget field alist)))
    (if (eq value 'undefined)
        'undefined
        (cl-ppcre:split "\\s*,\\s*" value))))

(defun daget (alist field)
  (let ((value (saget field alist)))
    (if (eq value 'undefined)
        'undefined
        (parse-tumblr-date value))))

(defun make-from-result (result class/instance &rest fields)
  (let ((instance (if (symbolp class/instance)
                      (make-instance class/instance)
                      class/instance)))
    (loop for fielddef in fields
          do (destructuring-bind (field &optional (get #'(lambda (result)
                                                           (saget result field))))
                 (if (listp fielddef) fielddef (list fielddef))
               (let ((val (funcall get result field)))
                 (unless (eq val 'undefined)
                   (setf (slot-value instance (find-symbol (string field) "HUMBLER"))
                         val)))))
    instance))

(defun field-getter (field &optional (fun #'saget))
  #'(lambda (result) (funcall fun result field)))

(defun field-iterator (field function)
  #'(lambda (result) (mapcar function (saget result field))))

(defun map-field (to from &optional (fun #'saget))
  (list to (field-getter from fun)))

(defun map-field-list (to from function)
  (list to (field-iterator from function)))

(defun make-photo (result)
  (make-from-result result 'photo
    :caption (list :sizes #'(lambda (result)
                              (mapcar #'make-photo-size (aget :sizes result))))))

(defun make-photo-size (result)
  (make-from-result result 'photo-size
    :width :height :url))

(defun make-dialogue (result)
  (make-from-result result 'dialogue
    :phrase :name :label))

(defun make-video-player (result)
  (make-from-result result 'video-player
    :width :embed-code))

(defun make-blog (result)
  (make-from-result result 'blog
    :name :url :title :avatar :description :updated :share-likes
    :admin :facebook-opengraph-enabled :twitter-send
    :twitter-enabled :can-send-fan-mail :followed :ask :ask-anon
    :ask-page-title :is-nsfw
    (map-field :draft-count :drafts)
    (map-field :queue-count :queue)
    (map-field :message-count :messages)
    (map-field :post-count :posts)
    (map-field :like-count :likes)))

(defun make-user (result)
  (let ((user (change-class (make-blog result) 'user)))
    (make-from-result result user
      :default-post-format
      (map-field :following-count :following)
      (map-field-list :blogs :blogs #'make-blog))
    user))

(defun make-raw-post (result)
  (make-from-result result 'post
    :id :blog-name :post-url :timestamp :reblog-key :liked
    (map-field :post-type :type #'kaget)
    (map-field :date :date #'daget)
    (map-field :text-format :format #'kaget)
    (map-field :tags :tags #'taget)
    (map-field :bookmarklet :bookmarklet #'naget)
    (map-field :mobile :mobile #'naget)
    (map-field :source-url :source-url #'naget)
    (map-field :source-title :source-title #'naget)
    (map-field :state :state #'kaget)
    ;; post-specific fields
    :title :body :caption :width :height :text :source :url
    :description :player :album-art :artist :album :track-name
    :track-number :year :asking-name :asking-url :question :answer
    (map-field-list :photos :photos #'make-photo)
    (map-field-list :dialogue :dialogue #'make-dialogue)
    (map-field :plays-count :plays)
    (map-field-list :players :player #'make-video-player)))

(defun make-post (result)
  (let ((post (make-raw-post result)))
    (change-class
     post (ecase (post-type post)
            (:text 'text-post)
            (:photo 'photo-post)
            (:quote 'quote-post)
            (:link 'link-post)
            (:chat 'chat-post)
            (:audio 'audio-post)
            (:video 'video-post)
            (:answer 'answer-post)))))
