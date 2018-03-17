#|
 This file is a part of Humbler
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defun saget (alist field)
  (aget field alist 'undefined))

(defun naget (alist field)
  (aget field alist))

(defun kaget (alist field)
  (let ((value (saget alist field)))
    (if (eq value 'undefined)
        'undefined
        (find-symbol (string-upcase value) "KEYWORD"))))

(defun taget (alist field)
  (let ((value (saget alist field)))
    (if (eq value 'undefined)
        'undefined
        (cl-ppcre:split "\\s*,\\s*" value))))

(defun daget (alist field)
  (let ((value (saget alist field)))
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
               (let ((val (funcall get result)))
                 (unless (eq val 'undefined)
                   (setf (slot-value instance (or (find-symbol (string field) "HUMBLER")
                                                  (error "Shinmera fucked up and ~a is not defined." field)))
                         val)))))
    instance))

(defun field-getter (field &optional (fun #'saget))
  #'(lambda (result) (funcall fun result field)))

(defun field-iterator (field function &optional (get #'saget))
  #'(lambda (result)
      (let ((value (funcall get result field)))
        (if (eq value 'undefined)
            'undefined
            (mapcar function value)))))

(defun map-field (to from &optional (fun #'saget))
  (list to (field-getter from fun)))

(defun map-field-list (to from function &optional (get #'saget))
  (list to (field-iterator from function get)))

(defun make-photo (result)
  (make-from-result result 'photo
    :caption (list :sizes #'(lambda (result)
                              (list*
                               (make-photo-size (aget :original-size result))
                               (mapcar #'make-photo-size (aget :alt-sizes result)))))))

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
    :twitter-enabled :can-send-fan-mail :followed :ask-anon
    :ask-page-title
    (map-field :ask-p :ask)
    (map-field :nsfw-p :is-nsfw)
    (map-field :draft-count :drafts)
    (map-field :queue-count :queue)
    (map-field :message-count :messages)
    (map-field :post-count :posts)
    (map-field :like-count :likes)))

(defun make-user (result)
  (let ((user (change-class (make-blog result) 'user)))
    (make-from-result result user
      (map-field :default-post-format :default-post-format #'kaget)
      (map-field :following-count :following)
      (map-field-list :blogs :blogs #'make-blog))
    user))

(defun make-trail (result)
  (make-from-result result 'trail
    :content :post
    (map-field :raw-content :content-raw)
    (map-field :root-p :is-root-item #'naget)
    (map-field :current-p :is-current-item #'naget)
    (map-field :blog :blog (lambda (r f) (make-blog (saget r f))))))

(defun make-raw-post (result)
  (make-from-result result 'post
    :id :blog-name :post-url :timestamp :reblog-key :liked :tags :short-url :summary
    (map-field :post-type :type #'kaget)
    (map-field :date :date #'daget)
    (map-field :text-format :format #'kaget)
    (map-field :bookmarklet :bookmarklet #'naget)
    (map-field :mobile :mobile #'naget)
    (map-field :source-url :source-url #'naget)
    (map-field :source-title :source-title #'naget)
    (map-field :state :state #'kaget)
    (map-field :note-count :note-count #'naget)
    (map-field-list :trail :trail #'make-trail #'naget)))

;; Here we define an extra keyword called 'submission', which is
;; an additionally possible STATE for a post, but isn't allowed
;; when making a post.
:submission

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
            (:answer 'answer-post)))
    (make-from-result result post
      ;; post-specific fields
      :title :body :caption :width :height :text :source :url
      :description :album-art :artist :album :track-name
      :track-number :year :asking-name :asking-url :question :answer
      (map-field-list :photos :photos #'make-photo)
      (map-field-list :dialogue :dialogue #'make-dialogue)
      (map-field :play-count :play-count))
    ;; Special handling for audio and video players
    ;; since they have the same field name but vastly different
    ;; results. Good job, tumblr.
    (case (post-type post)
      (:audio (make-from-result result post :player))
      (:video (make-from-result result post (map-field-list :players :player #'make-video-player))))
    post))
