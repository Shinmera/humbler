#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defgeneric blog= (a b)
  (:method ((a blog) (b blog))
    (string-equal (name a) (name b)))
  (:method ((a blog) (b string))
    (string-equal (name a) b))
  (:method ((a string) (b blog))
    (string-equal a (name b)))
  (:method ((a string) (b string))
    (string-equal a b)))

(defgeneric post= (a b)
  (:method ((a post) (b post))
    (= (id a) (id b)))
  (:method ((a post) (b fixnum))
    (= (id a) b))
  (:method ((a fixnum) (b post))
    (= a (id b)))
  (:method ((a fixnum) (b fixnum))
    (= a b)))

(defgeneric augment (target source)
  (:method (target source)
    (unless (eql (class-of target) (class-of source))
      (warn "Attempting to augment ~a with ~a, which do not match in class." target source))
    (flet ((slots (object)
             (mapcar #'c2mop:slot-definition-name (c2mop:class-slots (class-of object)))))
      (loop with target-slots = (slots target)
            for slot in (slots source)
            when (and (slot-boundp source slot)
                      (find slot target-slots))
              do (setf (slot-value target slot)
                       (slot-value source slot))))
    target)
  (:method ((target post) (source post))
    ;; raw post (somehow), change to source.
    (when (and (eql (class-of target) (find-class 'post))
               (not (eql (class-of source) (find-class 'post))))
      (warn "Changing class of raw-post target (~a) to that of source (~a)" target source)
      (setf target (change-class target (class-name (class-of source)))))
    ;; differing post sub-classes, error out.
    (unless (or (eql (class-of target) (class-of source))
                (eql (class-of source) (find-class 'post)))
      (error "Cannot augment ~a with ~a, different post-subclasses are incompatible." target source))
    (call-next-method target source)))

(defgeneric blog (name)
  (:method ((blog blog))
    (blog (blog-name blog)))
  (:method ((blog string))
    (make-blog (blog/info blog))))

(defgeneric followers (blog &key amount offset)
  (:method ((blog blog) &key (amount 20) (offset 0))
    (followers (blog-name blog) :amount amount :offset offset))
  (:method ((blog string) &key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-user (apply #'blog/followers args)))
               offset amount)))

(defgeneric follow (blog)
  (:method ((blog blog))
    (follow (blog-name blog)))
  (:method ((blog string))
    (user/follow blog)))

(defgeneric unfollow (blog)
  (:method ((blog blog))
    (unfollow (blog-name blog)))
  (:method ((blog string))
    (user/unfollow blog)))

(defgeneric likes (blog &key amount offset)
  (:method ((blog blog) &key (amount 20) (offset 0))
    (likes (blog-name blog) :amount amount :offset offset))
  (:method ((blog string) &key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-post (apply #'blog/likes args)))
               offset amount)))

;; needs special handling since there is no 'amount'.
(defgeneric submissions (blog &key amount offset)
  (:method ((blog blog) &key (amount 20) (offset 0))
    (submissions (blog-name blog) :amount amount :offset offset))
  (:method ((blog string) &key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-post (apply #'blog/posts/submission args)))
               offset amount)))

;; needs special handling since there is no 'offset' or 'amount'.
(defgeneric drafts (blog &key amount offset)
  (:method ((blog blog) &key (amount 20) (offset 0))
    (drafts (blog-name blog) :amount amount :offset offset))
  (:method ((blog string) &key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-post (apply #'blog/posts/draft args)))
               offset amount)))

(defgeneric queue (blog &key amount offset)
  (:method ((blog blog) &key (amount 20) (offset 0))
    (queue (blog-name blog) :amount amount :offset offset))
  (:method ((blog string) &key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-post (apply #'blog/posts/queue args)))
               offset amount)))

(defgeneric posts (blog &key type tag amount offset)
  (:method ((blog blog) &key type tag (amount 20) (offset 0))
    (posts (blog-name blog) :type type :tag tag :amount amount :offset offset))
  (:method ((blog string) &key type tag (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-post (apply #'blog/posts args)))
               offset amount :type type :tag tag)))

(defgeneric post (id &optional blog-name)
  (:method ((post post) &optional blog-name)
    (post (id post) (blog-name post)))
  (:method ((id fixnum) &optional blog-name)
    (make-post (blog/posts blog-name :id id))))

(defun %edit-post (post &rest args)
  (apply
   #'blog/post/edit
   (blog-name post) (id post)
   :tags (tags post) :state (state post) :tweet (tweet post)
   :date (date post) :format (post-format post) :slug (slug post)
   args))

(defun %save-post (post main &rest args)
  (apply
   (let ((name (string (class-name (class-of post)))))
     ;; hackery!
     (find-symbol (format NIL "BLOG/POST-~a"
                          (subseq name 0 (position #\- name)))))
   (blog-name post) main
   :tags (tags post) :state (state post) :tweet (tweet post)
   :date (date post) :format (post-format post) :slug (slug post)
   args))

(defgeneric save (post)
  (:method ((post text-post))
    (if (slot-boundp post 'id)
        (%edit-post post
                    :body (body post)
                    :title (title post))
        (%save-post post (body post)
                    :title (title post)))
    post)

  (:method ((post photo-post))
    (if (slot-boundp post 'id)
        (%edit-post post
                    :photo (file post)
                    :caption (caption post)
                    :link (source-url post))
        (%save-post post (photos post)
                    :caption (caption post)
                    :link (source-url post)))
    post)

  (:method ((post quote-post))
    (if (slot-boundp post 'id)
        (%edit-post post
                    :quote (text post)
                    :source (source post))
        (%save-post post (text post)
                    :source (source post)))
    post)

  (:method ((post link-post))
    (if (slot-boundp post 'id)
        (%edit-post post
                    :url (url post)
                    :description (description post)
                    :title (title post))
        (%save-post post (url post)
                    :description (description post)
                    :title (title post)))
    post)

  (:method ((post chat-post))
    (if (slot-boundp post 'id)
        (%edit-post post
                    :body (body post)
                    :title (title post))
        (%save-post post (body post)
                    :title (title post)))
    post)

  (:method ((post audio-post))
    (if (slot-boundp post 'id)
        (%edit-post post
                    :audio (file post)
                    :caption (caption post))
        (%save-post post (file post)
                    :caption (caption post)))
    post)

  (:method ((post video-post))
    (if (slot-boundp post 'id)
        (%edit-post post
                    :video (file post)
                    :caption (caption post))
        (%save-post post (file post)
                    :caption (caption post)))
    post)

  (:method ((post answer-post))
    (error "I don't know how to handle this (yet), the documentation doesn't say anything.")))

(defgeneric destroy (post)
  (:method ((post post))
    (blog/post/delete (blog-name post) (id post))
    post))

(defgeneric reblog (post &key comment)
  (:method ((post post) &key comment)
    (make-post
     (blog/post/reblog (blog-name post) (id post) (reblog-key post) :comment comment))))

(defgeneric like (post)
  (:method ((post post))
    (user/like (id post) (reblog-key post))
    post))

(defgeneric unlike (post)
  (:method ((post post))
    (user/unlike (id post) (reblog-key post))
    post))

(defgeneric refresh (post)
  (:method ((post post))
    (augment post (post post))))

(defgeneric dashboard (&key amount offset)
  (:method (&key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-post (apply #'user/dashboard args)))
               offset amount :reblog-info T :notes-info T)))

(defgeneric following (&key amount offset)
  (:method (&key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-blog (apply #'user/following args)))
               offset amount)))

(defgeneric myself ()
  (:method ()
    (make-user (user/info))))

(defgeneric my-followers (&key amount offset)
  (:method (&key (amount 20) (offset 0))
    (followers *user* :amount amount :offset offset)))

(defgeneric my-likes (&key amount offset)
  (:method (&key (amount 20) (offset 0))
    (likes *user* :amount amount :offset offset)))

(defgeneric my-submissions (&key amount offset)
  (:method (&key (amount 20) (offset 0))
    (submissions *user* :amount amount :offset offset)))

(defgeneric my-drafts (&key amount offset)
  (:method (&key (amount 20) (offset 0))
    (drafts *user* :amount amount :offset offset)))

(defgeneric my-queue (&key amount offset)
  (:method (&key (amount 20) (offset 0))
    (queue *user* :amount amount :offset offset)))

(defgeneric my-posts (&key type tag amount offset)
  (:method (&key type tag (amount 20) (offset 0))
    (posts *user* :type type :tag tag :amount amount :offset offset)))
