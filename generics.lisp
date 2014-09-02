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
    (unless (typep target (type-of source))
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

(defgeneric copy (thing)
  (:method (thing)
    (let ((copy (make-instance (class-name (class-of thing)))))
      (augment copy thing)
      copy)))

(defgeneric blog (name)
  (:method ((blog blog))
    (blog (name blog)))
  (:method ((blog string))
    (make-blog (blog/info blog))))

(defgeneric followers (blog &key amount offset)
  (:method ((blog blog) &key (amount 20) (offset 0))
    (followers (name blog) :amount amount :offset offset))
  (:method ((blog string) &key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-user (apply #'blog/followers blog args)))
               offset amount)))

(defgeneric follow (blog)
  (:method ((blog blog))
    (follow (name blog))
    blog)
  (:method ((blog string))
    (user/follow blog)
    blog))

(defgeneric unfollow (blog)
  (:method ((blog blog))
    (unfollow (name blog))
    blog)
  (:method ((blog string))
    (user/unfollow blog)
    blog))

(defgeneric likes (blog &key amount offset)
  (:method ((blog blog) &key (amount 20) (offset 0))
    (likes (name blog) :amount amount :offset offset))
  (:method ((blog string) &key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-post (apply #'blog/likes blog args)))
               offset amount)))

;; needs special handling since there is no 'amount'.
(defgeneric submissions (blog &key amount offset)
  (:method ((blog blog) &key (amount 20) (offset 0))
    (submissions (name blog) :amount amount :offset offset))
  (:method ((blog string) &key (amount 20) (offset 0))
    (flet ((call (offset)
             (blog/posts/submission blog :offset offset)))
      (loop for current-amount = amount
              then (- current-amount (length current-set))
            for current-offset = offset
              then (+ current-offset (length current-set))
            until (<= current-amount 0)
            for current-set = (call current-offset)
            while current-set
            nconcing current-set into result
            ;; We might have gathered too much, cut back.
            finally (if (< current-amount 0)
                        (subseq result 0 amount)
                        result)))))

;; needs special handling since there is no 'offset' or 'amount'.
(defgeneric drafts (blog &key amount offset before-id)
  (:method ((blog blog) &key (amount 20) (offset 0) (before-id 0))
    (drafts (name blog) :amount amount :offset offset :before-id before-id))
  (:method ((blog string) &key (amount 20) (offset 0) (before-id 0))
    (pageinate-id #'(lambda (&rest args)
                      (mapcar #'make-post (apply #'blog/posts/draft args)))
                  before-id offset amount)))

(defgeneric queue (blog &key amount offset)
  (:method ((blog blog) &key (amount 20) (offset 0))
    (queue (name blog) :amount amount :offset offset))
  (:method ((blog string) &key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-post (apply #'blog/posts/queue blog args)))
               offset amount)))

(defgeneric posts (blog &key type tag amount offset)
  (:method ((blog blog) &key type tag (amount 20) (offset 0))
    (posts (name blog) :type type :tag tag :amount amount :offset offset))
  (:method ((blog string) &key type tag (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-post (apply #'blog/posts blog args)))
               offset amount :type type :tag tag)))

(defgeneric post (id &optional blog-name)
  (:method ((post post) &optional blog-name)
    (post (id post) (or blog-name (blog-name post))))
  (:method ((id fixnum) &optional blog-name)
    (assert (not (null blog-name))
            () "Blog-name required when fetching a post by ID.")
    (make-post (blog/posts blog-name :id id))))

(defun %edit-post (post &rest args)
  (apply
   #'blog/post/edit
   (blog-name post) (id post)
   :tweet (tweet post) :tags (tags post)
   :slug (slug post) :date (date post)
   :state (or (state post) :published)
   :format (or (post-format post) (default-post-format *user*) :html)
   args))

(defun %save-post (post main &rest args)
  (setf (slot-value post 'blog-name)
        (or (blog-name post) (name *user*)))
  (setf (slot-value post 'id)
        (apply
         (let ((name (string (class-name (class-of post)))))
           ;; hackery!
           (find-symbol (format NIL "BLOG/POST-~a"
                                (subseq name 0 (position #\- name)))))
         (blog-name post) main
         :tweet (tweet post) :tags (tags post)
         :slug (slug post) :date (date post)
         :state (or (state post) :published)
         :format (or (post-format post) (default-post-format *user*) :html)
         args))
  post)

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
        (%save-post post (file post)
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

(defgeneric repost (post new-blog)
  (:method ((post post) (new-blog blog))
    (repost post (name new-blog)))
  (:method ((post post) (new-blog string))
    (let ((repost (copy post)))
      (slot-makunbound repost 'id)
      (setf (slot-value repost 'blog-name) new-blog)
      (save repost)
      repost)))

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

(defgeneric dashboard (&key amount offset type)
  (:method (&key (amount 20) (offset 0) type)
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-post (apply #'user/dashboard args)))
               offset amount :reblog-info T :notes-info T :type type)))

(defgeneric following (&key amount offset)
  (:method (&key (amount 20) (offset 0))
    (pageinate #'(lambda (&rest args)
                   (mapcar #'make-blog (apply #'user/following args)))
               offset amount)))

(defgeneric myself ()
  (:method ()
    (make-user (user/info))))

(defgeneric my-blogs ()
  (:method ()
    (blogs *user*)))

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

(defgeneric tag (tag &key amount offset before)
  (:method (tag &key (amount 20) (offset 0) before)
    (pageinate-time #'(lambda (&rest args)
                        (mapcar #'make-post (apply #'tagged tag args)))
                    before offset amount)))
