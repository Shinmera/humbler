#|
 This file is a part of Humbler
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.humbler)

(defun to-keyword (string)
  "Turns a key into a keyword.
Replaces _ with - and uppercases the string, then interns it
into the keyword package. This is useful to parse the request
responses into an alist."
  (let ((name (cl-ppcre:regex-replace-all "_" (string-upcase string) "-")))
    (or (find-symbol name "KEYWORD") (intern name "KEYWORD"))))

(defun from-keyword (keyword)
  "Turns a keyword into a key.
Replaces - with _ and downcases the keyword as a string.
This is useful to parse the request parameters from the
lisp representation into the api representation."
  (cl-ppcre:regex-replace-all "-" (string-downcase keyword) "_"))

(defun raw-request (url &key (method :get) parameters oauth (redirect 10) &allow-other-keys)
  (let ((drakma:*text-content-types* (cons '("application" . "json")
                                           (cons '("text" . "json")
                                                 drakma:*text-content-types*))))
    (if oauth
        (south:signed-request url :parameters parameters :method method :drakma-params `(:redirect ,redirect))
        (drakma:http-request url :method method :parameters parameters :redirect redirect))))

(defun raw-data-request (url &key parameters data-parameters (redirect 10) &allow-other-keys)
  (let ((drakma:*text-content-types* (cons '("application" . "json")
                                           (cons '("text" . "json")
                                                 drakma:*text-content-types*))))
    (south:signed-data-parameters-request url :data-parameters data-parameters :parameters parameters :drakma-params `(:redirect ,redirect))))

(defun request (url &rest args &key (method :get) parameters oauth (redirect 10) (request-fun #'raw-request) &allow-other-keys)
  (declare (ignore method parameters oauth redirect))
  (let ((data (multiple-value-list (apply request-fun url args))))
    (destructuring-bind (body status headers &rest dont-care) data
      (declare (ignore dont-care))
      (let ((data
              (let ((content-type (cdr (assoc :content-type headers))))
                (cond
                  ((search "json" content-type)
                   (cons (cdr (assoc :response (yason:parse body :object-as :alist :object-key-fn #'to-keyword)))
                         (cdr data)))
                  (T data)))))
        (unless (< status 400)
          (error "Error during request: ~s" data))
        (values-list data)))))

(defun data-request (url &rest args &key parameters data-parameters (redirect 10))
  (declare (ignore parameters redirect data-parameters))
  (apply #'request url :request-fun #'raw-data-request args))

(defun prepare (parameters)
  "Filters out empty key-value pairs and turns all values
into strings, ready to be sent out as request parameters.
This function is DESTRUCTIVE."
  (mapc #'(lambda (pair)
            (setf (car pair) (from-keyword (car pair)))
            (setf (cdr pair) (typecase (cdr pair)
                               (string (cdr pair))
                               (boolean "true")
                               (keyword (string-downcase (cdr pair)))
                               (t (princ-to-string (cdr pair))))))
        (delete () parameters :key #'cdr)))

(defmacro prepare* (&rest parameter-names)
  "Creates a PREPARE statement out of the provided variables."
  `(prepare (list ,@(mapcar #'(lambda (a)
                                (if (consp a)
                                    `(cons ,(from-keyword (car a)) ,(cdr a))
                                    `(cons ,(from-keyword a) ,a)))
                            parameter-names))))

(defun aget (key alist)
  (cdr (assoc key alist)))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun get-unix-time ()
  (- (get-universal-time) *unix-epoch-difference*))
