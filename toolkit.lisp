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
  "Performs a raw request, with JSON as text.

URL         --- Url to request, see DRAKMA:HTTP-REQUEST
METHOD      --- Request method, see DRAKMA:HTTP-REQUEST
PARAMETERS  --- List of parameters, see DRAKMA:HTTP-REQUEST
OAUTH       --- Whether to use oAuth signinf or not if T, will use SOUTH:SIGNED-REQUEST.
REDIRECT    --- How many redirects to allow, see DRAKMA:HTTP-REQUEST"
  (let ((drakma:*text-content-types* (cons '("application" . "json")
                                           (cons '("text" . "json")
                                                 drakma:*text-content-types*))))
    (if oauth
        (south:signed-request url :parameters parameters :method method :drakma-params `(:redirect ,redirect))
        (drakma:http-request url :method method :parameters parameters :redirect redirect))))

(defun raw-data-request (url &key parameters data-parameters (redirect 10) &allow-other-keys)
  "Same as RAW-REQUEST, except always uses SOUTH:SIGNED-DATA-PARAMETERS-REQUEST."
  (let ((drakma:*text-content-types* (cons '("application" . "json")
                                           (cons '("text" . "json")
                                                 drakma:*text-content-types*))))
    (south:signed-data-parameters-request url :data-parameters data-parameters :parameters parameters :drakma-params `(:redirect ,redirect))))

(defun request (url &rest args &key (method :get) parameters oauth (redirect 10) (request-fun #'raw-request) &allow-other-keys)
  "Wrapper around RAW-REQUEST, automatically parsing the JSON or producing a nice error on failure."
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
  "Same as REQUEST, except using RAW-DATA-REQUEST in the back."
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

(defun aget (key alist &optional default)
  (let ((cons (assoc key alist)))
    (if cons
        (cdr cons)
        default)))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun get-unix-time ()
  "Returns an integer representing the seconds since the unix epoch."
  (- (get-universal-time) *unix-epoch-difference*))

(defparameter *tumblr-datetime-format* '((:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2) " GMT")
  "Local-time format to produce tumblr's datetime format.")

(defun format-tumblr-date (timestamp)
  "Returns a string version of the local-time timestamp using the proper timezone and format."
  (local-time:format-timestring
   NIL (local-time:adjust-timestamp timestamp (offset :sec (- (nth-value 9 (local-time:decode-timestamp timestamp)))))
   :format *tumblr-datetime-format*))

(defun parse-tumblr-date (datestring)
  "Parses a tumblr datestring (2014-08-01 23:52:31 GMT) into a local-time timestamp.
If it fails to parse, the datestring is returned instead."
  (or (cl-ppcre:register-groups-bind (year month day hour min sec) ("(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+):(\\d+)" datestring)
        (local-time:encode-timestamp
         0 (parse-integer (or sec "")) (parse-integer (or min "")) (parse-integer (or hour ""))
         (parse-integer (or day "")) (parse-integer (or month "")) (parse-integer (or year ""))
         :timezone local-time:+gmt-zone+))
      datestring))

(defun pageinate (function offset amount &rest args)
  "Gather results from FUNCTION until AMOUNT is gathered.
The function needs to accept both OFFSET and LIMIT keywords.
As per default for tumblr calls, the objects are gathered in
steps of twenty."
  (flet ((call (offset amount)
           (apply function :offset offset :limit amount args)))
    (if (<= amount 20)
        (call offset amount)
        (loop for current-amount = amount
                then (- current-amount (length current-set))
              for current-offset = offset
                then (+ current-offset (length current-set))
              until (<= current-amount 0)
              for current-set = (call current-offset (if (<= current-amount 20)
                                                         current-amount 20))
              while current-set
              nconcing current-set))))

(defun pageinate-id (function before-id offset amount &rest args)
  "Gather results from FUNCTION until AMOUNT is gathered.
The function needs to accept a BEFORE-ID keyword and return
a list of objects that have an ID slot accessible through
the ID reader.

The returned set may be less than the requested amount."
  (flet ((call (before-id)
           (apply function :before-id before-id args)))
    (loop for current-amount = 0
            then (+ current-amount (length current-set))
          for current-id = before-id
            then (id (car (last current-set)))
          until (<= (+ amount offset) current-amount)
          for current-set = (call current-id)
          while current-set
          nconcing current-set into result
          ;; We might have gathered too much, cut back.
          finally (cond ((<= current-amount offset)
                         ())
                        ((<= current-amount (+ offset amount))
                         (subseq result offset))
                        (T
                         (subseq result offset (+ offset amount)))))))

(defun print-slots (object &key (omit-unbound T))
  "Prints all slots of the object and their values."
  (loop for slotdef in (c2mop:class-slots (class-of object))
        for slot = (c2mop:slot-definition-name slotdef)
        do (if (slot-boundp object slot)
               (format T "~a: ~s~%" slot (slot-value object slot))
               (unless omit-unbound
                 (format T "~a: UNBOUND~%" slot)))))
