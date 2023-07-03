(in-package #:org.tymoonnext.humbler)

(defparameter *oauth/request-token* "https://www.tumblr.com/oauth/request_token")
(defparameter *oauth/authorize* "https://www.tumblr.com/oauth/authorize")
(defparameter *oauth/access-token* "https://www.tumblr.com/oauth/access_token")

(defvar *user* NIL
  "The central user object storing a reference to whoever is currently logged in.

Note that accessing this variable directly might result in NIL
even if the oAuth handshake has been completed. Use (*USER*),
the function, instead to ensure that the returned value is valid.")

(defun *user* ()
  "Returns the central user object that the client is authenticated as."
  (or *user*
      (when *client*
        (setf *user* (myself)))))

(defvar *client* NIL
  "The client object used to access the Tumblr API.")

(defclass client (north:client)
  ()
  (:default-initargs
   :callback :spawn
   :request-token-uri *oauth/request-token*
   :authorize-uri *oauth/authorize*
   :access-token-uri *oauth/access-token*))

(defmacro with-maybe-drakma-handling (&body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk ()
              ,@body))
       (if (asdf:component-loaded-p (asdf:find-system :north-drakma))
           (progv (list (find-symbol ,(string :*TEXT-CONTENT-TYPES*) "DRAKMA"))
               '((("application" . "json") ("text")))
             (,thunk))
           (,thunk)))))

(defun raw-request (url &key (method :get) parameters oauth &allow-other-keys)
  "Performs a raw request, with JSON as text.

URL         --- Url to request
METHOD      --- Request method
PARAMETERS  --- List of parameters
OAUTH       --- Whether to use oAuth signing or not."
  (with-maybe-drakma-handling
    (if oauth
        (north:make-signed-request *client* url method :params parameters)
        (north:call (north:make-request url method :params parameters)))))

(defun raw-data-request (url &key parameters data-parameters &allow-other-keys)
  "Same as RAW-REQUEST, except uses a signed data payload request."
  (with-maybe-drakma-handling
    (north:make-signed-data-request *client* url data-parameters :params parameters)))

(defun request (url &rest args &key (method :get) parameters oauth (request-fun #'raw-request) &allow-other-keys)
  "Wrapper around RAW-REQUEST, automatically parsing the JSON or producing a nice error on failure."
  (declare (ignore method parameters oauth))
  (let* ((body (apply request-fun url args))
         (data (if (stringp body)
                   (cdr (assoc :response (yason:parse body :object-as :alist :object-key-fn #'to-keyword)))
                   body)))
    data))

(defun data-request (url &rest args &key parameters data-parameters (redirect 10))
  "Same as REQUEST, except using RAW-DATA-REQUEST in the back."
  (declare (ignore parameters redirect data-parameters))
  (apply #'request url :request-fun #'raw-data-request args))

(defun login (&optional (client *client*))
  "Performs a login with North.

This will load Hunchentoot and spawn an HTTP server if the
client's callback is :SPAWN. The server will be automatically
torn down again once the oAuth handshake is complete."
  (case (north:callback client)
    (:oob
     (error "OOB login method not available for Tumblr."))
    (:spawn
     (start-hunchentoot-server client)
     (north:initiate-authentication client))
    (T
     (north:initiate-authentication client))))

(defun logout ()
  "Resets the *client* and *user* to NIL, thus logging you out."
  (setf *client* NIL)
  (setf *user* NIL))

(defun handle-hunchentoot (server client request)
  (flet ((hunchentoot (symbol &rest args)
           (apply (find-symbol (string symbol) "HUNCHENTOOT") args)))
    (let ((token (hunchentoot :get-parameter "oauth_token" request))
          (verifier (hunchentoot :get-parameter "oauth_verifier" request)))
      (when (and token verifier)
        (let ((stream (hunchentoot :send-headers)))
          (handler-case
              (progn (north:complete-authentication client verifier token)
                     (write-sequence (flexi-streams:string-to-octets "Tumblr login done.") stream))
            (error (e)
              (write-sequence (flexi-streams:string-to-octets (format NIL "Tumblr login failed:~%~a" e)) stream)))
          (finish-output stream))
        (hunchentoot :stop server)))))

(defun start-hunchentoot-server (client &key (port (random-port)))
  #+quicklisp (ql:quickload :hunchentoot)
  #-quicklisp (asdf:load-system :hunchentoot)
  (flet ((hunchentoot (symbol)
           (find-symbol (string symbol) "HUNCHENTOOT")))
    (eval `(progn (defclass server (,(hunchentoot :acceptor)) ())
                  (defmethod ,(hunchentoot :acceptor-dispatch-request) ((server server) request)
                    (handle-hunchentoot server ,client request))))
    (let ((server (make-instance 'server :port port
                                         :message-log-destination NIL
                                         :access-log-destination NIL)))
      (funcall (hunchentoot :start) server)
      (setf (north:callback client) (format NIL "http://localhost:~d/" port)))))
