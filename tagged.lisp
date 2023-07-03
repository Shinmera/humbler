(in-package #:org.tymoonnext.humbler)

(defvar *tagged* "https://api.tumblr.com/v2/tagged")

(defun tagged (tag &key before (limit 20) filter)
  (assert (<= 1 limit 20)
          () "Limit must be between 1 and 20 (inclusive).")
  (assert (member filter '(NIL :text :raw))
          () "Filter must be one of (NIL :text :raw)")
  (request *tagged* :parameters (cons `("api_key" . ,(north:key *client*))
                                      (prepare* tag before limit filter))))
