(in-package :unpyo)

(defstruct cookie
  name
  value
  expires
  (path "/")
  domain
  (secure t)
  (http-only t))

(defun format-cookie-date (cookie)
  "(make-cookie :name \"a\" :value \"b\" :expires '(3 :minute))
   ;;⇒ a=b; expires=Sat, 02 Nov 2013 13:44:52 GMT"
  (labels ((f (timestamp)
             (local-time:format-timestring
              nil timestamp
              :format '(:short-weekday ", "
                        (:day 2) #\space
                        :short-month #\space
                        (:year 4) #\space
                        (:hour 2) #\:
                        (:min 2) #\:
                        (:sec 2) #\space
                        :timezone)
              :timezone local-time:+gmt-zone+)))
    (atypecase (cookie-expires cookie)
      (null nil)
      (local-time:timestamp
       (f it))
      (list
       (f (local-time:timestamp+ (local-time:now) (car it) (cadr it)))))))

(defmethod print-object ((cookie cookie) stream)
  (format
   stream
   "~a=~a~:[~;~:*; Expires=~a~]~:[~;~:*; Path=~a~]~:[~;~:*; Domain=~a~]~:[~;; Secure~]~:[~;; HttpOnly~]"
   (percent-encode (cookie-name cookie))
   (percent-encode (cookie-value cookie))
   (format-cookie-date cookie)
   (cookie-path cookie)
   (cookie-domain cookie)
   (cookie-secure cookie)
   (cookie-http-only cookie)))

(defun cookie (name)
  (or
   (loop for cookie in (response-cookies *response*)
           thereis (and (string= name (cookie-name cookie))
                        (cookie-value cookie)))
   (awhen (request-cookie *request*)
     (let ((value (cadr (find name (mapcar (lambda (x)
                                             (cl-ppcre:split "=" x :limit 2))
                                           (cl-ppcre:split ";\\s*" it))
                              :test #'string=
                              :key #'car))))
       (and value (percent-decode value))))))

(defun (setf cookie) (value name &rest cookie-init-args)
  (let ((cookies (response-cookies *response*)))
    (setf cookies (delete name cookies :key #'cookie-name :test #'equal))
    (push (apply #'make-cookie :name name :value value cookie-init-args)
          cookies)
    (setf (response-cookies *response*) cookies))
  value)

