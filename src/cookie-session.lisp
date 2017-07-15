(in-package :unpyo)

(defparameter *session-key* "_unpyo")
(defparameter *session-timeout* '(30 :minute))

(defvar *cookie-session-alist*)

(defclass cookie-session-mixin ()
  ((cookie-session-key)))

(defmethod initialize-instance :after ((self cookie-session-mixin)
                                       &key secret-key &allow-other-keys)
  (setf (slot-value self 'cookie-session-key)
        (generate-key secret-key "合理的無政府主義")))

(defmethod call :around ((self cookie-session-mixin))
  (let ((*cookie-session-alist* :unboud))
    (call-next-method)
    (unless (eq *cookie-session-alist* :unboud)
      (setf (cookie *session-key* :expires *session-timeout* :path "/" :http-only t)
            (encrypt (base64:string-to-base64-string
                      (prin1-to-string *cookie-session-alist*))
                     (slot-value self 'cookie-session-key))))))

(defun load-session-from-cookie ()
  (when (eq *cookie-session-alist* :unboud)
    (setf *cookie-session-alist*
          (awhen (cookie *session-key*)
            (with-standard-io-syntax
              (let ((*read-eval* nil))
                (read-from-string
                 (decrypt (base64:base64-string-to-usb8-array it)
                          (slot-value *application* 'cookie-session-key)))))))))

(defun session (key)
  (load-session-from-cookie)
  (cdr (assoc key *cookie-session-alist* :test #'string-equal)))

(defun (setf session) (value key)
  (load-session-from-cookie)
  (aif (assoc key *cookie-session-alist* :test #'string-equal)
       (setf (cdr it) value)
       (setf *cookie-session-alist* (acons key value *cookie-session-alist*))))

(defun rem-session (key)
  (load-session-from-cookie)
  (setf *cookie-session-alist*
        (remove-if (lambda (x)
                     (string-equal key (car x)))
                   *cookie-session-alist*)))
