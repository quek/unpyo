(in-package :unpyo.test)

(defparameter *test-host* "localhost")
(defparameter *test-port* 7775)

(alexandria:define-constant +crlf+ (coerce '(#\cr #\lf) 'string) :test 'equal)

(defun line (s)
  (let ((line (read-line s nil)))
    (when line
      (string-trim '(#\cr) line))))

(defun emit (stream format &rest args)
  (apply #'format stream (format nil "~a~a" format +crlf+) args))

(defclass test-app (unpyo:application)
  (qq(call-back :initform #'identity :accessor call-back-of)))

(unpyo:defaction /root (:path "/")
  (unwind-protect (funcall (call-back-of unpyo:*application*) unpyo:*request*)
    (setf (call-back-of unpyo:*application*) #'identity)))

(defvar *app* (make-instance 'test-app))

(defmacro with-test-server ((server) &body body)
  `(let ((,server (unpyo:make-server :app *app* :port *test-port*)))
     (unpyo:start ,server)
     (unwind-protect
          (progn ,@body)
       (unpyo:stop ,server))))

(defun test-url (path)
  (format nil "http://~a:~d~a" *test-host* *test-port* path))


(def-suite all)
