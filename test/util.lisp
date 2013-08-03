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

(defclass test-app ()
  ((env :accessor env-of)
   (call-back :initform #'identity :accessor call-back-of)))

(defmethod unpyo:call ((app test-app))
  (setf (env-of app) (unpyo:env-of unpyo:*request*))
  (unwind-protect (funcall (call-back-of app) unpyo:*request*)
    (setf (call-back-of app) #'identity))
  (unpyo:html "OK"))

(defvar *app* (make-instance 'test-app))

(defmacro with-test-server ((server) &body body)
  `(let ((,server (make-instance 'unpyo:server :app *app*)))
     (unpyo::add-tcp-listener ,server *test-host* *test-port*)
     (unpyo:run ,server)
     (unwind-protect
          (progn ,@body)
       (unpyo:stop ,server))))


(def-suite all)
