(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :unpyo)
  (ql:quickload :fiveam)
  (ql:quickload :drakma))

(defpackage :unpyo.test
  (:use :cl :fiveam))

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
  ((env :accessor env-of)))

(defmethod unpyo:call ((app test-app) env)
  (setf (env-of app) env)
  (values 200 nil '("OK")))

(defvar *app* (make-instance 'test-app))

(defmacro with-test-server ((server) &body body)
  `(let ((,server (make-instance 'unpyo:server :app *app*)))
     (unpyo::add-tcp-listener ,server *test-host* *test-port*)
     (unpyo:run ,server)
     (unwind-protect
          (progn ,@body)
       (unpyo:stop ,server))))

(def-suite all)

(def-suite unit :in all)
(in-suite unit)


(def-suite request :in all)
(in-suite request)

(test get-http/1.1
  (iolib.sockets:with-open-socket (s :remote-host *test-host* :remote-port *test-port*)
    (format s "GET / HTTP/1.1~aHost:localhost~a~a" +crlf+ +crlf+ +crlf+)
    (force-output s)
    (is (string= "HTTP/1.1 200 OK" (line s)))
    (let ((env (env-of *app*)))
      (is (string= "GET" (gethash "REQUEST_METHOD" env))))))

(test by-dramka
  (multiple-value-bind (body status)
      (drakma:http-request (format nil "http://~a:~d" *test-host* *test-port*))
    (is (= 200 status))))

(test post-http/1.1
  (iolib.sockets:with-open-socket (s :remote-host *test-host* :remote-port *test-port*)
    (emit s "POST / HTTP/1.1")
    (emit s "Host:localhost")
    (emit s "Content-Length:3")
    (emit s "")
    (format s "a=b")
    (force-output s)
    (is (string= "HTTP/1.1 200 OK" (line s)))
    (let ((env (env-of *app*)))
      (is (string= "POST" (gethash "REQUEST_METHOD" env)))
      (let ((buffer (fast-io:make-octet-vector 10)))
        (is (= 3 (read-sequence buffer (gethash unpyo::+unpyo-input+ env))))
        (is (equal "a=b" buffer))))))

(with-test-server (server)
    (debug!))