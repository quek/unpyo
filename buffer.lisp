(in-package :unpyo)

(defun make-buffer (&key static)
  (fast-io::make-output-buffer :output (if static :static nil)))

(defmethod reset ((buffer fast-io::output-buffer) &key fast-check)
  (declare (ignore fast-check))
  (setf (fast-io::output-buffer-fill buffer) 0
        (fast-io::output-buffer-len buffer) 0
        (fast-io::output-buffer-queue buffer) nil
        (fast-io::output-buffer-last buffer) nil))

(defun bwrite (buffer &rest args)
  (loop for i in args
        do (%brwite buffer i)))

(defgeneric %brwite (buffer x)
  (:method (buffer (x string))
    (fast-io:fast-write-sequence (string-to-octets x) buffer))
  (:method (buffer (x vector))
    (fast-io:fast-write-sequence x buffer))
  (:method (buffer x)
    (fast-io:fast-write-sequence (string-to-octets (princ-to-string x)) buffer)))

(defun buffer-to-vector (buffer)
  (fast-io::finish-output-buffer buffer))

(defmacro with-buffer ((buffer &key static) &body body)
  `(let ((,buffer (make-buffer :static ,static)))
     ,@body))

(defun buffer-length (buffer)
  (fast-io::output-buffer-len buffer))
