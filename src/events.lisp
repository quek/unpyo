(in-package #:unpyo)

(defclass events ()
  ((stdout :initarg :stdout :reader stdout-of)
   (stderr :initarg :stderr :reader stderr-of)
   (on-booted :initform ())))


(defmethod events-log ((self events) string)
  (write-line string (slot-value self 'stdout)))

(defmethod events-write ((self events) string)
  (write-string string (slot-value self 'stdout)))

(defmethod events-error ((self events) string)
  (format (slot-value self 'stderr) "ERROR: ~a" string)
  (error string))

(defmethod evets-parse-error ((self events) server env error)
  (with-slots (stderr) self
    (format stderr
           "~a: HTTP parse error, malformaed request (~a): ~a"
           (local-time:now)
           (or (value env "HTTP_X_FORWARDED_FOR")
               (value env "REMOTE_ADDR"))
           error)
    (format stderr
            "~a: ENV: ~a~&---~&"
            (local-time:now)
            env)))

(defmethod unknow-error ((self events) server error &optional (kind "Unknown"))
  (with-slots (stderr) self
    (format stderr "~a: ~a error: ~a"
            (local-time:now)
            kind
            error)
    (trivial-backtrace:print-backtrace error :output stderr)))

(defmethod on-booted ((self events) lambda)
  (push lambda (slot-value self 'on-booted)))

(defmethod fire-on-booted ((self events))
  (mapc #'cl:funcall (reverse (slot-value self 'on-booted))))

(defun make-strings-events ()
  (make-instance 'events :stdout (make-string-output-stream)
                         :stderr (make-string-output-stream)))

(defun make-stdio-events ()
  (make-instance 'events :stdout *standard-output*
                         :stderr *error-output*))

(defvar *default-events* (make-stdio-events))


(defclass pid-events (events)
  ())

(flet ((f (string)
         (format nil "[~a] ~a" (iolib.syscalls:getpid) string)))

  (defmethod events-log ((self pid-events) string)
    (call-next-method self (f string)))

  (defmethod events-write ((self pid-events) string)
    (call-next-method self (f string)))

  (defmethod events-error ((self pid-events) string)
    (call-next-method self (f string))))
