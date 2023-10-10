(in-package #:unpyo)

(defun str (&rest args)
  (format nil "~{~a~}" (remove nil args)))

(defun percent-decode (string)
  (when string
    (percent-encoding:decode string :encoding :utf-8 :www-form t)))

(defun percent-encode (string)
  (percent-encoding:encode string :encoding :utf-8 :www-form t
))

(defun split-once (delimiter sequence)
  (let ((position (position delimiter sequence)))
    (if position
        (values (subseq sequence 0 position)
                (subseq sequence (1+ position))))))

(defun map-env (function)
  (mapc (lambda (name=value)
          (multiple-value-call function (split-once #\= name=value)))
        (sb-ext:posix-environ)))


(defgeneric bytesize (x)
  (:method ((string string))
    (babel:string-size-in-octets string))
  (:method (sequence)
    (length sequence)))

(defun string-to-octets (string)
  (babel:string-to-octets string))

(defun octets-to-string (octets)
  (babel:octets-to-string octets))

(defun quote-string (string)
  "Quotes string according to RFC 2616's definition of `quoted-string'."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            unless (or (char< char #\Space)
                       (char= char #\Rubout))
              do (case char
                   ((#\\) (write-string "\\\\" out))
                   ((#\") (write-string "\\\"" out))
                   (otherwise (write-char char out)))))))

(defvar *invoke-debugger-p* t)

(defgeneric my-debugger (condition))

(defmethod my-debugger (e)
  (log4cl:log-error "Error ~a!~%~a" e
                    (with-output-to-string (out)
                      (sb-debug:print-backtrace :stream out)))
  (when *invoke-debugger-p*
    (with-simple-restart (continue "Return from here.")
      (invoke-debugger e))))

(defmacro with-debugger (&body body)
  `(handler-bind ((error #'my-debugger))
     ,@body))


(defun plist-to-query-string (plist)
  (with-output-to-string (out)
    (loop with sep = ""
          for (key value) on plist by #'cddr
          for name = (percent-encode (string-downcase key))
          for values = (alexandria:ensure-list value)
          do (loop for value in values
                   do (format out "~a~a=~a"
                              sep
                              name
                              (percent-encode (princ-to-string value)))
                      (setf sep "&")))))

(defun close-on-exec (socket)
  (let ((fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (sb-posix:fcntl fd sb-posix:f-setfd
                    (logior (sb-posix:fcntl fd sb-posix:f-getfd)
                            1))))       ;FD_CLOEXEC

(defstruct chunk
  vector
  start
  end)

(defun chunk (string)
  (let ((vector (sb-ext:string-to-octets string)))
    (make-chunk :vector vector :start 0 :end (length vector))))

(defun chunk= (a b)
  (and (= (- (chunk-end a) (chunk-start a)) (- (chunk-end b) (chunk-start b)))
       (not (loop with u = (chunk-vector a)
                  with v = (chunk-vector b)
                  for i from (chunk-start a) below (chunk-end a)
                  for j from (chunk-start b)
                    thereis (/= (aref u i) (aref v j))))))

(defun chunk-equal (a b)
  (and (= (- (chunk-end a) (chunk-start a)) (- (chunk-end b) (chunk-start b)))
       (not (loop with u = (chunk-vector a)
                  with v = (chunk-vector b)
                  for i from (chunk-start a) below (chunk-end a)
                  for j from (chunk-start b)
                    thereis (char-not-equal (code-char (aref u i))
                                            (code-char (aref v j)))))))

(defun chunk-to-string (chunk)
  (sb-ext:octets-to-string (chunk-vector chunk)
                           :start (chunk-start chunk)
                           :end (chunk-end chunk)))

#+nil
(defun dd (str &rest args)
  (apply #'format *trace-output* (concatenate 'string "~&" str) args))
(defun dd (str &rest args)
  (declare (ignore str args)))
