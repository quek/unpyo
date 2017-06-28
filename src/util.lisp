(in-package #:unpyo)

(defun str (&rest args)
  (format nil "~{~a~}" (remove nil args)))

(defun percent-decode (string)
  (when string
    (percent-encoding:decode string :encoding :utf-8 :www-form t)))

(defun percent-encode (string)
  (percent-encoding:encode string :encoding :utf-8 :www-form t))

(defun split-once (delimiter sequence)
  (let ((position (position delimiter sequence)))
    (if position
        (values (subseq sequence 0 position)
                (subseq sequence (1+ position))))))

(defun map-env (function)
  (mapc (lambda (name=value)
          (multiple-value-call function (split-once #\= name=value)))
        (sb-ext:posix-environ)))


(defun %epoll-ctl (fd epfd op &rest events)
  (cffi:with-foreign-object (ev '(:struct isys:epoll-event))
    (isys:bzero ev (isys:sizeof 'isys:epoll-event))
    (setf (cffi:foreign-slot-value ev '(:struct isys:epoll-event) 'isys:events)
          (apply #'logior events))
    (let ((epoll-data (cffi:foreign-slot-value ev '(:struct isys:epoll-event) 'isys:data)))
      (setf (cffi:foreign-slot-value epoll-data '(:union isys:epoll-data) 'isys:fd)
            fd))
    (handler-case
        (isys:epoll-ctl epfd op fd ev)
      (isys:ebadf ()
        (warn "FD ~A is invalid, cannot monitor it." fd))
      (isys:eexist ()
        (warn "FD ~A is already monitored." fd)))))

(defgeneric bytesize (x)
  (:method ((string string))
    (babel:string-size-in-octets string))
  (:method (sequence)
    (length sequence)))

(defun io-select (sockets &key timeout)
  (with-open-stream (epoll-fd (isys:epoll-create 1))
    (let ((hash (make-hash-table)))
      (loop for socket in sockets
            for fd = (fd-of socket)
            do (%epoll-ctl fd epoll-fd isys:epoll-ctl-add isys:epollin isys:epollpri)
               (setf (gethash fd hash) socket))
      (cffi:with-foreign-object (events '(:struct isys:epoll-event) (1- isys:fd-setsize))
        (isys:bzero events (* (1- isys:fd-setsize) (isys:sizeof 'isys:epoll-event)))
        (let ((ready-fds
                (isys:repeat-upon-condition-decreasing-timeout
                    ((isys:eintr) tmp-timeout timeout)
                  (isys:epoll-wait epoll-fd events (1- isys:fd-setsize)
                                   (iomux::timeout->milliseconds tmp-timeout)))))
          (macrolet ((epoll-slot (slot-name)
                       `(cffi:foreign-slot-value (cffi:mem-aref events 'isys:epoll-event i)
                                                 'isys:epoll-event ',slot-name)))
            (loop for i below ready-fds
                  for fd = (cffi:foreign-slot-value (epoll-slot isys:data) 'isys:epoll-data 'isys:fd)
                  for event-mask = (epoll-slot isys:events)
                  do (dd "event-mask ~a" event-mask)
                  when (logtest event-mask isys:epollhup)
                    do (dd "epollhup ~a" (gethash fd hash))
                  collect (gethash fd hash))))))))

#+nil
(defun io-select (sockets &key timeout)
  (let ((result ()))
    (iomux:with-event-base (event-base)
      (loop for socket in sockets
            do (iomux:set-io-handler event-base (fd-of socket)
                                     :read (let ((socket socket))
                                             (lambda (fd event-type errorp)
                                               (declare (ignorable fd event-type))
                                               (dd "io-select ~a ~a ~a" fd event-type errorp)
                                               (unless errorp
                                                 (push socket result))))))
      (iomux:event-dispatch event-base :one-shot t :timeout timeout))
    result))

(defun sysread (fd buffer-porinter buffer-size)
  (aprog1 (isys:read fd buffer-porinter buffer-size)
    (when (zerop it)
      (error 'end-of-file))))

(defmethod read-1 (fd)
  (static-vectors:with-static-vector (vec 1 :initial-element 0)
    (sysread fd (static-vectors:static-vector-pointer vec) 1)
    (aref vec 0)))

(defmethod write-1 (fd byte)
  (static-vectors:with-static-vector (vec 1 :initial-element byte)
    (isys:write fd (static-vectors:static-vector-pointer vec) 1)))

(defun cork (socket)
  (setf (iolib.sockets:socket-option socket :tcp-cork) t))

(defun uncork (socket)
  (setf (iolib.sockets:socket-option socket :tcp-cork) nil))

(defmacro with-cork ((socket) &body body)
  (alexandria:once-only (socket)
    `(progn
       (cork ,socket)
       (unwind-protect (progn ,@body)
         (uncork ,socket)))))

(defmethod close ((fd fixnum) &key abort)
  (declare (ignore abort))
  (isys:close fd))

(defun string-to-octets (string)
  (babel:string-to-octets string))

(defun octets-to-string (octets)
  (babel:octets-to-string octets))

(defun monotonic-time ()
  (iolib.syscalls:get-monotonic-time))

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
  (when *invoke-debugger-p*
    (with-simple-restart (continue "Return from here.")
      (invoke-debugger e))))

(defmacro with-debugger (&body body)
  `(handler-bind ((error #'my-debugger))
     ,@body))


(defun get-cipher (key)
  (ironclad:make-cipher
   :blowfish :mode :ecb :key (string-to-octets key)))

(defun encrypt (plaintext key)
  (let ((cipher (get-cipher key))
        (msg (string-to-octets plaintext)))
    (ironclad:encrypt-in-place cipher msg)
    (ironclad:octets-to-integer msg)))

(defun decrypt (ciphertext-int key)
  (let ((cipher (get-cipher key))
        (msg (ironclad:integer-to-octets ciphertext-int)))
    (ironclad:decrypt-in-place cipher msg)
    (octets-to-string msg)))

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

(defun chunk-to-string (chunk)
  (sb-ext:octets-to-string (chunk-vector chunk)
                           :start (chunk-start chunk)
                           :end (chunk-end chunk)))

#+nil
(defun dd (str &rest args)
  (apply #'format *trace-output* (concatenate 'string "~&" str) args))
(defun dd (str &rest args)
  (declare (ignore str args)))
