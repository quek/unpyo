(in-package :unpyo)

(defclass client ()
  ((io :initarg :io :reader io-of)
   (proto-env :initarg :env)
   (env :reader env-of)
   (parser :initform (make-instance 'http-parser))
   (parsed-bytes :initform 0)
   (read-header :initform t)
   (ready :initform nil :reader ready-p)
   (body :initform nil :reader body-of)
   (body-remain)
   (buffer :initform nil)
   (timeout-at :initform nil :reader timeout-at-of)
   (requests-served :initform 0)
   (hijacked :initform nil :reader hijacked-p)))

(defmethod initialize-instance :after ((self client) &key)
  (with-slots (env io proto-env) self
    (setf env (alexandria:copy-hash-table proto-env))))

(defmethod fd-of ((self client))
  (with-slots (io) self
    (fd-of io)))

#+nil
(defmethod call ((self client))
  (with-slots (io hijacked) self
    (setf hijacked t)
    (setf (gethash +hijack-io+ (env-of self)) io)))

(defmethod set-timeout ((self client) seconds)
  (with-slots (timeout-at) self
    (setf timeout-at (+ seconds (monotonic-time)))))

(defmethod reset ((self client) &key (fast-check t))
  (with-slots (env body buffer io parsed-bytes parser proto-env read-header ready) self
    (reset parser)
    (setf read-header t)
    (setf env (alexandria:copy-hash-table proto-env))
    (when body (close body))
    (setf body nil)
    (setf parsed-bytes 0)
    (setf ready nil)
    (cond (buffer
           (setf parsed-bytes (execute parser env (buffer-to-vector buffer) parsed-bytes))
           (cond ((finished-p parser)
                  (setup-body self))
                 ((>= parsed-bytes +max-header+)
                  (error 'http-parse-error
                         :format-control "HEADER is longer than allowed, aborting client early."))
                 (t nil)))
          ((and fast-check (io-select (list io) :timeout +fast-track-ka-timeout+))
           (print 'try-to-finish--from-client-reset)
           (try-to-finish self))
          (t nil))))

(defmethod close ((self client) &key abort)
  (dd "close client ~a" self)
  (with-slots (io) self
    (ignore-errors (close io :abort abort))))

(defvar *null-stream* (make-two-way-stream
                       (make-concatenated-stream)
                       (make-broadcast-stream)))

(defmethod close ((stream (eql *null-stream*)) &key abort)
  (declare (ignore abort)))

(defmethod setup-body :before ((self client))
  (with-slots (env) self
    (awhen (gethash "Content-Length" env)
      (setf (gethash "CONTENT_LENGTH" env) it))))

(defmethod eagerly-finish ((self client))
  (print 'eagerly-finish)
  (with-slots (ready io) self
    (cond (ready
           t)
          ((not (iomux:wait-until-fd-ready (fd-of io) :input 0 nil))
           (print 'io-not-ready)
           nil)
          (t
           (print 'tri-to-finish--from-eagerly-finish)
           (try-to-finish self)))))

(defmethod try-to-finish ((self client))
  (dd "try-to-finish")
  (with-slots (buffer env io parsed-bytes parser read-header) self
    (unless read-header
      (return-from try-to-finish (read-body self)))
    (static-vectors:with-static-vector (vec +chunk-size+)
      (let ((read-size (handler-case (sysread (fd-of io)
                                              (static-vectors:static-vector-pointer vec)
                                              +chunk-size+)
                         (isys:ewouldblock ()
                           (return-from try-to-finish nil))
                         ((or isys:syscall-error end-of-file) ()
                           (error 'connection-error
                                  :format-control "Connection error detected during read")))))
        (dd "read-size ~d in try-to-finish ~a" read-size
          (iomux:fd-ready-p (fd-of io) :input))
        (unless buffer (setf buffer (make-buffer)))
        (fast-io:fast-write-sequence vec buffer 0 read-size)))

    (setf parsed-bytes (execute parser env (buffer-to-vector buffer) parsed-bytes))
    (cond ((finished-p parser)
           (setup-body self))
          ((>= parsed-bytes +max-header+)
           (error 'http-parse-error
                  :format-control "HEADER is longer than allowed, aborting client early."))
          (t nil))))

(defmethod setup-body ((self client))
  (dd "setup-body")
  (with-slots (body body-remain buffer env parser read-header ready requests-served) self
    (let* ((parsed-body (body-of parser))
           (parsed-body-length (buffer-length parsed-body))
           (content-length (gethash "CONTENT_LENGTH" env)))
      (unless content-length
        ;; no body, parsed-body is a paret of next request?
        (setf buffer (if (zerop parsed-body-length)
                         nil
                         parsed-body))
        (setf body *null-stream*)
        (incf requests-served)
        (setf ready t)
        (return-from setup-body t))
      (let ((remain (- (parse-integer content-length)
                       parsed-body-length)))
        (when (<= remain 0)
          (setf body (flex:make-in-memory-input-stream (buffer-to-vector parsed-body)))
          (setf buffer nil)
          (incf requests-served)
          (setf ready t)
          (return-from setup-body t))
        (if (> remain +max-body+)
            (progn
              (setf body (temporary-file:open-temporary :direction :io :element-type '(unsigned-byte 8)))
              (write-sequence (buffer-to-vector parsed-body) body))
            (progn
              (setf body (flex:make-in-memory-output-stream))
              (write-sequence (buffer-to-vector parsed-body) body)))
        (setf body-remain remain)
        (setf read-header nil)
        nil))))

(defmethod read-body ((self client))
  (dd "read-body")
  (with-slots (body body-remain buffer io ready requests-served) self
    (let* ((remain body-remain)
           (want (min remain +chunk-size+)))
      (static-vectors:with-static-vector (vec want)
        (let ((read-size
                (handler-case
                    (sysread (fd-of io) (static-vectors:static-vector-pointer vec) want)
                  (isys:ewouldblock (e)
                    (print e)
                    (return-from read-body nil))
                  ((or isys:syscall-error end-of-file) (e)
                    (print e)
                    (error 'connection-error
                           :format-control "Connection error detected during read")))))
          (write-sequence vec body :end read-size)
          (decf remain read-size)
          (if (<= remain 0)
              (progn
                (typecase body
                  (flex:in-memory-output-stream
                   (setf body (prog1 (flex:make-in-memory-input-stream
                                      (flex:get-output-stream-sequence body))
                                (close body))))
                  (t
                   (file-position body 0)))
                (setf buffer nil)
                (incf requests-served)
                (setf ready t)
                t)
              (progn
                (setf body-remain remain)
                nil)))))))


#+nil
(multiple-value-bind (a b) (iolib.syscalls:pipe)
  (static-vectors:with-static-vector (vec 1 :initial-element 8)
    (isys:write b (static-vectors:static-vector-pointer vec) 1))
  (static-vectors:with-static-vector (vec 1 :initial-element 0)
    (isys:read a (static-vectors:static-vector-pointer vec) 1)
    (aref vec 0)))
;;⇒ 8


(defmethod write-400 ((self client))
  (with-slots (io) self
    (ignore-errors (write-string +error-400-response+ io))))

(defmethod write-500 ((self client))
  (with-slots (io) self
    (ignore-errors (write-string +error-500-response+ io))))
