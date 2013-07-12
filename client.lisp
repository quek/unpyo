(in-package :unpyo)

(defclass client ()
  ((io :initarg :io :reader io-of)
   (to-io :reader to-io-of)
   (proto-env :initarg :env)
   (env :reader env-of)
   (parser :initform (make-instance 'http-parser))
   (parsed-bytes :initform 0)
   (read-header :initform t)
   (ready :initform nil :reader ready-p)
   (body :initform nil :reader body-of)
   (body-remain)
   (buffer :initform nil)
   (buffer-size :initform 0)
   (timeout-at :initform nil :reader timeout-at-of)
   (request-served :initform 0)
   (hijacked :initform nil :reader hijacked-p)))

(defmethod initialize-instance :after ((self client) &key)
  (with-slots (env io proto-env to-io) self
    (setf to-io (to-io-of io))
    (setf env (alexandria:copy-hash-table proto-env))))

(defmethod call ((self client))
  (with-slots (io hijacked) self
    (setf hijacked t)
    (setf (gethash "unpyo.hijack_io" (env-of self)) io)))

(defmethod set-timeout ((self client) seconds)
  (with-slots (timeout-at) self
    (setf timeout-at (+ (get-universal-time) seconds))))

(defmethod reset ((self client) &key (fast-check t))
  (with-slots (env body buffer buffer-size parsed-bytes parser proto-env read-header ready to-io) self
    (setf reset parser)
    (setf read-header t)
    (setf env (alexandria:copy-hash-table proto-env))
    (setf body nil)
    (setf parsed-bytes 0)
    (serf ready nil)
    (cond (buffer
           (setf parsed-bytes (execute parser env buffer buffer-size parsed-bytes))
           (cond ((finished-p parser)
                  (setup-body self))
                 ((>= parsed-bytes +max-header+)
                  (error 'http-parse-error
                         :format-control "HEADER is longer than allowed, aborting client early."))
                 (t nil)))
          ((and fast-check (io-select (list to-io) :timeout +fast-track-ka-timeout+))
           (try-to-finish self))
          (t nil))))

(defmethod close ((self client) &key abort)
  (with-slots (io) self
    (ignore-errors (close io :abort abort))))

(defvar *null-stream* (make-two-way-stream
                       (make-concatenated-stream)
                       (make-broadcast-stream)))

(defmethod setup-body ((self client))
  (with-slots (body body-remain buffer buffer-size env parser read-header ready requests-served) self
    (let ((parsed-body (body-of parser))
          (content-length (gethash "CONTENT_LENGTH" env)))
      (unless content-length
        (setf buffer (if (zerop (length parsed-body))
                         nil
                         parsed-body)
              buffer-size (length buffer))
        (setf body *null-stream*)
        (incf requests-served)
        (setf ready t)
        (return-from setup-body t))
      (let ((remain (- (parse-integer content-length)
                       (bytesize body))))
        (when (<= remain 0)
          (setf body (make-string-input-stream parsed-body))
          (setf buffer nil
                buffer-size 0)
          (incf requests-served)
          (setf ready t)
          (return-from setup-body t))
        (if (> remain +max-body+)
            (progn
              (setf body (temporary-file:open-temporary :direction :io :external-format :utf-8))
              (write-string parsed-body body))
            (setf body (make-string-input-stream parsed-body)))
        (setf body-remain remain)
        (setf read-header nil)
        nil))))

(defmethod try-to-finish ((self client))
  (with-slots (buffer buffer-size env io parsed-bytes parser read-header) self
    (unless read-header
      (return-from try-to-finish (read-body self)))

    (let* ((vec (static-vectors:make-static-vector +chunk-size+))
           (read-size (isys:read (iolib.streams:fd-of io)
                                 (static-vectors:static-vector-pointer
                                  vec) +chunk-size+)))
      (if buffer
          (progn
            (let ((new-buffer (fast-io:make-octet-vector (+ buffer-size read-size))))
              (replace new-buffer buffer :end2 buffer-size)
              (replace new-buffer vec :start1 buffer-size :end2 read-size)
              (static-vectors:free-static-vector vec)
              (setf buffer new-buffer
                    buffer-size (+ buffer-size read-size))))
          (setf buffer vec
                buffer-size read-size)))
    (setf parsed-bytes (execute parser env buffer buffer-size parsed-bytes))
    (cond ((finished-p parser)
           (setup-body self))
          ((>= parsed-bytes +max-header+)
           (error 'http-parse-error
                  :format-control "HEADER is longer than allowed, aborting client early."))
          (t nil))))

(defmethod eagerly-finish ((self client))
  (with-slots (ready to-io) self
    (cond (ready
           t)
          ((not (io-select (list to-io) :timeout 0))
           nil)
          (t
           (try-to-finish self)))))

(defmethod read-body ((self client))
  (with-slots (body body-remain buffer buffer-size io ready requests-served) self
    (let* ((remain body-remain)
           (want (if (> remain +chunk-size+)
                     +chunk-size+
                     remain)))
      (static-vectors:with-static-vector (vec want)
        (let ((read-size (handler-case (isys:read (iolib.streams:fd-of io) vec want)
                           (isys:ewouldblock (e)
                             (print e)
                             (return-from read-body nil))
                           ((or isys:syscall-error stream-error) (e)
                             (print e)
                             (error 'connection-error
                                    :format-control "Connection error detected during read")))))
          (when (zerop read-size)       ;No chunk means a closed socket
            (close body)
            (ignore-errors (static-vectors:free-static-vector buffer))
            (setf buffer nil
                  buffer-size 0)
            (incf requests-served)
            (setf ready t)
            (error 'end-of-file))
          (write-sequence body vec :end read-size)
          (decf remain read-size)
          (when (<= remain 0)
            (file-position buffer 0)
            (setf buffer nil
                  buffer-size 0)
            (incf requests-served)
            (setf ready t)
            (return-from read-body t))
          (setf body-remain remain)
          nil)))))


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
