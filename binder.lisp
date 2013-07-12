(in-package :unpyo)

(defclass binder ()
  ((events :initarg :events)
   (listeners :initform () :reader listeners-of)
   (unix-paths :initform ())
   (ios :initform () :reader ios-of)
   (inherited-fds :initform (make-hash-table :test 'equal))
   (envs :initform (make-hash-table :test 'eq))
   (proto-env :initform (make-hash-table :test 'equal))
   (auto-trim-time :initform 1)))

(defmethod initialize-instance :after ((self binder) &key)
  (with-slots (proto-env) self
    (mapc (lambda (kv)
            (setf (gethash (car kv) proto-env) (cadr kv)))
          `(("CONTENT_TYPE" "text/plain")
            ("QUERY_STRING" "")
            ("SERVER_PROTOCOL" "HTTP/1.1")
            ("SERVER_SOFTWARE" +unpyo-version+)
            ("GATEWAY_INTERFACE" "CGI/1.2")))))

(defmethod close ((self binder) &key abort)
  (flet ((c (x)
           (close x :abort abort)))
    (mapc #'c (ios-of self))
    (mapc #'c (slot-value self 'unix-paths))))

(defmethod env ((self binder) sock)
  (gethash sock (slot-value self 'envs) (slot-value self 'proto-env)))

(defmethod import-form-env ((self binder))
  (with-slots (inherited-fds) self
   (let ((remove ()))
     (map-env (lambda (name value)
                (when (ppcre:scan "UNPYO_INHERIT_\\d+" name)
                  (multiple-value-bind (fd url) (split-once #\: value)
                    (setf (gethash url inherited-fds) (parse-integer fd))
                    (push name remove)))
                (when (and (ppcre:scan "LISTEN_FDS" name)
                           (= (parse-integer (or (isys:getenv "LISTEN_PID") "0"))
                              (iolib.syscalls:getpid)))
                  (dotimes (num (parse-integer value))
                    (let* ((fd (+ num 3))
                           (sock (iolib.sockets::create-socket :local :stream :active :utf-8 :fd fd)))
                      ;; TODO
                      sock)))))
     (dolist (name remove)
       (iolib.syscalls:unsetenv name)))))

(defmethod parse ((self binder) binds logger)
  (with-slots (inherited-fds listeners) self
    (let (io)
      (loop for bind in binds
            for uri = (puri:parse-uri bind)
            for scheme = (puri:uri-scheme uri)
            do (ecase scheme
                 (:tcp
                  (setf io
                        (aif (prog1 (gethash bind inherited-fds)
                               (remhash bind inherited-fds))
                             (inherit-tcp-listener
                              self (puri:uri-host uri) (puri:uri-port uri) it)
                             (add-tcp-listener
                              self (puri:uri-host uri) (puri:uri-port uri)))))
                 (:unix
                  (let ((path (concatenate 'string (puri:uri-host uri) (puri:uri-path uri))))
                    (setf io
                          (aif (prog1 (gethash bind inherited-fds)
                                 (remhash bind inherited-fds))
                               (inherit-unix-listener self path it)
                               (let ((umask))
                                 (awhen (puri:uri-query uri)
                                   (awhen (cdr (assoc "umask" (com.informatimago.common-lisp.http.hquery:query-parse it)
                                                :test 'equal))
                                     (setf umask (parse-integer it))))
                                 (add-unix-listener self path umask)))))))
               (push (cons bind io) listeners)))
    (loop for (bind . fd) in inherited-fds
          for uri = (puri:parse-uri bind)
          do (close fd)
             (when (eq :unix (puri:uri-scheme uri))
               (delete-file (concatenate 'string (puri:uri-host uri) (puri:uri-path uri)))))))

(defmethod add-tcp-listener ((self binder) host port &key (optimize-for-latency t)
                                                       (backlog 1024))
  (with-slots (ios) self
    (setf host (string-trim "[]" host))
    (aprog1 (iolib.sockets:make-socket :connect :passive
                                       :local-host host
                                       :local-port port
                                       :backlog backlog)
      (when optimize-for-latency
        (setf (iolib.sockets:socket-option it :tcp-nodelay) t))
      (setf (iolib.sockets:socket-option it :reuse-address) t)
      (push it ios))))

(defmethod inherit-tcp-listener ((self binder) host port (fd iolib.sockets:socket))
  (with-slots (ios) self
    (push fd ios)
    fd))

(defmethod inherit-tcp-listener ((self binder) host port (fd integer))
  (with-slots (ios) self
    (aprog1 (iolib.sockets:make-socket-from-fd fd :connect :passive)
      (push fd ios))))

(defmethod add-unix-listener ((self binder) path &optional (umask 0))
  (with-slots (ios unix-paths) self
    (push path unix-paths)
    (let ((old-mask (isys:umask umask)))
      (unwind-protect
           (progn
             (when (probe-file path)
               (handler-case
                   (iolib.sockets:make-socket :address-family :local
                                              :local-filename path)
                 (isys:syscall-error ()
                   (delete-file path))
                 (:no-error (old)
                   (close old)
                   (error "There is already a server bound to: ~a" path))))
             (aprog1 (iolib.sockets:make-socket :address-family :local
                                                :local-filename path)
               (push it ios)))
        (isys:umask old-mask)))))

(defmethod inherit-unix-listener :around ((self binder) path fd)
  (with-slots (ios unix-paths) self
    (push path unix-paths)
    (aprog1 (call-next-method)
      (push it ios))))

(defmethod inherit-unix-listener ((self binder) path (fd iolib.sockets:socket))
  fd)

(defmethod inherit-unix-listener ((self binder) path (fd integer))
  (iolib.sockets:make-socket-from-fd fd :connect :passive))
