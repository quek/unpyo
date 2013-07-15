(in-package #:unpyo)

(defclass server ()
  ((app :initarg :app)
   (events :initarg :events :initform (make-stdio-events))
   (check)
   (notify)
   (status :initform :stop)
   (thread-pool :initform nil)
   (binder :initarg :binder)
   (own-binder :initform t)
   (first-data-timeout :initform 30
                       :documentation "The default number of seconds to wait until we get the first data for the request")
   (reactor)
   (min-threads :initarg :min-thread :initform 1)
   (max-threads :initarg :max-thread :initform 2)
   (auto-trim-time :initform 1)
   (persistent-timeout :initform 20
                       :documentation "The number of seconds for another request within a persistent session.")
   (thread :initform nil)))

(defmethod initialize-instance :after ((self server) &key)
  (with-slots (binder check events notify) self
    (unless (slot-boundp self 'binder)
      (setf binder (make-instance 'binder :events events)))
    (setf (values check notify) (iolib.syscalls:pipe))))

(defmethod add-tcp-listener ((self server) host port &key (optimize-for-latency t)
                                                       (backlog 1024))
  (with-slots (binder) self
    (add-tcp-listener binder host port
                      :optimize-for-latency optimize-for-latency
                      :backlog backlog)))

(defmethod add-unix-listener ((self server) path &optional (umask 0))
  (with-slots (binder) self
    (add-unix-listener binder path umask)))

(defmethod run ((self server) &key (background t))
  (with-slots (auto-trim-time events reactor status thread thread-pool) self
    (setf status :run)
    (setf thread-pool
          (make-instance 'thread-pool
                         :min (slot-value self 'min-threads)
                         :max (slot-value self 'max-threads)
                         :server self))
    (setf reactor (make-instance 'reactor :server self :app-pool thread-pool))
    (run-in-thread reactor)
    (when auto-trim-time
      (auto-trim thread-pool auto-trim-time))
    (if background
        (setf thread (bt:make-thread (lambda ()
                                       (handle-servers self))
                                     :name (format nil "unpyo serevr ~a" self)))
        (handle-servers self))))

(defmethod handle-servers ((self server))
  (with-slots (binder check events notify own-binder reactor status thread-pool) self
    (unwind-protect
         (let ((sockets `(,check ,@(ios-of binder)))
               (pool thread-pool))
           (loop while (eq status :run)
                 do (handle-servers-loop self pool check sockets))
           (when (member status '(:stop :restart))
             (graceful-shutdown self))
           (when (eq status :restart)
             (clear reactor))
           (shutdown reactor))
      (isys:close check)
      (isys:close notify)
      (when (and (not (eq status :restart)) own-binder)
        (close binder)))))

(defun handle-servers-loop (server pool check sockets)
  (with-slots (binder events) server
    (handler-case
        (loop for sock in (io-select sockets)
              do (if (eql sock check)
                     (when (handle-check server)
                       (loop-finish))
                     (handler-case
                         (awhen (iolib.sockets:accept-connection sock :wait nil)
                           (let ((client (make-instance 'client :io it :env (env binder sock))))
                             (dd "add ~a to thread pool from server" client)
                             (<< pool client)))
                       (isys:syscall-error (e)
                         ;; ignore error
                         (print e)))))
      (isys:econnaborted (e) ;client closed the socket even before accept
        (print e))
      (error (e)
        (unknow-error events server e "Listen loop")))))

(defmethod handle-check ((self server))
  (with-slots (check status) self
    (case (read-1 check)
      (#.+stop-command+
       (setf status :stop)
       t)
      (#.+halt-command+
       (setf status :halt)
       t)
      (#.+restart-command+
       (setf status :restart)
       t)
      (t
       nil))))

(defmethod process-client ((self server) client buffer)
  (print 'process-client)
  (with-slots (events persistent-timeout reactor status) self
    (let ((close-socket t))
      (unwind-protect
           (handler-case
               (loop for result = (handle-request self client buffer)
                     do (cond ((null result)
                               (loop-finish))
                              ((eq result :async)
                               (setf close-socket nil)
                               (loop-finish))
                              (t
                               (reset buffer)
                               (unless (reset client :fast-check (eq status :run))
                                 (setf close-socket nil)
                                 (set-timeout client persistent-timeout)
                                 (dd "add ~a to reactor from server process-client" client)
                                 (<< reactor client)
                                 (loop-finish)))))
             (connection-error (e)
               (print e))
             (http-parse-error (e)
               (write-400 client)
               (evets-parse-error events self (env-of client) e))
             (error (e)
               (write-500 client)
               (unknow-error events self e "Read"))))
      (reset buffer)
      (handler-case
          (when close-socket
            (close client))
        ((or io-error sytem-call-error) ()
          ;; Already closed
          )
        (error (e)
          (unknow-error events self e "Client"))))))

(defmethod normalize-env ((self server) env client)
  (let ((host (gethash "HTTP_HOST" env)))
    (if host
        (aif (position #\: host)
             (setf (gethash "SERVER_NAME" env) (subseq host 0 it)
                   (gethash "SERVER_PORT" env) (subseq host (1+ it)))
             (setf (gethash "SERVER_NAME" env) host
                   (gethash "SERVER_PORT" env) (default-server-port env)))
        (setf (gethash "SERVER_NAME" env) "localhost"
              (gethash "SERVER_PORT" env) (default-server-port env))))
  (unless (gethash "REQUEST_PATH" env)
    (setf (gethash "REQUEST_PATH" env)
          (ignore-errors (puri:uri-path (puri:parse-uri (gethash "REQUEST_URI" env)))))
    (unless (gethash "REQUEST_PATH" env)
      (error "No REQUEST PATH")))
  ;; TODO REMOTE_ADDR
  )

(defun default-server-port (env)
  (if (equal "https" (gethash "HTTP_X_FORWARDED_PROTO" env))
      "443"
      "80"))

(defmethod handle-request ((self server) client buffer)
  (print 'handle-request)
  (with-slots (app events) self
    (let* ((env (env-of client))
           (client-socket (io-of client))
           (body (body-of client))
           (head-p (equal (gethash "REQUEST_METHOD" env) "HEAD"))
           (no-body head-p)
           (after-reply '(nil))
           keep-alive
           status
           headers
           res-body
           content-length
           allow-chunked
           include-keepalive-header
           response-hijack
           chunked)
      (normalize-env self env client-socket)
      (setf (gethash "unpyo.socket" env) client-socket)
      (setf (gethash +hijack-p+ env) t)
      (setf (gethash +hijack+ env) client-socket)
      (setf (env env +unpyo-input+) body)
      (setf (env env +unpyo-url-scheme+) (if (gethash "HTTPS" env) "https" "http"))
      (setf (env env +unpyo-after-reply+) after-reply)
      (unwind-protect
           (with-cork (client-socket)
             (handler-case
                 (progn
                   (setf (values status headers res-body)
                         (call app env))
                   (when (hijacked-p client)
                     (return-from handle-request :async))
                   (when (stringp status)
                     (setf status (parse-integer status)))
                   (when (= status -1)
                     (unless (and (null headers) (null res-body))
                       (error "async response must have empty headers and body"))
                     (return-from handle-request :async)))
               (error (e)
                 (unknow-error events self e "app")
                 (setf (values status headers res-body) (lowlevel-error self))))

             (when (and (listp res-body) (null (cdr res-body)))
               (setf content-length (bytesize (car res-body))))
             (if (equal "HTTP/1.1" (env env "HTTP_VERSION"))
                 (progn                 ;HTTP/1.1
                   (setf allow-chunked t)
                   (setf keep-alive (not (equal (gethash "HTTP_CONNECTION" env) "close")))
                   (setf include-keepalive-header nil)
                   (if (= status 200)
                       (bwrite buffer +http/1.1-200+)
                       (progn
                         (bwrite buffer "HTTP/1.1 " status " "
                                 (gethash status *http-status-codes*)
                                 +crlf+)
                         (setf no-body (or no-body
                                           (< status 200)
                                           (gethash status *status-with-no-entity-body*))))))

                 (progn                 ;HTTP/1.0
                   (setf allow-chunked nil)
                   (setf keep-alive (equal "Keep-Alive" (gethash "HTTP_CONNECTION" env)))
                   (setf include-keepalive-header keep-alive)
                   (if (= status 200)
                       (bwrite buffer +http/1.0-200+)
                       (progn
                         (bwrite buffer "HTTP/1.0 " status " "
                                 (gethash status *http-status-codes*)
                                 +crlf+)
                         (setf no-body (or no-body
                                           (< status 200)
                                           (gethash status *status-with-no-entity-body*)))))))

             (loop for (k . v) in headers
                   do (cond ((string= k "Content-Length")
                             (setf content-length v))
                            ((string= k "Content-Type"))
                            ((string= k +hijack+)
                             (setf response-hijack v))
                            (t
                             (when (string= k "Transfer-Encoding")
                               (setf allow-chunked nil)
                               (setf content-length nil))
                             (loop for v in (ppcre:split +newline+ v)
                                   do (bwrite buffer k +colon+ v +crlf+)))))

             (when no-body
               (bwrite +crlf+ buffer)
               (fast-write client-socket buffer)
               (return-from handle-request keep-alive))

             (cond (include-keepalive-header
                    (bwrite buffer #.(concatenate 'string "Connection: Keep-Alive" +crlf+)))
                   ((not keep-alive)
                    (bwrite buffer #.(concatenate 'string "Connection: close" +crlf+))))

             (unless response-hijack
               (if content-length
                   (progn
                     (bwrite buffer "Content-Length: " content-length +crlf+)
                     (setf chunked nil))
                   (progn
                     (bwrite buffer
                             #.(concatenate 'string "Transfer-Encoding: chunked" +crlf+))
                     (setf chunked t))))

             (bwrite buffer +crlf+)

             (fast-write client-socket buffer)

             (when response-hijack
               (funcall response-hijack client-socket)
               (return-from handle-request :async))


             (loop for part in res-body
                   do (with-buffer (buf :static t)
                        (if chunked
                            (progn
                              (bwrite buf (format nil "~x" (bytesize part))
                                      +crlf+ part +crlf+))
                            (bwrite buf part))
                        (fast-write client-socket buf)))
             (when chunked
               (with-buffer (buf :static t)
                 (bwrite buf #.(concatenate 'string "0" +crlf+ +crlf+))
                 (fast-write client-socket buf))))
        ;; cleanup forms of unwind-protect
        (close body)
        (ignore-errors (close res-body))
        (loop for o in after-reply
              if o
                do (funcall o)))
      keep-alive)))

(defmethod graceful-shutdown ((self server))
  (with-slots (thread-pool) self
    (shutdown thread-pool)))

(defmethod stop ((self server) &key sync)
  (with-slots (notify thread) self
    (write-1 notify +stop-command+)
    (when (and thread sync)
      (bt:join-thread thread))))

(defmethod halt ((self server) &key sync)
  (with-slots (notify thread) self
    (write-1 notify +halt-command+)
    (when (and thread sync)
      (bt:join-thread thread))))

(defmethod begin-restart ((self server))
  (with-slots (notify) self
    (write-1 notify +restart-command+)))


(defun fast-write (socket buffer)
  (let ((fd (fd-of socket))
        (vector (fast-io::finish-output-buffer buffer)))
    (flet ((w (pointer length)
             (loop
               (handler-case
                   (progn
                     (print 'fast-write)
                     (return (isys:write fd pointer length)))
                 (isys:ewouldblock ()
                   (print 'fast-write-ewouldblock)
                   (iomux:wait-until-fd-ready fd :output 1 nil))))))
      (unwind-protect
           (loop with length = (length vector)
                 for pointer = (static-vectors:static-vector-pointer vector)
                   then (static-vectors::inc-pointer pointer write-size)
                 for write-size = (w pointer length)
                 while (plusp (decf length write-size)))
        (static-vectors:free-static-vector vector)))))
