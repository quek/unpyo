(in-package #:unpyo)

(defvar *server* nil)
(defvar *request* nil)
(defvar *response* nil)
(defvar *application* nil)

(defstruct fragment
  vector
  start
  end)

(defstruct server
  (mailbox (sb-concurrency:make-mailbox))
  (socket (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
            (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
            socket))
  (port 1958)
  (host #(0 0 0 0))
  (threads ())                          ;TODO weak
  (min-threads 2)
  (max-threads 10)
  (app (make-instance 'status-app)))

(defun start (server &key (backgroundp t))
  (let* ((socket (server-socket server)))
    (sb-bsd-sockets:socket-bind socket (server-host server) (server-port server))
    (sb-bsd-sockets:socket-listen socket 7)
    (loop repeat (server-min-threads server) do (add-thread server))
    (if backgroundp
        (sb-thread:make-thread 'server-loop :arguments (list server) :name "unpyo server")
        (server-loop server))
    (setf *server* server)))

(defun stop (&optional (server *server*))
  (loop repeat (length (server-threads server))
        do (sb-concurrency:send-message (server-mailbox server) nil))
  (loop for i in (server-threads server) do (ignore-errors (sb-thread:join-thread i)))
  (sb-bsd-sockets:socket-close (server-socket server)))

(defun server-loop (server)
  (loop with socket = (server-socket server)
        with mailbox = (server-mailbox server)
        with nowait = 0
        do (sb-concurrency:send-message mailbox
                                        (sb-bsd-sockets:socket-accept socket))
           (setf (server-threads server) (loop for thread in (server-threads server)
                                               if (sb-thread:thread-alive-p thread)
                                                 collect thread))
           (let ((threads-length (length (server-threads server)))
                 (mailbox-count (sb-concurrency:mailbox-count mailbox)))
             (if (and (< (server-min-threads server) threads-length)
                      (<= mailbox-count 1))
                 (when (< 10 (incf nowait))
                   (setf nowait 0)
                   (sb-concurrency:send-message (server-mailbox server) nil))
                 (setf nowait 0))
             (when (and (< threads-length (server-max-threads server))
                        (< threads-length mailbox-count))
               (add-thread server)))))

(defun add-thread (server)
  (push
   (sb-thread:make-thread 'request-loop
                          :arguments (list server)
                          :name "unpyo worker")
   (server-threads server)))

(defun request-loop (server)
  (let ((mailbox (server-mailbox server)))
    (loop with request = (make-request)
          with response = (make-response)
          with app = (server-app server)
          for socket = (sb-concurrency:receive-message mailbox)
          while socket
          do (handler-case (unwind-protect
                                (progn
                                  (reset-request request socket)
                                  (reset-response response)
                                  (handle-request request response app)
                                  (loop for f in (request-cleanup request)
                                        do (funcall f)))
                             (sb-bsd-sockets:socket-close socket))
               (error (e) (trivial-backtrace:print-backtrace e))))))

(defun handle-request (request response app)
  (multiple-value-bind (request-header-length read-length) (read-request-header request)
    (parse-request-line request request-header-length)
    (print (list (request-method request) (request-path request)))
    (let ((*request* request)
          (*response* response)
          (*application* app)
          (response-body (response-body response)))
      (with-debugger
        (prepare-params request request-header-length read-length)
        (info.read-eval-print.html:with-html-buffer (response-body)
          (info.read-eval-print.css:with-css-buffer (response-body)
            (call app))))
      (let ((b (sb-ext:string-to-octets (make-response-header response))))
        (sb-sys:with-pinned-objects (b)
          (sb-posix:write (sb-bsd-sockets:socket-file-descriptor (request-socket request))
                          (sb-sys:vector-sap b) (length b))))
      (writev (sb-bsd-sockets:socket-file-descriptor (request-socket request))
              response-body))))

(defun read-request-header (request)
  (loop with buffer = (request-buffer request)
        with buffer-size = (length buffer)
        with fd = (sb-bsd-sockets:socket-file-descriptor (request-socket request))
        with read-length = 0
        for n = (sb-sys:with-pinned-objects (buffer)
                  (sb-posix:read fd
                                 (sb-sys:sap+ (sb-sys:vector-sap buffer) read-length)
                                 (- buffer-size read-length)))
        do (when (zerop n)
             ;; When a TCP connection is closed on one side read() on the other side returns 0 byte.
             ;; https://stackoverflow.com/questions/2416944/can-read-function-on-a-connected-socket-return-zero-bytes#2416979
             (error "read zero!"))
           (incf read-length n)
           (awhen (search #.(string-to-octets (format nil "~a~a~a~a" #\cr #\lf #\cr #\lf))
                          buffer :end2 read-length)
             (return-from read-request-header (values it read-length)))
           (when (= buffer-size read-length)
             (error "too large request header!"))))

(defun parse-request-line (request end)
  (let* ((buffer (request-buffer request))
         (s1 (position #.(char-code #\space) buffer :end end)))
    (unless s1 (error "invalid method request!"))
    (setf (request-method request)
          (let ((c1 (aref buffer 0)))
            (cond ((= c1 #.(char-code #\G))
                   :get)
                  ((= c1 #.(char-code #\P))
                   (if (= (aref buffer 1) #.(char-code #\O))
                       :post
                       :put))
                  ((= c1 #.(char-code #\D))
                   :delete)
                  ((= c1 #.(char-code #\H))
                   :head)
                  ((= c1 #.(char-code #\O))
                   :options)
                  ((= c1 #.(char-code #\T))
                   :trace)
                  (t (error "invalid method request!")))))
    (let* ((s1 (1+ s1))
           (s2 (position #.(char-code #\space) buffer :start s1 :end end)))
      (unless s2 (error "invalid path request!"))
      (setf (request-path request) (sb-ext:octets-to-string buffer :start s1 :end s2)))))


#|
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
   (max-threads :initarg :max-thread :initform 4)
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
                           (let ((client (make-instance 'client :io it :env (fetch-env binder sock))))
                             (dd "add ~a to thread pool from server" client)
                             (<< pool client)))
                       (isys:syscall-error (e)
                         ;; ignore error
                         (trivial-backtrace:print-backtrace e)))))
      (isys:econnaborted () ;client closed the socket even before accept
        )
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
  (with-slots (events persistent-timeout reactor status) self
    (let ((close-socket t))
      (unwind-protect
           (handler-case
               (with-debugger
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
                                   (loop-finish))))))
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
        (iolib.syscalls:syscall-error ()
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
  (let* ((path (gethash "REQUEST_URI" env))
         (? (position #\? path)))
    (unless (gethash "REQUEST_PATH" env)
      (setf (gethash "REQUEST_PATH" env) (subseq path 0 ?))
      (unless (gethash "REQUEST_PATH" env)
        (error "No REQUEST PATH")))
    (when ?
      (setf (gethash "QUERY_STRING" env) (subseq path (1+ ?)))))
  ;; TODO REMOTE_ADDR
  )

(puri:uri-query (puri:parse-uri "http://localhost:7780/foo?a=%E3%81%82"))
;;⇒ "a=ã"


(defun default-server-port (env)
  (if (equal "https" (gethash "HTTP_X_FORWARDED_PROTO" env))
      "443"
      "80"))

(defmethod handle-request ((self server) client buffer)
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
      (setf (gethash +unpyo-input+ env) body)
      (setf (gethash +unpyo-url-scheme+ env) (if (gethash "HTTPS" env) "https" "http"))
      (setf (gethash +unpyo-after-reply+ env) after-reply)
      (let* ((*request* (make-request app env))
             (response-buffer (body-of *request*)))
        (unwind-protect
             (with-cork (client-socket)
               (handler-case
                   (progn
                     (with-debugger
                       (info.read-eval-print.html:with-html-buffer (response-buffer)
                         (info.read-eval-print.css:with-css-buffer (response-buffer)
                           (call app))))
                     (setf (values status headers res-body)
                           (values (status-of *request*)
                                   (response-headers-of *request*)
                                   response-buffer))
                     (when (hijacked-p client)
                       (return-from handle-request :async))
                     (when (stringp status)
                       (setf status (parse-integer status)))
                     (when (= status -1)
                       (unless (and (null headers) (zerop (length res-body)))
                         (error "async response must have empty headers and body"))
                       (return-from handle-request :async)))
                 (error (e)
                   (unknow-error events self e "app")
                   (setf (values status headers res-body) (lowlevel-error self e))))

               (if (equal "HTTP/1.1" (gethash "HTTP_VERSION" env))
                   (progn               ;HTTP/1.1
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

                   (progn               ;HTTP/1.0
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
                              ((string= k +hijack+)
                               (setf response-hijack v))
                              (t
                               (when (string= k "Transfer-Encoding")
                                 (setf allow-chunked nil)
                                 (setf content-length nil))
                               (loop for v in (ppcre:split +newline+ v)
                                     do (bwrite buffer k +colon+ v +crlf+)))))

               (when no-body
                 (bwrite buffer +crlf+)
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

               (with-buffer (buf :static t)
                 (bwrite buf (format nil "~x" (loop for i across res-body
                                                    sum (length i)))
                         +crlf+)
                 (fast-write client-socket buf))
               (vector-push-extend
                #.(string-to-octets (concatenate 'string +crlf+ "0" +crlf+ +crlf+))
                res-body)
               (writev (fd-of client-socket) res-body))
          ;; cleanup forms of unwind-protect
          (close body)
          (ignore-errors (close res-body))
          (loop for o in after-reply
                if o
                  do (funcall o))))
      keep-alive)))

(defmethod lowlevel-error ((self server) error)
  (values 500 nil
          (vector (format nil "~a~%" error)
                  (with-output-to-string (out)
                    (trivial-backtrace:print-backtrace error :output out :verbose t)))))

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
        (vector (typecase buffer
                  (fast-io::output-buffer
                   (fast-io::finish-output-buffer buffer))
                  (t buffer))))
    (flet ((w (pointer length)
             (loop
               (handler-case
                   (progn
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
        (when (typep buffer 'fast-io::output-buffer)
          (static-vectors:free-static-vector vector))))))

(defun make-server (&key (app (make-instance 'status-app)) (port 1958) (host "localhost"))
  (let ((server (make-instance 'server :app app)))
    (add-tcp-listener server host port)
    server))
|#
