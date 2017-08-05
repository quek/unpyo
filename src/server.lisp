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
  socket
  (port 1958)
  (host #(0 0 0 0))
  (threads ())
  server-thread
  (stop-p nil)
  (min-threads 2)
  (max-threads 10)
  (app (make-instance 'status-app))
  pid-file)

(defun create-socket (server)
  (log:debug (fd-from-env))
  (aif (fd-from-env)
       (prog1 (setf (server-socket server)
                    (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp :descriptor it))
         (with-open-file (in (server-pid-file server))
           (let ((pid (read in)))
             (log:debug 'kill pid sb-unix:sigterm)
             (sb-posix:kill pid sb-unix:sigterm))))
       (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
         (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
         (sb-bsd-sockets:socket-bind socket (server-host server) (server-port server))
         (sb-bsd-sockets:socket-listen socket 7)
         (setf (server-socket server) socket))))

(defun start (server &key (backgroundp t))
  (setf *server* server)
  (create-socket server)
  (awhen (server-pid-file server)
    (with-open-file (out it :if-exists :supersede :direction :output)
      (write (sb-posix:getpid) :stream out)))
  (loop repeat (server-min-threads server) do (add-thread server))
  (if backgroundp
      (setf (server-server-thread server)
            (sb-thread:make-thread 'server-loop :arguments (list server) :name "unpyo server"))
      (server-loop server))
  server)

(defun stop (&optional (server *server*))
  (setf (server-stop-p server) t)
  (loop repeat (length (server-threads server))
        do (sb-concurrency:send-message (server-mailbox server) nil))
  (loop for i in (server-threads server) do (ignore-errors (sb-thread:join-thread i :timeout 5)))
  (sb-bsd-sockets:socket-close (server-socket server)))

(defun server-loop (server)
  (loop with socket = (server-socket server)
        with mailbox = (server-mailbox server)
        with nowait = 0
        do (handler-case
               (sb-ext:with-timeout 1
                 (sb-concurrency:send-message mailbox
                                              (sb-bsd-sockets:socket-accept socket)))
             (sb-ext:timeout ()
               (when (server-stop-p server)
                 (return-from server-loop t))))
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
          do (handler-case
                 (sb-ext:with-timeout 60
                   (unwind-protect
                        (progn
                          (reset-request request socket)
                          (reset-response response)
                          (handle-request request response app)
                          (loop for f in (request-cleanup request)
                                do (funcall f)))
                     (sb-bsd-sockets:socket-close socket)))
               (read-zero-error ())
               (sb-ext:timeout (e)
                 (log:error "Timeout ~a!" e))
               (error (e)
                 (log:error "Error ~a!" e))))))

(defun handle-request (request response app)
  (multiple-value-bind (request-header-length read-length) (read-request-header request)
    (parse-request-line request request-header-length)
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
             (error 'read-zero-error))
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
