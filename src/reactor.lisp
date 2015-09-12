(in-package :unpyo)

(defclass reactor ()
  ((server :initarg :server)
   (events)
   (app-pool :initarg :app-pool)
   (mutex :initform (bt:make-lock))
   (ready)
   (trigger)
   (input :initform ())
   (sleep-for :initform +reactor-default-sleep-for+)
   (timeouts :initform ())
   (sockets)
   (thread)))

(defmethod initialize-instance :after ((self reactor) &key server)
  (with-slots (events ready sockets trigger) self
    (setf events (slot-value server 'events))
    (setf (values ready trigger) (isys:pipe))
    (setf sockets (list ready))))

(defmethod run ((self reactor) &key)
  (with-slots (app-pool events input mutex ready server sleep-for sockets timeouts trigger) self
    (unwind-protect
         (loop for cs = (io-select sockets :timeout sleep-for) do
           (loop for c in cs do
             (if (eql c ready)
                 (bt:with-lock-held (mutex)
                   (let ((command (read-1 c)))
                     (dd "reactor read command ~a" command)
                     (case command
                       (#.+reactor-add-command+
                        (setf sockets (nconc sockets input))
                        (setf input ()))
                       (#.+reactor-clear-command+
                        (setf sockets (delete-if (lambda (s)
                                                   (unless (eql s ready)
                                                     (close s)
                                                     t))
                                                 sockets)))
                       (#.+reactor-shutdown-command+
                        (return-from run nil)))))
                 (progn
                   (when (timeout-at-of c)
                     (bt:with-lock-held (mutex)
                       (setf timeouts (delete c timeouts))))
                   (handler-case
                       (with-debugger
                         (when (try-to-finish c)
                           (dd "add ~a to thread pool from reactor" c)
                           (<< app-pool c)
                           (setf sockets (delete c sockets))))
                     (http-parse-error (e)
                       (setf sockets (delete c sockets))
                       (write-400 c)
                       (close c)
                       (evets-parse-error events server (env-of c) e))
                     (connection-error (e)
                       (declare (ignore e))
                       (setf sockets (delete c sockets))
                       (close c))
                     (error (e)
                       (setf sockets (delete c sockets))
                       (write-500 c)
                       (close c)
                       (trivial-backtrace:print-backtrace e))))))
           (when timeouts
             (bt:with-lock-held (mutex)
               (loop with now = (monotonic-time)
                     while (and timeouts (< (timeout-at-of (car timeouts)) now))
                     for c = (pop timeouts)
                     do (setf sockets (delete c sockets))
                        (close c))
               (calculate-sleep self))))
      (isys:close trigger)
      (isys:close ready))))

(defmethod run-in-thread ((self reactor))
  (with-slots (thread) self
    (setf thread (bt:make-thread
                  (lambda ()
                    (loop do (handler-case
                                 (progn (run self)
                                        (loop-finish))
                               (error (e)
                                 (format *error-output*
                                         "Error in reactor loop escaped: ~a"
                                         e)
                                 (trivial-backtrace:print-backtrace e :output  *error-output*)))))
                  :name (format nil "unpyo reactor ~a" self)))))


(defmethod calculate-sleep ((self reactor))
  (with-slots (timeouts sleep-for) self
    (if timeouts
        (let ((diff (- (timeout-at-of (car timeouts)) (monotonic-time))))
          (setf sleep-for (if (minusp diff) 0 diff)))
        (setf sleep-for +reactor-default-sleep-for+))))

(defmethod << ((self reactor) client)
  (with-slots (input mutex timeouts trigger) self
    (bt:with-lock-held (mutex)
      (push client input)
      (write-1 trigger +reactor-add-command+)
      (when (timeout-at-of client)
        (push client timeouts)
        (setf timeouts (sort timeouts (lambda (x y)
                                        (<= (timeout-at-of x)
                                            (timeout-at-of y)))))
        (calculate-sleep self)))))

(defmethod clear ((self reactor))
  (with-slots (trigger) self
    (write-1 trigger +reactor-clear-command+)))

(defmethod shutdown ((self reactor))
  (with-slots (thread trigger) self
    (write-1 trigger +reactor-shutdown-command+)
    (bt:join-thread thread)))
