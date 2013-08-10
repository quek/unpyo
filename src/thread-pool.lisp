(in-package #:unpyo)

(require :sb-concurrency)

(defmacro with-thread-pool-lock (thread-pool &body body)
  `(bt:with-lock-held ((slot-value ,thread-pool 'lock))
     ,@body))

(defclass thread-pool ()
  ((min :initarg :min :initform 1)
   (max :initarg :max :initform 1)
   (todo :initform (sb-concurrency:make-queue))
   (process :initarg :process)
   (auto-trim :initform nil)
   (trim-requested :initform 0 :reader trim-requested-of)
   (lock :initform (bt:make-lock "thread pool lock"))
   (condition-variable :initform (bt:make-condition-variable))
   (shutdown :initform nil)
   (spawned :initform 0 :reader spawned-of)
   (waiting :initform 0)
   (workers :initform ())
   (server :initarg :server)))

(defmethod initialize-instance :after ((self thread-pool) &key)
  (with-slots (min) self
    (with-thread-pool-lock self
      (dotimes (i min)
        (spawn-thread self)))))

(defmethod backlog ((self thread-pool))
  (with-thread-pool-lock self
    (with-slots (todo) self
      (sb-concurrency:queue-count todo))))

(defmethod spawn-thread ((self thread-pool))
  "Must be called with-thread-pool-lock!"
  (with-slots (condition-variable lock server spawned shutdown todo
               trim-requested waiting workers) self
    (incf spawned)
    (aprog1
        (bt:make-thread
            (lambda ()
              (loop with client = nil
                    with continue = t
                    with buffer = (make-buffer :static t)
                    do (with-thread-pool-lock self
                         (loop while (sb-concurrency:queue-empty-p todo)
                               do (when (plusp trim-requested)
                                    (decf trim-requested)
                                    (setf continue nil)
                                    (loop-finish))
                                  (when shutdown
                                    (setf continue nil)
                                    (loop-finish))
                                  (incf waiting)
                                  (bt:condition-wait condition-variable lock)
                                  (decf waiting))
                         (when continue
                           (let (ok)
                             (setf (values client ok) (sb-concurrency:dequeue todo))
                             (assert ok))))
                       (unless continue (loop-finish))
                       (cook-client self client buffer))
              (with-thread-pool-lock self
                (decf spawned)
                (setf workers (delete (bt:current-thread) workers))))
            :name (format nil "unpyo thread pool ~a" self))
      (push it workers))))

(defmethod cook-client ((self thread-pool) client buffer)
  (with-slots (server) self
    (with-slots (events reactor) server
      (handler-case
          (eagerly-finish client)
        (http-parse-error (e)
          (write-400 client)
          (close client)
          (evets-parse-error events self (env-of client) e))
        (connection-error (e)
          (print e)
          (close client))
        (:no-error (process-now)
          (if process-now
              (process-client server client buffer)
              (progn
                (set-timeout client (slot-value server 'first-data-timeout))
                (dd "add ~a to reactor from cook-client" client)
                (<< reactor client))))))))


(defmethod << ((self thread-pool) (client client))
  (with-thread-pool-lock self
    (with-slots (condition-variable max spawned todo waiting shutdown) self
      (when shutdown
        (error "Unable to add client while shutting down"))
      (sb-concurrency:enqueue client todo)
      (when (and (zerop waiting)
                 (< spawned max))
        (spawn-thread self))
      (bt:condition-notify condition-variable))))

(defmethod trim ((self thread-pool) &optional force)
  (with-thread-pool-lock self
   (with-slots (condition-variable min spawned trim-requested waiting) self
     (when (and (or force (plusp waiting))
                (> (- spawned trim-requested) min))
       (incf trim-requested)
       (bt:condition-notify condition-variable)))))


(defclass auto-trim ()
   ((pool :initarg :pool)
    (timeout :initarg :timeout)
    (running :initform nil)
    (thread :initform nil)))

(defmethod start ((self auto-trim))
  (with-slots (pool running thread timeout) self
    (setf running t)
    (setf thread (bt:make-thread
                  (lambda ()
                    (catch :wakeup
                      (loop while running
                            do (trim pool)
                               (sleep timeout))))
                  :name (format nil "unpyo auto-trim ~a" self)))))

(defmethod stop ((self auto-trim) &key)
  (with-slots (running thread) self
    (setf running nil)
    (bt:interrupt-thread thread (lambda () (throw :wakeup t)))))


(defmethod auto-trim ((self thread-pool) &optional (timeout 5))
  (with-slots (auto-trim) self
    (setf auto-trim (make-instance 'auto-trim :pool self :timeout timeout))
    (start auto-trim)))

(defmethod shutdown ((self thread-pool))
  (with-slots (auto-trim condition-variable spawned workers shutdown) self
    (with-thread-pool-lock self
      (setf shutdown t)
      (sb-thread:condition-broadcast condition-variable)
      (when auto-trim
        (stop auto-trim)))
    (dolist (worker workers)
      (bt:join-thread worker))
    (setf spawned 0)
    (setf workers ())))