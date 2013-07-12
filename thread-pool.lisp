(in-package #:unpyo)

(defmacro with-thread-pool-lock (thread-pool &body body)
  `(bt:with-lock-held ((slot-value ,thread-pool 'lock))
     ,@body))

(defclass thread-pool ()
  ((min :initarg :min :initform 1)
   (max :initarg :max :initform 1)
   (todo :initform (queues:make-queue :simple-queue))
   (process :initarg :process)
   (auto-trim :initform nil)
   (trim-requested :initform 0 :reader trim-requested-of)
   (lock :initform (bt:make-lock "thread pool lock"))
   (condition-variable :initform (bt:make-condition-variable))
   (shutdown :initform nil)
   (spawned :initform 0 :reader spawned-of)
   (waiting :initform 0)
   (extra :initarg :extra :initform ())
   (workers :initform ())))

(defmethod initialize-instance :after ((self thread-pool) &key)
  (with-slots (min) self
    (with-thread-pool-lock self
      (dotimes (i min)
        (spawn-thread self)))))

(defmethod backlog ((self thread-pool))
  (with-thread-pool-lock self
    (with-slots (todo) self
      (queues:qsize todo))))

(defmethod spawn-thread ((self thread-pool))
  "Must be called with-thread-pool-lock!"
  (with-slots (condition-variable extra lock process spawned shutdown todo
               trim-requested waiting workers) self
    (incf spawned)
    (aprog1
        (bt:make-thread
         (lambda ()
           (let ((extra (mapcar #'make-instance extra)))
             (loop with work = nil
                   with continue = t
                   do (with-thread-pool-lock self
                        (loop while (zerop (queues:qsize todo))
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
                          (setf work (queues:qpop todo))))
                      (unless continue (loop-finish))
                      (apply process work extra))
             (with-thread-pool-lock self
               (decf spawned)
               (setf workers (delete (bt:current-thread) workers))))))
      (push it workers))))

(defmethod << ((self thread-pool) work)
  (with-thread-pool-lock self
    (with-slots (condition-variable max spawned todo waiting shutdown) self
      (when shutdown
        (error "Unable to add work while shutting down"))
      (queues:qpush todo work)
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
                               (sleep timeout))))))))

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
