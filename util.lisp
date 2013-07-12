(in-package #:unpyo)

(defun split-once (delimiter sequence)
  (let ((position (position delimiter sequence)))
    (if position
        (values (subseq sequence 0 position)
                (subseq sequence (1+ position))))))

(defun map-env (function)
  (mapc (lambda (name=value)
          (multiple-value-call function (split-one #\= name=value)))
        (sb-ext:posix-environ)))


(defun %epoll-ctl (fd epfd op &rest events)
  (cffi:with-foreign-object (ev '(:struct isys:epoll-event))
    (isys:bzero ev isys:size-of-epoll-event)
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

(defun bytesize (string)
  (babel:string-size-in-octets string))

(defun io-select (sockets &key (timeout -1))
  (with-open-stream (epoll-fd (isys:epoll-create 1))
    (let ((hash (make-hash-table)))
      (loop for socket in sockets
            for fd = (iolib.streams:fd-of socket)
            do (%epoll-ctl fd epoll-fd isys:epoll-ctl-add isys:epollin isys:epollpri)
               (setf (gethash fd hash) socket))
      (cffi:with-foreign-object (events '(:struct isys:epoll-event) iomux::+epoll-max-events+)
        (isys:bzero events (* iomux::+epoll-max-events+ isys:size-of-epoll-event))
        (let ((ready-fds (handler-case
                             (isys:epoll-wait epoll-fd events iomux::+epoll-max-events+ timeout)
                           (isys:eintr ()
                             (warn "epoll-wait EINTR")
                             0))))
          (loop for i below ready-fds
                for event = (cffi:mem-aref events '(:struct isys:epoll-event) i)
                for event-data = (cffi:foreign-slot-value
                                  event '(:struct isys:epoll-event) 'isys:data)
                for fd = (cffi:foreign-slot-value event-data '(:union isys:epoll-data) 'isys:fd)
                collect (gethash fd hash)))))))

(defmethod read-1 (fd)
  (static-vectors:with-static-vector (vec 1 :initial-element 0)
    (isys:read fd (static-vectors:static-vector-pointer vec) 1)
    (aref vec 0)))

(defmethod write-1 (fd byte)
  (static-vectors:with-static-vector (vec 1 :initial-element byte)
    (isys:write fd (static-vectors:static-vector-pointer vec) 1)))

(defun cork (socket)
  (iolib.sockets::set-socket-option-int
   (iolib.streams:fd-of socket) iolib.sockets::ipproto-tcp iolib.sockets::tcp-cork 1))

(defun uncork (socket)
  (iolib.sockets::set-socket-option-int
   (iolib.streams:fd-of socket) iolib.sockets::ipproto-tcp iolib.sockets::tcp-cork 0))

(defmacro with-cork ((socket) &body body)
  (alexandria:once-only (socket)
    `(progn
       (cork ,socket)
       (unwind-protect (progn ,@body)
         (uncork ,socket)))))

