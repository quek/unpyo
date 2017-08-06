(in-package :unpyo)

(defconstant +iov-max+ 1024)

(cffi:defcstruct iovec
  ;; Starting address
  (iov-base :pointer)
  ;; Number of bytes to transfer
  (iov-len :uint64))

(sb-posix::define-call* "writev" sb-alien:ssize-t minusp
  (fd sb-posix:file-descriptor) (iov (* t)) (iovcnt sb-alien:size-t))


(declaim (inline %writev))
(defun %writev (fd vector start end)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array * (*)) vector)
           (type fixnum fd start end))
  (let ((iovcnt (- end start)))
    (labels ((call-with-pinned-objects (i fun)
               (declare (type (function () t) fun))
               (if (< i end)
                   (sb-sys:with-pinned-objects ((aref vector i))
                     (call-with-pinned-objects (1+ i) fun))
                   (funcall fun))))
      (declare (inline call-with-pinned-objects))
      (call-with-pinned-objects
       start
       (lambda ()
         (cffi:with-foreign-object (iov '(:struct iovec) iovcnt)
           (loop for p = iov then (cffi:inc-pointer p (cffi:foreign-type-size '(:struct iovec)))
                 for index from start below end
                 for i = (aref vector index)
                 do (cffi:with-foreign-slots ((iov-base iov-len) p (:struct iovec))
                      (setf iov-base (sb-sys:vector-sap i)
                            iov-len (length i))))
           (loop with length fixnum = iovcnt
                 for write-size fixnum = (sb-posix::writev fd iov iovcnt)
                 while (plusp (decf length write-size))
                 do (loop for p = iov then (cffi:inc-pointer p (cffi:foreign-type-size '(:struct iovec)))
                          do (cffi:with-foreign-slots ((iov-base iov-len) p (:struct iovec))
                               (if (< (the fixnum iov-len) write-size)
                                   (progn
                                     (decf write-size (the fixnum iov-len))
                                     (setf iov-len 0))
                                   (progn
                                     (cffi:inc-pointer p write-size)
                                     (decf (the fixnum iov-len) write-size)
                                     (loop-finish))))))))))))

(defun writev (fd vector)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array * (*)) vector)
           (type fixnum fd))
  (handler-case
   (loop with length fixnum = (length vector)
         for start fixnum = 0 then end
         for end fixnum  = (min (+ start +iov-max+) length)
         while (< start length)
         do (%writev fd vector start end))
    (sb-posix:syscall-error  (error)
      (if (and (eq (sb-posix:syscall-name error) 'sb-posix::writev)
               (= (the fixnum (sb-posix:syscall-errno error))
                  (the fixnum sb-posix:epipe)))
          (error 'connection-closed-by-peer)
          (error error)))))
