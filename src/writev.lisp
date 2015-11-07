(in-package :unpyo)

(cffi:defcstruct iovec
  ;; Starting address
  (iov-base :pointer)
  ;; Number of bytes to transfer
  (iov-len iolib.syscalls:size-t))

(defun writev (fd vector)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array * (*)) vector)
           (type fixnum fd))
  (labels ((call-with-pinned-objects (i fun)
             (declare (type (function () t) fun))
             (if (< i (length vector))
                 (sb-sys:with-pinned-objects ((aref vector i))
                   (call-with-pinned-objects (1+ i) fun))
                 (funcall fun))))
    (declare (inline call-with-pinned-objects))
    (call-with-pinned-objects
     0
     (lambda ()
       (let ((iovcnt (length vector)))
         (cffi:with-foreign-object (iov '(:struct iovec) iovcnt)
           (loop for p = iov then (cffi:inc-pointer p (cffi:foreign-type-size '(:struct iovec)))
                 for i across vector
                 do (cffi:with-foreign-slots ((iov-base iov-len) p (:struct iovec))
                      (setf iov-base (sb-sys:vector-sap i)
                            iov-len (length i))))
           (loop
             (handler-case
                 (progn
                   (return (iolib.syscalls:writev fd iov iovcnt)))
               (isys:ewouldblock ()
                 (print 'fast-write-ewouldblock)
                 (iomux:wait-until-fd-ready fd :output 1 nil))))))))))
