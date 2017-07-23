(in-package :unpyo)

(defclass logging-mixin ()
  ())

(defmethod call :around ((app logging-mixin))
  (let ((path (request-path *request*))
        (start (local-time:now)))
    (log:info "start ~a" path)
    (unwind-protect
         (call-next-method)
      (multiple-value-bind (m n)
          (truncate (ceiling (local-time:timestamp-difference (local-time:now) start)
                             0.000001)
                    1000)
       (log:info "end ~a (~d.~dms)" path m n)))))
