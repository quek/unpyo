(in-package :unpyo)

(defclass logging-mixin ()
  ())

(defmethod call :around ((app logging-mixin))
  (let ((path (request-path *request*)))
    (log:info "start ~a" path)
    (unwind-protect
         (call-next-method)
      (log:info "end ~a" path))))
