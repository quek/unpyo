(in-package :unpyo)

(define-condition unpyo-error (simple-error)
  ())

(define-condition connection-error (unpyo-error)
  ())

(defmethod my-debugger ((e connection-error))
  'デバッガ起動しない)

(define-condition http-parse-error (unpyo-error)
  ())
