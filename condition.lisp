(in-package :unpyo)

(define-condition unpyo-error (simple-error)
  ())

(define-condition connection-error (unpyo-error)
  ())

(define-condition http-parse-error (unpyo-error)
  ())
