(in-package :unpyo)


(defparameter *session-key* "_s")
(defparameter *session-secret* "TODO この直は変えてね")
(defparameter *session-timeout* '(30 :minute))

(defun session-alist ()
  (let ((session (cookie *session-key*)))
    (when session
      (read-from-string (decrypt session *session-secret*)))))

(defun session (key)
  (cdr (assoc key (session-alist) :test #'string-equal)))

(defun (setf session) (value key)
  (let ((session (session-alist)))
    (aif (assoc key session :test #'string-equal)
         (setf (cdr it) value)
         (setf session (acons key value session)))
   (setf (cookie *session-key* :expires *session-timeout* :path "/")
         (encrypt (prin1-to-string session) *session-secret*))))

#|
(let ((test (encrypt "あほんだら羊" *session-secret*)))
  (print test)
  (decrypt test *session-secret*))
;;→ 
;;   15721455238159905589947132050530217400516234 
;;⇒ "あほんだら羊"
|#

