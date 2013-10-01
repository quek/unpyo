(in-package :unpyo)

(defvar *application* nil)

(defclass application (app-routes-mixin)
  ())

(defmethod call :around ((app application))
  (let ((*application* app))
    (call-next-method)))

(defclass status-app ()
  ())

(defmethod call ((self status-app))
  (html
    (:html
      (:head
          (:meta :charset "utf-8")
        (:title "env"))
      (:body
          (:p (local-time:now))
        (:ul
            (maphash (lambda (k v)
                       (html (:li k " " v)))
                     (env-of *request*)))))))
