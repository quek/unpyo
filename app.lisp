(in-package :unpyo)

(defclass status-app ()
  ())

(defmethod call ((self status-app) env)
  (values 200 nil '("OK")))