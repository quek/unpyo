(in-package :unpyo)

(defclass status-app ()
  ())

(defmethod call ((self status-app) env)
  (print 'call-app)
  (values 200 nil `(,(format nil "<html><body><p>~a</p><ul>~%" (local-time:now))
                    ,@(multiple-value-bind (k v) (scan-hash env)
                        (collect (format nil "<li>~a ~a</li>~%" k v)))
                    "</ul></body></html>")))