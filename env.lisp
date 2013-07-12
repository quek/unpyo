(in-package #:unpyo)

(defclass env ()
  ((env :initform (make-hash-table :test 'equal))))

(defmethod value ((env env) key)
  (gethash key (slot-value env 'env)))

(defmethod (setf value) (value (env env) key)
  (setf (gethash key (slot-value env 'env)) value))

