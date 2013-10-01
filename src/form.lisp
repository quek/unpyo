(in-package :unpyo)

(defvar *form-model* nil)
(defvar *form-key* nil)

(defmacro with-form ((&key model key) &body body)
  `(let ((*form-model* ,model)
         (*form-key* ,key))
     ,@body))

(defun input-field-name (name)
  (format nil "~(~a[~a]~)" (class-name (class-of *form-model*)) name))

(defmacro text (name &rest args)
  `(html (:input :type "text"
           :name ,(input-field-name name)
           :value (slot-value *form-model* ',name)
           ,@args)))


(defclass example-form-model ()
  ((first-name :initarg :first-name :initform "Momo")
   (last-name :initarg :last-name :initform "Nu")))

#+nil
(with-form (:model (make-instance 'example-form-model) :key :create-model)
  (text first-name))


#|
(with-form (:model user :key :register-user)
  (text name)
  (password password)
  (password password-confirmation)
  (textarea description)
  (submit :value "登録"))

(apply #'make-instance 'user (params-for :register-user))
(build-instance 'user (params-for :register-user))

(let ((user (find-user @id)))
  (setf (attributes user) (params-for :update-user)))


(defmacro with-form ((&key model key) &body body)
  '(let ((*model* ,model)
         (*key* key))
    ,@body))

(defun text (name &rest args)
  (add-param :name)
  (html `(:input :type :text :name ,name :value ,(param *model* name) ,@args)))
|#
