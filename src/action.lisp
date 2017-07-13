(in-package :unpyo)


(defvar *routes* ())

(defstruct route
  action
  name
  method
  path
  function
  priority)

(defun route= (x y)
  (and (eq (route-name x) (route-name y))
       (or (null (route-method x))
           (null (route-method y))
           (eq (route-method x) (route-method y)))))

(defmacro with-@param (&body body)
  (labels ((@p (thing)
             (and (symbolp thing)
                  (> (length (symbol-name thing)) 1)
                  (string= thing "@" :end1 1)))
           (keys (symbol)
             (mapcar (lambda (x) (intern x :keyword))
                     (split-sequence:split-sequence #\. (subseq (symbol-name symbol) 1))))
           (f (form)
             (cond ((@p form)
                    `(param ,@(keys form)))
                   ((atom form)
                    form)
                   (t
                    (cons (f (car form)) (f (cdr form)))))))
    `(progn ,@(f body))))

(defmacro defaction (name (&key
                             (method :get)
                             (path (string-downcase name))
                             route-function
                             (route-priority (compute-route-priority path)))
                     &body body)
  (let ((name-method (if method (intern (str name ":" method)) name)))
    `(progn
       (defun ,name-method ()
         (with-@param ,@body))
       ,@(when (eq method :get) (make-path-function-form name))
       (add-to-routes (make-route :action ',name-method
                                  :name ',name
                                  :method ,method
                                  :path ,path
                                  :function ,(or route-function
                                                 `(make-route-function ,path ,method))
                                  :priority ,route-priority)))))

(defun make-path-function-form (name)
  (let* ((format (str (ppcre:regex-replace-all "@[^/]+" (string-downcase name) "~a")
                      "~@[?~a~]"))
         (function-name (intern (str name "-PATH")))
         (args (loop for match in (ppcre:all-matches-as-strings "@[^/]+" (string-downcase name))
                     collect (intern (string-upcase (subseq match 1))))))
    (let ((query-parameters (gensym)))
     `((defun ,function-name (,@args &rest ,query-parameters)
         (format nil ,format
                 ,@(mapcar (lambda (x) `(percent-encode (princ-to-string ,x))) args)
                 (and ,query-parameters (plist-to-query-string ,query-parameters))))))))

(defun add-to-routes (route)
  (setf *routes* (delete route *routes* :test #'route=))
  (push route *routes*)
  (setf *routes* (sort *routes*
                       (lambda (a b)
                         (<= (route-priority a) (route-priority b))))))

(defun make-route-function (route-path route-method)
  (if (position #\@ route-path)
      (let* ((parts (split-sequence:split-sequence #\/ route-path :remove-empty-subseqs t))
             binds
             (regex (format nil "\\A/~{~a~^/~}"
                            (loop for part in parts
                                  if (char= #\@ (char part 0))
                                    collect (progn
                                              (push (subseq part 1) binds)
                                              "([^/]+)")
                                  else
                                    collect (ppcre:quote-meta-chars part)))))
        (lambda (url method)
          (and (or (null route-method)
                   (string-equal method route-method))
               (multiple-value-bind (match-p groups) (ppcre:scan-to-strings regex url)
                 (when match-p
                   (loop for group across groups
                         for bind in binds
                         do (setf (param bind)
                                  (percent-encoding:decode
                                   group :encoding :utf-8
                                         :www-form t)))
                   t)))))
      (lambda (url method)
        (and (equal url route-path)
             (or (null route-method)
                 (string-equal method route-method))))))

(defun compute-route-priority (path)
  (if (position #\@ path)
      (- 100 (* (count #\/ path) 10))
      most-negative-fixnum))

(define-condition not-handlable-request (error) ())

(defun call-next-action ()
  (error 'not-handlable-request))

(defclass app-routes-mixin ()
  ())

(defmethod call ((app app-routes-mixin))
  (let* ((url (request-path *request*))
         (url (aif (position #\? url) (subseq url 0 it) url))
         (method (request-method *request*)))
      (loop for route in *routes*
        if (funcall (route-function route) url method)
          do (handler-case
                 (return-from call (funcall (route-action route)))
               (not-handlable-request ())))
    (404-not-found app)))

(defmethod 404-not-found ((app app-routes-mixin))
  (setf (response-status *response*) 404)
  (html "404 not found"))
