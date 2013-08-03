(in-package :unpyo)


(defvar *routes* ())

(defstruct route
  action
  path
  function
  priority)

(defmacro with-@param (&body body)
  (labels ((@p (thing)
             (and (symbolp thing)
                  (plusp (length (symbol-name thing)))
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
                             (path (string-downcase name))
                             route-function
                             (route-priority (compute-route-priority path)))
                     &body body)
  `(progn
     (defun ,name ()
       (with-@param ,@body))
     (add-to-routes ',name
                    ,path
                    ,(or route-function `(make-route-function ,path))
                    ,route-priority)))

(defun add-to-routes (action path route-function route-priority)
  (setf *routes* (delete-if (lambda (x) (eq action (route-action x))) *routes*))
  (let ((route (make-route :action action
                           :path path
                           :function route-function
                           :priority route-priority)))
    (push route *routes*)
    (setf *routes* (sort *routes*
                         (lambda (a b)
                           (<= (route-priority a) (route-priority b)))))))

(defun make-route-function (path)
  (if (position #\@ path)
      (let* ((parts (split-sequence:split-sequence #\/ path :remove-empty-subseqs t))
             binds
             (regex (format nil "~{~a~^/~}"
                            (loop for part in parts
                                  if (char= #\@ (char part 0))
                                    collect (progn
                                              (push (subseq part 1) binds)
                                              "([^/]+)")
                                  else
                                    collect (ppcre:quote-meta-chars part)))))
        (lambda (url)
          (multiple-value-bind (match-p groups) (ppcre:scan-to-strings regex url)
            (when match-p
              (loop for group across groups
                    for bind in binds
                    do (setf (param bind)
                             (percent-encoding:decode
                              group :encoding (external-format-of *request*))))
              t))))
      (lambda (url) (equal url path))))

(defun compute-route-priority (path)
  (aif (position #\@ path)
       (- 100 (* (count #\/ (subseq path 0 it)) 10))
       most-negative-fixnum))

(defun url-to-action (url)
  (collect-first (choose (let ((route (scan *routes*)))
                           (if (funcall (route-function route) url)
                               (route-action route))))))

(defclass app-routes-mixin ()
  ())

(defmethod call ((app app-routes-mixin))
  (let* ((url (gethash "REQUEST_PATH" (env-of *request*)))
         (action (url-to-action url)))
    (if action
        (funcall action)
        (progn
          (setf (status-of *request*) 400)
          (html "404 not found")))))