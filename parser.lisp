(in-package :unpyo)

(defvar *parser* nil)

(defclass http-parser ()
  ((parse-function :initform #'parse-method)
   (finished :initform nil :reader finished-p)
   (body :initform nil :reader body-of)))

(defmethod reset ((self http-parser) &key fast-check)
  (declare (ignore fast-check))
  (with-slots (parse-function finished body) self
    (setf parse-function #'parse-method
          finished nil
          body nil)))

(defmethod execute ((self http-parser) env buffer start &optional (end (length buffer)))
  (let ((*parser* self))
    (with-slots (finished parse-function) self
      (prog ()
       :restart
         (multiple-value-bind (ok position next) (funcall parse-function buffer start end env)
           (if ok
               (if next
                   (setf parse-function next
                         start position)
                   (progn
                     (setf finished t)
                     (return position)))
               (progn
                 (setf parse-function next)
                 (return position))))
         (go :restart)))))

(defmacro request-method-matche-p (method buffer start)
  `(and ,@(loop for i below (length method)
                collect `(= ,(char-code (aref method i)) (aref ,buffer (+ ,i ,start))))
        (= #x20 (aref ,buffer (+ ,(length method) ,start)))))

(defun parse-method (buffer start end env)
  (if (< (- end start) #.(length "GET / HTTP/1.1"))
      (values nil start #'parse-method)
      (cond ((request-method-matche-p "GET" buffer start)
             (setf (gethash "REQUEST_METHOD" env) "GET")
             (parse-request-uri buffer (+ start 4) end env))
            ((request-method-matche-p "POST" buffer start)
             (setf (gethash "REQUEST_METHOD" env) "POST")
             (parse-request-uri buffer (+ start 5) end env))
            (t (error 'invalid-request)))))

(defun parse-request-uri (buffer start end env)
  (aif (position #x20 buffer :start start :end end)
       (progn
         (setf (gethash "REQUEST_URI" env) (babel:octets-to-string buffer :start start :end it))
         (parse-protocol buffer (1+ it) end env))
       (values nil start #'parse-request-uri)))

(defun parse-protocol (buffer start end env)
  (aif (position #x0d buffer :start start :end end)
       (let ((protocol (babel:octets-to-string buffer :start start :end it)))
         (setf (gethash "SERVER_PROTOCOL" env) protocol
               (gethash "HTTP_VERSION" env) protocol)
         (parse-header buffer (1+ it) end env))
       (values nil start #'parse-protocol)))

(defun store-header (buffer start end env)
  (let ((colon (position #.(char-code #\:) buffer :start start :end end)))
    (unless colon
      (error 'http-parse-error :format-control "store-hader :buffer ~a :start ~a"
                               :format-arguments (list buffer start)))
    (let ((name (babel:octets-to-string buffer :start start :end colon))
          (value (babel:octets-to-string buffer :start (1+ colon) :end end)))
      (setf (gethash name env) value))))

(defun parse-header (buffer start end env)
  (prog ()
   :start
     (when (= start end)
       (return (values nil start #'parse-header)))
     (when (/= #x0a (aref buffer start))
       (error 'http-parse-error :format-control "parse-header :buffer ~a :start ~a"
                                :format-arguments (list buffer start)))
     (aif (position #x0d buffer :start (incf start) :end end)
          (if (= it start)
              (return (parse-header-end buffer (1+ start) end env))
              (progn
                (store-header buffer start it env)
                (setf start (1+ it))
                (go :start)))
          (return (values nil (1- start) #'parse-header)))))

(defun parse-header-end (buffer start end env)
  (declare (ignore env))
  (cond ((= start end)
         (values nil start #'parse-header-end))
        ((/= #x0a (aref buffer start))
         (error 'http-parse-error :format-control "parse-header-end :buffer ~a :start ~a"
                                  :format-control (list buffer start)))
        (t
         (incf start)
         (let ((out (make-buffer)))
           ;; (fast-io:fast-write-sequence buffer out start end)
           (fast-io:fast-write-sequence (subseq buffer start end) out)
           (setf (slot-value *parser* 'body) out))
         (values t start nil))))
