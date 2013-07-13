(in-package :unpyo)

(defvar *parser* nil)

(defclass http-parser ()
  ((parse-function :initform #'parse-method)
   (finished :initform nil :reader finished-p)
   (body :initform nil :reader body-of)))

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
         (setf (gethash "SERVER_PROTOCOL" env) protocol)
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
  (cond ((= start end)
         (values nil start #'parse-header-end))
        ((/= #x0a (aref buffer start))
         (error 'http-parse-error :format-control "parse-header-end :buffer ~a :start ~a"
                                  :format-control (list buffer start)))
        ((not (equal (gethash "REQUEST_METHOD" env) "POST"))
         (values t (1+ start) nil))
        (t
         (incf start)
         (if (string= (gethash "Transfer-Encoding" env) "chunked")
             (parse-chunked-post-data buffer start end env)
             (pares-length-post-data buffer start end env)))))

(defun parse-chunked-post-data (buffer start end env)
  (with-slots (body) *parser*
    (setf body (make-array 4096 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer t))
    (parse-chunked-post-data-length buffer start end env)))

(defun parse-chunked-post-data-length (buffer start end env)
  (aif (search #(#x0d #x0a) buffer :start2 start :end2 end)
       (progn
         (let ((len (parse-integer (babel:octets-to-string buffer :start start :end it)
                                   :radix 16)))
           (if (zerop len)
               (parse-chunked-post-data-end buffer (+ it 2) end env)
               (funcall (make-parse-chunked-post-data-data len)
                        buffer (+ it 2) end env))))
       (values nil start #'parse-chunked-post-data-length)))

(defun make-parse-chunked-post-data-data (len)
  (lambda (buffer start end env)
    (loop with body = (slot-value *parser* 'body)
          do (cond ((= start end)
                    (return (values nil start (make-parse-chunked-post-data-data len))))
                   ((zerop len)
                    (return (parse-chunked-post-data-data-end buffer start end env)))
                   (t
                    (vector-push-extend (aref buffer start) body)
                    (incf start)
                    (decf len))))))

(defun parse-chunked-post-data-data-end (buffer start end env)
  (if (> (+ 2 start) end)
      (values nil start #'parse-chunked-post-data-data-end)
      (if (and (= #x0d (aref buffer start))
               (= #x0a (aref buffer (1+ start))))
          (parse-chunked-post-data-length buffer (+ 2 start) end env)
          (error 'invalid-env))))

(defun parse-chunked-post-data-end (buffer start end env)
  (declare (ignore env))
  (aif (search #(#x0d #x0a) buffer :start2 start :end2 end)
       (values t (+ it 2) nil)
       (values nil start #'parse-chunked-post-data-end)))

(defun pares-length-post-data (buffer start end env)
  (with-slots (body) *parser*
    (let ((length (parse-integer (gethash "Content-Length" env))))
      (setf body (make-array length :element-type '(unsigned-byte 8)))
      (funcall (make-parse-length-post-data-data 0 length)
               buffer start end env))))

(defun make-parse-length-post-data-data (start1 end1)
  (lambda (buffer start2 end2 env)
    (declare (ignore env))
    (let ((body (slot-value *parser* 'body)))
      (replace body buffer
               :start1 start1
               :end1 end1
               :start2 start2
               :end2 end2)
      (if (<= (- end1 start1) (- end2 start2))
          (values t end2 nil)
          (values nil end2 (make-parse-length-post-data-data (+ start1 (- end2 start2)) end1))))))

(defun make-parse-post-data (read-length content-length)
  (lambda (buffer start end env)
    (declare (ignore env))
    (with-slots (body) *parser*
      (replace body buffer
               :start1 start
               :end1 end
               :start2 read-length
               :end2 content-length)
      (if (<= (- content-length read-length) (- end start))
          (values t end nil)
          (values nil end (make-parse-post-data (+ read-length (- end start))
                                                content-length))))))


