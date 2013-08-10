(in-package :unpyo)

(defvar *request* nil "a request object.")

(defclass request (response-mixin)
  ((app :initarg :app :reader app-of)
   (env :initarg :env :reader env-of)
   (io :initarg :io :reader io-of)
   (params)
   (external-format :initform :utf-8 :accessor external-format-of)
   (cleanup :initform nil)))

(defun make-request (app env)
  (make-instance 'request :app app :env env))

(defmethod initialize-instance :after ((request request) &key)
  (prepare-params request))

(defmethod cleanup ((request request))
  (with-slots (cleanup) request
    (dolist (x cleanup)
      (funcall x))))


(defun env (key &optional (request *request*))
  (gethash key (env-of request)))

(defun (setf env) (value key &optional (request *request*))
  (setf (gethash key (env-of request)) value))


(defun param (&rest keys)
  (with-slots (params) *request*
    (apply #'%param params keys)))

(defun (setf param) (value &rest keys)
  (with-slots (params) *request*
    (setf params
          (%%prepare-params
           (mapcar (lambda (x)
                     (typecase x
                       (string x)
                       (t (string-downcase x))))
                   keys)
           value params))))

(defun %param (params &rest keys)
  (reduce (lambda (value key)
            (typecase key
              (number
               (nth key value))
              (symbol
               (cdr (assoc key value :test 'string-equal)))
              (t
               (cdr (assoc key value :test 'equal)))))
          keys
          :initial-value params))

(defmethod prepare-params ((request request))
  (with-slots (env external-format params) request
    (let ((query-string (gethash "QUERY_STRING" env)))
      (if (string/= query-string "")
          (setf params (%prepare-params query-string external-format))
          (setf params nil)))
    (when (string-equal (gethash "REQUEST_METHOD" env) "POST")
      (cond ((alexandria:starts-with-subseq "application/x-www-form-urlencoded"
                                            (gethash "Content-Type" env))
             (parse-request-body request))
            ((alexandria:starts-with-subseq "multipart/form-data"
                                            (gethash "Content-Type" env))
             (parse-multipart-form-data request))))))

(defun %prepare-params (query-string external-format)
  (let (params)
    (loop for %k=%v in (split-sequence:split-sequence #\& query-string)
          for (%k %v) = (split-sequence:split-sequence #\= %k=%v)
          for k = (percent-decode %k external-format)
          for v = (percent-decode %v external-format)
          when (and k v)
            do (setf params (%%prepare-params
                             (mapcar (lambda (x) (string-right-trim "]" x))
                                     (split-sequence:split-sequence #\[ k))
                             v params)))
    params))

(defun %%prepare-params (ks v params)
  (if (endp ks)
      v
      (let ((key (car ks)))
        (if (every #'digit-char-p key)
            (progn
              (setf key (or (parse-integer key :junk-allowed t) (length params)))
              (let ((lack (1+ (- key (length params)))))
                (when (plusp lack)
                  (setf params (nconc params (make-list lack))))
                (setf (nth key params) (%%prepare-params (cdr ks) v (nth key params)))
                params))
            (let ((assoc (assoc key params :test 'equal)))
              (if (and assoc (consp (cdr assoc)))
                  (progn (rplacd assoc (%%prepare-params (cdr ks) v (cdr assoc)))
                         params)
                  (acons key (%%prepare-params (cdr ks) v nil) params) ))))))

(defun parse-request-body (request)
  (with-slots (env external-format params) request
    (let* ((content-length (parse-integer (gethash "Content-Length" env)))
           (buffer (fast-io:make-octet-vector content-length))
           (data (progn
                   (read-sequence buffer (gethash "unpyo.input" env))
                   (babel:octets-to-string buffer :encoding external-format))))
      (setf params (append (%prepare-params data external-format) params)))))


(defun rfc2388::make-tmp-file-name ()
  (temporary-file::generate-random-pathname "/tmp/unpyo/%" 'temporary-file::generate-random-string))

(defun parse-multipart-form-data (request)
  (with-slots (cleanup env external-format params) request
    (let ((boundary (cdr (rfc2388:find-parameter
                          "boundary"
                          (rfc2388:header-parameters
                           (rfc2388:parse-header (gethash "Content-Type" env) :value)))))
          (stream (flex:make-flexi-stream
                   (gethash +unpyo-input+ env)
                   :external-format #.(flex:make-external-format :latin1 :eol-style :lf))))
      (when boundary
        (setf params
              (append
               (loop for part in (rfc2388:parse-mime stream boundary)
                     for headers = (rfc2388:mime-part-headers part)
                     for content-disposition-header = (rfc2388:find-content-disposition-header headers)
                     for name = (cdr (rfc2388:find-parameter
                                      "name"
                                      (rfc2388:header-parameters content-disposition-header)))
                     when name
                       collect (cons name
                                     (let ((contents (rfc2388:mime-part-contents part)))
                                       (if (pathnamep contents)
                                           (progn
                                             (push (lambda () (delete-file contents)) cleanup)
                                             (list contents
                                                   (rfc2388:get-file-name headers)
                                                   (rfc2388:content-type part :as-string t)))
                                           (babel:octets-to-string
                                            (map '(vector (unsigned-byte 8) *) #'char-code contents)
                                            :encoding external-format)))))
               params))))))

(defun status ()
  (status-of *request*))

(defun (setf status) (value)
  (setf (status-of *request*) value))

(defun header (key)
  (cdr (assoc key (response-headers-of *request*) :test #'string-equal)))

(defun (setf header) (value key)
  (with-slots (response-headers) *request*
    (aif (assoc key response-headers :test #'string-equal)
         (rplacd it value)
         (setf response-headers (acons key value response-headers)))))

(defun redirect (url)
  (setf (status-of *request*) 302)
  (setf (header "Location") url))

(defun redirect-permanently (url)
  (setf (status-of *request*) 301)
  (setf (header "Location") url))

(defun request-uri ()
  (env "REQUEST_URI"))

(defun (setf content-type) (value)
  (setf (header "Content-Type") value))

(defun authorization ()
  (let ((authorization (env "Authorization")))
    (ppcre:register-groups-bind (data) ("Basic \(.*\)" authorization)
      (let ((user-password (base64:base64-string-to-string data)))
        (awhen (position #\: user-password)
          (values (subseq user-password 0 it)
                  (subseq user-password (1+ it))))))))

(defun require-authorization (&optional (realm "Unpyo"))
  (setf (status) 401
        (header "WWW-Authenticate")
        (format nil "Basic realm=\"~A\"" (quote-string realm))))