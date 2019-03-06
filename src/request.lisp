(in-package :unpyo)

(defstruct request
  socket
  (buffer (make-array 4096 :element-type '(unsigned-byte 8)))
  method
  path
  params
  (%headers +unbound+)
  (body-start 0)
  (body-end 0)
  %body
  (cleanup ()))

(defun reset-request (request socket)
  (setf (request-socket request) socket
        (request-method request) nil
        (request-path request) nil
        (request-params request) nil
        (request-%headers request) +unbound+
        (request-body-start request) 0
        (request-body-end request) 0
        (request-%body request) nil
        (request-cleanup request) ()))

(defun request-uri (request)
  (let ((path (request-path request)))
    (aif (position #\? path)
         (subseq path 0 it)
         path)))

(defun request-query-string (request)
  (let ((path (request-path request)))
    (aif (position #\? path)
         (subseq path (1+ it))
         nil)))

(let ((key1 (chunk "Content-Type"))
      ;; webpack-dev-server が小文字にしちゃう https://github.com/webpack/webpack-dev-server/issues/534
      (key2 (chunk "content-type")))
  (defun request-content-type (request)
    (awhen (cdr (or (assoc key1 (request-header-chunks request) :test #'chunk=)
                    (assoc key2 (request-header-chunks request) :test #'chunk=)))
      (request-header-value-string it))))

(let ((key1 (chunk "Content-Length"))
      (key2 (chunk "content-length")))
  (defun request-content-length (request)
    (awhen (cdr (or (assoc key1 (request-header-chunks request) :test #'chunk=)
                    (assoc key2 (request-header-chunks request) :test #'chunk=)))
      (parse-integer (request-header-value-string it)))))

(let ((key1 (chunk "Cookie"))
      (key2 (chunk "cookie")))
  (defun request-cookie (request)
    (awhen (cdr (or (assoc key1 (request-header-chunks request) :test #'chunk=)
                    (assoc key2 (request-header-chunks request) :test #'chunk=)))
      (request-header-value-string it))))

(defun request-header-value-string (chunk)
  (ppcre:regex-replace-all (ppcre:create-scanner "^\\s+" :multi-line-mode t) (chunk-to-string chunk) "" ))

(defun request-header-chunks (request)
  (if (eq (request-%headers request) +unbound+)
      (setf (request-%headers request)
            (loop with buffer = (request-buffer request)
                  with i = (position #.(char-code #\cr) buffer)
                  until (search #.(sb-ext:string-to-octets (format nil "~c~c~c~c" #\cr #\lf #\cr #\lf))
                                buffer :start2 i :end2 (+ i 4))
                  collect (let* ((colon (position #.(char-code #\:) buffer :start (+ i 3)))
                                 (key (make-chunk :vector buffer :start (+ i 2) :end colon))
                                 (cr (request-header-value-end-position buffer (1+ colon)))
                                 (val-start (loop for i from (1+ colon) below cr
                                                  if (/= (aref buffer i)
                                                         #.(char-code #\space))
                                                    do (loop-finish)
                                                  finally (return i)))
                                 (val (make-chunk :vector buffer :start val-start :end cr)))
                            (setf i cr)
                            (cons key val))))
      (request-%headers request)))

(defun request-header-value-end-position (buffer start)
  (let* ((cr (position #.(char-code #\cr) buffer :start start))
         (next-line (aref buffer (+ cr 2))))
    (if (or (= next-line #.(char-code  #\space))
            (= next-line #.(char-code  #\tab)))
        (request-header-value-end-position buffer (+ cr 2))
        cr)))

(defun param (&rest keys)
  (%param (request-params *request*) keys))

(defun (setf param) (value &rest keys)
  (setf (request-params *request*)
        (%%prepare-params (mapcar (lambda (x)
                                    (typecase x
                                      (string x)
                                      (t (string-downcase x))))
                                  keys)
                          value
                          (request-params *request*))))

(defun %param (params keys)
  (if (or (typep params 'jsonq:obj)
          (typep params 'jsonq:arr))
      (apply #'jsonq:q params keys)
   (if (endp keys)
       (values params t)
       (let ((key (car keys)))
         (typecase key
           (number
            (%param (nth key params) (cdr keys)))
           (symbol
            (let ((cons (assoc key params :test 'string-equal)))
              (if cons
                  (%param (cdr cons) (cdr keys))
                  (values nil nil))))
           (t
            (let ((cons (assoc key params :test 'equal)))
              (if cons
                  (%param (cdr cons) (cdr keys))
                  (values nil nil)))))))))

(defun prepare-params (request request-header-length read-length)
  (setf (request-params request)
        (aif (request-query-string request)
             (%prepare-params it)
             nil))
  (when (member (request-method request) '(:post :patch :put))
    (setf (request-body-start request) (+ 4 request-header-length) ;4 is crlfcrlf
          (request-body-end request) read-length)
    (let ((content-type (request-content-type request)))
      (cond ((alexandria:starts-with-subseq "application/x-www-form-urlencoded"
                                            content-type)
             (parse-request-body request))
            ((alexandria:starts-with-subseq "multipart/form-data"
                                            content-type)
             (parse-multipart-form-data request))
            ((alexandria:starts-with-subseq "application/json" content-type)
             (request-json-body-to-params request))))))

(defun %prepare-params (query-string)
  (let (params)
    (loop for %k=%v in (split-sequence:split-sequence #\& query-string)
          for (%k %v) = (split-sequence:split-sequence #\= %k=%v)
          for k = (percent-decode %k)
          for v = (percent-decode %v)
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
  (setf (request-params request)
        (append (%prepare-params (request-body request))
                (request-params request))))

(defun rfc2388::make-tmp-file-name ()
  (temporary-file::generate-random-pathname "/tmp/unpyo/%" 'temporary-file::generate-random-string))

(defun parse-multipart-form-data (request)
  (let ((boundary (cdr (rfc2388:find-parameter
                        "boundary"
                        (rfc2388:header-parameters
                         (rfc2388:parse-header (request-content-type request) :value)))))
        (body (request-body request :latin1)))
    (when boundary
      (let ((params (request-params request)))
        (loop for part in (rfc2388:parse-mime body boundary)
              for headers = (rfc2388:mime-part-headers part)
              for content-disposition-header = (rfc2388:find-content-disposition-header headers)
              for name = (cdr (rfc2388:find-parameter
                               "name"
                               (rfc2388:header-parameters content-disposition-header)))
              when name
                do (setf params
                         (%%prepare-params
                          (mapcar (lambda (x) (string-right-trim "]" x))
                                  (split-sequence:split-sequence #\[ name))
                          (let ((contents (rfc2388:mime-part-contents part)))
                            (if (pathnamep contents)
                                (progn
                                  (push (lambda () (delete-file contents))
                                        (request-cleanup request))
                                  (list contents
                                        (decode-file-name (rfc2388:get-file-name headers))
                                        (rfc2388:content-type part :as-string t)))
                                (sb-ext:octets-to-string
                                 (map '(vector (unsigned-byte 8) *) #'char-code contents)
                                 :external-format :utf8)))
                          params)))
        (setf (request-params request) params)))))

(defun decode-file-name (file-name)
  (sb-ext:octets-to-string
   (sb-ext:string-to-octets file-name :external-format :latin1)
   :external-format :utf8))

(defun request-json-body-to-params (request)
  (aif (request-body request)
       (setf (request-params request)
             (jsonq:lisp (jsonq:read-json-from-string it)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; stream
(defclass request-stream (trivial-gray-streams::fundamental-binary-input-stream)
  ((request :initarg :request)))

(defun request-stream (request)
  (make-instance 'request-stream :request request))

(defmethod trivial-gray-streams:stream-read-sequence ((stream request-stream)
                                                      sequence
                                                      start end &key &allow-other-keys)
  (with-slots (request) stream
    (with-slots (body-start body-end buffer socket) request
      (when (< body-start body-end)
        (replace sequence buffer
                 :start1 start :end1 end
                 :start2 body-start :end2 body-end)
        (let ((size (min (- end start) (- body-end body-start))))
          (incf body-start size)
          (incf start size)
          (when (= start end)
            (return-from trivial-gray-streams:stream-read-sequence start))))
      (loop for n = (sb-sys:with-pinned-objects (sequence)
                      (sb-posix:read (sb-bsd-sockets:socket-file-descriptor socket)
                                     (sb-sys:sap+ (sb-sys:vector-sap sequence) start)
                                     (- end start)))
            do (incf start n)
               (when (or (= start end)
                         (zerop n))
                 (loop-finish)))))
  start)

(defun request-body (&optional (request *request*) (external-format :utf8))
  (sif (request-%body request)
       it
       (setf it
             (let ((content-length (request-content-length request)))
               (if content-length
                   (let* ((buffer (fast-io:make-octet-vector content-length))
                          (stream (request-stream request)))
                     (read-sequence buffer stream)
                     (sb-ext:octets-to-string buffer :external-format external-format))
                   "")))))

(defun request-headers (&optional (request *request*))
  (loop for (key . value) in (request-header-chunks request)
        collect (cons (chunk-to-string key)
                      (chunk-to-string value))))

(defun request-header-value (name &optional (request *request*))
  (loop for (key . value) in (request-headers request)
          thereis (and (string-equal name key) value)))

#|
(defun cookie (name)
  (and *request*
       (or
        (loop for cookie in (slot-value *request* 'set-cookies)
                thereis (and (string= name (cookie-name cookie))
                             (cookie-value cookie)))
        (awhen (env "Cookie")
          (let ((value (cadr (find name (mapcar (lambda (x)
                                                  (cl-ppcre:split "=" x :limit 2))
                                                (cl-ppcre:split ";\\s*" it))
                                   :test #'string=
                                   :key #'car))))
            (and value (percent-decode value :utf-8)))))))

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
|#
