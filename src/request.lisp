(in-package :unpyo)

(defstruct request
  socket
  (buffer (make-array 4096 :element-type '(unsigned-byte 8)))
  method
  path
  params
  (%headers +unbound+)
  (cleanup ()))

(defun reset-request (request socket)
  (setf (request-socket request) socket
        (request-method request) nil
        (request-path request) nil
        (request-params request) nil
        (request-%headers request) +unbound+
        (request-cleanup request) ()))

(defun query-string (request)
  (let ((path (request-path request)))
    (aif (position #\? path)
         (subseq path (1+ it))
         "")))

(let ((key (chunk "Content-Type")))
  (defun request-content-type (request)
    (awhen (cdr (assoc key (request-headers request) :test #'chunk=))
      (request-header-value-string it))))

(let ((key (chunk "Content-Length")))
  (defun request-content-length (request)
    (awhen (cdr (assoc key (request-headers request) :test #'chunk=))
      (parse-integer (request-header-value-string it)))))

(defun request-header-value-string (chunk)
  (ppcre:regex-replace-all (ppcre:create-scanner "^\\s+" :multi-line-mode t) (chunk-to-string chunk) "" ))

(defun request-headers (request)
  (if (eq (request-%headers request) +unbound+)
      (setf (request-%headers request)
            (loop with buffer = (request-buffer request)
                  with i = (position #.(char-code #\cr) buffer)
                  until (search #.(sb-ext:string-to-octets (format nil "~c~c~c~c" #\cr #\lf #\cr #\lf))
                                buffer :start2 i :end2 (+ i 4))
                  collect (let* ((colon (position #.(char-code #\:) buffer :start (+ i 3)))
                                 (key (make-chunk :vector buffer :start (+ i 2) :end colon))
                                 (cr (request-header-value-end-position buffer (1+ colon)))
                                 (val (make-chunk :vector buffer :start (1+ colon) :end cr)))
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
  (apply #'%param (request-params *request*) keys))

(defun (setf param) (value &rest keys)
  (setf (request-params *request*)
        (%%prepare-params (mapcar (lambda (x)
                                    (typecase x
                                      (string x)
                                      (t (string-downcase x))))
                                  keys)
                          value
                          (request-params *request*))))

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

(defun prepare-params (request request-header-length read-length)
  (let ((query-string (query-string request)))
    (if (string/= query-string "")
        (setf (request-params request) (%prepare-params query-string))
        (setf (request-params request) nil)))
  (when (eq (request-method request) :post)
    (let ((content-type (request-content-type request)))
      (cond ((alexandria:starts-with-subseq "application/x-www-form-urlencoded"
                                            content-type)
             (parse-request-body request request-header-length read-length))
            ((alexandria:starts-with-subseq "multipart/form-data"
                                            content-type)
             (parse-multipart-form-data request request-header-length read-length))))))

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

(defun read-request-body (request request-header-length read-length)
  (let ((content-length (request-content-length request)))
    (when (zerop content-length)
      (return-from read-request-body ""))
    (let* ((header (request-buffer request))
           (buffer (fast-io:make-octet-vector content-length)))
      (loop for i from (+ request-header-length 4) below read-length
            for j from 0
            do (setf (aref buffer j) (aref header i)))
      (sb-sys:with-pinned-objects (buffer)
        (loop with offset = (- read-length (+ request-header-length 4))
              for n = (sb-posix:read (sb-bsd-sockets:socket-file-descriptor (request-socket request))
                                     (sb-sys:sap+ (sb-sys:vector-sap buffer) offset)
                                     (- content-length offset))
              if (zerop n)
                do (error "closed by client in reading request body!")
              do (incf offset n)
                 (when (= offset content-length)
                   (loop-finish))))
      (sb-ext:octets-to-string buffer :external-format :latin-1))))

(defun parse-request-body (request request-header-length read-length)
  (setf (request-params request)
        (append (%prepare-params (read-request-body request request-header-length read-length))
                (request-params request))))

(defun rfc2388::make-tmp-file-name ()
  (temporary-file::generate-random-pathname "/tmp/unpyo/%" 'temporary-file::generate-random-string))

(defun parse-multipart-form-data (request request-header-length read-length)
  (let ((boundary (cdr (rfc2388:find-parameter
                        "boundary"
                        (rfc2388:header-parameters
                         (rfc2388:parse-header (request-content-type request) :value)))))
        (body (read-request-body request request-header-length read-length)))
    (when boundary
      (setf (request-params request)
            (append
             (loop for part in (rfc2388:parse-mime body boundary)
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
                                           (push (lambda () (delete-file contents))
                                                 (request-cleanup request))
                                           (list contents
                                                 (rfc2388:get-file-name headers)
                                                 (rfc2388:content-type part :as-string t)))
                                         (sb-ext:octets-to-string
                                          (map '(vector (unsigned-byte 8) *) #'char-code contents))))))
             (request-params request))))))

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
