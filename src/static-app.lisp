(in-package :unpyo)

;; TODO コンパイル時に digest つけてメモリに持てるやつもほしい
;; 1バイナリにできるように
(defclass  static-application ()
  ((static-mappings :initarg :static-mappings :initform nil)))

(defmethod call ((self static-application))
  (let ((url (percent:decode (request-uri *request*))))
    (or (handle-static-file url (slot-value self 'static-mappings))
        (call-next-method))))

(defun handle-static-file (url static-mappings)
  (when (or (some (lambda (x) (search x url))
                  '(".." "//"))
            (char= #\/ (alexandria:last-elt url)))
    (return-from handle-static-file nil))
  (loop for (uri-prefix base-path) in static-mappings
          thereis (and (etypecase uri-prefix
                         (string (alexandria:starts-with-subseq uri-prefix url))
                         (function (funcall uri-prefix url)))
                       (read-static-file url uri-prefix base-path))))

(defun read-static-file (url uri-prefix base-path)
  (let ((file (etypecase uri-prefix
                (string (merge-pathnames (subseq url (length uri-prefix)) base-path))
                (function base-path))))
    (when (probe-file file)
      (setf (response-content-type *response*) (mimes:mime file))
      (with-open-file (in file :element-type '(unsigned-byte 8))
        (alexandria:copy-stream in (response-stream *response*)
                                :element-type '(unsigned-byte 8))))))
