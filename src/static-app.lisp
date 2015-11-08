(in-package :unpyo)

(defclass  static-application ()
  ((static-mappings :initarg :static-mappings :initform nil)))

(defmethod call ((self static-application))
  (let ((url (gethash "REQUEST_PATH" (env-of *request*))))
    (or (handle-static-file url (slot-value self 'static-mappings))
        (call-next-method))))

(defun handle-static-file (url static-mappings)
  (unless (char= #\/ (alexandria:last-elt url))
    (loop for (uri-prefix base-path) in static-mappings
            thereis (and (etypecase uri-prefix
                           (string (alexandria:starts-with-subseq uri-prefix url))
                           (function (funcall uri-prefix url)))
                         (read-static-file url uri-prefix base-path)))))

(defun read-static-file (url uri-prefix base-path)
  (let ((file (etypecase uri-prefix
                (string (merge-pathnames (subseq url (length uri-prefix)) base-path))
                (function base-path))))
    (when (probe-file file)
      (setf (content-type) (mimes:mime url))
      (with-open-file (in file :element-type '(unsigned-byte 8))
        (alexandria:copy-stream in *request* :element-type '(unsigned-byte 8))))))
