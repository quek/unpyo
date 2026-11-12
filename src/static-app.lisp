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
  ;; バックスラッシュは、SBCL のパス名ではエスケープの文字。".\." が ".." と読まれて、
  ;; 配信元の外を指せてしまうので通さない（read-static-file でも確かめる）。
  (when (or (some (lambda (x) (search x url))
                  '(".." "//" "~" "\\"))
            (char= #\/ (alexandria:last-elt url)))
    (return-from handle-static-file nil))
  (loop for (uri-prefix base-path) in static-mappings
          thereis (and (etypecase uri-prefix
                         (string (alexandria:starts-with-subseq uri-prefix url))
                         (function (funcall uri-prefix url)))
                       (read-static-file url uri-prefix base-path))))

(defun static-file-under-base-p (file base-path)
  "FILE（URL を BASE-PATH に merge したもの）が、BASE-PATH のディレクトリの下を指すか。
ディレクトリに .. や . や :up :back があるもの、ワイルドカードや論理パス名になるものは、
配信元の外を指しうるので通さない。"
  (let ((base (pathname-directory (pathname base-path)))
        (dir (pathname-directory file)))
    (and (typep file '(and pathname (not logical-pathname)))
         (not (wild-pathname-p file))
         (consp base)
         (eq :absolute (car base))
         (listp dir)
         (<= (length base) (length dir))
         (equal base (subseq dir 0 (length base)))
         (every (lambda (x)
                  (and (stringp x)
                       (string/= x "..")
                       (string/= x ".")))
                (nthcdr (length base) dir))
         (not (member (pathname-name file) '("." "..") :test #'equal)))))

(defun read-static-file (url uri-prefix base-path)
  (let ((file (etypecase uri-prefix
                (string (let ((file (merge-pathnames (subseq url (length uri-prefix)) base-path)))
                          (and (static-file-under-base-p file base-path)
                               file)))
                (function base-path))))
    (when (and file (probe-file file))
      (setf (response-content-type *response*) (mimes:mime file))
      (with-open-file (in file :element-type '(unsigned-byte 8))
        (alexandria:copy-stream in (response-stream *response*)
                                :element-type '(unsigned-byte 8))))))
