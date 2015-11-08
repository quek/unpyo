(in-package :unpyo)

(defclass response-mixin (trivial-gray-streams:fundamental-character-output-stream)
  ((status :initform 200 :accessor status-of)
   (response-headers :initform (list (cons "Content-Type" "text/html")) )
   (set-cookies :initform ())
   (body :initform (make-array 128 :adjustable t :fill-pointer 0))))

(defmethod response-headers-of ((self response-mixin))
  (append (slot-value self 'response-headers)
          (mapcar (lambda (cookie)
                    (cons "Set-Cookie" (princ-to-string cookie)))
                  (slot-value self 'set-cookies))))

(defmethod trivial-gray-streams:stream-write-sequence ((response-mixin response-mixin) seq start end &key)
  (with-slots (body) response-mixin
    (when (plusp (- end start))
      (vector-push-extend (subseq seq start end) body))
    seq))

(defmethod trivial-gray-streams:stream-write-string ((response-mixin response-mixin) string
                                                     &optional (start 0) end)
  (with-slots (body) response-mixin
    (or end (setf end (length string)))
    (when (plusp (- end start))
      (vector-push-extend (string-to-octets (subseq string start end)) body))
    string))

(defmethod trivial-gray-streams:stream-write-char ((response-mixin response-mixin) character)
  (with-slots (body) response-mixin
    (vector-push-extend (string-to-octets (string character)) body)
    character))

(defmethod body-of ((response-mixin response-mixin))
  (with-slots (body) response-mixin
    body))


#+nil
(let ((r (make-instance 'response-mixin)))
  (write-sequence "hello" r)
  (write-sequence "world" r)
  (write-string "foo" r)
  (write-char #\a r)
  (format r "~a ~s ~d" 'x "bar" 123)
  (body-of r))
;;⇒ ("hello" "world" "foo" "a" "X" " " "\"" "b" "a" "r" "\"" " " "1" "2" "3")

