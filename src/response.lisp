(in-package :unpyo)

(defstruct response
  (body (make-array 256 :adjustable t :fill-pointer 0))
  (status 200)
  (content-type "text/html")
  (headers ()))

(defun reset-response (response)
  (setf (response-status response) 200
        (fill-pointer (response-body response)) 0
        (response-content-type response) "text/html"
        (response-headers response) ()))

(defun make-response-header (response)
  (format nil "HTTP/1.1 ~d ~a
Content-Length: ~d
Content-Type: ~a
~:{~a: ~a
~}
"
          (response-status response)
          (gethash (response-status response) *http-status-codes*)
          (loop for i across (response-body response) sum (length i))
          (response-content-type response)
          (response-headers response)))

;;(make-response-header (make-response :headers '(("a" "b"))))
;;⇒ "HTTP/1.1 200 OK
;;   Content-Length: 0
;;   Content-Type: text/html
;;   a: b
;;   
;;   "

(defun response-header (response name)
  (aif (find name (response-headers response) :test #'equal)
       (cadr it)))

(defun (setf response-header) (value response name)
  (aif (find name (response-headers response) :test #'equal)
       (setf (cadr it) value)
       (push (list name value) (response-headers response))))

(defun redirect (url)
  (setf (response-status *response*) 302)
  (setf (response-header *response* "Location") url))

(defun redirect-permanently (url)
  (setf (response-status *response*) 301)
  (setf (response-header *response* "Location") url))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; stream
(defclass response-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((body :initarg :body)))

(defun response-stream (response)
  (make-instance 'response-stream :body (response-body response)))

(defmethod trivial-gray-streams:stream-write-sequence ((stream response-stream) seq start end &key)
  (with-slots (body) stream
    (when (plusp (- end start))
      (vector-push-extend (subseq seq start end) body)))
  seq)

(defmethod trivial-gray-streams:stream-write-string ((stream response-stream) string
                                                     &optional (start 0) end)
  (or end (setf end (length string)))
  (with-slots (body) stream
    (when (plusp (- end start))
      (vector-push-extend (string-to-octets (subseq string start end)) body)))
  string)

(defmethod trivial-gray-streams:stream-write-char ((stream response-stream) character)
  (with-slots (body) stream
   (vector-push-extend (string-to-octets (string character)) body))
  character)

#+nil
(let ((r (response-stream (make-response))))
  (write-sequence "hello" r)
  (write-sequence "world" r)
  (write-string "foo" r)
  (write-char #\a r)
  (format r "~a ~s ~d" 'x "bar" 123)
  (slot-value r 'body))
;;⇒ #("hello" "world" #(102 111 111) #(97) #(88) #(32) #(34) #(98) #(97) #(114)
;;     #(34) #(32) #(49) #(50) #(51))

