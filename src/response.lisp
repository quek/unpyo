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
