(in-package :unpyo.test)

(def-suite request :in all)
(in-suite request)

(test get-http/1.1
  (iolib.sockets:with-open-socket (s :remote-host *test-host* :remote-port *test-port*)
    (format s "GET / HTTP/1.1~aHost:localhost~a~a" +crlf+ +crlf+ +crlf+)
    (force-output s)
    (is (string= "HTTP/1.1 200 OK" (line s)))
    (let ((env (env-of *app*)))
      (is (string= "GET" (gethash "REQUEST_METHOD" env))))))

(test by-dramka
  (multiple-value-bind (body status)
      (drakma:http-request (format nil "http://~a:~d" *test-host* *test-port*))
    (is (= 200 status))))

(test post-http/1.1
  (iolib.sockets:with-open-socket (s :remote-host *test-host* :remote-port *test-port*)
    (emit s "POST / HTTP/1.1")
    (emit s "Host:localhost")
    (emit s "Content-Length:3")
    (emit s "")
    (format s "a=b")
    (force-output s)
    (is (string= "HTTP/1.1 200 OK" (line s)))
    (let ((env (env-of *app*)))
      (is (string= "POST" (gethash "REQUEST_METHOD" env)))
      (let ((buffer (fast-io:make-octet-vector 10)))
        (is (= 3 (read-sequence buffer (gethash unpyo::+unpyo-input+ env))))
        (is (string= "a=b" (babel:octets-to-string buffer :end 3)))))))

(test post-big-data
  (let* ((size (+ unpyo::+max-body+ unpyo::+chunk-size+))
         (data (make-string size :initial-element #\Q))
         (buffer (fast-io:make-octet-vector size))
         (read-size -1))
    (setf (call-back-of *app*)
          (lambda (env)
            (setf read-size (read-sequence buffer (gethash unpyo::+unpyo-input+ env)))))
    (iolib.sockets:with-open-socket (s :remote-host *test-host* :remote-port *test-port*)
      (emit s "POST / HTTP/1.1")
      (emit s "Host:localhost")
      (emit s "Content-Length:~d" size)
      (emit s "")
      (emit s data)
      (force-output s)
      (is (string= "HTTP/1.1 200 OK" (line s)))
      (is (= size read-size))
      (is (equal (subseq data 0 20) (subseq (babel:octets-to-string buffer) 0 20))))))


(with-test-server (server)
  (debug!))