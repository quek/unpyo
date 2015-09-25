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
          (lambda (request)
            (setf read-size (read-sequence buffer (unpyo:env unpyo::+unpyo-input+ request)))))
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

(test head-http/1.1
  (iolib.sockets:with-open-socket (s :remote-host *test-host* :remote-port *test-port*)
    (format s "HEAD / HTTP/1.1~aHost:localhost~a~a" +crlf+ +crlf+ +crlf+)
    (force-output s)
    (is (string= "HTTP/1.1 200 OK" (line s)))
    (let ((env (env-of *app*)))
      (is (string= "HEAD" (gethash "REQUEST_METHOD" env))))))


(unpyo:defaction /method (:method :get)
  (unpyo:html "method is get"))
(unpyo:defaction /method (:method :post)
  (unpyo:html "method is post"))
(unpyo:defaction /method (:method :put)
  (unpyo:html "method is put"))
(unpyo:defaction /method (:method :delete)
  (unpyo:html "method is delete"))

(test defaction-method
  (is (ppcre:scan "method is get" (drakma:http-request (test-url "/method") :method :get)))
  (is (ppcre:scan "method is post" (drakma:http-request (test-url "/method") :method :post)))
  (is (ppcre:scan "method is put" (drakma:http-request (test-url "/method") :method :put)))
  (is (ppcre:scan "method is delete" (drakma:http-request (test-url "/method") :method :delete))))


(unpyo:defaction /set-cookie ()
  (setf (unpyo:cookie "foo") "bar")
  (setf (unpyo:cookie "あ い;,う"
                      :expires (local-time:encode-timestamp 0 1 2 3 4 5 2014
                                                            :timezone local-time:+gmt-zone+)
                      :path "/a"
                      :domain *test-host*
                      :secure t
                      :http-only t)
        "か き;,く"))

(test set-cookie
  (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
    (drakma:http-request (test-url "/set-cookie") :cookie-jar cookie-jar)
    (let ((cookies (drakma:cookie-jar-cookies cookie-jar)))
      (is (= 2 (length cookies)))
      (let ((foo (find "foo" cookies :key #'drakma:cookie-name :test #'string=)))
        (is (string= "bar" (drakma:cookie-value foo)))
        (is (null (drakma:cookie-expires foo)))
        (is (eq nil (drakma:cookie-securep foo)))
        (is (eq nil (drakma:cookie-http-only-p foo))))
      (let ((x (find "あ い;,う" cookies :key #'(lambda (x)
                                                  (unpyo::percent-decode
                                                   (drakma:cookie-name x) :utf-8))
                                         :test #'string=)))
        (is (string= "か き;,く" (unpyo::percent-decode (drakma:cookie-value x) :utf-8)))
        (is (string= "/a" (drakma:cookie-path x)))
        (is (string= *test-host* (drakma:cookie-domain x)))
        (is (eq t (drakma:cookie-securep x)))
        (is (eq t (drakma:cookie-http-only-p x)))))))


(unpyo:defaction /set-session ()
  (setf (unpyo:session "foo") '(1 2 3)))

(unpyo:defaction /get-session ()
  (let ((foo (unpyo:session "foo")))
    (push 0 foo)
   (unpyo:html foo)))

(test session
  (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
    (drakma:http-request (test-url "/set-session") :cookie-jar cookie-jar)
    (is (equal "(0 1 2 3)
"
               (drakma:http-request (test-url "/get-session") :cookie-jar cookie-jar)))))

(with-test-server (server)
  (debug!))
