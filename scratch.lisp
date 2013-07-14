(ql:quickload :unpyo)
(in-package :unpyo)

(defvar *server* (make-instance 'server :app (make-instance 'status-app)))
;;⇒ *SERVER*

(add-tcp-listener *server* "localhost" 7779)
(add-unix-listener *server* "/tmp/unpyo.sock")
;;⇒ #<active local stream socket, unconnected {100C77AD13}>

(run *server* :background t)
;;⇒ #<SB-THREAD:THREAD "Anonymous thread" RUNNING {10045FC153}>


(iolib.sockets:with-open-socket (s :remote-host "localhost" :remote-port 7779)
  (format s "GET / HTTP/1.0~a~a" +crlf+ +crlf+)
  (force-output s)
  (sleep 0.1)
  (loop for i = (read-line s nil)
        while i
        do (print i))
  (print s))

(iolib.sockets:with-open-socket (s :remote-host "localhost" :remote-port 7779)
  (format s "GET / HTTP/1.1~aHost: localhost~a~a" +crlf+ +crlf+ +crlf+)
  (force-output s)
  (sleep 0.1)
  (loop for i = (read-line s nil)
        while i
        do (write-line i)))

(iolib.sockets:with-open-socket (s :remote-host "localhost" :remote-port 7779)
  (format s "GET / HTTP/1.1~aHost: localhost~a~a" +crlf+ +crlf+ +crlf+)
  (force-output s)
  (sleep 0.1)
  (loop for c = (read-char-no-hang s)
        while c
        do (write-char c)))