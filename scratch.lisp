(ql:quickload :unpyo)
(in-package :unpyo)

(defvar *server* (make-instance 'server))
;;⇒ *SERVER*

(add-tcp-listener *server* "localhost" 7779)
(add-unix-listener *server* "/tmp/unpyo.sock")
;;⇒ #<active local stream socket, unconnected {100C77AD13}>

(run *server* :background t)


(iolib.sockets:with-open-socket (s :remote-host "localhost" :remote-port 7779)
  (format s "GET / HTTP/1.0~a~a" +crlf+ +crlf+)
  (force-output s)
  (print (read-char-no-hang s))
  (print s))
