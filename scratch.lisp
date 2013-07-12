(ql:quickload :unpyo)
(in-package :unpyo)

(defvar *server* (make-instance 'server))
;;⇒ *SERVER*

(add-unix-listener *server* "/tmp/unpyo.sock")
;;⇒ #<active local stream socket, unconnected {100C77AD13}>

(run *server* :background nil)




