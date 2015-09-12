;;;; unpyo.asd

(asdf:defsystem #:unpyo
  :serial t
  :description "Describe unpyo here"
  :author "TAHARA Yoshinori <read.eval.print@gmail.com>"
  :license "Specify license here"
  :pathname "src/"
  :components ((:file "package")
               (:file "type")
               (:file "var")
               (:file "protocol")
               (:file "util")
               (:file "condition")
               (:file "const")
               (:file "buffer")
               (:file "client")
               (:file "thread-pool")
               (:file "events")
               (:file "reactor")
               (:file "parser")
               (:file "binder")
               (:file "cookie")
               (:file "session")
               (:file "response-mixin")
               (:file "request")
               (:file "server")
               (:file "action")
               (:file "app")
               (:file "static-app")
               (:file "form"))
  :depends-on (:anaphora
               :local-time
               :trivial-backtrace
               :iolib
               :puri
               :percent-encoding
               :rfc2388
               :split-sequence
               :cl-ppcre
               :sb-concurrency
               :temporary-file
               :fast-io
               :flexi-streams
               :com.informatimago.common-lisp.http
               :info.read-eval-print.html
               :cl-base64
               :ironclad))
