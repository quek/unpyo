;;;; unpyo.asd

(asdf:defsystem #:unpyo
  :serial t
  :description "Describe unpyo here"
  :author "TAHARA Yoshinori <read.eval.print@gmail.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "type")
               (:file "var")
               (:file "protocol")
               (:file "util")
               (:file "condition")
               (:file "const")
               (:file "buffer")
               (:file "thread-pool")
               (:file "events")
               (:file "reactor")
               (:file "parser")
               (:file "client")
               (:file "binder")
               (:file "response-mixin")
               (:file "request")
               (:file "server")
               (:file "action")
               (:file "app"))
  :depends-on (:anaphora
               :local-time
               :trivial-backtrace
               :iolib
               :puri
               :percent-encoding
               :rfc2388
               :split-sequence
               :cl-ppcre
               :queues.simple-queue
               :temporary-file
               :fast-io
               :flexi-streams
               :info.read-eval-print.series-ext
               :com.informatimago.common-lisp.http
               :info.read-eval-print.html))
