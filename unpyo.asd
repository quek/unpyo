;;;; unpyo.asd

(asdf:defsystem #:unpyo
  :serial t
  :description "Describe unpyo here"
  :author "TAHARA Yoshinori <read.eval.print@gmail.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "util")
               (:file "protocol")
               (:file "condition")
               (:file "const")
               (:file "thread-pool")
               (:file "env")
               (:file "events")
               (:file "reactor")
               (:file "client")
               (:file "binder")
               (:file "server"))
  :depends-on (:anaphora
               :local-time
               :trivial-backtrace
               :iolib
               :puri
               :cl-ppcre
               :queues.simple-queue
               :temporary-file
               :fast-io
               :info.read-eval-print.series-ext
               :com.informatimago.common-lisp.http))
