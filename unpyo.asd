;;;; unpyo.asd

(asdf:defsystem #:unpyo
  :serial t
  :description "Describe unpyo here"
  :author "TAHARA Yoshinori <read.eval.print@gmail.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "type")
               (:file "util")
               (:file "protocol")
               (:file "condition")
               (:file "const")
               (:file "buffer")
               (:file "env")
               (:file "thread-pool")
               (:file "events")
               (:file "reactor")
               (:file "parser")
               (:file "client")
               (:file "binder")
               (:file "server")
               (:file "app"))
  :depends-on (:anaphora
               :local-time
               :trivial-backtrace
               :iolib
               :puri
               :cl-ppcre
               :queues.simple-queue
               :temporary-file
               :fast-io
               :flexi-streams
               :info.read-eval-print.series-ext
               :com.informatimago.common-lisp.http))
