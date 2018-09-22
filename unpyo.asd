;;;; unpyo.asd

(asdf:defsystem #:unpyo
  :serial t
  :description "Describe unpyo here"
  :author "TAHARA Yoshinori <read.eval.print@gmail.com>"
  :license "BSD"
  :version "0.1.0"
  :pathname "src"
  :components ((:file "package")
               (:file "protocol")
               (:file "util")
               (:file "condition")
               (:file "const")
               (:file "buffer")
               (:file "writev")
               (:file "request")
               (:file "response")
               (:file "graceful")
               (:file "server")
               (:file "action")
               (:file "app")
               (:file "static-app")
               (:file "cookie"))
  :depends-on (:anaphora
               :cffi
               :local-time
               :trivial-backtrace
               :trivial-mimes
               :puri
               :percent-encoding
               :rfc2388
               :split-sequence
               :cl-ppcre
               :sb-concurrency
               :temporary-file
               :fast-io
               :info.read-eval-print.html
               :info.read-eval-print.css
               :cl-base64
               :jsonq
               :log4cl))
