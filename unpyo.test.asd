;;;; unpyo.asd

(asdf:defsystem :unpyo.test
  :serial t
  :pathname "test/"
  :components ((:file "package")
               (:file "util")
               (:file "ut")
               (:file "request"))
  :depends-on (:unpyo
               :fiveam
               :drakma))
