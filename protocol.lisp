(in-package :unpyo)

(defgeneric run (x &key &allow-other-keys))
(defgeneric stop (x &key &allow-other-keys))

(defgeneric read-1 (fd))
(defgeneric write-1 (fd byte))
