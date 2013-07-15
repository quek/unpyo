(in-package :unpyo)

(defgeneric run (x &key &allow-other-keys))
(defgeneric stop (x &key &allow-other-keys))


(defgeneric env (env key)
  (:method ((env hash-table) key)
    (gethash key env)))
(defgeneric (setf env) (value env key)
  (:method (value (env hash-table) key)
    (setf (gethash key env) value)))
(defgeneric env-of (env-holder))


(defgeneric read-1 (fd))
(defgeneric write-1 (fd byte))

(defgeneric fd-of (stream-fd)
  (:method ((stream iolib.streams:dual-channel-fd-mixin))
    (iolib.streams:fd-of stream))
  (:method ((fd fixnum))
    fd))
