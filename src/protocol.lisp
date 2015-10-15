(in-package :unpyo)

(defgeneric run (x &key &allow-other-keys))
(defgeneric stop (x &key &allow-other-keys))

(defgeneric call (app))
(defgeneric 404-not-found (app))


(defgeneric read-1 (fd))
(defgeneric write-1 (fd byte))

(defgeneric fd-of (stream-fd)
  (:method ((stream iolib.streams:dual-channel-fd-mixin))
    (iolib.streams:fd-of stream))
  (:method ((fd fixnum))
    fd))
