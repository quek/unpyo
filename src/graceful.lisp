(in-package :unpyo)

(defvar *old-sighup-handler* nil)
(defvar *exit-function* nil)

(defun fd-from-env ()
  (aif (sb-posix:getenv "UNPYO_FD")
       (parse-integer it)))

(cffi:defcfun execv :int
  (path :pointer)
  (argv :pointer))

(defun sighup-handler (signal code context)
  (log:debug signal)
  (sb-posix:setenv "UNPYO_FD"
                   (princ-to-string
                    (sb-bsd-sockets:socket-file-descriptor
                     (unpyo::server-socket *server*)))
                   1)
  (cffi:with-foreign-strings ((path (car sb-ext:*posix-argv*)))
    (cffi:with-foreign-object (argv :pointer 2)
      (setf (cffi:mem-aref argv :pointer 0) path
            (cffi:mem-aref argv :pointer 1) (cffi:null-pointer))
      (let ((pid (sb-posix:posix-fork)))
        (if (zerop pid)
            (progn
              (execv path argv)
              (log:debug "after execv... execv failed" sb-ext:*posix-argv*)
              (sb-ext:exit :code 255))
            (setf *exit-function*
                  (lambda ()
                    (if (functionp *old-sighup-handler*)
                        (funcall *old-sighup-handler*
                                 signal code context)))))))))

(defun enable-graceful-restart ()
  (setf *old-sighup-handler* (sb-c::enable-interrupt sb-vm::sighup #'sighup-handler)))
