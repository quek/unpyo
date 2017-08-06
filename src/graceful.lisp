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
  (let ((pid (sb-posix:posix-fork)))
    (log:debug pid)
    (if (zerop pid)
        (let ((arg0 (car sb-ext:*posix-argv*)))
          (log:debug sb-ext:*posix-argv*)
          (cffi:with-foreign-strings ((path arg0))
            (cffi:with-foreign-object (argv :pointer 2)
              (setf (cffi:mem-aref argv :pointer 0) path
                    (cffi:mem-aref argv :pointer 1) (cffi:null-pointer))
              (sb-posix:setenv "UNPYO_FD"
                               (princ-to-string
                                (sb-bsd-sockets:socket-file-descriptor
                                 (unpyo::server-socket *server*)))
                               1)
              (log:debug "before execv" arg0)
              (execv path argv)
              (log:debug "after execv... execv failed" arg0)
              (sb-ext:exit :code 255))))
        (setf *exit-function*
              (lambda ()
                (log:debug *exit-function*)
                (if (functionp *old-sighup-handler*)
                    (funcall *old-sighup-handler*
                             signal code context)))))))

(defun enable-graceful-restart ()
  (setf *old-sighup-handler* (sb-c::enable-interrupt sb-vm::sighup #'sighup-handler)))
