(in-package :unpyo)

(defvar *old-sighup-handler* nil)
(defvar *exit-function* nil)

(defun fd-from-env ()
  (aif (sb-posix:getenv "UNPYO_FD")
       (parse-integer it)))

(defun sighup-handler (signal code context)
  (log:debug signal)
  ;; SBCL 2.5.2 の run-program は :preserve-fds に指定しても fd 3 を閉じるので、
  ;; 4 以上に複製してから渡す。
  (let ((fd (sb-posix:fcntl (sb-bsd-sockets:socket-file-descriptor
                             (unpyo::server-socket *server*))
                            sb-posix:f-dupfd 4)))
    (sb-posix:setenv "UNPYO_FD" (princ-to-string fd) 1)
    ;; sb-posix:fork はスレッドが複数あるとエラーになるので使えない。
    (sb-ext:run-program (car sb-ext:*posix-argv*) nil
                        :wait nil
                        :preserve-fds (list fd)
                        :input t :output t :error t))
  (setf *exit-function*
        (lambda ()
          (if (functionp *old-sighup-handler*)
              (funcall *old-sighup-handler*
                       signal code context)))))

(defun enable-graceful-restart ()
  (setf *old-sighup-handler* (sb-c::enable-interrupt sb-vm::sighup #'sighup-handler)))
