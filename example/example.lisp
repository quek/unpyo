(ql:quickload :unpyo)

(defpackage :unpyo.example
  (:use :cl :unpyo))

(in-package :unpyo.example)

(defclass scratch-app (app-routes-mixin)
  ())

(defvar *server* (make-instance 'server :app (make-instance 'scratch-app)))

(unpyo::add-tcp-listener *server* "localhost" 8889)

(run *server* :background t)


(defun dump-env ()
  (html
    (:ul
        (maphash (lambda (k v)
                   (html (:li (:pre k " => " v))))
                 (env-of *request*)))))

(defmacro default-template ((&key (title "title")) &body body)
  `(html
     (:html
       (:meta :charset "utf-8")
       (:title ,title))
     (:body
         ,@body)))

(defaction root (:path "/")
  (default-template (:title "サンプルのトップ")
    (:h1 "サンプル")
    (:ul
        (loop for path in (sort (mapcar #'unpyo::route-path unpyo::*routes*) #'string<=)
              do (html (:li (:a :href path path)))))))

(defaction /env ()
  (html
    (:html
      (:body
          (:pre (with-output-to-string (*standard-output*)
                  (describe *request*)))
        (dump-env)))))

(defaction /hello/@who ()
  (html
    (:html
      (:body
          (format nil "Hello ~a!" @who)))))

(defaction /room ()
  (html
    (:html
      (:body
          (:h1 "room")
        (:pre
            (with-output-to-string (*standard-output*) (room)))))))

(defaction /form/a ()
  (html
    (:html
      (:head
          (:meta :charset :utf-8))
      (:body
          (:form :action "/form/aa" :method :post
            (:input :type :text :name :a :value "あいう")
            (:input :type :submit :value "サブミット"))))))

(defaction /form/aa ()
  (html
    (:html
      (:body
          (:p (local-time:now))
        (:p "[" @a"]")
        (dump-env)))))

(defaction /form/file ()
  (html
    (:html
      (:head
          (:meta :charset :utf-8))
      (:body
          (:form :action "/form/filex" :method :post :enctype "multipart/form-data"
            (:input :type :text :name "a" :value "あいう")
            (:input :type :file :name "file")
            (:input :type :submit :value "サブミット"))))))

(defaction /form/filex ()
  (when @file
    (alexandria:copy-file (car @file) "/tmp/unpyo-scranch-upload-file"))
  (html
    (:html
      (:body
          (:p "[" @a "]")
        (:pre @file)
        (:pre (format nil "~s" (slot-value *request* 'unpyo::params)))
        (:img :src "/form/file/uploaded")
        (:ul
            (maphash (lambda (key value)
                       (html (:li (format nil "~a = ~a" key value))))
                     (slot-value *request* 'env)))))))

(defaction /form/file/uploaded ()
  (write-sequence (alexandria:read-file-into-byte-vector "/tmp/unpyo-scranch-upload-file")
                  *request*))
