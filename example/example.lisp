(ql:quickload :unpyo)

(defpackage :unpyo.example
  (:use :cl :unpyo))

(in-package :unpyo.example)

(defclass scratch-app (app-routes-mixin)
  ())

(defvar *server* (make-server :app (make-instance 'scratch-app)))

(run *server*)
;; (stop *server*)


(defun dump-env ()
  (html (:ul (maphash (lambda (k v)
                        (html (:li (:pre k " => " v))))
                      (env-of *request*)))))

(defmacro with-default-template ((&key (title "title")) &body body)
  `(html
     (:html
       (:meta :charset "utf-8")
       (:title ,title))
     (:body ,@body)))

(defaction root (:path "/")
  (with-default-template (:title "サンプルのトップ")
    (:h1 "サンプル")
    (:ul
        (loop for path in (sort
                           (mapcar #'unpyo::route-path
                                   (remove-if (lambda (x)
                                                (not (or (null (unpyo::route-method x))
                                                         (eq (unpyo::route-method x) :get))))
                                              unpyo::*routes*))
                           #'string<=)
              do (html (:li (:a :href path path)))))))

(defaction /env ()
  (with-default-template (:title "env の内味")
    (:pre (with-output-to-string (*standard-output*)
            (describe *request*)))
    (dump-env)))

(defaction /hello/@who ()
  (with-default-template (:title "URL からのパラメータ")
    (format nil "Hello ~a!" @who)
    (:p (:a :href "/hello/あいう" "/hello/あいう"))))

(defaction /room ()
  (with-default-template (:title "room")
    (:h1 "room")
    (:pre
        (with-output-to-string (*standard-output*) (room)))))

(defaction /form/a (:method :get)
  (with-default-template (:title "フォーム")
    (:form :action "/form/a" :method :post
      (:input :type :text :name :a :value "あいう")
      (:input :type :submit :value "サブミット"))))

(defaction /form/a (:method :post)
  (with-default-template (:title "フォームの先")
    (:p (local-time:now))
    (:p "[" @a "]")
    (dump-env)))

(defaction /form/file (:method :get)
  (with-default-template (:title "画像のアップロード")
    (:form :action "/form/file" :method :post :enctype "multipart/form-data"
      (:input :type :text :name "a" :value "あいう")
      (:input :type :file :name "file")
      (:input :type :submit :value "サブミット"))))

(defaction /form/file (:method :post)
  (when @file
    (alexandria:copy-file (car @file) "/tmp/unpyo-scranch-upload-file"))
  (with-default-template (:title "画像のアップロード先")
    (:p "[" @a "]")
    (:pre @file)
    (:pre (format nil "~s" (slot-value *request* 'unpyo::params)))
    (:img :src "/form/file/uploaded")
    (:ul
        (maphash (lambda (key value)
                   (html (:li (format nil "~a = ~a" key value))))
                 (slot-value *request* 'env)))))

(defaction /form/file/uploaded ()
  (write-sequence (alexandria:read-file-into-byte-vector "/tmp/unpyo-scranch-upload-file")
                  *request*))
