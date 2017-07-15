(in-package :unpyo)

(cffi:define-foreign-library libssl
  (t (:default "libssl")))

(cffi:use-foreign-library libssl)

;; int PKCS5_PBKDF2_HMAC_SHA1(const char *pass, int passlen,
;;                            const unsigned char *salt, int saltlen, int iter,
;;                            int keylen, unsigned char *out);
(cffi:defcfun "PKCS5_PBKDF2_HMAC_SHA1" :int
  (pass :pointer)
  (passlen :int)
  (salt :pointer)
  (saltlen :int)
  (iter :int)
  (keylen :int)
  (out :pointer))

(cffi:defcfun "EVP_aes_256_gcm" :pointer)
(cffi:defcfun "EVP_CIPHER_CTX_new" :pointer)
(cffi:defcfun "EVP_CIPHER_CTX_free" :void
  (ctx :pointer))
;; int EVP_EncryptInit(EVP_CIPHER_CTX *ctx, const EVP_CIPHER *cipher,
;;                     const unsigned char *key, const unsigned char *iv);
(cffi:defcfun "EVP_EncryptInit" :int
  (ctx :pointer)
  (cipher :pointer)
  (key :pointer)
  (iv :pointer))
;; int EVP_EncryptUpdate(EVP_CIPHER_CTX *ctx, unsigned char *out, int *outl,
;;                       const unsigned char *in, int inl);
(cffi:defcfun "EVP_EncryptUpdate" :int
  (ctx :pointer)
  (out :pointer)
  (outl :pointer)
  (in :pointer)
  (inl :int))
;; int EVP_EncryptFinal(EVP_CIPHER_CTX *ctx, unsigned char *out, int *outl);
(cffi:defcfun "EVP_EncryptFinal_ex" :int
  (ctx :pointer)
  (out :pointer)
  (outl :pointer))
;; int EVP_CIPHER_CTX_ctrl(EVP_CIPHER_CTX *ctx, int type, int arg, void *ptr);
(cffi:defcfun "EVP_CIPHER_CTX_ctrl" :int
  (ctx :pointer)
  (type :int)
  (arg :int)
  (ptr :pointer))
;; int EVP_DecryptInit(EVP_CIPHER_CTX *ctx, const EVP_CIPHER *cipher,
;;                     const unsigned char *key, const unsigned char *iv);
(cffi:defcfun "EVP_DecryptInit" :int
  (ctx :pointer)
  (cipher :pointer)
  (key :pointer)
  (iv :pointer))
;; int EVP_DecryptUpdate(EVP_CIPHER_CTX *ctx, unsigned char *out, int *outl,
;;                       const unsigned char *in, int inl);
(cffi:defcfun "EVP_DecryptUpdate" :int
  (ctx :pointer)
  (out :pointer)
  (outl :pointer)
  (in :pointer)
  (inl :int))
;; int EVP_DecryptFinal_ex(EVP_CIPHER_CTX *ctx, unsigned char *outm, int *outl);
(cffi:defcfun "EVP_DecryptFinal_ex" :int
  (ctx :pointer)
  (out :pointer)
  (outl :pointer))

(defconstant EVP_CTRL_GCM_GET_TAG #x10)
(defconstant EVP_CTRL_GCM_SET_TAG #x11)

(defun generate-key (password salt)
  (let* ((password (sb-ext:string-to-octets password))
         (salt (sb-ext:string-to-octets salt))
         (iteration #.(expt 2 16))
         (key-length 32)
         (key (make-array key-length :element-type '(unsigned-byte 8))))
   (sb-sys:with-pinned-objects (password salt key)
     (unless (= 1 (pkcs5-pbkdf2-hmac-sha1 (sb-sys:vector-sap password) (length password)
                                          (sb-sys:vector-sap salt) (length salt)
                                          iteration
                                          key-length
                                          (sb-sys:vector-sap key)))
       (error "pkcs5-pbkdf2-hmac-sha1"))
     key)))


(defun encrypt (message key)
  (let* ((message (sb-ext:string-to-octets message))
         (message-length (length message))
        (out (make-array (+ message-length 16 12) :element-type '(unsigned-byte 8)))
        (iv (make-array 12 :element-type '(unsigned-byte 8))))
    (loop for i below 12
          do (setf (aref iv i) (random 256)))
    (cffi:with-foreign-objects ((outl :int 1))
      (let ((ctx (evp-cipher-ctx-new))
            (pos 0))
        (unwind-protect
             (sb-sys:with-pinned-objects (message out iv key)
               (let ((message (sb-sys:vector-sap message))
                     (out (sb-sys:vector-sap out))
                     (iv (sb-sys:vector-sap iv))
                     (key (sb-sys:vector-sap key)))

                 (evp-encryptinit ctx (evp-aes-256-gcm) key iv)

                 (unless (= 1 (evp-encryptupdate ctx out outl message message-length))
                   (error "evp-encryptupdate"))
                 (incf pos (cffi:mem-ref outl :int 0))

                 (unless (= 1 (evp-encryptfinal-ex ctx (sb-sys:sap+ out pos) outl))
                   (error "evp-encryptfinal-ex"))
                 (incf pos (cffi:mem-ref outl :int 0))

                 (unless (= 1 (evp-cipher-ctx-ctrl ctx EVP_CTRL_GCM_GET_TAG 16 (sb-sys:sap+ out pos)))
                   (error "evp-cipher-ctx-ctrl"))
                 (incf pos 16)))
          (evp-cipher-ctx-free ctx))
        (replace out iv :start1 pos)))))

(defun decrypt (message key)
  (let* ((message-length (length message))
         (out-length (- message-length 16 12))
         (ctx (evp-cipher-ctx-new)))
    (unwind-protect
         (cffi:with-foreign-objects ((outl :int 1) (out :unsigned-char out-length))
           (sb-sys:with-pinned-objects (message key)
             (let* ((message (sb-sys:vector-sap message))
                    (key (sb-sys:vector-sap key))
                    (tag (sb-sys:sap+ message out-length))
                    (iv (sb-sys:sap+ tag 16)))
               (evp-decryptinit ctx (evp-aes-256-gcm) key iv)

               (evp-decryptupdate ctx out outl message out-length)

               (evp-cipher-ctx-ctrl ctx EVP_CTRL_GCM_SET_TAG 16 tag)

               (prog1 (cffi:foreign-string-to-lisp out :count out-length)
                 (unless (= 1 (evp-decryptfinal-ex ctx out outl))
                   (error "evp-decryptfinal-ex"))))))
      (evp-cipher-ctx-free ctx))))

#+nil
(let ((key (generate-key "password" "salt")))
  (decrypt (encrypt "hello" key) key))
