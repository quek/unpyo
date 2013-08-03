(in-package :unpyo)

(alexandria:define-constant +unpyo-version+ "0.0.0" :test 'equal)

(defconstant +fast-track-ka-timeout+ 0.2 "seconds")

(defconstant +stop-command+ (char-code #\?))
(defconstant +halt-command+ (char-code #\!))
(defconstant +restart-command+ (char-code #\R))

(defconstant +reactor-add-command+ (char-code #\*))
(defconstant +reactor-clear-command+ (char-code #\c))
(defconstant +reactor-shutdown-command+ (char-code #\!))
(defconstant +reactor-default-sleep-for+ 5 "seconds")


(defconstant +max-header+  (* 1024 (+ 80 32))
  "This is the maximum header that is allowed before a client is booted.
The parser detects this, but we'd also like to do this as well.")

(defconstant +max-body+ +max-header+
  "Maximum request body size before it is moved out of memory and into a tempfile for reading.")

(defconstant +chunk-size+ (* 16 1024)
  "The basic max request size we'll try to read.")


(alexandria:define-constant +crlf+ (coerce '(#\cr #\lf) 'string) :test 'equal)
(alexandria:define-constant +colon+ ": " :test 'equal)
(alexandria:define-constant +newline+ (string #\lf) :test 'equal)


(alexandria:define-constant +error-400-response+
    (concatenate 'string "HTTP/1.1 400 Bad Request" +crlf+ +crlf+)
  :test 'equal
  :documentation "Indicate that we couldn't parse the request")

(alexandria:define-constant +error-404-response+
    (format nil "HTTP/1.1 404 Not Found~aContent-Type: text/html~aConnection: close~aServer: Unpyo ~a~a~aNOT FOUND"
            +crlf+ +crlf+ +crlf+ +unpyo-version+ +crlf+ +crlf+)
  :test 'equal
  :documentation "The standard empty 404 response for bad requests.
Use Error4040Handler for custom stuff.")

(alexandria:define-constant +error-500-response+
    (concatenate 'string "HTTP/1.1 500 Internal Server Error" +crlf+ +crlf+)
  :test 'equal
  :documentation "Indicate that there was an internal error, obviously.")

(alexandria:define-constant +error-503-response+
    (concatenate 'string "HTTP/1.1 503 Service Unavailable" +crlf+ +crlf+ "BUSY")
  :test 'equal
  :documentation "A common header for indicating the server is too busy.  Not used yet.")


(alexandria:define-constant +http/1.1-200+
  (concatenate 'string "HTTP/1.1 200 OK" +crlf+) :test 'equal)
(alexandria:define-constant +http/1.0-200+
  (concatenate 'string "HTTP/1.0 200 OK" +crlf+) :test 'equal)

(alexandria:define-constant +unpyo-input+ "unpyo.input" :test 'equal)
(alexandria:define-constant +unpyo-url-scheme+ "unpyo.url_scheme" :test 'equal)
(alexandria:define-constant +unpyo-after-reply+ "unpyo.after_reply" :test 'equal)

(alexandria:define-constant +hijack-p+ "unpyo.hijack?" :test 'equal)
(alexandria:define-constant +hijack+ "unpyo.hijack" :test 'equal)
(alexandria:define-constant +hijack-io+ "unpyo.hijack_io" :test 'equal)

(alexandria:define-constant +status-with-no-entity-body+
    '(100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
      116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131
      132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147
      148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163
      164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179
      180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195
      196 197 198 199 204 205 304)
  :test 'equal)

(defvar *status-with-no-entity-body*
  (aprog1 (make-hash-table)
    (dolist (i +status-with-no-entity-body+)
      (setf (gethash i it) t)))
  "For some HTTP status codes the client only expects headers.")

(defvar *http-status-codes*
  (aprog1 (make-hash-table)
    (setf (gethash 100 it) "Continue")
    (setf (gethash 101 it) "Switching Protocols")
    (setf (gethash 102 it) "Processing")
    (setf (gethash 200 it) "OK")
    (setf (gethash 201 it) "Created")
    (setf (gethash 202 it) "Accepted")
    (setf (gethash 203 it) "Non-Authoritative Information")
    (setf (gethash 204 it) "No Content")
    (setf (gethash 205 it) "Reset Content")
    (setf (gethash 206 it) "Partial Content")
    (setf (gethash 207 it) "Multi-Status")
    (setf (gethash 226 it) "IM Used")
    (setf (gethash 300 it) "Multiple Choices")
    (setf (gethash 301 it) "Moved Permanently")
    (setf (gethash 302 it) "Found")
    (setf (gethash 303 it) "See Other")
    (setf (gethash 304 it) "Not Modified")
    (setf (gethash 305 it) "Use Proxy")
    (setf (gethash 306 it) "Reserved")
    (setf (gethash 307 it) "Temporary Redirect")
    (setf (gethash 400 it) "Bad Request")
    (setf (gethash 401 it) "Unauthorized")
    (setf (gethash 402 it) "Payment Required")
    (setf (gethash 403 it) "Forbidden")
    (setf (gethash 404 it) "Not Found")
    (setf (gethash 405 it) "Method Not Allowed")
    (setf (gethash 406 it) "Not Acceptable")
    (setf (gethash 407 it) "Proxy Authentication Required")
    (setf (gethash 408 it) "Request Timeout")
    (setf (gethash 409 it) "Conflict")
    (setf (gethash 410 it) "Gone")
    (setf (gethash 411 it) "Length Required")
    (setf (gethash 412 it) "Precondition Failed")
    (setf (gethash 413 it) "Request Entity Too Large")
    (setf (gethash 414 it) "Request-URI Too Long")
    (setf (gethash 415 it) "Unsupported Media Type")
    (setf (gethash 416 it) "Requested Range Not Satisfiable")
    (setf (gethash 417 it) "Expectation Failed")
    (setf (gethash 418 it) "I'm a Teapot")
    (setf (gethash 422 it) "Unprocessable Entity")
    (setf (gethash 423 it) "Locked")
    (setf (gethash 424 it) "Failed Dependency")
    (setf (gethash 426 it) "Upgrade Required")
    (setf (gethash 500 it) "Internal Server Error")
    (setf (gethash 501 it) "Not Implemented")
    (setf (gethash 502 it) "Bad Gateway")
    (setf (gethash 503 it) "Service Unavailable")
    (setf (gethash 504 it) "Gateway Timeout")
    (setf (gethash 505 it) "HTTP Version Not Supported")
    (setf (gethash 506 it) "Variant Also Negotiates")
    (setf (gethash 507 it) "Insufficient Storage")
    (setf (gethash 510 it) "Not Extended")))

