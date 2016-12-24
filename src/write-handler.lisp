(in-package :cl-user)
(defpackage libyaml.write-handler
  (:use :cl)
  (:import-from :cffi
		:callback
		:defcallback
		:defcfun
		:foreign-string-to-lisp)
  (:import-from :libyaml.util
		:size-t)
  (:export :write-handler
	   :*write-handler-callback*
	   :*write-handler-stream*)
  (:documentation ""))
(in-package :libyaml.write-handler)

(defvar *write-handler-stream* nil
  "Stream that write-handler will output to")

(defcfun strlen size-t
  "Calculate the length of a string"
  (s :string))

(defcallback write-handler :int ((data :pointer)
				 (buffer (:pointer :char))
				 (size size-t))
  (declare (ignore data))
  "Write buffer to *write-handler-stream* and return the number of bytes written"
  (let ((str (cffi:foreign-string-to-lisp buffer :count size)))
    (write-string str *write-handler-stream*)
    (strlen str)))

(defparameter *write-handler-callback* (callback write-handler)
  "Function pointer to write-handler")
