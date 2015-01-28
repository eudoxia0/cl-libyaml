(in-package :cl-user)
(defpackage libyaml.lib
  (:use :cl :cffi))
(in-package :libyaml.lib)

(define-foreign-library libyaml
  (:darwin "libyaml.dylib")
  (:unix  "libyaml.so")
  (:win32 "libyaml.dll")
  (t (:default "libyaml")))

(use-foreign-library libyaml)
