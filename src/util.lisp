(in-package :cl-user)
(defpackage libyaml.util
  (:use :cl :cffi)
  (:export :size-t))
(in-package :libyaml.util)

(defctype size-t :unsigned-int)
