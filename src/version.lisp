(in-package :cl-user)
(defpackage libyaml.version
  (:use :cl :cffi)
  (:export :get-version-string
	   :version-directive-t
	   :allocate-version-directive)
  (:documentation "Stuff for dealing with version information."))
(in-package :libyaml.version)

(defcstruct version-directive-t
  "The version directive data."
  (major :int)
  (minor :int))

(defcfun ("yaml_get_version_string" get-version-string)
  :string
  "Get the library version as a string.")

(defun allocate-version-directive ()
  (foreign-alloc '(:struct version-directive-t)))
