(in-package :cl-user)
(defpackage libyaml.version
  (:use :cl :cffi)
  (:export :get-version-string))
(in-package :libyaml.version)

(defcfun ("yaml_get_version_string" get-version-string)
  :string
  "Get the library version as a string.")
