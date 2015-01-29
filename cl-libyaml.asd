(defsystem cl-libyaml
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :homepage ""
  :version "0.1"
  :depends-on (:cffi
               :cffi-libffi)
  :components ((:module "src"
                :serial t
                :components
                ((:file "library")
                 (:file "version")
                 (:file "util")
                 (:file "basic-types")
                 (:file "style")
                 (:file "token")
                 (:file "event")
                 (:file "node")
                 (:file "document")
                 (:file "parser")
                 (:file "emitter")
                 (:file "macros"))))
  :description "A binding to the libyaml library."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-libyaml-test))))
