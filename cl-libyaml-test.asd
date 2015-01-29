(defsystem cl-libyaml-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:cl-libyaml
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "cl-libyaml")))))
