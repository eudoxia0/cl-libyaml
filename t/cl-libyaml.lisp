(in-package :cl-user)
(defpackage cl-libyaml-test
  (:use :cl :fiveam))
(in-package :cl-libyaml-test)

(def-suite tests
  :description "cl-libyaml tests.")
(in-suite tests)

(test version
  (let ((version))
    (finishes
     (setf version (libyaml.version:get-version-string)))
    (is (stringp version))))

;(test allocate-parser
;  (finishes
;   (libyaml.macros:with-parser (parser "[1,2,3]")
;     t)))

(run! 'tests)
