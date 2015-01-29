(in-package :cl-user)
(defpackage libyaml.macros
  (:use :cl)
  (:import-from :cffi
                :with-foreign-object
                :with-foreign-string)
  (:export :with-parser)
  (:documentation "Some macros to simplify managing foreign objects."))
(in-package :libyaml.macros)

(defmacro with-parser ((parser input-string) &rest body)
  "Create a parser using input-string as the YAML input, execute body, then
delete the parser."
  `(with-foreign-object (,parser 'libyaml.parser:parser-t)
     (with-foreign-string (string ,input-string)
       (libyaml.parser:initialize ,parser)
       (libyaml.parser:set-input-string ,parser
                                        string
                                        (length string))
       (unwind-protect
            (progn
              ,@body)
         (libyaml.parser:parser-delete)))))
