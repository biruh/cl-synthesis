;;;; genetic.asd
(asdf:defsystem #:cl-synthesis
  :description "Genetic Programming and Program Synthesis"
  :author "Mekonnen Biruh<biruh.t@gmail.com>"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ()
  :components ((:module "src"
                :components ((:file "package")
                             (:file "instructions")
                             (:file "main"))))
  :in-order-to ((test-op (test-op :cl-synthesis/test))))


;;;; genetic.asd
(asdf:defsystem #:cl-synthesis/test
  :description "Test for cl-synthesis"
  :author "Mekonnen Biruh<biruh.t@gmail.com>"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on (:fiveam :cl-synthesis)
  :components ((:module "test"
                :components ((:file "package") 
                             (:file "all")
                             (:file "instructions-test"))))
  :perform (test-op (op c)
                  (symbol-call :fiveam :run!
                                (find-symbol* :cl-synthesis :cl-synthesis/test))))
