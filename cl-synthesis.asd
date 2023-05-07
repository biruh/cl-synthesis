;;;; genetic.asd
(asdf:defsystem #:cl-synthesis
  :description "Genetic Programming and Program Synthesis"
  :author "Mekonnen Biruh<biruh.t@gmail.com>"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on (SQLITE :parachute)
  :components ((:file "src/package")
               (:file "src/instructions")
               (:file "src/instructions-test")
               (:file "src/main")))
