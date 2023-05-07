(in-package :cl-synthesis/test)

(def-suite synthesis :in whole)

(in-suite synthesis)

(test replace-test
  (is (equal (replace-token `(or expr_bool t) `(1) nil) `(or nil t)))
  (is (equal (replace-token `(+ (* expr_int 3) 2) `(1 1) 6) `(+ (* 6 3) 2))))


