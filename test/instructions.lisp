(in-package :cl-synthesis/test)

(def-suite instructions :in whole)

(in-suite instructions)

(test find-match-test
   (is (equal (find-match `(+ 2 expr_int) 'expr_int) `((2))))
   (is (equal (find-match `(+ 2 expr_int expr_int) 'expr_int) `((3) (2))))
   (is (equal (find-match `(+ 2 (+ 3 "test" expr_str) expr_str) 'expr_str) `((3) (2 3)))))


(test find-parameters-test
   (is (equal (find-parameters `(if expr_bool (+ expr_int (* 5 (/ expr_int const_int)))) `(expr_bool expr_int const_int)) 
            `((EXPR_BOOL (1)) (EXPR_INT (2 1)) (EXPR_INT (2 2 2 1)) (CONST_INT (2 2 2 2))))))


