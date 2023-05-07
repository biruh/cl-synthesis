(in-package :cl-synthesis)





(parachute:define-test find-match 
   (parachute:true (equal (find-match `(+ 2 expr_int) 'expr_int) `((2))))
   (parachute:true (equal (find-match `(+ 2 expr_int expr_int) 'expr_int) `((3) (2))))
   (parachute:true (equal (find-match `(+ 2 (+ 3 "test" expr_str) expr_str) 'expr_str) `((3) (2 3)))))


(parachute:define-test find-parameters
       (parachute:true (equal
       (find-parameters `(if expr_bool (+ expr_int (* 5 (/ expr_int const_int)))) `(expr_bool expr_int const_int))
       `((EXPR_BOOL ((1))) (EXPR_INT ((2 1) (2 2 2 1))) (CONST_INT ((2 2 2 2))))
       )))


(parachute:define-test test-instructions
    (parachute:test 'find-match)
    (parachute:test 'find-parameters)
    )