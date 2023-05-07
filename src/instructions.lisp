#|

Copyright 2023 Mekonnen Biruh 

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

|#
(in-package :cl-synthesis)

(defvar *instruction-db* (sqlite:connect ":memory:"))

(sqlite:execute-non-query *instruction-db* "create table instructions if not exists (id text primary key, category text not null, expr text not null, retval text not null)") 
(sqlite:execute-non-query *instruction-db* "create table parameters if not exists (id text not null, param text not null)") 


(defun m+ (expr)
    (loop :for i :in expr
          :for j :from 0
          :if (and (listp i) (not (eql (car i) 'tor)))
          :append (let ((n (m+ i)))
                       (loop :for p :in n
                             collect `((,j ,@(car p)) ,(car (cdr p))))
                       )
          :if (and (listp i)
                  (eql (car i) 'tor))
          :collect (list (list j) (cdr i) )))


(defun replace-deep (seq idxarr v)
    (if (eql (length idxarr) 1)
        (let ((i (car idxarr)))
            (setf (nth i seq) v)
            seq)
        (let ((i (car idxarr)))
            (replace-deep (nth i seq) (cdr idxarr) v)
            seq
            )))

(defun mcp (store idx items)
    (let ((new-list '()))
    (loop for k in store do (loop for j in items
                  do (let* ((r (copy-tree k)))
                               (replace-deep r idx j)
                               (push r new-list))))
         new-list))

(defun expand_instruction (l)
    (let* ((rs (m+ l))
           (r (reduce (lambda (a i)
                        (mcp a (car i) (second i))
                              ) rs
                  :initial-value (list l))))
           r))


(defun register-instruction (expr ret)
  `(let ((id (sxhash (format nil "~a ~a" ',expr ',ret))))
     (add-in-set ',ret id)
     (add-in-assoc id :expr ',expr)
     (add-in-assoc id :ret ',ret)))

(defparameter *core-x->bool* `(
  (((tor and or) expr_bool expr_bool) bool)
  ((not expr_bool) bool)
  (((tor > >= < <= eql) (tor expr_float expr_int) (tor expr_float expr_int const_float)) bool)
  ((reduce (lambda (a i) 
             (expr_bool_with (a bool) (i float))) 
      (tor expr_float_1d_array const_float_1d_array) 
      :initial-value expr_bool) bool)
  ((reduce (lambda (a i) 
             (expr_bool_with (a bool) (i int))) 
      (tor expr_int_1d_array const_int_1d_array)
      :initial-value expr_bool) bool)
  ))


(defparameter *core-x->int* `(
  (((tor + - * / mod max min) expr_int (tor expr_int const_int )) int)
  ((if expr_bool 
        (tor expr_int const_int) 
        (tor expr_int const_int)) int)

  (((tor floor ceiling) expr_float) int)
  ((get-array-index (tor expr_int_1d_array const_int_1d_array) expr_int) int)
  ((get-array-index expr_int_1d_array const_int_guarded) int)
  ((reduce (lambda (a i)
              (expr_int_with (a int) (i int)))
                               (tor expr_int_1d_array
                                    const_int_1d_array)) int)
  ))

(defparameter *core-x->float* `(
  (((tor + - * / max min) expr_float (tor expr_float expr_int const_int const_float)) float)
  ((if expr_bool 
        (tor expr_float const_float) 
        (tor expr_float const_float)) float)
  ((tor expr_int const_int) float)
  ((get-array-index (tor expr_float_1d_array const_float_1d_array) expr_int) float)
  ((get-array-index expr_float_1d_array const_int_guarded) float)
  ((reduce (lambda (a i)
              (expr_float_with (a float) (i float)))
                               (tor expr_float_1d_array
                                    const_float_1d_array)) float)
  ))


(defparameter *core-x->float_1d_array* `(
  ((if expr_bool 
      (tor expr_float_1d_array const_float_1d_array) 
      (tor expr_float_1d_array const_float_1d_array)) float_1d_array)
  
  ;from arrays
  ((loop :for i :in (tor expr_bool_1d_array const_bool_1d_array)
         :collect (expr_float_with (i bool))) float_1d_array)

  ((loop :for i :in (tor expr_int_1d_array const_int_1d_array)
         :collect (expr_float_with (i int))) float_1d_array)

  ((loop :for i :in (tor expr_float_1d_array const_float_1d_array)
         :collect (expr_float_with (i float))) float_1d_array)
   

  ;from single
  ((loop :for i :upto const_int_guarded
         :for k = expr_int
         :collect (expr_float_with (k int))) float_1d_array)

  ((loop :for i :upto const_int_guarded
         :for k = expr_bool
         :collect (expr_float_with (k bool))) float_1d_array)

  ((loop :for i :upto const_int_guarded
         :for k = expr_float
         :collect (expr_float_with (k float))) float_1d_array)
              ))



(defparameter *core-x->int_1d_array* `(
  ((if expr_bool 
      (tor expr_int_1d_array const_int_1d_array) 
      (tor expr_int_1d_array const_int_1d_array)) float_1d_array)
  
  ;from arrays
  ((loop :for i :in (tor expr_bool_1d_array const_bool_1d_array)
         :collect (expr_int_with (i bool))) int_1d_array)

  ((loop :for i :in (tor expr_float_1d_array const_float_1d_array)
         :collect (expr_int_with (i float))) int_1d_array)

  ((loop :for i :in (tor expr_int_1d_array const_int_1d_array)
         :collect (expr_int_with (i int))) int_1d_array)
   

  ;from single
  ((loop :for i :upto const_int_guarded
         :for k = expr_int
         :collect (expr_int_with (k int))) int_1d_array)

  ((loop :for i :upto const_int_guarded
         :for k = expr_bool
         :collect (expr_int_with (k bool))) int_1d_array)

  ((loop :for i :upto const_int_guarded
         :for k = expr_float
         :collect (expr_int_with (k float))) int_1d_array)
              ))




(defparameter *type-placeholders* `(expr_bool const_bool expr_float const_float expr_int const_int const_int_guarded
                                              expr_int_with expr_bool_with expr_float_with 
                                              ))