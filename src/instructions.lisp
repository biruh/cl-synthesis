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

(defclass instruction () 
              ((id :initarg :id :accessor id)
               (category :initarg :category :accessor category) 
               (expression :initarg :expression :accessor expression)
               (return-type :initarg :return-type :accessor return-type) 
               (parameters :initarg :parameters :accessor parameters)))

(defmethod print-object ((inst instruction) out)
  (with-slots (category expression return-type parameters) inst 
    (print-unreadable-object (inst out :type t)
      (format out "~A| ~A -> ~A | ~A"  category expression return-type parameters))))

(defparameter *instructions* (make-hash-table :test 'equal))
(defparameter *instruction-ret* (make-hash-table :test 'equal))
(defparameter *instruction-param* (make-hash-table :test 'equal))
(defparameter *instruction-cat* (make-hash-table :test 'equal))

(defun register-instruction (inst)
   (setf (gethash (id inst) *instructions*) inst)
   (setf (gethash (category inst) *instruction-cat*) (id inst))
   (setf (gethash (return-type inst) *instruction-ret*) (id inst))
   (loop :for param :in (parameters inst)
         :do (setf (gethash (first param) *instruction-param*) (id inst))))

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




(defun find-match (code target)
  (let ((r '()))
    (loop :for i :in code
          :for j :from 0
          :do (cond
                ((eql i target) (pushnew (list  j) r))
                ((listp i) (let ((n (find-match i target)))
                            (when n
                              (loop :for k :in n
                                    :do (pushnew (concatenate 'list (list j) k) r))
                              )))))
    r))

(defun find-parameters (code possible-params)
   (loop :for param :in possible-params
         :for coord = (find-match code param)
         :when coord
         :append (loop :for x :in coord
                       :collect (list param x))))



(defparameter *type-placeholders* `(expr_bool const_bool 
                                    expr_float const_float 
                                    expr_int const_int const_int_guarded
                                    expr_int_with expr_bool_with expr_float_with 
                                    expr_float_1d_array const_float_1d_array
                                    expr_int_1d_array const_int_1d_array
                                    expr_bool_1d_array const_bool_1d_array))


(defun create-instructions (category expr ret-type)
  (let ((id (sxhash (format nil "~a ~a" expr ret-type)))
        (expanded (expand_instruction expr)))
       (loop :for inst :in expanded
             :collect (let ((params (find-parameters inst *type-placeholders*)))
                    (make-instance 'instruction :id id :expression inst :category category :return-type ret-type :parameters params)))))

(defparameter *core-x->bool* `(
  (((tor and or) expr_bool (tor const_bool expr_bool)) bool)
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
         :collect (expr_int_with (k float))) int_1d_array)))


(defun init-core-instructions ()
       (loop :for coll :in (list *core-x->bool* *core-x->float* *core-x->int* *core-x->float_1d_array* *core-x->int_1d_array*) 
             :do (loop :for line :in coll 
                       :do (loop :for inst :in (create-instructions :core (first line) (second line))
                                 :do (register-instruction inst)))))

(init-core-instructions)