(in-package :cl-synthesis)


(defun copy-array (array &key
                           (element-type (array-element-type array))
                           (fill-pointer (and (array-has-fill-pointer-p array)
                                              (fill-pointer array)))
                           (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

(defun copy-value (v)
  (cond
    ((listp v) (copy-tree v))
    ((arrayp v) (copy-array v))
    (t v)))


(defun replace-token (code loc value)
  (cond
    ((null loc) (copy-value value))
    ((atom code) (if (and (eql 1 (length loc))
                          (eql (nth 0 loc) 0))
                     (copy-value value)
                     code))
    ((eql (length loc) 1) (progn
                            (setf (nth (car loc) code) (copy-value value))
                            code))
    (t  (progn
          (replace-token (nth (car loc) code) (cdr loc) value)
          code))))

