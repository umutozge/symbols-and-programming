(load "utils.lisp")

(defparameter *dataframe* (tsv-to-list "data/test.tsv"))


(defun get-index (elm lst &optional (index 0))
  "returns the index of the first occurrence of elm in lst
   returns NIL if elm is not in lst"
  (cond ((endp lst) nil)
        ((eql elm (car lst))
         index)
        (t (get-index elm (cdr lst) (+ index 1)))))

(defun columns (df) (car df))
(defun rows (df) (cdr df))

(defun get-col (df colname)
    (mapcar
      #'(lambda (row)
          (nth
            (get-index colname (columns df))
            row))
      (rows df)))

(defun get-row (df row-index)
  (nth row-index (rows df)))
