(load "utils.lisp")

(defparameter *dataframe* (tsv-to-list "data/test.tsv"))
;; (defparameter *dataframe* (tsv-to-list "data/cereal.tsv"))

(defun col-name-id (lst cn &optional (cid 0))
  (if lst
      (if (equal (car lst) cn)
          cid
          (col-name-id (cdr lst) cn (+ cid 1)))))

(defun get-row (lst i)
  (if lst
      (if (equal i 0)
          (car lst)
          (get-row (cdr lst) (- i 1)))))

(defun get-col (lst cn)
  (let ((data-lst (cdr lst))
        (col-id (col-name-id (car lst) cn)))
    (mapcar #'(lambda (x) (get-row x col-id)) data-lst)))

;; (defun get-index (elm lst &optional (index 0))
;;   "returns the index of the first occurrence of elm in lst
;;    returns NIL if elm is not in lst"
;;   (cond ((endp lst) nil)
;;         ((eql elm (car lst))
;;          index)
;;         (t (get-index elm (cdr lst) (+ index 1)))))

;; (defun columns (df) (car df))
;; (defun rows (df) (cdr df))

;; (defun get-col (df colname)
;;     (mapcar
;;       #'(lambda (row)
;;           (nth
;;             (get-index colname (columns df))
;;             row))
;;       (rows df)))

;; (defun get-row (df row-index)
;;   (nth row-index (rows df)))

