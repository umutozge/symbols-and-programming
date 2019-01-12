
;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 12
;;


;; Here is a function to test your permutation function

(defun unique (lst)
  (let (store)
  (dolist (i lst store)
    (if (not (member i store :test #'equal))
      (push i store)))))


;; Q1

(defun mmerge (lstA lstB) 
                   (cond ((endp lstA) lstB)
                         ((endp lstB) lstA)
                         (t (if (> (car lstA) (car lstB))
                             (cons (car lstB) (mmerge lstA (cdr lstB)))
                             (cons (car lstA) (mmerge (cdr lstA) lstB))))))


;; Q2

(defun divide (lst &optional store)
                   (if (>= (length store) (length lst))
                     (list lst store)
                     (divide (cdr lst) (append store (list (car lst))))))

(defun merge-sort (lst)
  (if (endp (cdr lst))
    lst
    (let ((pair (divide lst)))
      (mmerge
        (merge-sort (car pair))
        (merge-sort (cadr pair))))))


;; Q3

(defun rem-pos (pos lst)
  "remove the item at index pos from the list lst"
  (cond ((endp lst) nil)
        ((zerop pos) (cdr lst))
        (t (cons (car lst) (rem-pos (- pos 1) (cdr lst))))))


(defun shuffle (lst &optional store)
  (if (endp lst)
    store
    (let ((index (random (length lst))))
      (shuffle (rem-pos index lst) (cons (nth index lst) store)))))

;; Q4

(defun permute (seq &optional (store (list (cons nil seq))))
  (dotimes (k (length seq))
    (dolist (i store)
      ;;first surgically alter the current element
      (setf (car i) (cons (car (cdr i)) (car i) ))
      (setf (cdr i) (cdr (cdr i)))
      ;;create and add variations to the store 
      (dotimes (j (length (cdr i)))
        (setf store (cons 
                      (cons (cons (elt (cdr i) j) (cdr (car i))) (cons (car (car i)) (remove (elt (cdr i) j) (cdr i))))
                      store)))))
  store)


;; Q5

(defun bin-tree-p (lst)
  (if (endp lst)
    t
    (and
      (= 3 (length lst))
      (bin-tree-p (cadr lst))
      (bin-tree-p (caddr lst)))))


;; Q6

(defun bst-insert (item lst)
  (if (endp lst)
    (list item nil nil)
    (if (<= item (car lst))
      (list (car lst)
            (cadr lst)
            (bst-insert item (caddr lst)))
      (list (car lst)
            (bst-insert item (cadr lst))
            (caddr lst)))))

(defun make-bst (lst &optional store)
  (if (endp lst)
    store
    (make-bst
      (cdr lst)
      (bst-insert (car lst) store))))

