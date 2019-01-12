;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 13
;;


;; Q1

(defun median (lst)
  (let* ((newlst (sort lst #'<))
         (length (length lst))
         (mid (floor (/ length 2))))
    (if (oddp length)
      (nth mid newlst)
      (/ (+ (nth mid newlst)
            (nth (- mid 1) newlst))
         2))))

;; Q2

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

;; If you couldn't solve the problem yourself, carefully trace this solution to
;; understand how it works.

(defun traverse (agenda &optional store)
  (if (endp agenda)
    store
    (let ((current (car agenda)))
      (cond ((null current)
             (traverse (cdr agenda) store))
            ((equal (car current) 'seen)
             (traverse (cdr agenda) (cons (cadr current) store)))
            (t
              (traverse
                (cons (cadr current)
                      (cons (list 'seen (car current))
                            (cons (caddr current) (cdr agenda))))
                store))))))

(defun tree-sort (lst)
  (let ((bt (make-bst lst)))
    (traverse (list bt))))


;; Q3

(lambda (x y) (if (> x y) x y)) 

(lambda (x y) (or (zerop (rem x y)) (zerop (rem y x)))) 

(lambda (lst) (float (/ (apply #'+ lst) (length lst))))
; or
(lambda (lst) (float (/ (apply '+ lst) (length lst))))

(lambda (lst) (apply #'+ (mapcar #'factorial lst)))

;; Q4

(defun replace-if (&key test seq item)
  "replaces the elements in the seq that pass the test with item"
  (mapcar #'(lambda (x)
              (if (funcall test x)
                item
                x))
          seq))

;; Q5

(defun zip (lst1 lst2)
  (mapcar #'(lambda (x y) (list x y)) lst1 lst2))  

(defun trio (lst1 lst2 flst)
  (mapcar #'(lambda (x y z) (funcall z x y)) lst1 lst2 flst))

