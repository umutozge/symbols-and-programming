;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 03
;;

;; Q3

; a
(defun wrap-2 (lst)
  (list (list (car lst)) (list (cadr lst))))

;b
(defun wrap-2 (lst)
  (cons (cons (car lst) nil) (cons (cons (cadr lst) nil) nil)))

;; Q4 

(defun dewrap-2 (lst)
  (list 
    (if (listp (car lst)) (caar lst) (car lst))
    (if (listp (cadr lst)) (caadr lst) (cadr lst))))

;; Q5

(defun swap (lst)
  (if (null (cdr lst))
    lst
    (cons
      (cadr lst)
      (cons
        (car lst) (cddr lst)))))

;; Q6 

(defun greater-of-two (n1 n2)
  (if (not (and (numberp n1) (numberp n2)))
    'not-number
    (if (>= n1 n2) n1 n2)))

;; Q7 

(defun greatest-of-three (n1 n2 n3)
  (if (not (and (numberp n1) (numberp n2) (numberp n3)))
    'not-number
    (if (>= n1 n2)
      (if (>= n1 n3) n1 n3)
      (if (>= n2 n3) n2 n3))))

;; Q8 

(defun after-first (lst1 lst2)
  (cons (car lst1) (append lst2 (cdr lst1))))

;; Q9 

(defun my-member (x lst)
  (if (member x lst)
    (list x 'is 'a 'member 'of lst)
    (list x 'is 'not 'a 'member 'of lst)))

;; 10 

(defun order (x lst)
  (if (null (member x lst))
    nil
    (+ (- (length lst) (length (member x lst))) 1)))


