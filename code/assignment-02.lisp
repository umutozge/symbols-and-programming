;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 03
;;

;; Q5

(defun insert-2nd (item lst)
  (cons (car lst) (cons item (cdr lst))))

;; Q6

(defun replace-2nd (item lst)
  (cons (car lst) (cons item (cdr (cdr lst)))))

;; Q7

(defun swap (lst)
  (list (cadr lst) (car lst)))
