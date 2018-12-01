;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 08
;;

;; Q1

(defun rem-first (item lst)
  "remove the first occurrence of item in lst"
  (cond ((endp lst) nil)
        ((equal (car lst) item) (cdr lst))
        (t (cons (car lst) (rem-first item (cdr lst))))))

;; Q2


; without an accumulator

(defun d-how-many? (item lst)
  (cond ((endp lst) 0)
		((equal (car lst) item) (+ 1 (d-how-many? item (cdr lst))))
		((atom (car lst)) (d-how-many? item (cdr lst)))
		(t (+
			 (d-how-many? item (car lst))
			 (d-how-many? item (cdr lst))))))


;; Q3

(defun rlast (lst)
  (cond ((endp lst) nil) 
        ((endp (cdr lst)) (car lst))
        (t (rlast (cdr lst)))))

;; Q4

(defun chop-end (lst)
  (if (or (endp lst) (= 1 (length lst)))
    nil
    (cons (car lst) (chop-end (cdr lst)))))

;; Q5

(defun sum (lst)
  (cond ((endp lst) 0)
        ((endp (cdr lst)) (car lst))
        (t (+ (car lst) (sum (cdr lst))))))

;; you can have a tail recursive version, where you do not leave "waiting" calls behind during while recursing.

(defun sum2 (lst)
  (cond ((endp lst) 0)
        ((endp (cdr lst)) (car lst))
		(t (sum2 (cons (+ (car lst) (cadr lst)) (cddr lst))))))

;; here is how to do it by using an accumulator

(defun sum3 (lst &optional (accu 0))
  (cond ((endp lst) accu)
        ((endp (cdr lst)) (+ accu (car lst)))
		(t (sum3 (cdr lst) (+ accu (car lst))))))

;; Q6

(defun palindrome (lst)
  (or (endp lst)
	(and
	  (equal (car lst) (rlast lst))
	  (palindrome (cdr (chop-end lst))))))

; you can do the same with COND

(defun palindrome2 (lst)
  (cond ((endp lst) t)
		((equal (car lst) (rlast lst)) (palindrome2 (cdr (chop-end lst))))))

;; Q7

(defun count-length (lst &optional (counter 0))
  (if (endp lst)
    counter
    (count-length (cdr lst) (+ counter 1))))

;; Q8

; the input list's being a list of pairs is totally non-essential for the solution
; here is a function that sums all the numbers in a list even some numbers come inside
; embedded lists; d for deep.

(defun d-sum (lst)
  (cond ((atom lst) (or lst 0)) ; no need to test for empty list, NIL is an atom -- OR turns NIL to 0
		((atom (car lst)) (+ (car lst) (d-sum (cdr lst))))
		(t (+ (d-sum (car lst)) (d-sum (cdr lst))))))

;; Q9

(defun l-prod (n lst)
  (if (endp lst)
	nil
	(cons (* n (car lst)) (l-prod n (cdr lst)))))

;; Q10

(defun pair-prod (list-of-pairs)
  (if (endp list-of-pairs)
	nil
	(cons (* (caar list-of-pairs) (cadar list-of-pairs)) (pair-prod (cdr list-of-pairs)))))


;; Q11


(defun blank-n (n lst)
  (cond ((endp lst) nil) 
		((numberp n) (blank-n (list 1 n) lst)) ; in the first encounter we turn n into a pair of a counter and n
		((equal (car n) (cadr n)) (cons 'X 
										(blank-n (list 1 (cadr n)) (cdr lst))))
		(t (cons (car lst)
				 (blank-n (list (+ (car n) 1) (cadr n)) (cdr lst))))))


; or perhaps a little easier -- or harder, you decide -- to read:

(defun blank-n (n lst)
  (cond ((endp lst) nil) 
		((numberp n) (blank-n (list 1 n) lst))
		(t 
		  (let ((counter (car n))
				(num (cadr n)))
			(if (eql counter num)
			  (cons 'X (blank-n (list 1 num) (cdr lst)))
			  (cons (car lst) (blank-n (list (+ counter 1) num) (cdr lst))))))))


; by using a separate counter

(defun blank-n (n lst &optional (counter 1))
  (cond ((endp lst) nil)
        ((= counter n) (cons 'x (blank-n n (cdr lst) 1)))
        (t (cons (car lst) (blank-n n (cdr lst) (+ counter 1))))))

;; Q12

; with using MEMBER
(defun multi-member (x lst)
  "Checks if its first argument occurs more than once in the second"
  (cond ((endp lst) nil)
        ((equal x (car lst)) (member x (cdr lst)))
        (t (multi-member x (cdr lst))))) 


;; Q13


;; Q14 & Q15


; here is the code for deep subs, shallow subs should be obvious looking at this
(defun subs (new old expr)
  "Substitutes new with old in expr"
  (cond ((and (listp expr) (endp expr)) nil)
        ((atom expr) (if (equal old expr)
                              new
                              expr
                              ))
        (t (cons (subs new old (car expr)) (subs new old (cdr expr))))))
