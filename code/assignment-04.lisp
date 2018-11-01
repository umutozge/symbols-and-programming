;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 04
;;

;; Q1

(defun snoc1 (elm lst)
  (append lst (list elm)))

(defun snoc2 (elm lst)
  (reverse (cons elm (reverse lst))))

;; Q2

(defun palind (lst)
  (equal lst (reverse lst)))

;; Q3

(defun howcompute (x y z)
  (cond ((eql (+ x y) z) 'added)
		((eql (- x y) z) 'subtracted)
		((eql (* x y) z) 'multiplied)
		((eql (/ x y) z) 'divided)
		(t 'dont-know)))

;; Q4

(defun listpro (lst)
  (if (endp lst)   ; without this check the function would
	nil            ; return 1 for an empty list
	(let ((product 1))
	  (dolist (x lst product)
		(if (numberp x)
		  (setf product (* product x)))))))

;; Q5 

(defun reverse2 (lst)
  (let ((result nil))
	(dolist (x lst result)
	  (setf result (cons x result)))))

;; Q6

(defun nestedp (lst)
  "checks whether lst has any list element"
  (let ((am-i-nested nil))
	(dolist (x lst am-i-nested)
	  (if (listp x)
		(setf am-i-nested t)))))

;; Q7 

(defun remove2 (elm lst)
  "reomves elm from lst"
  (let ((store nil))
	(dolist (x lst (reverse store))
	  (if (not (equal elm x))
		(setf store (cons x store))))))

;; Q8

(defun remove3 (elm lst) 
  "removes elm from lst if it's preceded by an X -- Xs stay"
  (let ((store nil)
		(previous nil))
	(dolist (x lst (reverse store))
	  (if (not
			(and
			  (equal x elm)
			  (equal previous 'x)))
		(setf store (cons x store)))
	  (setf previous x))))

(defun remove3 (elm lst) 
  "removes elm from lst if it's preceded by an X -- Xs stay"
  (let ((store nil)
		(previous nil))
	(dolist (x lst (reverse store))
	  (if (not (equal x elm))
		(setf store (cons x store))
		(if (not (eql previous 'x))
		  (setf store (cons x store))))
	  (setf previous x))))

;; Q9

(defun types (lst)
  (dolist (k lst)
	(cond ((listp k) (print 'list))
		  ((numberp k) (print 'number))
		  ((symbolp k) (print 'symbol))
		  (t (print 'unknown-type)))))

; here is a way to save some typing

(defun types (lst)
  (dolist (k lst)
	(print (cond ((listp k) 'list)
				 ((numberp k) 'number)
				 ((symbolp k) 'symbol)
				 (t 'unknown-type))))

;; Q10

(defun last2 (lst)
  "returns the last element of lst"
  (let ((pos 0)
		(end (length lst))
		(result nil))
	(dolist (x lst result)
	  (if (eql pos (- end 1))
		(setf result x))
	  (setf pos (+ pos 1)))))

; a simpler solution is keeping the previous element in a variable

(defun last-simple (lst)
  "returns the last element of lst"
  (let ((prev nil))
	(dolist (x lst prev)
	  (setf prev x))))

; but with some minor modification the more complex solution using a counter can be
; handy for finding nth element from the end; see the next assignment.

;; Q11

(defun remove-last (lst)
  (let ((pos 0)
		(end (length lst))
		(store nil))
	(dolist (x lst (reverse store))
	  (setf pos (+ pos 1))
	  (if (< pos end) 
		(setf store (cons x store))))))
