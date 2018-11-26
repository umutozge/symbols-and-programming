;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 07
;;

;; Q1

(defun uniq (lst)
  (let ((seen nil))
	(dolist (x lst (reverse seen))
	  (if (not (member x seen))
		(push x seen)))))

;; Q2

(defun run-mean (lst)
  (let ((store nil)
		(sum 0))
	(dotimes (i (length lst) (reverse store))
	  (incf sum (nth i lst))  ; equivalent to (setf sum (+ sum (nth i lst))) 
	  (push (/ sum (+ i 1)) store))))

;; as (incf sum (nth i lst)), besides updating sum, also returns the new updated value of sum, you can define the same function as:

(defun run-mean2 (lst)
  (let ((store nil)
		(sum 0))
	(dotimes (i (length lst) (reverse store))
	  (push (/ (incf sum (nth i lst)) (+ i 1)) store))))


;; Q3

(defun pairlists (list-of-lists)
  "pairs(!) the lists given in the list-of-lists; assumes they are of equal length -- keeps the original order"
  (let ((len (length (car list-of-lists)))
		(store nil))
	(dotimes (i len (reverse store))
	  (push 
		(reverse
		  (let ((ministore nil))
			(dolist (j list-of-lists ministore)
			  (push (nth i j) ministore))))
		store))))

;; An alternative would be to make the outer iteration over list-of-lists
;; that solution requires a use of setf that we haven't seen so far; namely setf'ing 
;; not a variable but a postion in a list, e.g.\ a car or an nth expression.

;; Q4

(defun search-pos (search-item search-list)
  "return the list of positions search-item (a list) matches in search-list"
  (let ((win (make-list(length search-item)))
		(store nil))
	(dotimes (i (length search-list) store)
	  (setf win (append
				  (cdr win)
				  (list (nth i search-list))))
	  (if (equal win search-item)
		(push (- (+ i 1) (length win)) store)))))

;; Q5

(defun reverse0 (lst)
  (let ((result nil))
	(dolist (x lst result)
	  (push (if (listp x) (reverse0 x) x) result))))

;; Q6

(defun reverse1 (lst)
  (if (endp lst) 
	nil
	(append (reverse1 (cdr lst)) (list (car lst)))))

;; Q7

(defun reverse2 (lst)
  (if (endp lst)
	nil 
	(append
	  (reverse2 (cdr lst))
	  (list 
		(let ((elm (car lst)))
		  (if (atom elm) elm (reverse2 elm)))))))

;; Note here the use of LET to save one from computing (car lst) three times
;; LET is so good for defining such local variables without interfering with 
;; the return value

;; or


(defun reverse2 (lst)
  (if (or (atom lst) (endp lst)) ; note that (or (endp lst) (atom lst)) wouldn't work here -- can you see why? 
	lst
	(append
	  (reverse2 (cdr lst))
	  (list (reverse2 (car lst))))))

;; Q8

(defun how-many? (item lst)
  (cond ((endp lst) 0)
		((equal item (car lst)) (+ 1 (how-many? item (cdr lst))))
		(t (how-many? item (cdr lst)))))

;; Q9

(defun r-remove (item lst)
  (cond ((endp lst) nil)
        ((equal (car lst) item) (r-remove item (cdr lst)))
        (t (cons (car lst) (r-remove item (cdr lst))))))
