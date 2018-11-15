
;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 06
;;

;; Q1

; a. (cond (u nil) 
;          (t t))

; b. (cond (x (cond (y (cond (z z))))))

; c. (OR X Y Z)

; c. (defmacro mor (x y z)
;      `(cond (,x ,x)
;             (,y ,y)
;             (,z ,z)))

;; Q2

(defun split (n lst)
  (let ((front nil)
		(end nil)
		(counter 0))
	(dolist (x lst (list (reverse front) (reverse end)))
	  (incf counter)
	  (if (<= counter n)
		(push x front)
		(push x end)))))

;; Q3

(defun split-two (lst)
  (let ((halfcount (if (evenp (length lst))
					 (/ (length lst) 2)
					 (/ (- (length lst) 1) 2)))
		(front nil)
		(end nil)
		(counter 0))
	(dolist (x lst (list (reverse front) (reverse end)))
	  (incf counter)
	  (cond ((<= counter halfcount) (push x front))
			((= counter (+ 1 halfcount)) (if (oddp (length lst))
										   (let ((half (/ x 2))) ; thanks to LET, no need for PROGN
											 (push half front)
											 (push half end))
										   (push x end)))
			(t (push x end))))))
		
; This could be slightly simplified by using the function FLOOR, which gives the whole number part of a division and using DOTIMES

(defun split-two2 (lst)
  (let ((halfcount (floor (/ (length lst) 2)))
		(front nil)
		(end nil))
	(dotimes (i (length lst) (list (reverse front) (reverse end)))
	  (let ((elm (nth i lst)))
		(cond ((< i halfcount) (push elm front))
			  ((= i halfcount) (if (oddp (length lst))
								 (let ((half (/ elm 2)))
								   (push half front)
								   (push half end))
								 (push elm end)))
			  (t (push elm end)))))))

; Here is a way to do it with DO; don't worry if you don't understand it now, even if you know how DO works, the code is very cryptic. 

(defun split-two3 (lst)
  (do ((x lst (cdr x))
	   (y nil (cons (car x) y))
	   (fwd 0 (+ fwd 1))
	   (bck (length lst) (- bck 1)))
	((< (- bck fwd) 2)
	 (if (zerop (- bck fwd))
	   (list (reverse y) x)
	   (let ((half (/ (car x) 2)))
		 (list (reverse (cons half y)) (cons half (cdr x))))))))

	
