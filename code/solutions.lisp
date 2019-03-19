
;;
;; Question 
;;
;; Define a procedure that takes three numbers and gives back the second largest of them. 
;; 

(defun seclarge (x y z)
  (if (<= x y)
    (if (<= y z)
      y
      (if (< x z)
        z
        x))
    (if (<= x z)
      x
      (if (> z y)
        z
        y))))

;;
;; Question 
;; 
;; Define a procedure that takes three numbers and gives back the sum of the squares of the larger two. 
;; 

(defun sqr (x) (* x x))

(defun sql2 (x y z) 
  (+ (sqr (max x y z))
     (sqr (seclarge x y z))))


;; Question
;; 
;; The following definition is meant to mimic the behavior of IF using
;; AND and OR.
;; 
;; (defun custom-if (test succ fail) ; wrong!
;;      (or (and test succ) fail))
;; 
;; But it is unsatisfactory in one case, what is it?  Define a better procedure
;; which avoids this failure (Touretzky 1990).
;; 

; Take for instance (custom-if 3 (< 5 4) 8), this incorrectly would return 8 instead of NIL. 
; Here is a correct version

(defun custom-if (test succ fail)
  (or (and test succ) (and (not test) fail)))


;; 
;; Question
;; 
;; Define a procedure that multiplies two integers using only addition as a
;; primitive arithmetic operation. Assume that the first operand will always be
;; greater than or equal to 0.
;; 

; version 1

(defun multiply1 (x y)
  "does not work for x < 0"
  (cond ((zerop x) 0)
        (t (+ y (multiply1 (- x 1) y)))))


; version 2

(defun mult (x y pro)
  (if (zerop x)
    pro
    (mult (- x 1) y (+ pro y))))

(defun multiply2 (x y)
  "does not work for x < 0"
  (mult x y 0))

; you can also write version 2 using &optional

(defun multiply3 (x y &optional (pro 0))
  "does not work for x < 0"
  (if (zerop x)
    pro
    (multiply3 (- x 1) y (+ pro y))))

;; 
;; Question
;; 
;; Define a procedure that multiplies two integers using only addition as a
;; primitive arithmetic operation. It should work for positive and negative
;; integers. 
;; 

(defun multiply4 (x y &optional (pro 0))
  (cond ((< x 0) (- (multiply4 (- x) y)))
        ((zerop x) pro)
        (t (multiply4 (- x 1) y (+ pro y)))))


;; 
;; Question
;; 
;; Define a procedure \Verb+COLL+ that implements the function computing a
;; Collatz' sequence. 
;; 


; a version that prints the computed numbers

(defun coll (n)
  (if (= 1 n)
    t
    (if (evenp n)
      (coll (print (/ n 2)))
      (coll (print (+ (* 3 n) 1))))))

;; 
;; Question
;; 
;; Define a procedure that takes two integers, say $x$ and $y$, and returns the
;; sum of all the integers in the range including and between $x$ and $y$. 
;; 

; without accumulator

(defun sumrange (x y)
  "assumes x <= y"
  (if (= x y)
    y
    (+ x (sumrange (+ x 1) y))))

; with accumulator

(defun sumr (x y &optional (sum 0))
  "assumes x <= y"
  (if (= x y)
    (+ sum y)
    (sumr (+ x 1) y (+ x sum))))
