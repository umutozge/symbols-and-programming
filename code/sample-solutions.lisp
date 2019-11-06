;;
;; COGS 502 Symbols and Programming
;; METU Informatics
;;
;; Umut Ozge
;; https://github.com/umutozge/symbols-and-programming 

;; Sample solutions for programming exercises

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

;;
;; Question 
;; 
;; Define a procedure that halves a given number until the result becomes less than 1 -- solve the problem by making your procedure call itself.
;;

(defun halver (n)
  (cond ((< n 1) n)
        (t (halver (/ n 2)))))

;;
;; Question
;; 
;; Rewrite (AND X Y Z W) by using COND (Touretzky 1990).
;;

; (cond (X (cond (Y (cond (Z (cond (W))))))))

;;
;; Question
;; 
;; Write COND statements equivalent to:
;; (NOT U), (OR X Y Z) (Winston and Horn 1984).
;;

; (NOT U): 
; (cond (U nil)
;       (t))

; (OR X Y Z):
; (cond (X) (Y) (Z)) 


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

;; 
;; Question
;; 
;; Define a procedure that gives the Fibonacci number of given integer.
;; 

; no accumulator

(defun fib (n)
  "assumes n is a non-negative integer"
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

; with accumulator

; version 1
; where n itself is used as a counter
(defun fibo (n &optional (oneback 1) (twoback 0))
  "assumes n is a non-negative integer"
  (cond ((zerop n) twoback)
        ((= 1 n) oneback)
        (t (fibo (- n 1) (+ oneback twoback) oneback))))

; version 2
; where a counter different from n is used

(defun fiboc (n &optional (oneback 1) (twoback 0) (counter 2))
  "assumes n is a non-negative integer"
  (cond ((< n 2) n)
        ((= counter n) (+ oneback twoback))
        (t (fiboc n (+ oneback twoback) oneback (+ counter 1)))))

;;
;; Question
;;
;; Square roots by Newton's method
;;

(defun square (x) (* x x))

(defun update-guess (x guess)
  (/ (+ (/ x guess) guess) 2))

(defun close-enough? (x y)
  (< (abs (- x y)) 0.000001))

(defun sqroot (x &optional (guess 1.0))
  (if (close-enough? x (square guess))
    guess
    (sqroot x (update-guess x guess))))



