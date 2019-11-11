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


;;
;; Question
;;
;; Define a procedure AFTER-FIRST that takes two lists and inserts all the
;; elements in the second list after the first element of the first list. Given
;; (A D E) and (B C), it should return (A B C D E).
;;

(defun after-first (lst1 lst2)
  (cons (car lst1) (append lst2 (cdr lst1))))


;; 
;; Question 
;;
;; Define a procedure that gives the last element of a list or gives NIL if the
;; list is empty. Name your procedure LASTT in order not to clash
;; with LISP’s built-in LAST.
;; 

; first define a procedure SINGLETONP that checks whether its argument is a
; single-element list or not. 

(defun singletonp (xs)
  (and (consp xs) (null (cdr xs))))

; now use SINGLETONP to define LASTT

(defun lastt (xs)
  (cond ((endp xs) nil)
        ((singletonp xs) (car xs))
        (t (lastt (cdr xs)))))

;; 
;; Question
;; 
;; Define a procedure that checks whether a given list of symbols is a palindrome.  Use CAR and your solution LASTT 
;; 

;; for this question you need to be able remove the first and last element of a list. There are many ways to do this.
;; here is one:

(defun strip (xs)
  (reverse (cdr (reverse (cdr xs)))))

(defun palind (xs)
  (cond ((or (endp xs) (singletonp xs)) t)
        (t (if (equal (car xs) (lastt xs))
             (palind (strip xs))
             nil))))




;;
;; Question
;;
;; Define your own version of \Verb+NTH+.
;;

(defun mynth (n lst)
  "When this returns NIL, there is no way to tell whether the index got out of range or NIL was an element."
  (if (zerop n)
    (car lst)
    (mynth (- n 1) (cdr lst))))


;;
;; Question
;;
;; Define a function MULTI-MEMBER that checks if its first argument occurs more
;; than once in the second.
;;

; with MEMBER

(defun multi-member (x lst)
  "Checks if its first argument occurs more than once in the second"
  (cond ((endp lst) nil)
        ((equal x (car lst)) (member x (cdr lst)))
        (t (multi-member x (cdr lst))))) 

; without MEMBER

(defun multi-member (x lst &optional seen-before?)
  "Checks if its first argument occurs more than once in the second"
  (cond ((endp lst) nil)
        ((equal x (car lst)) (if seen-before?
                               t
                               (multi-member x (cdr lst) t)))
        (t (multi-member x (cdr lst) seen-before?))))


;;
;; Question
;;
;; Define your own procedure APPEND2 that appends two list arguments into a
;; third list. You are not allowed to use APPEND, LIST and REVERSE -- use just
;; CONS.
;;

(defun app (lstA lstB)
  (if lstA
    (cons (car lstA) (app (cdr lstA) lstB))
    lstB))

;;
;; Question
;; 
;; Define a procedure HOW-MANY? that counts the top-level occurrences of an item in a list.
;;

; no accumulator

(defun how-many? (item lst)
  (cond ((endp lst) 0)
		((equal item (car lst)) (+ 1 (how-many? item (cdr lst))))
		(t (how-many? item (cdr lst)))))

; with accumulator

(defun how-many? (item lst &optional (counter 0))
  (if lst 
    (how-many?
      item
      (cdr lst)
      (+ counter 
         (if (equal (car lst) item) 1 0)))
    counter))

;;
;; Question
;; 
;; The built-in REVERSE reverses a list. Define your own version of reverse.
;;

; no accumulator

(defun revers (lst)
  (if lst
    (append (revers (cdr lst)) (list (car lst)))))

; with accumulator

(defun revers (lst &optional acc)
  (if lst
    (revers (cdr lst) (cons (car lst) acc))
    acc))


;;
;; Question
;; 
;; Define a predicate that tells whether its argument is a dotted list or not.
;;

(defun dotted (lst)
  (if (consp lst)
    (if (atom (cdr lst))
      t
      (dotted (cdr lst)))))



