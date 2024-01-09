;;
;; COGS 502 Symbols and Programming
;; METU Informatics
;;
;; Umut Ozge
;; https://github.com/umutozge/symbols-and-programming
;;
;; with contributions from Alaz Aydın and Tunç Güven Kaya

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
;; Define a procedure that takes an integer -- you do not need to check for this, simply assume that the input will always be an integer.
;;
;; - return it as it is, if it is divisible by both 3 and 5;
;; - multiply the number with 5 and return the result, if the number is divisible by 3 but not 5;
;; - multiply the number with 3 and return the result, if the number is divisible by 5 but not 3;
;; - return \Verb+NIL+ if the number is divisible neither by 3 nor 5.
;;

(defun div-by-p (n m)
  (zerop (rem n m)))


(defun foo (n)
  (or (and
        (div-by-p n 3)
        (div-by-p n 5)
        n)
      (and (div-by-p n 3) (* 5 n))
      (and (div-by-p n 5) (* 3 n))))


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


;;
;; Question
;;
;; Solve the changer-cond task in the lecture notes by using only AND, OR and NOT


(defun changer (n)
  (and
    (numberp n)
    (or
      (and (integerp n) (or
                          (and (evenp n) (/ n 2))
                          (and (zerop (rem n 3)) (+ (* 3 n) 1))
                          n))
      (changer (round n)))))



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
;; Define a procedure \Verb+TOSS+ that takes a non-negative integer $n$, tosses a coin $n$ number of times, printing the result (0 or 1) on the screen in each toss.
;;

(defun toss (n)
  (if (zerop n)
      t
      (and (print (random 2)) (toss (- n 1)))))








;;
;; Question
;;
;; Define a recursive procedure that computes the sum of the squares of the first $n$ non-negative integers.
;;

(defun sos (n)
  (if (zerop n)
      0
      (+ (* n n) (sos (- n 1)))))


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
;; Define a procedure that gives the Fibonacci number of a given integer.
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
;; Ackermann-Peter function
;;

(defun ap (m n)
  (cond ((zerop m) (+ n 1))
        ((zerop n) (ap (- m 1) 1))
        (t (ap (- m 1) (ap m (- n 1))))))

;;
;; Question
;;
;; Define a two operand procedure that raises its first operand to the power of
;; the second. You are allowed to use multiplication and subtraction. Define
;; two versions, with and without an accumulator. You can check the behavior of
;; your procedure by comparing it with \Verb+LISP+'s \Verb+EXPT+, which does
;; the same thing.
;;

; no accu

(defun power (x y)
  (if (zerop y)
    1
    (* x (power x (- y 1)))))


; with accu

(defun powerr (x y &optional (pro 1))
  (if (zerop y)
    pro
    (powerr x (- y 1) (* x pro))))

;;
;; Question
;;
;;
;; Define a recursive procedure that returns the sum of a geometric progression
;; with $n$ terms and the initial term $a$ and the common ratio $r$.
;;

(defun geo (a r n &optional (product 1) (sum 0))
  (if (zerop n)
      (* a sum)
      (geo a r (- n 1) (* product r) (+ sum product))))


;; Question
;;
;; Compute the average of two integers m and n (where m < n)
;; using only addition and subtraction.
;;

(defun aver (m n)
  (if (= m n)
    m
    (aver (+ m 0.5) (- n 0.5))))


;;
;; Question
;;
;; Define a procedure that takes a list and an object, and returns a list where the object is added to the end of the list.
;;

(defun foo (lst x)
  (append lst (list x)))

;; or

(defun foo (lst x)
  (reverse (cons x (reverse lst))))

;;
;; Question
;;
;; Using CAR and CDR, define a procedure to return the fourth element of a list.\footnote{Graham (1996), p.\ 29, ex.\ 3.}
;;

(defun myfourth (lst)
  (car (cdr (cdr (cdr lst)))))

;;
;; Question
;;
;; Using CAR and CDR, define a procedure to return the fourth element of a list.\footnote{Graham (1996), p.\ 29, ex.\ 3.}
;;

;; Define a procedure named \Verb+insert-2nd+, which takes a list and an object,
;; and gives back a list where the element is inserted after the first element
;; of the given list. Assume that the input list will have at least one element.
;;

(defun insert-2nd (x lst)
  (cons (car lst) (cons x (cdr lst))))

;; this gives (NIL X) if the lst argument is NIL; if you want (X) for such a case:

(defun insert-2nd (x lst)
  (if (endp lst)
    (list x)
    (cons (car lst) (cons x (cdr lst)))))


;;
;; Question
;;
;; Define a procedure named replace-2nd, which is like
;; insert-2nd, but replaces the element at the 2nd position.
;; Assume that the input list will always have at least two elements.
;;

(defun replace-2nd (x lst)
  (cons (car lst) (cons x (cdr (cdr lst)))))

;;
;; Question
;;
;; Define a procedure \Verb+SWAP+, that takes a two element list and switches
;; the order of the elements.
;;

(defun swap (lst)
  (cons (car (cdr lst)) (cons (car lst) nil)))

; or

(defun swap2 (lst)
  (append (cdr lst) (list (car lst))))

;;
;; Question
;;
;; Define a procedure WRAP-2 that  takes a two element list, and wraps each element inside a list.


;;  using LIST, CAR and CADR

(defun wrap-2 (lst)
  (list (list (car lst)) (list (cadr lst))))

;;using CONS, CAR and CADR

(defun wrap-2 (lst)
  (cons (cons (car lst) nil) (cons (cons (cadr lst) nil) nil)))

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
;; The built-in MEMBER takes and object and a list and checks whether the object ap-
;; pears in the list or not; discover how it works. Using MEMBER, define a function
;; MY-MEMBER that behaves as follows:
;;
;; * (my-member 'b '(a b c))
;; (B IS A MEMBER OF (A B C))
;; * (my-member 'z '(a b c))
;; (Z IS NOT A MEMBER OF (A B C))
;; *

; version 1

(defun my-member (x lst)
  (if (member x lst)
      (list x 'is 'a 'member 'of lst)
      (list x 'is 'not 'a 'member 'of lst)))

; version 2

(defun my-member (x lst)
  (append (list x 'is) (if (member x lst) '(a) '(not a)) (list 'member 'of lst)))


;;
;; Question
;;
;; A given set A is a subset of another set B if and only if all the members of
;; A are also a member of B. Two sets are equivalent, if and only if they are
;; subsets of eachother. For this problem you will represent sets via lists.
;;
;; 1. Define a procedure SUBSETP that takes two list arguments and
;; decides whether the first is a subset of the second.
;;
;; 2. Define a procedure EQUIP that takes two list arguments and decides
;; whether the two are equivalent.
;;
;; 3. Define a procedure IDENP that takes two list arguments and decides
;; whether the two have the same elements in the same order – do not directly
;; compare the lists with EQUALP, you are required to do a element by element
;; comparison.
;;

(defun subsetpp (lstA lstB)
  (if (endp lstA)
      t
      (if (member (car lstA) lstB)
          (subsetpp (cdr lstA) lstB))))

(defun equip (lstA lstB)
  (and (subsetpp lstA lstB) (subsetpp lstB lstA)))

(defun idenp (lstA lstB)
  (if (and (endp lstA) (endp lstB))
      t
      (if (equal (car lstA) (car lstB))
          (idenp (cdr lstA) (cdr lstB)))))

;;
;; Question
;;
;;  Assume you have data that pairs employees’ last names with their monthly salaries.
;;  Define a procedure that takes as input employee data and a threshold salary (an
;;  integer), and returns in a list the last names of all the employees that earn above the
;;  threshold salary. Define two versions, one with, and one without an accumulator.
;;


; Example input data: '((smith 3000) (jones 4200) (wills 2400))

; no accu

(defun salary (emp-list thresh)
  (if emp-list
      (if (> (cadr (car emp-list)) thresh)
          (cons (caar emp-list) (salary (cdr emp-list) thresh))
          (salary (cdr emp-list) thresh))))

; accu

(defun salary (emp-list thresh &optional store)
  (if (endp emp-list)
      store
      (salary
        (cdr emp-list)
        thresh
        (append store (if (> (cadar emp-list) thresh)
                          (list (caar emp-list)))))))

;;
;; Question
;;
;;  Using MEMBER and LENGTH, write a function ORDER which gives the order of an item
;;  in a list. You can do this by combining LENGTH and MEMBER in a certain way. It
;;  should behave as follows:
;;  * (order 'a '(a b c))
;;  1
;;  * (order 'c '(a b c))
;;  3
;;  * (order 'z '(a b c))
;;  NIL
;;

(defun order (x lst)
  (let ((mem (member x lst)))
    (if mem
        (+ 1
           (- (length lst) (length mem))))))


;;
;; Question
;;
;; Compute the sum of a list of numbers; with and without an accumulator
;;

(defun summer (lst)
  (if (endp lst)
    0
    (+ (car lst) (summer (cdr lst)))))

(defun summer (lst &optional (sum 0))
  (if (endp lst)
    sum
    (summer (cdr lst) (+ sum (car lst)))))


;;
;; Question
;;
;;  Define a procedure LISTPRO that returns the product of all the numbers in a list;
;;  allow for non-numbers in the list, ignore them when you come across during your
;;  iteration. Write a version with and a version without an accumulator.
;;

; no accu

(defun listpro (lst &optional (first-call? t))
  "GLITCH: gives 1 for lists that consist of only non-numbers"
  (cond ((endp lst) (if first-call? nil 1))
        (t (* (let ((x (car lst))) (if (numberp x) x 1)) (listpro (cdr lst) nil)))))

; with accu

(defun listpro (lst &optional (acc 1) (first-call? t))
  "GLITCH: gives 1 for lists that consist of only non-numbers"
  (cond ((endp lst) (if first-call? nil acc))
        (t (listpro (cdr lst) (* (let ((x (car lst))) (if (numberp x) x 1)) acc) nil))))


;;
;; Question
;;
;; Define a recursive member procedure that checks whether a given item is
;; found in the given list. The item is not required to be a top-most element.
;;

(defun rec-mem (item lst)
  (cond ((endp lst) nil)
        ((equal (car lst) item) t)
        (t (if (consp (car lst))
               (or (rec-mem item (car lst))
                   (rec-mem item (cdr lst)))
               (rec-mem item (cdr lst))))))

; tail-recursive version

(defun recmem (term lst)
  (cond ((endp lst) nil)
        ((equal (car lst) term) t)
        (t (if (listp (car lst))
               (recmem term (append (car lst) (cdr lst)))
               (recmem term (cdr lst))))))


;;
;; Question
;;
;; Define a procedure \Verb+SPLIT-TWO+ that splits a list of numbers into two
;; equal length lists. If the original list has an odd length, there will be a
;; middle element. Split this middle element between the two halves; half of it
;; goes to the end of the first list, half of it goes to the beginning of the
;; second list.
;;

(defun split-two (lst &optional store)
  (cond ((= (length store) (length lst)) (list (reverse store) lst))
        ((= (length store) (- (length lst) 1)) (let ((num (/ (car lst) 2)))
                                                     (list
                                                       (reverse (cons num store))
                                                       (cons num (cdr lst)))))
        (t (split-two (cdr lst) (cons (car lst) store)))))



;;
;; Question
;;
;; Define a procedure that gives the last element of a list or gives NIL if the
;; list is empty. Name your procedure LASTT in order not to clash with LISP’s
;; built-in LAST.
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
;;  Define a procedure NESTEDP that takes a list and returns T if at least one of
;;  its elements is a list, and returns NIL otherwise.
;;

(defun nestedp (lst)
  (if (endp lst)
      nil
      (if (listp (car lst))
          t
          (nestedp (cdr lst)))))

;;
;; Question
;;
;;  Define a procedure REMOVE2 that takes an element and a list; and returns a
;;  list where all the occurrences of the element are removed from the list.
;;

(defun remove2 (x lst)
  (cond ((endp lst) nil)
        ((equal (car lst) x) (remove2 x (cdr lst)))
        (t (cons (car lst) (remove2 x (cdr lst))))))

;;
;; Question
;;
;;  Define a procedure REMAFTER that takes an element, a list and a
;;  pivot element; and returns a list where all the occurrences of the
;;  element that are preceded by the pivot element are removed from
;;  the list.
;;

(defun remafter (x lst pivot &optional prev)
  (cond ((endp lst) nil)
        ((and (equal (car lst) x) (equal prev pivot)) (remafter x (cdr lst) pivot x))
        (t (cons (car lst) (remafter x (cdr lst) pivot (car lst))))))

;;
;; Question
;;
;;  Define a procedure TYPES that takes a list and prints out the type of the elements
;;  encountered. It’s enough that it can tell between number, list and symbol; so use
;;  NUMBERP, LISTP and SYMBOLP. In case you cannot match to any of these, print
;;  UNKNOWN-TYPE. Put a string in your list for testing, it answers NIL to all these
;;  predicates.
;;

(defun type? (obj)
  (cond ((listp obj) 'list)
        ((numberp obj) 'number)
        ((symbolp obj) 'symbol)
        (t 'unknown)))


(defun types (lst)
  (if (endp lst)
      'DONE
      (and (print (type? (car lst))) (types (cdr lst)))))


;;
;; Question
;;
;; A chain in a sequence of numbers is such that each number in the chain is
;; either equal to or greater than the one before it. For instance, 2 5 9 12 17
;; 21 is a chain, but not 2 5 9 17 12 21, because the 17 12 sub-sequence breaks
;; the chain. Define a recursive procedure that finds and returns the longest
;; chain in a sequence of numbers.
;;

(defun longer (seqA seqB)
  "return the longer of the two sequences, return the first if they are of equal length"
  (if (>= (length seqA) (length seqB))
      seqA
      seqB))

(defun longest-chain (seq &optional prev chain backup)
  (cond ((endp seq) (if (null backup)
                        (reverse chain)
                        (reverse (longer backup chain))))
        ((null prev) (longest-chain
                       (cdr seq)
                       (car seq)
                       (list (car seq))
                       backup))
        ((>= (car seq) prev) (longest-chain
                              (cdr seq)
                              (car seq)
                              (cons (car seq) chain)
                              backup))
        (t (longest-chain
             (cdr seq)
             (car seq)
             (list (car seq))
             (longer backup chain)))))

;;
;; Question
;;
;;  Define a procedure LAST2 that takes a list and returns the last element of the list.
;;  Of course, don’t use LAST. One way could be to keep a counter,
;;  so that you can compare this to the length of the list to recognize whether you are
;;  close enough to the end of the list.
;;

(defun last2-aux (lst len)
  (if (= 1 len)
      (car lst)
      (last2-aux (cdr lst) (- len 1))))

(defun last2 (lst)
  "when returns NIL, it is ambiguous between an empty input and an input with NIL as the last element"
  (if lst
   (last2-aux lst (length lst))))

;;
;; Question
;;
;;  Define a procedure CHOP-LAST, which removes the final element of the
;;  given list – its like CDR from the back. You are NOT allowed to make
;;  (REVERSE (CDR (REVERSE LST))). Nothing to be done for an empty list, just
;;  return it as it is; but a single element list gets “nilled”.
;;

(defun singletonp (lst)
  (and (listp lst) (= (length lst) 1)))

(defun chop-last (lst)
  (if (or (singletonp lst) (endp lst))
        nil
        (cons (car lst) (chop-last (cdr lst)))))

;;
;; Question
;;
;; Define a procedure that checks whether a given list of symbols is a palindrome.  Use CAR and your solution LASTT
;;

; for this question you need to be able remove the first and last element of a list. There are many ways to do this.
; here is one:

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
;; Define your own version of NTH.
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
    (if (not (listp (cdr lst)))
      t
      (dotted (cdr lst)))))


;;
;; Question
;;
;; Define a recursive procedure D-HOW-MANY? that counts all not only top-level
;; occurrences of an item in a list.
;; For instance (D-HOW-MANY? 'A '((A B) (C (A X)) A)) should return 3.
;;

; with accumulator

(defun d-how-many? (x lst &optional (counter 0))
  (cond ((endp lst) counter)
        ((equal x (car lst)) (d-how-many? x (cdr lst) (1+ counter)))
        ((listp (car lst)) (d-how-many? x (cdr lst) (d-how-many? x (car lst) counter)))
        (t (d-how-many? x (cdr lst) counter))))

; w/o accumulator

(defun d-how-many? (x lst)
  (cond ((endp lst) 0)
        ((equal x (car lst)) (+
                               1
                               (d-how-many? x (cdr lst))))
        ((listp (car lst)) (+
                             (d-how-many? x (car lst))
                             (d-how-many? x (cdr lst))))
        (t (d-how-many? x (cdr lst)))))

;;
;; Question
;;
;; Deep-reverse a list.
;; For instance (reverse0 '(a (b c (x d)) k)) should give (K ((D X) C B) A)
;;

(defun reverse0 (lst &optional acc)
  (cond ((null lst) acc)
        ((atom lst) lst)
        (t (reverse0
             (cdr lst)
             (cons (reverse0 (car lst)) acc)))))


;;
;; Question
;;
;; Define a three argument procedure REMOVE-NTH, which removes every nth
;; occurrence of an item from a list.
;;

(defun remove-nth (item n lst &optional (counter 0))
  (cond ((endp lst) nil)
        ((equal (car lst) item)
         (if (= counter (- n 1))
           (remove-nth item n (cdr lst) 0)
           (cons item (remove-nth item n (cdr lst) (+ counter 1)))))
        (t (cons (car lst) (remove-nth item n (cdr lst) counter)))))


;;
;; Question
;;
;; Return the largest number in a list.
;;

;; without acc

(defun max1 (lst)
  (cond ((endp lst) nil)
        ((null (cdr lst)) (car lst))
        (t (let ((maxrest (max1 (cdr lst))))
             (if (> (car lst) maxrest)
               (car lst)
               maxrest)))))

;; you can shorten the above code by using LISP's built-in MAX:

(defun max12 (lst)
  (cond ((endp lst) nil)
        ((null (cdr lst)) (car lst))
        (t (max (car lst) (max12 (cdr lst))))))


;; with acc

(defun max2 (lst &optional acc)
  (cond ((endp lst) acc)
        ((null acc) (max2 (cdr lst) (car lst)))
        (t (max2
             (cdr lst)
             (max (car lst) acc)))))

;; you can get rid off the (null acc) clause by defining two procedures:

(defun max4 (lst)
  (if lst
    (max3 (cdr lst) (car lst))
    nil))

(defun max3 (lst &optional acc)
  (cond ((endp lst) acc)
        (t (max3 (cdr lst) (max acc (car lst))))))


;;
;; Question
;;
;; Define a procedure that takes a list of integers and an integer n, and
;; returns the nth largest integer in the list.
;;

; there are many ways to solve this problem; we will start by not caring about efficiency.

; recursively find the maximum of the list and remove it while counting the removals.


(defun maxx (lst)
  (max2 lst)) ; max2 defined above, replace it with your choice of max function working on lists

(defun nthlarge (n lst)
  (cond ((endp lst) nil)
        ((= n 1) (maxx lst))
        (t (nthlarge
             (- n 1)
             (remove (maxx lst) lst)))))

; now a bare-hands solution:

(defun bubble (x lst)
  "inserts x in a position in lst such that everything to the left of x is smaller than it"
  (if (endp lst)
    (list x)
    (if (>= x (car lst))
      (cons (car lst)
            (bubble x (cdr lst)))
      (cons x lst))))

(defun nth-large (n lst &optional store)
  (cond
    ((endp lst) (if (= (length store) n)
                  (car store)))
    (t (nth-large
         n
         (cdr lst)
             (if (< (length store) n)
           (bubble (car lst) store)
           (cdr (bubble (car lst) store)))))))


;;
;; Question
;;
;; Define a procedure UNIQ that takes a list and removes all the repeated
;; elements in the list keeping only the first occurrence. For instance:
;; (uniq '(a b r a c a d a b r a)) should give (A B R C D).
;; Don’t use REMOVE (built-in or in-house), you may use MEMBER.
;;

(defun uniq (lst &optional acc)
  (if lst
    (let ((current (car lst)))
      (if (member current acc)
        (uniq (cdr lst) acc)
        (uniq (cdr lst) (append acc (list current)))))
    acc))


;;
;; Question
;;
;; Define a procedure UNIQ that takes a list and removes all the repeated
;; elements in the list keeping only the last occurrence. For instance:
;; (uniq '(a b r a c a d a b r a)) should give (C D B R A).
;; Don’t use REMOVE (built-in or in-house), you may use MEMBER.
;;

(defun uniq (lst)
  (if lst
    (let ((current (car lst)))
      (if (member current (cdr lst))
        (uniq (cdr lst))
        (cons current (uniq (cdr lst)))))))


;;
;; Question
;;
;; Define a procedure REMLAST which removes the last occurrence of an item from
;; a list. Do not use MEMBER or REVERSE.
;;


(defun remlast (x lst &optional guess backup)
  (cond ((endp lst) guess)
        ((equal x (car lst))
         (remlast
           x
           (cdr lst)
           backup
           (append backup (list x))))
        (t (remlast
             x
             (cdr lst)
             (append guess (list (car lst)))
             (append backup (list (car lst)))))))


;; here is a version that uses a final reverse (and is therefore more
;; efficient), uses LABELS for helper procedures, and has keyword
;; arguments 

(defun remlast (lst elm &key (backup nil) (guess nil))
  (labels ((headp (elm lst)
             "checks whether the CAR of lst is elm"
             (eql (car lst) elm))
           (transhead (lstA lstB)
             "conses lstA's CAR to lstB and returns the result"
             (cons (car lstA) lstB)))
    (cond ((endp lst) (reverse guess))
          ((headp elm lst)
            (remlast
              (cdr lst)
              elm
              :backup (transhead lst backup)
              :guess backup))
          (t (remlast
               (cdr lst)
               elm
               :backup (transhead lst backup)
               :guess  (transhead lst guess))))))

;;
;; Question
;;
;; Recursively count non-nil elements in a list.
;;

(defun count-atoms (lst &optional (counter 0))
  (cond ((null lst) counter) ;; note that endp wouldn't work here
        ((atom lst) (+ counter 1))
        (t (count-atoms
              (cdr lst) (count-atoms (car lst) counter)))))

;;
;; Question
;;
;; Bring to front
;;

(defun bring-to-front (item lst &optional store)
  (cond ((endp lst) store)
        ((equal item (car lst))
         (bring-to-front item (cdr lst) (cons item store)))
        (t
          (bring-to-front item (cdr lst) (append store (list (car lst)))))))

;;
;; Question
;;
;; Group repetitions in a list. E.g. (1 2 2 3 4 4) should yield ((1) (2 2) (3) (4 4)).
;;

(defun group (lst &optional ministore store)
  (if (endp lst)
    (if ministore
      (append store (list ministore))
      store)
    (if ministore
      (if (equal (car lst) (car ministore))
        (group (cdr lst) (cons (car lst) ministore) store)
        (group (cdr lst) (list (car lst)) (append store (list ministore))))
      (group (cdr lst) (list (car lst)) store))))

;;
;; Question
;;
;; Define a recursive  procedure SUBSTITUTE with 3 arguments,
;; say old new exp such that every occurrence of
;; old at the top-level of exp is replaced by new. By
;; 'top-level' we mean the function should not check embedded levels in
;; lists. E.g. (substitute 'x 'k '(x (x y) z)) should return (k
;; (x y) z)

;;
;; Then modify SUBSTITUTE to D-SUBS (for 'deep substitute'), so that it does
;; the replacement for all occurrences of old, no matter how
;; deeply embedded.

; here is the code for deep subs, shallow subs should be obvious looking at this
(defun subs (new old expr)
  "Substitutes new with old in expr"
  (cond ((and (listp expr) (endp expr)) nil)
        ((atom expr) (if (equal old expr)
                              new
                              expr
                              ))
        (t (cons (subs new old (car expr)) (subs new old (cdr expr))))))

; subs using store

(defun subs (new old expr &optional store)
  (cond ((and (listp expr) (endp expr)) store)
		((atom (car expr)) (subs new old (cdr expr)
								 (append store
										 (if (equal old (car expr))
										   (list new)
										   (list (car expr))))))
		(t (subs new old (cdr expr)
				 (append store (list (subs new old (car expr))))))))



;;
;; Question
;;
;; Define a procedure VALS that takes a list of one argument procedures and an
;; argument, and returns the values obtained by applying the procedures to the
;; argument in the given order.
;;

; procedures for testing

(defun f (x) (expt x 2))
(defun g (x) (expt x 3))
(defun h (x) (log x))

(defun vals (funcs n)
  "collect the values obtained by applying each func in funcs to n"
  (if funcs
    (cons (funcall (car funcs) n) (vals (cdr funcs) n))))

;;
;; Question
;;
;; Define a procedure \Verb+PAIRVALS+ that takes a list of one argument
;; procedures and an argument, and returns the list of dotted pairs where each
;; procedure is paired with the value obtained by applying it to the argument.
;;

(defun pairvals (funcs n)
  "pair the funcs with the value (funcall func n) as a dotted pair"
  (if funcs
    (cons (cons (car funcs)
                (funcall (car funcs) n))
          (pairvals (cdr funcs) n))))
;;
;; Question
;;
;; Define a procedure that takes a list of predicate symbols (e.g.
;; CONSP, NUMBERP etc.) and an object, and returns the list of
;; predicates that the object satisfies.
;;

; without mapcar

(defun sati (list-of-preds obj)
  (if list-of-preds
    (if (funcall (car list-of-preds) obj)
       (cons (car list-of-preds) (sati (cdr list-of-preds) obj))
       (sati (cdr list-of-preds) obj))))

; with mapcar

(defun sati2 (list-of-preds obj)
  (remove-if #'null
             (mapcar
               #'(lambda (x) (and (funcall x obj) x))
               list-of-preds)))

;;
;; Question
;;
;; Define a procedure that takes a list of predicate symbols (e.g.\
;; CONSP, NUMBERP etc.) and a list of objects, collects and
;; returns all the objects that answer yes to at least one predicate in the
;; predicate list.
;;

; without mapcar

(defun sat-one (preds obj)
  "check if at least one pred in preds is satisfied by obj"
  (if preds
    (or (funcall (car preds) obj)
        (sat-one (cdr preds) obj))))

(defun mati (preds lst)
  (if lst
    (if (sat-one preds (car lst))
      (cons (car lst) (mati preds (cdr lst)))
      (mati preds (cdr lst)))))

; with mapcar

(defun mati2 (preds lst)
  (remove-if #'null
             (mapcar
               #'(lambda (x) (and (sat-one preds x) x))
               lst)))

;;
;; Question
;;
;; Define a procedure that takes a list of one argument numerical procedures
;; (define your own and/or use the built-ins you know for testing) and a number, and
;; returns the name of the procedure that yields the maximum value when applied
;; to the number argument.
;;

; many ways to solve this, here is one strategy
; pair funcs with their values obtained by applying them to n

(defun pairvals (funcs n)
  "pair the funcs with the value (funcall func n) as a dotted pair"
  (if funcs
    (cons (cons (car funcs)
                (funcall (car funcs) n))
          (pairvals (cdr funcs) n))))

; now define a procedure that returns the maximum in a list of pairs; the
; comparison will be on the basis of cdr's

(defun maxpair (lst)
  "get the maximum pair (dotted pairs of the form (A . B)) in a list of pairs compared according to the cdr of each element"
  (if lst
      (let ((maxrest (if (null (cdr lst))
                         (car lst)
                         (maxpair (cdr lst)))))
        (if (> (cdar lst) (cdr maxrest))
            (car lst)
            maxrest))))

;; a farily roundabout alternative definition would use OR instead of checking for (null (cdr lst))

(defun maxpair (lst)
  (if lst
    (let ((maxrest (maxpair (cdr lst))))
     (if (> (cdar lst) (or (cdr maxrest) (- (cdar lst) 1))); the OR clause avoids the error of comparing NIL with a number
       (car lst)
       maxrest))))

; now the main procedure
(defun kati (funcs n)
  (car
    (maxpair
      (pairvals funcs n))))


;;
;; Question
;;
;; Write LAMBDA expressions that

;; (i) returns the greatest of two integers.

(lambda (m n) (if (> m n) m n))

;; (ii) given two integers,  returns T if one or the other divides the other without remainder.

(lambda (m n) (or (zerop (rem m n)) (zerop (rem n m))))

;; (iii) given a list of integers, returns the mean.

(lambda (xs) (/ (apply #'+ xs) (length lst)))

;; (iv) given a list of integers, returns the sum of their factorials -- use your factorial solution.

(defun factorial (n &optional (product 1))
  (if (<= n 0)
    product
    (factorial (- n 1) (* product n))))

(lambda (xs) (apply #'+ (mapcar #'factorial xs)))


;;
;; Question
;;
;; Define a procedure PAIR-PROD using MAPCAR and LAMBDA, which takes a list of
;; two element lists of integers and returns a list of products of these pairs.
;; E.g.  an input like ((7 8) (1 13) (4 1)) should yield (56 13 4)
;;

(defun pair-prod (xs)
  (mapcar
    #'(lambda (x) (* (car x) (cadr x)))
    xs))



;;
;; Question
;;
;;  Define a procedure that takes two lists as input and returns the list of
;;  their pairwise averages. Use only MAPCAR, LAMBDA and arithmetic operations
;;  in your definition.
;;

(defun p-avg (lstA lstB)
  "computes the pairwise average of lstA and lstB"
  (mapcar
    #'(lambda (x y) (float (/ (+ x y) 2)))
    lstA
    lstB))

;;
;; Question
;;
;; Define LENGTH using MAPCAR, LAMBDA, + and APPLY.
;;

(defun mlength (lst)
  (apply
    #'+
    (mapcar
      #'(lambda (x) 1)
      lst)))

;; This gets "The variable X is defined but never used." warning. If you don't like this, you can define your lambda as:

;; (lambda (x) (declare (ignore x)) 1)
;; or
;; (lambda (x) (and x 1))
;;
;; In any case, this warning is totally harmless.
;;


;;
;; Question
;;
;; Define your own REMOVE-IF.
;;

(defun rif (test xs &optional store)
  (if xs
    (if (funcall test (car xs))
      (rif
        test
        (cdr xs)
        store)
      (rif
        test
        (cdr xs)
        (cons (car xs) store)))
    (reverse store)))

;; another alternative is to use MAPCAN, which is similar to MAPCAR except that
;; it appends the results it gets for each element, rather than putting them
;; into a list. Therefore MAPCAN's procedure should output a list. Don't worry

(defun rif2 (test xs)
  (mapcan
    #'(lambda (x)
        (if (not (funcall test x)) (list x)))
    xs))


;;
;; Question
;;
;; Define a procedure that takes an integer n and gives a list of n random
;; single digit numbers. Use the built-in RANDOM, MAKE-LIST, MAPCAR and LAMBDA
;; in your solution. Check the definition of the builtins you are not
;; familiar with from reference books on the website or on the web.
;;

(defun n-rand (n)
  "return a list of n single digit random numbers"
  (mapcar
    #'(lambda (x) (random x))
    (make-list n :initial-element 10)))

;;
;; Question
;;
;; Define a procedure that takes two lists: a list N of numbers and a list P of
;; symbols with function bindings, i.e. symbols used to define some single
;; argument mathematical procedure with DEFUN. Your procedure should return a
;; list with the same size as N, whose elements are lists consisting of values
;; obtained by applying all the procedures in P to the corresponding element in
;; N. For example, if you provide your procedure with a list of symbols naming
;; square, absolute value and float functions, e.g. (sqr abs float), and the
;; list (1 -2 3), it should return: ((1 1 1.0) (4 2 -2.0) (9 3 3.0)) You are
;; NOT allowed to use any procedure (built-in or user-defined) other than #',
;; DEFUN, MAPCAR, LAMBDA and FUNCALL.
;;

(defun sqr (x) (* x x)) ; for testing

(defun foo (nlist flist)
  (mapcar
    #'(lambda (x)
        (mapcar
          #'(lambda (y)
              (funcall y x))
          flist))
    nlist))

;;
;; Question
;;
;; Define a procedure \Verb+APPLIER+ that takes a procedure PROC, an input
;; INPUT and a count CNT and gives the result of applying PROC to INPUT CNT
;; times. For instance, (APPLIER #'CDR '(1 2 3) 2) should give (3)
;;

(defun applier (proc input cnt)
  (if (zerop cnt)
    input
    (applier
      proc
      (funcall proc input)
      (- cnt 1))))


;;
;; Question
;;
;; Define a procedure REPLACE-IF, which takes three arguments: a list LST, an
;; item ITEM and a function TEST, and replaces every element of LST that passes
;; the TEST with ITEM. You may find using keyword arguments useful (see the
;; lecture notes). Make use of MAPCAR, LAMBDA and FUNCALL in your solution.
;;

;; shorter solution with MAPCAR and LAMBDA (what the question wants)

(defun replace-if (lst item test)
  (mapcar
    #'(lambda (x) (if (funcall test x) item x))
    lst))

;; longer solution

(defun replace-if (lst item test &optional store)
  (if lst
    (if (funcall test (car lst))
      (replace-if
        (cdr lst)
        item
        test
        (cons item store))
      (replace-if
        (cdr lst)
        item
        test
        (cons (car lst) store)))
    (reverse store)))

;;
;; Question
;;
;;
;; MAPCAR can work on any number of lists; you only need to be careful to
;; provide a function with the correct number of arguments. For instance
;;
;; (mapcar #'(lambda (x y) (+ x y)) '(1 2 3) '(4 5 6)) gives (5 7 9). Don't
;; worry if lists are not of equal length, MAPCAR goes as far as the shortest
;; list.
;;
;; Define procedures that use MAPCAR and LAMBDA and
;;
;; (i) zip two lists together -- (zip '(a b) '(1 2)) should give
;; ((A 1) (B 2))
;;

(defun zipper (lst1 lst2)
  (mapcar
    #'(lambda (x y) (list x y))
    lst1
    lst2))


;; (ii) take three lists: first two will be lists of integers, and the third is
;; a list of functions. Apply the corresponding function to corresponding
;; arguments.

(defun foo (lst1 lst2 procs)
  (mapcar
    #'(lambda (x y z) (funcall z x y))
    lst1
    lst2
    procs))

;;
;; Question
;;
;; Find the numbers in a given range that have the same Collatz length using
;; applicative programming techniques.
;;

;; first the procedures for computing Colaltz length

(defun collatz-sequence (n)
  (labels ((col-next (n)
                     (if (<= n 1)
                       1
                       (if (evenp n)
                         (/ n 2)
                         (+ (* 3 n) 1))))
           (col-seq (lst)
                    (if (= (car lst) 1)
                      (reverse lst)
                      (col-seq (cons (col-next (car lst))
                                     lst)))))
    (col-seq (list n))))

(defun collatz-length (n)
  (- (length (collatz-sequence n)) 1))

;; now you need some sort of table that stores the numbers according to their
;; Collatz length. For this, let cl_1, cl_2,...,cl_i denote specific Collatz
;; lengths, and n_i_1, n_i_2,...,n_i_j denote specific numbers that have
;; Collatz length i. Then one way to represent a table would be to have a list
;; with the following structure:
;;
;; ((cl_1 n_1_1 n_1_2 n_1_3 ...) (cl_2 n_2_1 n_2_2 n_2_3 ...) .  .  .)
;;
;; in other words a list whose elements are lists whose first element is a
;; Collatz length and the rest of its elements are numbers with that Collatz
;; length

;; you need a procedure to update the information in this table.

;; Given a pair (collatz_length number):
;;  -- add (collatz_length number) to the table, if there is no entry with the given collatz_length.
;;  -- append (number) to the list starting with collatz_length
;;
;; this is best done with hash tables (or assoc lists), but here we will do it wihtout these.
;;

(defun add-pair (pair table)
  (if table
    (if (= (car pair) (caar table))
      (cons
        (append (car table) (cdr pair))
        (cdr table))
      (cons
        (car table)
        (add-pair pair (cdr table))))
    (list pair)))

;; you need a sequence procedure

(defun seq (start end &optional acc)
  "give the integer sequence from start to end inclusive of both ends"
  (if (< end start)
    acc
    (seq
      start
      (- end 1)
      (cons end acc))))


;; now the final procedure

(defun collect-equal-cl (start end)
  (labels ((pair-cl (n)
                    "pair n with its collatz length in the form (cl-of-n n)"
                    (list (collatz-length n) n))
           (collect (lst table)
                    (if (endp lst)
                      table
                      (collect
                        (cdr lst)
                        (add-pair (pair-cl (car lst)) table)))))
    (collect (seq start end) nil)))

;;
;; Question
;;
;; Define a procedure NCONT that takes an element X, a list
;; LST and an integer N; and inserts the element to the end of
;; the list. Your procedure should make sure that the returned list is never
;; longer than N. Do NOT use LENGTH, you may use REVERSE
;;


(defun firstn (lst n &optional store)
  (if (or (endp lst) (zerop n))
    (reverse store)
    (firstn (cdr lst) (- n 1) (cons (car lst) store))))

(defun ncont (x lst n)
  (append (reverse (firstn (reverse lst) (- n 1))) (list x)))

;;
;; Question
;;
;; Define a procedure RNTH that takes a list and an integer and returns
;; the nth element from the back. E.g.\ (rnth '(1 2 3 4) 2) should
;; return 3. Do NOT use NTH, LENGTH, REVERSE; you
;; can use NCONT of the previous question.
;;

; long way
(defun rnth (lst n &optional store)
  (if (endp lst)
    (car store)
    (rnth (cdr lst) n (ncont (car lst) store n))))

; short way

(defun rnth2 (lst n)
  (car (ncont 'x lst (+ n 1))))


;;
;; Question
;;
;; Define a procedure APPEND2 that appends two lists.
;;
;; Use iterative constructs
;;

(defun append2 (lstA lstB)
  (dolist (x (reverse lstA) lstB)
    (push x lstB)))

;;
;; Question
;;
;; Define an iterative procedure UNIQ that takes a list and removes all the
;; repeated elements in the list keeping only the first occurrence. This is the
;; expected behavior:
;;
;; * (uniq '(a b r a c a d a b r a))
;; (A B R C D)
;;
;; Use iterative constructs
;;

(defun uniq (lst)
  (let ((seen nil))
	(dolist (x lst (reverse seen))
	  (if (not (member x seen))
		(push x seen)))))


;;
;; Question
;;
;; The mean of $n$ numbers is computed by dividing their sum by $n$. A running
;; mean is a mean that gets updated as we encounter more numbers. Observe the
;; following input-output sequences:
;;
;; * (run-mean '(3 5 7 9))
;; (3 4 5 6)

;; The first element 3 is the mean of the list (3), the second element 4 is the
;; mean of (3 5), and so on. Implement RUN-MEAN by using DOTIMES and NTH.
;;
;;
;; Using iterative constructs
;;

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

;; without using iterative constructs

(defun run-mean (lst &optional (acc nil) (sum 0) (ctr 0))
  (if lst
      (run-mean (cdr lst)
                (cons (/ (+ sum (car lst)) (+ 1 ctr)) acc)
                (+ sum (car lst)) (+ 1 ctr))
      (reverse acc)))

;;
;; Question
;;
;; Define a procedure SEARCH-POS that takes a list as search item, another list
;; as a search list and returns the list of positions that the search item is
;; found in the search list. As usual, positioning starts with 0. Use DOTIMES.
;; A sample interaction:
;;
;; * (search-pos '(a b) '(a b c d a b a b))
;; (6 4 0)
;;
;; * (search-pos '(a a) '(a a a a b a b))
;; (2 1 0)
;;
;;
;; Using iterative constructs
;;

(defun search-pos (search-item search-list)
  "return the list of positions search-item (a list) matches in search-list"
  (let ((win (make-list (length search-item)))
        (store nil))
    (dotimes (i (length search-list) store)
      (setf win (append
                  (cdr win)
                  (list (nth i search-list))))
      (if (equal win search-item)
          (push (- (+ i 1) (length win)) store)))))


;; without iteration

(defun check-overlap (elm lst)
  (if elm
      (and (equal (car elm) (car lst))  (check-overlap (cdr elm) (cdr lst)))
      T))

(defun search-pos (elm lst &optional (acc nil) (ctr 0))
  (if lst
      (search-pos elm (cdr lst) (if (check-overlap elm lst)
                                    (cons ctr acc)
                                    acc) (+ 1 ctr))
      acc))


;;
;; Question
;;
;; Define a procedure that reverses the elements in a list including its
;; sublists as well.
;;

(defun reverse0 (lst)
  (let ((result nil))
	(dolist (x lst result)
	  (push (if (listp x) (reverse0 x) x) result))))

;;
;; Question
;;
;; See the PAIRLISTS in lecture notes. Define a procedure that "pairs" an
;; arbitrary number of lists. Here is a sample interaction:
;;
;; * (pairlists '((a b) (= =) (1 2) (+ -) (3 9)))
;; ((A = 1 + 3) (B = 2 - 9))
;;
;;
;; Use iterative constructs
;;

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


; without using iteration

(defun pairlists (lst &optional p1 p2)
  (if lst
      (pairlists (cdr lst) (cons (caar lst) p1) (cons (cadar lst) p2))
      (cons (reverse p1) (cons (reverse p2) nil))))


;;
;; Question
;;
;; Define a procedure \Verb+MOST+ takes a list and a procedure argument, and
;; returns the element in the list that gives the highest score when provided
;; as an argument to the given procedure.
;;

(defun most (list &key (proc #'identity))
  (reduce
    #'(lambda (x y)
        (if (>= (cadr x) (cadr y))
            x
            y))
        (mapcar
            #'(lambda (x)
                (list x (funcall proc x)))
            list)))


;;
;; Question
;;
;; The built-in FIND-IF returns the first element in its second argument that
;; returns T for its first argument:
;;
;; * (find-if #'(lambda (x) (> x 3)) '(1 3 9 0 4)) 9 Define your own version of
;; FIND-IF, which returns the index together with the element. Remember
;; that indexing starts with 0. For instance your procedure should return (2 9)
;; for the above invocation, where 2 is the index of 9.

(defun findif (proc list &optional (index 0))
  (if list
      (if (funcall proc (car list))
          (list index (car list))
          (findif proc (cdr list) (+ index 1)))))


;;
;; Question
;;
;; Define a procedure SHUFFLE that takes a list and returns a random
;; permutation of the list. A random permutation of a list is one of all the
;; possible orderings of the elements of the list. You can follow any strategy
;; you like – recursive or iterative. You might find two built-ins especially
;; useful: RANDOM takes an integer and gives a random number from 0 to one less
;; than the given integer; NTH takes an integer and a list, returning the
;; element at the position of the given integer – remember that positions are
;; counted starting from 0.;
;;

;; one solution will be to randomly remove an item from a list and add it to an accumulator

(defun rem-pos (lst pos)
  "return the version of lst with the item at pos removed"
  (cond ((endp lst) nil)
        ((zerop pos) (cdr lst))
        (t (cons
             (car lst)
             (rem-pos
               (cdr lst)
               (- pos 1))))))

(defun shuffle (lst &optional acc)
  (if (endp lst)
      acc
      (let ((pos (random (length lst))))
        (shuffle (rem-pos lst pos) (cons (nth pos lst) acc)))))



;;
;; Question
;;
;; You can represent a table in lisp by making each row a list and collecting all the rows in a list. E.g. a table like,
;;
;; 9 4 8
;; 2 5 6
;; 7 3 1
;;
;; gets represented as:
;;
;; ((9 4 8) (2 5 6) (7 3 1))
;;

;; Define a procedure FOO that takes a table and a procedure, and returns a two
;; element list. The first element of the returned list will be the number of
;; the column which yields the highest value when the argument procedure is
;; applied, the second element of the returned list will be the result obtained
;; for this column. For instance, when the procedure is called with the above
;; table and a procedure that gives the average of a list of numbers, the
;; output should be (1 6), where 1 is the number of the column with the highest
;; average and 6 is the average of that column. Note that column counting
;; starts with 1.

;; first we need a procedure that collects the columns of a table into a list

(defun get-columns (table)
  (let ((columns nil))
    (dotimes (i (length (car table)) (reverse columns))
      (push (let ((acc nil))
              (dolist (row table (reverse acc))
                (push
                  (nth i row)
                  acc )))
            columns))))

;; then a procedure that computes the values obtained by applying an argument procedure to lists

(defun bar (proc lst)
  (mapcar
    #'(lambda (x) (apply proc x))
    lst))

;; then a procedure that indexes a list starting from 1

(defun index (lst &optional (index 1))
  (if lst
      (cons (list index (car lst))
            (index (cdr lst) (+ index 1)))))

;; now a max procedure that runs on indexed lists retruning the list of index and value

(defun indexed-max (indexed-lst)
  (reduce
    #'(lambda (x y) (if (> (cadr x) (cadr y)) x y))
    indexed-lst))

;; now the solution

(defun foo (table proc)
  (indexed-max
    (print (index
      (bar
        proc
        (get-columns table))))))

;;
;; Question
;;
;; Define a procedure \Verb+MATCHES+ that takes two lists, a pattern and a
;; text, and returns the count of the occurrences of the pattern in the text.
;; You need to be careful about overlapping matches. For instance, \Verb+(A C
;; A)+ has 3 occurrences in \Verb+(A C A C A T G C A C A T G C)+.

(defun singletonp (lst)
  (and (consp lst) (null (cdr lst))))

(defun slide-window (item window &optional (store (list item)))
  "inserts item to the front of the window removing the last element"
  (if window
      (if (singletonp window)
          (reverse store)
          (cons (car window) store))))

(defun matches ()
  (labels ((singletonp (lst)
             (and (consp lst) (null (cdr lst))))
           (slide-window (item window &optional (store (list item)))
             "inserts item to the front of the window removing the last element"
             (if window
                 (if (singletonp window)
                     (reverse store)
                     (cons (car window) store))))))
  (slide-window 'a (b c d)))

;;
;; Question
;;
;; Define a procedure named \Verb+SUMSEQ+ that takes a sequence in the above
;; format (E.g.\ \Verb+((1 1) (2 4) (3 8) (4 16))+) of any length and a
;; function, and returns the sum of the terms where the index and the term are
;; related by the provided function. For instance if your procedure takes the
;; example sequence and a function that checks whether its second argument is
;; the square of the index. The above example will yield 21 as result, as in
;; the first, second and the fourth terms the term is the square of its index.
;; Solve the problem using \Verb+REMOVE-IF+ and \Verb+REDUCE+.

(defun summ (seq proc)
  (cadr
    (reduce #'(lambda (x y)
              (list nil (+ (cadr x) (cadr y))))
          (remove-if #'(lambda (x)
                 (let ((index (car x)) (term (cadr x)))(not (= (funcall proc index) term))))
                     seq))))

;;
;; Question
;;
;; A growing difference sequence is a recursive sequence where each non-initial
;; term in the sequence is greater than the one before it by a difference that
;; steadily grows with the terms. For instance 1, 4, 8, 13, 26... is such a
;; sequence where the second term is obtained by adding 3 to the first, third
;; term is obtained by adding 4 to the second, fourth term is obtained by
;; adding 5 to the third, and so on. In tabular form:
;;
;; index term difference
;; 1       1      3
;; 2       4      4
;; 3       8      5
;; 4      13      6
;; 5      19      7
;; 6      26      8
;; 7      34      9
;;
;; Our sequences will always start with 1. How the difference starts and grows
;; may change from sequence to sequence. For instance the difference in the
;; following sequence starts with 2 and grows as the square of the previous
;; difference.

;; index  term  difference
;; 1        1        2
;; 2        3        4
;; 3        7        16
;; 4        23      256
;; 5        279     65536

;; Define a procedure \Verb+GDS+ that generates a growing difference sequence
;; where the length of the sequence, the initial value of the difference and
;; how difference grows will be given as parameters. An example output for the
;; first 7 terms in the first example above would be \Verb+((1 1) (2 4) (3 8)
;; (4 13) (5 19) (6 26) (7 34))+.

(defun gds (len diff next &optional (counter 1) (term 1) store)
  (if (zerop len)
      (reverse store)
      (gds
        (- len 1)
        (funcall next diff)
        next
        (+ counter 1)
        (+ term diff)
        (cons (list counter term) store))))



;; Define a procedure named 'get-column'that will take a dataframe like we have
;; stored in *dataframe*, a column name (as a quoted symbol) and returns the 
;; column as a list. For test.tsv, your procedure should behave as:
;; (get-column *dataframe* 'b) -> (2 5 8)

;; Your second task is to define a procedure named 'get-row' that will again 
;; take a dataframe, and, this time, a row index (starting from 0) and will
;; return that row. Again for test.tsv
;; (get-row *dataframe* 1) -> (4 5 6)

(defun col-name-id (lst cn &optional (cid 0))
  (if lst
      (if (equal (car lst) cn)
          cid
          (col-name-id (cdr lst) cn (+ cid 1)))))

(defun get-row (lst i)
  (if lst
      (if (equal i 0)
          (car lst)
          (get-row (cdr lst) (- i 1)))))

(defun get-col (lst cn)
  (let ((data-lst (cdr lst))
        (col-id (col-name-id (car lst) cn)))
    (mapcar #'(lambda (x) (get-row x col-id)) data-lst)))