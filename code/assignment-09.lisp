;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 09
;;

;; Q1

(defun d-how-many? (item lst &optional (count 0))
  (cond ((endp lst) count)
        ((equal (car lst) item) (d-how-many? item (cdr lst) (+ count 1)))
        ((atom (car lst)) (d-how-many? item (cdr lst) count))
        (t (d-how-many?
             item
             (cdr lst)
             (d-how-many? item (car lst) count)))))

;; Q2

(defun rem-second (item lst &optional seen-once)
  "Remove the second occurrence of item in lst"
  (cond ((endp lst) nil)
        ((equal (car lst) item) (if seen-once
                                  (cdr lst)
                                  (cons (car lst) (rem-second item (cdr lst) t))))
        (t (cons (car lst) (rem-second item (cdr lst) seen-once)))))


;; Q3

(defun rem-nth (item n lst)
    (cond ((endp lst) nil)
          ((equal item (car lst)) (if (= 1 n)
                                    (cdr lst)
                                    (cons (car lst) (rem-nth item (- n 1) (cdr lst)))))
          (t (cons (car lst) (rem-nth item n (cdr lst))))))

;; Q4

(defun multi-member (x lst &optional seen-before?)
  "Checks if its first argument occurs more than once in the second"
  (cond ((endp lst) nil)
        ((equal x (car lst)) (if seen-before?
                               t
                               (multi-member x (cdr lst) t)))
        (t (multi-member x (cdr lst) seen-before?))))



;; Q5

(defun random-n1 (n)
  "return a list of n single digit random numbers"
  (if (zerop n)
    nil
    (cons (random 10) (random-n1 (- n 1)))))

;; Q6

; using adjoin 
(defun random-n2 (n &optional acc)
  "return a list of n single digit random numbers without repetitions"
    (if (= (length acc) n) 
      acc
      (random-n2 n (adjoin (random 10) acc))))

; using member 
(defun random-n3 (n &optional acc)
  "return a list of n single digit random numbers without repetitions"
  (let ((num (random 10)))
    (cond ((zerop n) acc)
          ((member num acc) (random-n3 n acc))
          (t (random-n3 (- n 1) (cons num acc))))))


;; Q7

(defun flatten (lst)
  "removes any nesting but the topmost in a given list"
  (cond ((endp lst) nil)
        ((atom (car lst)) (cons (car lst) (flatten (cdr lst))))
        (t (append (flatten (car lst)) (flatten (cdr lst))))))


;; Q9


; first, recursion without an accumulator

(defun maxx (lst)
  "find the largest number in a list of numbers"
  (if (endp lst)
    nil
    (let ((next-maxx (maxx (cdr lst))))
      (if (or (null next-maxx) (> (car lst) next-maxx))
        (car lst)
        next-maxx))))

; now, with an accumulator

(defun maxx (lst &optional guess)
  "find the largest number in a list of numbers"
  (if (endp lst)
    guess
    (maxx
      (cdr lst)
      (if (or (null guess) (> (car lst) guess))
        (car lst)
        guess))))

;; Q10

(defun second-large (lst &optional first second)
  "find the second largest number in a list of numbers"
  (cond ((endp lst) second)
        ((null first) (second-large (cdr lst) (car lst) second))
        ((or (null second) (> (car lst) second)) (if (> (car lst) first)
                                                   (second-large (cdr lst) (car lst) first)
                                                   (second-large (cdr lst) first (car lst))))
        (t (second-large (cdr lst) first second))))

;; Q11

(defun rem-last (x lst &optional (backup nil) (guess nil))
  "Removes the last occurrence of its first argument from the second"
  (cond ((endp lst) guess)
        ((equal x (car lst)) (rem-last
                               x 
                               (cdr lst)
                               (append backup (list (car lst)))
                               backup))
        (t (rem-last
             x 
             (cdr lst)
             (append backup (list (car lst)))
             (append guess  (list (car lst)))))))


;; Q12
;; there are many ways to solve this problem; we will start by not caring about efficiency.

;; recursively find the maximum of the list and remove it while counting the removals.

(defun nthlarge (n lst)
  (cond ((endp lst) nil)
        ((= n 1) (maxx lst))
        (t (nthlarge 
             (- n 1)
             (remove (maxx lst) lst)))))

;; now a bare-hands solution:

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
