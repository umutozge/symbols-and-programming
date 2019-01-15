;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions for Problem Set 
;;


;; Q1

(defun lengt (lst)
  (apply #'+ (mapcar #'(lambda (x) 1) lst)))

;; Q2

;; Q3

(defun rev (lst)
  (reduce #'(lambda (x y) (cons y x)) (cons nil lst)))

;; Q4

(defun pos+ (lst &optional (i 0))
  (if (endp lst)
    nil
    (cons (+ (car lst) i) (pos+ (cdr lst) (+ i 1)))))

; or 

(defun pos+ (lst &optional (i 0))
  (if lst
    (cons (+ (car lst) i) (pos+ (cdr lst) (+ i 1)))))

;; Q5

; most starighforward solution is the following, but does not work when
; the sym is nil

(defun prec (sym lst &optional acc)
  (cond ((endp lst) acc)
        ((equal (cadr lst) sym)
         (prec sym (cdr lst) (append acc (list (car lst)))))
        (t (prec sym (cdr lst) acc))))

; the following is an improvement but does not work for nil preceeding nil; 
; for instance gives (A) to the input (prec 'nil '(a nil nil)), whereas it
; should give (A NIL)

(defun prec (sym lst &optional prev)  
  (cond ((endp lst) nil)
        ((null prev) (prec sym (cdr lst) (car lst)))
        ((equal (car lst) sym)
         (cons prev (prec sym (cdr lst) (car lst))))
        (t (prec sym (cdr lst) (car lst)))))

; the following works for all,

(defun prec (sym lst &optional prev start)  
  (cond ((endp lst) nil)
        ((null start) (prec sym (cdr lst) (car lst) t))
        ((equal (car lst) sym)
         (cons prev (prec sym (cdr lst) (car lst) t)))
        (t (prec sym (cdr lst) (car lst) t))))

;; Q6

; assumes n is never negative

(defun fibo (n)
  (if (< n 2)
    n
    (+ (fibo (- n 1)) (fibo (- n 2)))))

; this is very inefficient, it keeps computing the same values again and again.
; here is a more efficient solution 
(defun fib (n &optional (last-two '(1 1)))
  (if (= n 2)
    (cadr last-two)
    (fib (- n 1) (cons 
                   (cadr last-two)
                   (list (apply #'+ last-two))))))

(defun fibo (n)
  (if (< n 2)
    n
    (fib n)))

;; Q7

;; Q8

(defun defrag (lst &optional acc)
  (cond ((endp lst) acc)
        ((equal '* (car lst)) (defrag (cdr lst) (cons (car lst) acc)))
        ((equal '+ (car lst)) (defrag (cdr lst) (append acc (list (car lst)))))
        (t (defrag (cdr lst) acc))))

;; Q9

(defun toss-coin (n)
  (if (zerop n)
    nil
    (cons (cdr (assoc (random 2) '((0 . t) (1 . h)))) (toss-coin (- n 1)))))

; with lambda

(defun toss-coin2 (n)
  (if (zerop n)
    nil
    (cons ((lambda (x)
             (if (zerop x) 'h 't))
           (random 2)) (toss-coin2 (- n 1)))))
;; Q10

(defun success-count (trials &key (success 'h))
  (count success trials))

;; or 

(defun success-count (trials &key (success 'h))
  (count-if #'(lambda (x) (equal x success)) trials))

;; Q11

(defun success-rate (trials &key (success 'h))
  (float (/ 
           (success-count trials :success success)
           (length trials))))

; or in a roundabout way:

(defun success-rate2 (trials &key (success 'h))
  "Compute the proportion of successes in a series of Bernoulli trials"
  (float (/
           (reduce #'+ (mapcar 
                         #'(lambda (x)
                             (if (equal x success) 1 0))
                         trials))
           (length trials))))

;; Q12

(defun make-experiment ()
  (toss-coin 100))

(defun make-exp-series (n &optional acc)
  (if (zerop n)
    acc
    (make-exp-series (- n 1) (cons (make-experiment) acc))))

;; Q13

;; Keep the table as an assoc list -- see notes and/or Graham.
;; First write an update function that
;; takes a key and  an assoc list and updates
;; the count corresponding to the key. If the
;; key does not exist in the list, adds the
;; value with a count of 1.

(defun update-table (key table)
  (let ((count (cdr (assoc key table)))) ; look up the count, nil if it doesn't exist
    (if (not count) 
      (cons (cons key 1) table)   ; if the value is missing insert it with a count 1
      (cons (cons key (+ 1 count)) ; otherwise insert a pair with an incremented count 
            (remove-if             ; but remove the older pair
              #'(lambda (x) (equal (car x) key))
              table)))))

;; now you can easily construct the table

(defun freq-table (lst &optional acc)
  (if (endp lst)
    acc
    (freq-table (cdr lst) (update-table (car lst) acc))))

;; Q14

(freq-table (mapcar #'success-count (make-exp-series 100000)))

;; Q15

(sort (freq-table (mapcar #'success-count (make-exp-series 100000))) #'(lambda (x y) (< (car x) (car y))))
