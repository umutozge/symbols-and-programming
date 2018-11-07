;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 04
;;

;; Q1

(defun l-prod (n lst)
  (let ((store nil))
    (dolist (x lst (reverse store))
      (push (* x n) store))))

;; Q2

(defun pair-prod (list-of-pairs)
  (let ((store nil))
    (dolist (x list-of-pairs (reverse store))
      (push (* (car x) (cadr x)) store))))

;; Q3

; there is one drawback of the following solution, can you see it?

(defun nth2 (n lst)
  (let ((counter -1)
        (result nil))
    (dolist (x lst result)
      (incf counter)
      (if (= counter n)
        (setf result x)))))

;; Q4

(defun subs (old new lst)
  (let ((store nil))
    (dolist (x lst (reverse store))
      (if (equal x old)
        (push new store)
        (push x store)))))
        
;; Q5
    
(defun last-nth (n lst)
  "returns the nth element from the end"
  (let ((pos 0)
        (end (length lst))
        (result nil))
    (dolist (x lst result)
      (if (eql pos (- end n))
        (setf result x))
      (incf pos))))

;; Q6

(defun remove-n (elm n lst)
  (let ((store)
        (counter 0))
    (dolist (x lst (reverse store))
      (if (equal x elm)
        (progn 
          (incf counter)
          (if (not (zerop (rem counter n)))
            (push x store)))
        (push x store)))))
    
;; Q7

(defun multi-member (elm lst)
  (let ((count-elm 0))
    (dolist (x lst (if (> count-elm 1) t))
      (if (equal x elm)
        (incf count-elm)))))

;; Q8

(defun append2 (lst1 lst2)
  (let ((store lst2)
        (new-lst1 (reverse lst1)))
    (dolist (x new-lst1 store)
      (push x store))))

;; Q9


(defun largest (lst)
  "largest number in lst -- assumes all members are numbers"
  (let ((largest (car lst)))
    (dolist (x (cdr lst) largest)
      (if (> x largest)
        (setf largest x)))))

;; Q10


;; Be careful, your program should answer 3 to a list like (4 3 4)

(defun second-largest (lst)
  (let ((largest)
        (sec-largest)) 
    (dolist (x (cdr lst) sec-largest)
      (cond ((null largest) (setf largest x))
            ((null sec-largest) (if (> x largest)
                                  (progn
                                    (setf sec-largest largest)
                                    (setf largest x))
                                  (setf sec-largest x)))
            ((> x largest) (setf sec-largest largest)
                           (setf largest x))
            ((> x sec-largest) (setf sec-largest x))))))
