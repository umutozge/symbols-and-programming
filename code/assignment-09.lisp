;;; Question 1

(defun random-n1 (n)
  "return a list of n single digit random numbers"
  (if (zerop n)
    nil
    (cons (random 10) (random-n1 (- n 1)))))

;;; Question 2

; using adjoin 
(defun random-n2 (n &optional acc)
    (if (= (length acc) n) 
      acc
      (random-n2 n (adjoin (random 9) acc))))


; using member 
(defun random-n3 (n &optional acc)
  "return a list of n single digit random numbers without repetitions"
  (setf num (random 9))
  (cond ((zerop n) acc)
        ((member num acc) (random-n3 n acc))
        (t (random-n3 (- n 1) (cons num acc)))))

; local variables are usually established by let bindings rather than setf
(defun random-n4 (n &optional acc)
  "return a list of n single digit random numbers without repetitions"
  (let ((num (random 9)))
  (cond ((zerop n) acc)
        ((member num acc) (random-n4 n acc))
        (t (random-n4 (- n 1) (cons num acc))))))

; Here is another -- quite roundabout -- method that uses an auxiliary function bring-to-front

(defun bring-to-front (n lst &optional acc)
  "brings to front the nth element in a list -- first item is the 0th"
  (if (zerop n)
    (append (cons (car lst) acc) (cdr lst))
    (bring-to-front (- n 1) (cdr lst) (append acc (list (car lst))))))

(defun random-n5 (n &optional (bag '(0 1 2 3 4 5 6 7 9)))
  "return a list of n single digit random numbers without repetitions"
    (if (or (zerop n) (endp bag))
      nil
      (let ((btfed (bring-to-front (random (length bag)) bag)))
        (cons (car btfed) (random-n5 (- n 1) (cdr btfed))))))

;;; Question 3

(defun flatten (lst)
  "removes any nesting but the topmost in a given list"
  (cond ((endp lst) nil)
        ((atom (car lst)) (cons (car lst) (flatten (cdr lst))))
        (t (append (flatten (car lst)) (flatten (cdr lst))))))


;;; Question 5

(defun maxx (lst &optional guess)
  "find the largest number in a list of numbers"
  (cond ((endp lst) guess)
        ((null guess) (maxx (cdr lst) (car lst)))
        (t
          (if (> (car lst) guess)
            (maxx (cdr lst) (car lst))
            (maxx (cdr lst) guess)))))

;;; Question 6

(defun second-large (lst &optional first second)
  "find the second largest number in a list of numbers"
  (cond ((endp lst) second)
        ((null first) (second-large (cdr lst) (car lst) second))
        ((or (null second) (> (car lst) second)) (if (> (car lst) first)
                                                   (second-large (cdr lst) (car lst) first)
                                                   (second-large (cdr lst) first (car lst))))
        (t (second-large (cdr lst) first second))))

;;; Question 7
;; there are many ways to solve this problem; we will start by not caring about efficiency.

;; recursively find the maximum of the list and remove it while counting the removals.

(defun nthlarge (n lst)
  (cond ((endp lst) nil)
        ((= n 1) (maxx lst))
        (t (nthlarge 
             (- n 1)
             (remove (maxx lst) lst)))))

;;; EOF
