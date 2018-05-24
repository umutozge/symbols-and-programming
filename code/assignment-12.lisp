;;; Question 1

(defun defrag (lst &optional acc)
  (cond ((endp lst) acc)
		((equal '* (car lst)) (defrag (cdr lst) (cons (car lst) acc)))
		((equal '+ (car lst)) (defrag (cdr lst) (append acc (list (car lst)))))
		(t (defrag (cdr lst) acc))))

;;; Question 2

; with assoc lists

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


;;; Question 3

(defun success-count (trials &key (success 'h))
  (count success trials))

;; or 

(defun success-count (trials &key (success 'h))
  (count-if #'(lambda (x) (equal x success)) trials))


;;; Question 4

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

;;; Question 5

(defun make-experiment ()
  (toss-coin 100))

(defun make-exp-series (n &optional acc)
  (if (zerop n)
	acc
	(make-exp-series (- n 1) (cons (make-experiment) acc))))

;;; Question 6

;; Keep the table as an assoc list.
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

;;; Question 7

; (freq-table (mapcar #'success-count (make-exp-series 100000)))

;;; Question 8

; (sort (freq-table (mapcar #'success-count (make-exp-series 100000))) #'(lambda (x y) (< (car x) (car y))))

;;; EOF
