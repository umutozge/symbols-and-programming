;;;
;;; COGS 502 Symbols and Programming
;;; Sample solutions for Mid-term Exam 1
;;;

;; Q3

(defun isect (set1 set2)
  (let ((store nil))
	(dolist (x set1 (reverse store))
	  (if (member x set2)
		(setf store (cons x store))))))

;; Q4 

(defun nthcdrr (n lst)
  (let ((counter 1)
		(result nil))
	(dolist (x lst (reverse result))
	  (if (> counter n)
		(setf result (cons x result)))
	  (setf counter (+ counter 1)))))

(defun nthcdrr2 (n lst)
  (let ((counter 1)
		(result lst))
	(dolist (x lst result)
	  (if (<= counter n)
		(setf result (cdr result)))
	  (setf counter (+ counter 1)))))

;; Q5

(defun rotate-left (lst)
  (let ((elm (car lst))
		(take nil)
		(store nil))
	(dolist (x lst (append store (list elm)))
	  (if take 
		(setf store (append store (list x))))
	  (setf take t))))
