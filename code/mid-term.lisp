;;; Question 2
;;;
;;;
;;;  k
;;;   \
;;;    \
;;; 	[*|*]--->[*|*]--->NIL
;;; 	|        |
;;; 	v        v
;;; 	A        B     j
;;;     ^        ^      \ 
;;;     |        |       \
;;;     [*|*]--->[*|*]--> [*|*]--->NIL
;;;    /                  |
;;;   /                   v
;;;  i                    C
;;;
;;;

;;; Question 4

(defun firstn (n lst &optional (acc nil))
  (cond ((zerop n) acc)
		((endp lst) nil)
		(t (firstn (- n 1) (cdr lst) (append acc (list (car lst)))))))

(defun c-nthcdr (n lst)
  (cond ((zerop n) lst)
		(t (c-nthcdr (- n 1) (cdr lst)))))


; with LENGTH 
(defun trim-last-l (n lst)
  (if (<= (length lst) n)
	nil
	(cons (car lst) (trim-last-l n (cdr lst)))))

; without LENGTH
(defun trim-last (n lst &optional  (counter 1) (backup nil) (guess nil))
  (cond ((endp lst) guess)
		((<= counter n) (trim-last 
						  n
						  (cdr lst)
						  (+ counter 1)
						  (append backup (list (car lst)))))
		(t (trim-last 
			 n 
			 (cdr lst)
			 counter
			 (append (cdr backup) (list (car lst)))
			 (append guess (list (car backup)))))))

;;; Question 5

(defun count-occurs (x y)
  (cond ((equal x y) 1)
		((listp y) (if (endp y)
					 0
					 (+ 
					   (count-occurs x (car y))
					   (count-occurs x (cdr y)))
					 ))
		(t 0)))
