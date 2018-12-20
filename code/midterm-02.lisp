;;;
;;; COGS 502 Symbols and Programming -- Fall 2018
;;; Sample solutions for Mid-term Exam 2
;;;

;; Q1


;; (a)

(defun count-atoms (lst)
  (cond ((null lst) 0) ;; note that endp wouldn't work here
		((atom lst) 1)
		(t (+ (count-atoms (car lst))
			  (count-atoms (cdr lst))))))

;; (b)

(defun count-atoms (lst &optional (counter 0))
  (cond ((null lst) counter) ;; note that endp wouldn't work here
		((atom lst) (+ counter 1))
		(t (count-atoms
			  (cdr lst) (count-atoms (car lst) counter)))))


;; Q2

(defun bring-to-front (item lst &optional store)
  (cond ((endp lst) store)
		((equal item (car lst)) 
		 (bring-to-front item (cdr lst) (cons item store)))
		(t
		  (bring-to-front item (cdr lst) (append store (list (car lst)))))))

;; Q3

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

;; Q4

(defun rem-pen (x lst &optional backup guess1 guess2)
  "remove the one before the last occurrence of x from lst"
  (cond ((endp lst)
		 (if guess2
		   guess2
		   backup))
		((equal x (car lst))
		 (rem-pen x (cdr lst)
				 (append backup (list x))
				 backup
				 (append guess1 (list x))))
		(t
		  (rem-pen x (cdr lst)
				  (append backup (list (car lst)))
				  (append guess1 (list (car lst)))
				  (append guess2 (list (car lst)))))))
  
  




