(defun factorial (n)
  (if (zerop n)
	1
	(* n (factorial (- n 1)))
	)
  )

(defun factorial (n)
  (cond ((or (not (integerp n)) (< n 0)) (error "Factorial undefined!"))
		((zerop n) 1)
		(t (* n (factorial (- n 1))))
		)
  )

(defun c-length (lst)
  (if (endp lst) 0 (+ 1 (c-length (cdr lst))))
  )

(defun c-member (item lst)
  (cond ((endp lst) nil)
		((equal item (car lst)) lst)
		(t (c-member item (cdr lst)))
		)
  )

(defun power (x y)
  (cond ((zerop y) 1)
		((= 1 y) x)
		(t (* x (power x (- y 1))))
		)
  )

(defun range (n)
  (if (zerop n)
	nil
	(append (range (- n 1)) (list (- n 1)))
	)
  )

(defun s-range (n)
  (cond ((or (not (integerp n)) (< n 0)) (error "Range undefined!"))
		((zerop n) nil)
		(t (append (range (- n 1)) (list (- n 1))))
		)
  )

(defun count-length (lst counter)
  (if (endp lst)
	counter
	(count-length (cdr lst) (+ counter 1))
	)
  )

(defun count-length-user (lst)
  (count-length lst 0)
  )

(defun c-length (lst &optional (counter 0))
  (if (endp lst)
	counter
	(c-length (cdr lst) (+ counter 1))
	)
  )

(defun f-range (n &optional (store nil))
	(if (zerop n)
	  store
	  (f-range (- n 1) (cons (- n 1) store))
	  )
	)


(defun c-reverse (lst)
  (if (endp lst)
	nil
	(append (c-reverse (cdr lst)) (list (car lst)))
	)
  )

(defun f-reverse (lst &optional (store nil))
  (if (endp lst)
	store
	(f-reverse (cdr lst) (cons (car lst) store))
	)
  )

(defun mapp (func lst)
  (if (endp lst)
	nil
	(cons (funcall func (car lst)) (mapp func (cdr lst)))
	)
  )
