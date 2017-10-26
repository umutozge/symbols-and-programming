#!/usr/bin/sbcl

(defparameter *size* 8)

(defvar *solutions* nil)

(defvar *transforms* nil)

(defun set-equalp (x y)
	(cond ((and (endp x) (endp y)) t)
		  ((or (endp x) (endp y)) nil)
		  (t (set-equalp (rest x) (remove (first x) y :test #'equal))))	
  )

(defun print-board (queens)
  (let ((board (make-array (list *size* *size*) :initial-element '-)))
	(dolist (q queens)
	 	(setf (aref board (first q) (second q)) 'x) 
	  )
	(print board)
	)
 )


(defun transform (queens mode)
  "transforms a chess board; modes: (rotate) left, (rotate) right, (flip) vertical, (flip) horizontal"
  (let* ((result nil)
		(dim (- *size* 1))
		(function-table 
		  (list 
			(list 'right #'(lambda (x y) (list y (- dim x))))
			(list 'left #'(lambda (x y) (list (- dim y) x)))
			(list 'vertical #'(lambda (x y) (list (- dim x) y)))
			(list 'horizontal #'(lambda (x y) (list x (- dim y) )))))
		)	
		  (dolist (q queens result)
			(setf result (cons 
						   (apply (second (assoc mode function-table)) q)
						   result)))))


(defun transform-closure (queens)
	(let ((result (list queens)))
	  (dolist (i (list 'left 'right 'vertical 'horizontal) result)
		(setf result (cons (transform queens i) result))
	  )
	  )
  )


(defun consistentp (queen queens)
   (dolist (q queens t)
	(when (or
			(= (first q) (first queen))
			(= (second q) (second queen))
			(= (abs (- (first q) (first queen))) (abs (- (second q) (second queen))))
			)
	  (return nil)
	  )
	)
  )

(defun eight-queens (q-stack)
  (cond ((= (length q-stack) *size*)
		 (unless (member q-stack *transforms* :test #'set-equalp)
		   (setf *transforms* (append (transform-closure q-stack) *transforms*))
		   (setf *solutions* (cons q-stack *solutions*)))
		 )
		(t (dotimes (i *size*) 
			 (dotimes (j *size*)
			   (when (consistentp (list i j) q-stack)
				 (eight-queens (cons (list i j) q-stack))
				 )
			   )
			 )
		   )
  )
  )

(defun *main* ()
	(time 
	  (progn 
		(eight-queens nil)
		(dolist (i *solutions*)
		  (print-board i)
		  ))))

(sb-ext:save-lisp-and-die "8queens" :executable t :toplevel '*main*)
