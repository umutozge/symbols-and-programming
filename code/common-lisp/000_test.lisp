(defun testbin ()
  (let* ((inc 1)
		 (start 536870910)
		 (count1 start)
		 (count2 start)
		 )
  (loop 
	(print (list count1 count2))
	(cond ((not (eq count1 count2)) (return count1))
	      (t 
			(setf count1 (+ count1 inc)) 
			(setf count2 (+ count2 inc))
			)
	  )
	)
  )
  )



(defvar k '(a b c)) 
(defmacro enqueue (item queue)
  `(progn 
	 (if ,queue
	   (setf (rest (last ,queue)) (list ,item))
	   (setf ,queue (list ,item)))
	 ,queue)
  )


(defmacro dequeue (queue-name)
  `(when (symbolp ',queue-name)
	 (setf (first ,queue-name) (rest ,queue-name))
	 )
  )


(defun do-fact (n)
  (let ((result 1)) 
	  (if (zerop n)
		1
		(dotimes (k n result)
		  (setf result (* result (+ 1 k))))
		)
	  )
  )

(defun adder (c)
  #'(lambda (x) (+ x c)))


(defmacro while (test &rest body)
  `(loop (unless ,test (return nil)) 
		  ,@body
		  )
  )

(defmacro define (proclist &rest body)
  `(defun ,(first proclist) ,(rest proclist) ,@body)
  )

(defvar *k*)

(defun times (*k* x)
  (* *k* x))
