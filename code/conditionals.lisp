(defun emptyp (lst)  
  (if (null lst)
	'empty
	'not-empty
	)
  )

(defun emptyp (lst)
  (if (endp lst)
	'empty
	'not-empty
	)
  )

(defun emptyp (lst)
  (if lst
	'not-empty
	'empty
	)
  )

(defun replace-if-odd (number list)
  (if (oddp number)
	(setf (car list) number)
	)
  )

(defun replace-if-odd (number list)
  (if (oddp number)
	(cons number (cdr list))
	list
	)
  )

(defun set-add-number (n numbers)
  (cond ((not (numberp n)) numbers)
		((member n numbers) numbers)
		(t (cons n numbers))
		)
  )

(defun set-add-number (n numbers)
  (if (not (numberp n))
	numbers
	(if (member n numbers) 
	  numbers
	  (cons n numbers)
	  )
	)
  )

(defun custom-if (test succ fail) ; wrong!
  (or (and test succ) fail)
  )

(defun custom-if (test succ fail)
  (and (or test fail) (or (and test succ) (and succ fail)))
  )
