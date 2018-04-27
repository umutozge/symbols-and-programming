(defparameter *grades* 
  '(86 98 79 45 0 75 96 83 91 90 0 70 85 82 91 47 0 70))

(defparameter *letter-rules*
  '((89 AA) (84 BA) (79 BB) (74 CB) (69 CC) (64 DC) (59 DD) (54 FD) (0 FF) (-1 NA)))

(defun grades-total (lst) ; WRONG
  (+ lst))

(defun grades-total (lst)
  (apply #'+ lst))

(defun letter (grade)
  (cond ((> grade 89) 'AA)
		((> grade 84) 'BA)
		((> grade 79) 'BB)
		((> grade 74) 'CB)
		((> grade 69) 'CC)
		((> grade 64) 'DC)
		((> grade 59) 'DD)
		((> grade 54) 'FD)
		(t 'FF)))

(defun append-letter (grade)
  (list grade (letter grade)))

(defun class-mean (grades-list)
  (float (/
		   (reduce #'+ (remove-if #'zerop grades-list))
		   (length (remove-if #'zerop grades-list)))))

(defun class-mean (grades-list)
  (let ((real-grades (remove-if #'zerop grades-list))) 
	(float (/ (reduce #'+ real-grades) (length real-grades)))))


(defun collatz-generate (n)
  (if (= n 1) 
	'(1)
	(let ((new-value (if (evenp n)
					   (/ n 2)
					   (+ (* n 3) 1))))
	  (cons n (collatz-generate new-value)))))

(defun collatz-length (n)
  (- (length (collatz-generate n)) 1))


(defun ranger (&key (start -1) (end 9) (step 1) (acc nil))
  (cond ((>= start end) (cons start acc))
		(t (ranger :acc (cons end acc)
				   :start start
				   :end (- end step)
				   :step step))))

;;; end of file
