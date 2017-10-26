;;
;; A Common Lisp Workbook
;;

;;
;;======================================================
;; QUESTION:
;;
;; Given two forms, count the number of times the first occurs in the second at any level
;;
;; SOLUTION:
;;

(defun count-occurs (x y)
  (cond ((equalp x y) 1)
	((atom y) 0)
	((endp y) 0)
	((+
	  (count-occurs x (car y))
	  (count-occurs x (cdr y))))
	)
  )

;;
;;======================================================
;; QUESTION:
;;
;; Recursively compute the factorial of a given number
;;
;; SOLUTION:
;;

(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)) )
      )
  )

; a tail recursive version

(defun factorial (n &optional (store 1))
  (if (= n 0)
      store
      (factorial (- n 1) (* n store))
      )
  )

;;
;;======================================================
;; QUESTION:
;;
;; Iteratively compute the factorial of a number.
;;
;; SOLUTION:
;;

;;; with dotimes

(defun factorial (n)
  (let ((result 1))
    (dotimes (i n result)
      (setf result (* result (+ i 1)))
    )
  )
  )

;;; with do

(defun factorial (n)
  (do* ((result 1 (* result i))
	(i 1 (+ i 1)))
       ((> i n) result)
    )
  )


;;
;;======================================================
;; QUESTION:
;;
;; Implement (choose n k) - number of distinct ways to select k items out of n
;; Req: do not use factorial function on n, using it on k is fine.
;;
;; SOLUTION:
;;

(defun choose (n k)
  (/
	(let ((result 1))
	  (dotimes (counter k result)
		(setf result (* result (- n counter)))
		)
	  )
	(* (factorial k))
	)
  )

;;
;;======================================================
;; QUESTION:
;;
;; Reverse a list recursively.
;;
;; SOLUTION:
;;




;;
;;======================================================
;; QUESTION:
;;
;; Reverse a list iteratively
;;
;; SOLUTION:
;;


;;
;;======================================================
;; QUESTION:
;;
;; Retrieve the nth element - counting from 1 - from a list with a recursive procedure, do not use NTHCDR.
;;
;; SOLUTION:
;;


(defun nth-item (n list)
  (if (= n 1)
      (first list)
      (nth-item (- n 1) (rest list))
      )
  )


;;
;;======================================================
;; QUESTION:
;;
;; Test if a list is a palindrome - do not use reverse
;;
;; SOLUTION:
;;



;;
;;======================================================
;; QUESTION:
;;
;; Filter all top-level occurrences of an item from a list -- use EQL for comparison.
;;
;; SOLUTION:
;;

(defun filter (item list)
  (cond ((endp list) nil)
	((eql item (car list)) (filter item (cdr list)))
	(t (cons (car list) (filter item (cdr list))))
	)
  )

;;
;;======================================================
;; QUESTION:
;;
;; Filter all occurrences of an item from a list -- use EQL for comparison.
;;
;; SOLUTION:
;;

(defun filter (item list)
  (cond ((endp list) nil)
	((eql item (car list)) (filter item (cdr list)))
	((atom (car list)) (cons
			    (car list)
			    (filter item (cdr list))))
	(t (cons
	     (filter item (car list))
	     (filter item (cdr list)))
	    )
	 )
  )


;;
;;======================================================
;; QUESTION:
;;
;; An affixation procedure that returns all the possible insertions of an item into a sequence.
;; Ex. given (a b) and x, returen ((x a b) (a x b) (a b x))
;;
;; SOLUTION:
;; 

(defun affix (item seq &key (prefix nil) (store nil))
  (if (endp seq)
	(cons (append prefix (list item)) store)
	(affix 
	  item 
	  (rest seq) 
	  :prefix (append prefix (list (car seq)))
	  :store (cons (append prefix (list item) seq) store)
	  )
	)
  )

;;
;;======================================================
;; QUESTION:
;;
;; Given a list of symbols, return a list of its permutations
;;
;; SOLUTION:
;; 

;;; inefficient
(defun permute (seq)
  (if (= (length seq) 1)
	(list seq)
	(let ((result nil))
	  (dolist (i (permute (rest seq)) result)
		(setf result (append result (affix (first seq) i)))
		)
	  )
	)
  )


;;; more efficient due to tail recursion 
(defun permute (seq &optional (store nil))
  (cond ((endp store)
		   (dolist (i seq)
			 (setf store (cons (list i) store))
		   )
		   (permute seq store)
		 )
		((= (length (first store)) (length seq)) store)
		(t
		  (let ((result nil))
			(permute seq
					 (dolist (i store result)
					   (dolist (j seq)
						 (unless (member j i)
						   (setf result (cons (cons j i) result)
								 )
						   )
						 )
					   )
					 )
			)
		  )
		)
  )

;;
;;======================================================
;; QUESTION:
;;
;; Print instructions to solve Tower of Hanoi given the names of three pegs and number of discs to be moved from first to third.
;;
;; SOLUTION:
;;

(defun tower-of-hanoi (n source target hedge)
  (labels
      ((move-disk (from to)
	 (format t "~%Move a disk from ~a to ~a" from to)))
    (if (= n 1)
	(move-disk source target)
n	(progn (tower-of-hanoi (- n 1) source hedge target)
	       (move-disk source target)
	       (tower-of-hanoi (- n 1) hedge target source)
	       )
      )
    )
  )

;;
;;======================================================
;; QUESTION:
;;
;;
;;
;; SOLUTION:
;;

;;
;;======================================================
;; QUESTION:
;;
;;
;;
;; SOLUTION:
;;

;;
;;======================================================
;; QUESTION:
;;
;;
;;
;; SOLUTION:
;;

;;
;;======================================================
;; QUESTION:
;;
;;
;;
;; SOLUTION:
;;

;;
;;======================================================
;; QUESTION:
;;
;;
;;
;; SOLUTION:
;;

;;
;;======================================================
;; QUESTION:
;;
;;
;;
;; SOLUTION:
;;
