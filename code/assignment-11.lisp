;; Let us first obtain an association list for collatz lengths of integers 
;; in a given range 

;; for this we need a function to generate collatz sequences:

(defun collatz-generate (n)
  (if (= n 1) 
    '(1)
    (let ((new-value (if (evenp n)
                       (/ n 2)
                       (+ (* n 3) 1))))
      (cons n (collatz-generate new-value)))))

(defun collatz-length (n)
  (- (length (collatz-generate n)) 1))

;; we will need a range function to generate integers
;; in a given range -- so that we can map this to a collatz length
;; table simply by mapcar
;; In the ranger function below, we also correct the shortcoming in 
;; the lecture notes version.

(defun ranger (&key (start 0) (end 9) (step 1) (acc 'untouched))
  (cond ((>= start end) (cons start acc))
        ; we detect the initial call to adjust the end value
        ((equal acc 'untouched) (ranger :start start 
                                        :end (if (zerop (rem (- end start) step))
                                               end
                                               (- end (rem (- end start) step)))
                                        :step step
                                        :acc nil)) 
        (t (ranger :acc (cons end acc)
                   :start start
                   :end (- end step)
                   :step step))))

;; we are ready to define a function that generates a table of collatz lengths paired
;; with integers in a given range.

(defun collatz-length-table (&key (start 1) (end 100))
  (mapcar #'(lambda (x)
              (cons x (collatz-length x)))
          (ranger :start start :end end)))

;; Now the task is to collect all the numbers with the same 
;; collatz length into lists. It would be good to have the
;; collatz length as part of the structure as well, so that
;; we can inspect which collatz-length is shared by which 
;; numbers.
;; You need to decide on a representation;  we will pick
;; (<length> <number_1> ... <number_n>); in other words, 
;; we will generate a list of lists where the car of each member
;; will be the collatz length shared by the numbers in its cdr.

;; In doing all this, the builtin ASSOC and RASSOC functions will
;; not be helpful. They retrieve only the first occurrence of a
;; pair, whereas we need all the occurrences. Second, there is an 
;; efficiency issue. Once we check for some length, we will not 
;; need to look at any pair with that length anymore. Therefore, we
;; would gain in efficiency if we find a way to get rid of pairs at 
;; the point we are sure we will no longer need them.

;; To achieve all this we will need to write a custom assoc function
;; We want it to return (i) all the pairs we are interested in; and 
;; (ii) what happens to the original assoc list after these pairs are removed.
;; We will write a function that separates the pairs we are interested in
;; from the pairs that should remain after we take out those pairs.
;; For our present purposes we will need to check for cdr's of the pairs,
;; something similar to what rassoc does. Let us parametrize where to look
;; in searching for the pairs, we will call it test -- it requires a two place
;; function, first argument will be the key we are searching, and the second 
;; argument will be the pair coming from the assoc list

(defun divide-list (key lst &key 
                              matches ; collect here the matching pairs
                              remains ; collect here the non-matching pairs
                              (test #'(lambda (x y)
                                                 (equal x (car y)))))
  (cond ((endp lst) (cons matches (list remains))) ; return a two element list, first elm is matches, second is remains
        (t (if (funcall test key (car lst))
             (divide-list key (cdr lst)
                                :matches (cons (car lst) matches)
                                :remains remains
                                :test test)
             (divide-list key (cdr lst)
                                :matches matches
                                :remains (cons (car lst) remains)
                                :test test)))))

;;; once we fetch all the matching pairs, we need a function to
;;; obtain a list starting with the collatz length and followed
;;; by all the integers with that collatz length. Let's call this
;;; function ORGANIZE

(defun organize (alist)
	(cons 
	  (cdar alist)
	  (mapcar #'car alist)))

;;; Finally we can collect all our solutions in a single function
;;; It takes a collatz length table as input

(defun compute-equivalence (coll-table &optional acc)
	(if (equal coll-table  nil)
	  acc
	  (let ((division (divide-list (cdar coll-table) coll-table :test #'(lambda (x y) (equal x (cdr y))))))
		(compute-equivalence 
		  (cadr division)
		  (cons (organize (car division)) acc)))))

;;; EOF
