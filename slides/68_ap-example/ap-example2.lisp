(defun collatz-next (n)    
  "assumes n is an integer > 1"
  (if (evenp n)
    (/ n 2)
    (+ (* 3 n) 1)))

(defun collatz-seq (n)
  (if (= 1 n)
    (list 1)
    (cons n (collatz-seq (collatz-next n)))))

(defun collatz-len (n)
  (- (length (collatz-seq n)) 1))

(defun seq (n &optional acc)
  (if (zerop n)
    acc
    (seq (- n 1) (cons n acc))))

(defun maxx (lst &optional (hook #'(lambda (x) x)))
  (reduce 
    #'(lambda (x y)
        (if (> (funcall hook x) (funcall hook y)) x y))
    lst))

(defun pair-cl (lst)
  (mapcar
    #'(lambda (x) (list x (collatz-len x)))
    lst))


(mapcar
  #'(lambda (range)
      (maxx (pair-cl (seq range)) #'cadr))
  (mapcar #'(lambda (x) (expt 10 x)) (seq 6)))
