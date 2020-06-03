(defun collatz-next (n)
  (if (evenp n)
    (/ n 2)
    (+ (* 3 n) 1)))

(defun collatz-gen (n)
  (if (= 1 n)
    (list n)
    (cons n (collatz-gen (collatz-next n)))))

(defun collatz-length (n)
  (- (length (collatz-gen n)) 1))

(defun seq (n &optional acc)
  (if (zerop n)
    acc
    (seq (- n 1) (cons n acc))))

(defun maxx (lst &key (hook #'(lambda (x) x)))
  (reduce
    #'(lambda (x y)
        (if (> (funcall hook x) (funcall hook y)) x y))
    lst))


(defun foo ()
  (mapcar
    #'(lambda (n)
        (maxx
          (mapcar
            #'collatz-length
            (seq (expt 10 n)))))
    '(1 2 3 4 5 6)))

(defun report-cl (lst)
  (mapcar
    #'(lambda (x)
        (list x (collatz-length x)))
    lst))

(defun int-w-max-cl (range)
  (maxx
    (report-cl (seq range))
    :hook #'cadr))

(defun bar ()
  (mapc #'(lambda (x)
              (let ((result (int-w-max-cl x)))
                (format t "~%In the range of ~A, ~A has the maximum collatz length of ~A" x (car result) (cadr result))))
          (mapcar
            #'(lambda (x)
                (expt 10 x))
            '(1 2 3 4 5 6))))
