
(defun factorial (n)
  (labels ((fact (n acc)
                 (if (<= n 0)
                   acc
                   (fact (- n 1) (* n acc)))))
    (fact n 1)))


(defun col-seq (n)
  (labels ((col-n (n)
                  (if (<= n 1)
                    1
                    (if (evenp n)
                      (/ n 2)
                      (+ (* 3 n) 1))))
           (c-seq (n acc)
                  (if (<= n 1)
                    (reverse (cons 1 acc))
                    (c-seq (col-n n) (cons n acc)))))
    (c-seq n nil)))
