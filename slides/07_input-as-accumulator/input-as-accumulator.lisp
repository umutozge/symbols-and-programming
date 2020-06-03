


; (defun c-seq (n &optional acc)
;   (if (<= n 1)
;     (reverse (cons n acc))
;     (c-seq (col-n n) (cons n acc))))



(defun col-seq (n)
  (labels ((c-next (n)
                   (if (<= n 1)
                     1
                     (if (evenp n)
                       (/ n 2)
                       (+ (* 3 n) 1))))
           (c-seq (lst)
                  (if (<= (car lst) 1)
                    (reverse lst)
                    (c-seq (cons
                             (c-next (car lst))
                             lst)))))
   (c-seq (list n))))
