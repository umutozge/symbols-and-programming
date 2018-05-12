(defun convert-temp (temp unit)
  ;; you may choose how to interpret the unit parameter
  ;; here we take it to be the base of the conversion
  (if (equal 'c unit)
    (list temp 'c 'is (+ (* 1.8 temp) 32) 'f)
    (list temp 'f 'is (/ (- temp 32) 1.8) 'c)
    )
  )

(defun maxx (x y z)
  (if (>= x y)
    (if (>= z x) z x)
    (if (>= z y) z y)
    )
  )

(defun divisible-by (n m)
  (if (zerop (rem n m))
    (list n 'is 'divisible 'by m)
    (list n 'is 'not 'divisible 'by m)
    )
  )

(defun my-member (x lst)
  (if (member x lst)
    (list x 'is 'a 'member 'of lst)
    (list x 'is 'not 'a 'member 'of lst)
    )
  )

(defun order (x lst)
  (+ (- (length lst) (length (member x lst))) 1)
  )

(defun order2 (x lst)
  (if (null (member x lst))
    nil
    (+ (- (length lst) (length (member x lst))) 1)
    )
  )

(defun palindromep (lst)
  (equal lst (reverse lst))
  )

(defun dummy (x y)
  (if (endp y)
    nil
    (if (equal x (car y))
      y
      (dummy x (cdr y))
      )
    )
  )
;;; EOF
