(defun mat-make (nrows ncols &optional (generate #'(lambda () (random 10))))
  "creates a random matrix with the given shape "
  (mat-print
    (mapcar
      #'(lambda (m)
          (mapcar
            #'(lambda (n)
                (funcall generate))
            (make-list ncols)))
      (make-list nrows))))

(defun mat-sprod (matrix scalar)
  (mapcar
    #'(lambda (lst)
        (mapcar
          #'(lambda (num)
              (* num scalar))
         lst ))
    matrix))


(defun mat-print (matrix)
  (or
    (format t "~{~{~3D~^ ~}~%~}~%" matrix)
    matrix))

(defun range (n)
  (reverse (_range n))
  )
(defun _range (n)
  (if (= n 0)
      nil
      (cons (- n 1) (_range (- n 1)))))

(defun mat-transpose (matrix)
  (mapcar
    #'(lambda (x)
        (mapcar
          #'(lambda (lst)
              (nth x lst)
              )
          matrix))
   (range (length (car matrix)))))

(defun mat-trans (matrix)
  (apply #'mapcar #'list matrix))

(defun unit-prod (left right)
 (apply
   #'+
   (mapcar
     #'*
     left
     right)))

(defun mat-prod (left right)
  (mapcar
    #'(lambda (x)
     (mapcar
       #'(lambda (y)
           (unit-prod x y))
       (mat-transpose right)))
    left))

;; Check wether the following variant works fine or not, and why?
(defun mat-prod-alt (left right)
  (mapcar
    #'(lambda (x)
     (mapcar
       #'unit-prod x
       (mat-transpose right)))
    left))
