
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
