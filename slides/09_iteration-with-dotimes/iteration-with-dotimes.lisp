
(defun factorial (n)
  (let ((result 1))
    (dotimes (i n result)
      (setf result (* result (+ i 1))))))

(defun pairlists (lst1 lst2)
  (reverse 
    (let ((store nil))
      (dotimes (i (min (length lst1) (length lst2)) store)
        (push
          (list (nth i lst1) (nth i lst2))
          store)))))
