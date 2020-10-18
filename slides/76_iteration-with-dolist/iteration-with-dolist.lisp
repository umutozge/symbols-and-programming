
(defun fact (n &optional (acc 1))
  (if (<= n 0)
    acc
    (fact (- n 1) (print (* n acc)))))

(defun printlist (lst)
  (dolist (x lst 'done)
    (print x)))


(defun lengthlist (lst)
  (let ((counter 0))
    (dolist (x lst counter)
      (setf counter (+ counter 1)))))

(defun lenlist (lst)
  (let ((counter 0))
    (dolist (x lst counter)
      (incf counter))))

