(defun factorial (n)
  (if (zerop n)
    1
    (* n (factorial (- n 1)))))


(load "utils.lisp")

