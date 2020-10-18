
(defun seq (n &optional acc)
  (if (zerop n)
    acc
    (seq (- n 1) (cons (- n 1) acc))))
