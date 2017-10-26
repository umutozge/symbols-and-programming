(defun moves (disks)
  (cond ((= 1 disks) 1)
		(t (+ 1 (* 2 (moves (- disks 1))))))
  )
