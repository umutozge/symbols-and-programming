
(defun randy (&key (size 5) (start 0) (end 10))
  (mapcar
    #'(lambda (x)
        (+ start (random x)))
    (make-list
      size
      :initial-element (- end start))))
