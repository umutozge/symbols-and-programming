(defun occurs-count (thing seq &optional (counter 0))
  "recursively count occurrences of thing in seq"
  (cond ((null seq) counter)
		((equal thing (first seq)) (occurs-count thing (rest seq) (+ counter 1)))
		((listp (first seq)) (occurs-count thing (rest seq) (+ counter (occurs-count thing (first seq)))))
		(t (occurs-count thing (rest seq) counter))
		)
  )
