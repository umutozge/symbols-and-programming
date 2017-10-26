(defun fringe (tree &optional (mode 'head) (store ())) 
  "compute the fringe of a constituent tree"
  (case mode 
	(head 
	  (fringe (rest tree) 'arg store)
	  )
	(arg 
	  (cond 
		((null tree) store)
		((atom (first tree)) (fringe (rest tree) 'arg (cons (first tree) store)))
		(t (append 
			 (fringe (first tree) 'head ()) 
			 (fringe (rest tree) 'arg store)))
		)
	  )
	)
  )

(defun adjoin-to (adjunct host side)
  "chomsky-adjoin the adjunct to host on side"
  (let ((label (first host)))
	  (case side 
		(left (list label adjunct host))
		(right (list label host adjunct))
		)
		)
  )
