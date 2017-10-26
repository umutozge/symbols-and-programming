(defun random-element (seq)
  (elt seq (random (length seq)))
  )

(defun x-product (args)
  (defun set-product (pivot seq store)
	(cond ((null seq) store)
		  (t (set-product 
			   pivot 
			   (rest seq) 
			   (cons (if (atom pivot) (list pivot (first seq)) (append pivot (list (first seq)))) store)))
		  )
  )
  (defun binary-product (left right store)
	  (cond ((null left) store)
			(t (binary-product 
				 (rest left)
				 right
				 (append (set-product (first left) right nil) store)))
			)
  )
  (defun aux (store args)
	(cond ((null args) store)
		  (t (aux (binary-product store (first args) nil) (rest args))))
	
	)
  (aux (first args) (rest args))
  )

(defun cartesian-product (lseq rseq)
  (let ((result nil))
	(dolist (n lseq (reverse result))
	  (dolist (j rseq)
		(push `(,n ,j) result)
		)
	  )
	)
  )
