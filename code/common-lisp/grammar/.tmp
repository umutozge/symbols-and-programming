(defparameter *sample-grammar* 
  '((s -> (np vp))
    (np -> (d n) john mary)
	(vp -> (v np))
	(n -> boy girl cat dog)
	(d -> the a )
	(v -> saw chased)
	))

(defparameter *sample-constree*
  '(s 
	 (np 
	   ((det the) 
		(n girl))) 
	 (vp 
	   ((v saw) 
		(np (det a) (n dog)))))
  )

(defvar *grammar* *sample-grammar*)
(defvar *constree* *sample-constree*)
(defvar *indent* "~T~T~T~T")

(defun print-constree (constree &optional (depth 0))
  (defun multiply-string (strng degree)
	(let ((result ""))
	  (dotimes (n degree result)
		(setf result (concatenate 'string result strng))
		)
	  )
	)
  (defun print-node (node depth)
	(format t (concatenate 'string "~%" (multiply-string *indent* depth) (string node)))
	)
  (cond ((null constree))
		((and (null (rest constree)) (atom (first constree)))
		  (print-node (string (first constree)) depth))
		((atom (first constree))
		  (print-node (string (first constree)) depth)
		  (print-constree (rest constree) (+ depth 1))
		  )
		(t 
		  (print-constree (first constree) depth)
		  (print-constree (rest constree) depth)
		  )
		)

  )

(defun random-expand (category grammar)
  "randomly expand category according to grammar"
  (let ((rule (assoc category grammar))
		(rhs nil))
	(cond ((null rule) `(lex ,category))
		  (t (setf rhs (rest (rest rule)))
			 (elt rhs (random (length rhs))); randomly pick one element from rhs
		  )
	)
	)
  )

(defun generate-constituent (category grammar)
  "generates a random constituent (fringe only) using grammar"
  (cond ((null category) nil)
		((atom category)
		 	(generate-constituent (random-expand category grammar) grammar))
		((eql (first category) 'lex) 
		 	(rest category))
		(t
		 	(append 
			  (generate-constituent (first category) grammar) 
			  (generate-constituent (rest category) grammar)))
	)
  )

(defun generate-constree (category grammar)
  "generates a random constituent tree using grammar"
  (cond ((null category))
		((atom category)
		 	(append 
			  (list category) 
			  (when category (generate-constree (random-expand category grammar) grammar))))
		((eql (first category) 'lex)
		 	nil)
		(t
		  (append
				  (list (generate-constree (first category) grammar))
				  (when (rest category) (generate-constree (rest category) grammar))
				  )
			  )
			)
	)
