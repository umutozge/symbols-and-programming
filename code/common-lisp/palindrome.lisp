(defun palindrome? (a-list) ;takes empty string to be a palindrome
  (defun aux (a-list stack)
	(cond ((equal stack a-list) t)
		  ((equal stack (rest a-list)) t)
		  ((null a-list) nil) ;or compare lengths, if length is cheaper 
		  (t (aux (rest a-list) (cons (first a-list) stack)))
		  )
   )
  (aux a-list ())
  )
