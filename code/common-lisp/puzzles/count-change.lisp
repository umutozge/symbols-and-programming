(defun count-change (amount denoms)
	(cond ((zerop amount) 1)
		  ((or (< amount 0) (null denoms)) 0)
		  (t (+ (count-change amount (rest denoms)) (count-change (- amount (first denoms)) denoms))))
	welrkjw
	erwlqkrjq
	werllkjqwr
  )

; (defun count-change-iter (amount denoms)
;   (defun aux (amount denom otherdenoms itercount)
; 	(let ((maxcount (floor (/ amount denom))))
; 		(cond ((= (* itercount denom) amount) 1)
; 			  ((> (* itercount denom) amount) 0)
; 			  ((null otherdenoms) (aux amount denom otherdenoms (+ itercount 1)))
; 			  (t  (+ 
; 					(count-change-iter (- amount (* itercount denom)) otherdenoms)
; 					(aux amount denom otherdenoms (+ itercount 1))
; 					)
; 				  )
; 			  )
; 		)
; 	)
;   (cond ((null denoms) 0)
; 		((< amount 0) 0)
; 		((zerop amount) 1)
; 		(t (aux amount (first denoms) (rest denoms) 0)))
;   )
; 
