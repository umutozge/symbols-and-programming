; recursive 

(defun level (term lst)
  (cond
    ((endp lst) nil)
    ((equal term (car lst)) 0)
    ((listp (car lst))
     (let ((try (level term (car lst))))
       (if (null try)
           (level term (cdr lst))
           (+ 1 try))))
    (t (level term (cdr lst)))))

; tail-recursive 
; keep the level info and return it at the point you find the term

(defun level (term lst &optional (level 0))
  (cond 
    ; term not found
    ((endp lst) nil)
    ; see below for the meaning of UP 
    ((equal (car lst) 'UP) (level term (cdr lst) (- level 1)))
    ; term found, so return the current level
    ((equal (car lst) term) level)
    ; go on searching,
    (t
     (if (listp (car lst))
         ; plunge into a deeper level, putting a mark to get back to upper level 
         (level term (append (car lst) (cons 'UP (cdr lst))) (+ 1 level))
         ; of go on at the same level 
         (level term (cdr lst) level)))))
