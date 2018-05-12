; with using MEMBER
(defun multi-member (x lst)
  "Checks if its first argument occurs more than once in the second"
  (cond ((endp lst) nil)
        ((equal x (car lst)) (member x (cdr lst)))
        (t (multi-member x (cdr lst))))) 

; without using MEMBER
(defun multi-member (x lst &optional (seen-before? nil))
  "Checks if its first argument occurs more than once in the second"
  (cond ((endp lst) nil)
        ((equal x (car lst)) (if seen-before?
                               t
                               (multi-member x (cdr lst) t)))
        (t (multi-member x (cdr lst) seen-before?))))

(defun rem-last (x lst &optional (backup nil) (guess nil))
  "Removes the last occurrence of its first argument from the second"
  (cond ((endp lst) guess)
        ((equal x (car lst)) (rem-last
                               x 
                               (cdr lst)
                               (append backup (list (car lst)))
                               backup))
        (t (rem-last
             x 
             (cdr lst)
             (append backup (list (car lst)))
             (append guess  (list (car lst)))))))

; here is the code for deep subs, shallow subs should be obvious looking at this
(defun subs (new old expr)
  "Substitutes new with old in expr"
  (cond ((and (listp expr) (endp expr)) nil)
        ((not (listp expr)) (if (equal old expr)
                              new
                              expr
                              ))
        (t (cons (subs new old (car expr)) (subs new old (cdr expr))))))
;;; EOF
