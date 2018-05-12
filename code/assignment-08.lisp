(defun blank-n (n lst &optional (counter 1))
  (cond ((endp lst) nil)
        ((= counter n) (cons 'x (blank-n n (cdr lst) 1)))
        (t (cons (car lst) (blank-n n (cdr lst) (+ counter 1))))))

(defun c-find (x lst &optional (backup 0) (guess 0))
  "Find the position of the last occurrence of x in lst"
  (cond ((endp lst) guess)
        ((equal x (car lst)) (c-find x (cdr lst) (+ 1 backup) (+ 1 backup)))
        (t (c-find x (cdr lst) (+ 1 backup) guess))))

; a naive method
(defun nsub-list (x y)
  (cond 
    ; admit failure if y gets shorter than x
    ((< (length y) (length x)) nil)
    ; empty list is a nsub-list of every list 
    ; so succeed if you manage to reduce x to nil
    ; or find it as such from the start.
    ; also note that the cond element only has a test -- no further elements
    ; if you do not provide anything after the test, T is returned if the test succeeds 
    ((endp x))
    ; if you have both lists starting with the same element
    ; go on with the cdrs of both x and y
    ((equal (car x) (car y)) 
     (nsub-list (cdr x) (cdr y)))
    ; if the first elements do not match, 
    ; try x with  the cdr of y 
    (t (nsub-list x (cdr y)))))



; a naive method
; keep a flag telling 
; whether you have already started matching lists
(defun sub-list (x y &optional (in-match nil))
  (cond 
    ((< (length y) (length x)) nil)
    ((endp x) in-match)
    ((equal (car x) (car y)) 
     (or (sub-list (cdr x) (cdr y) t)
         (sub-list x (cdr y)) nil))
    (t (if in-match
         nil
         (sub-list x (cdr y) nil)))))

