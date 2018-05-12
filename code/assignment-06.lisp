(defun c-last (lst)
  (cond ((endp lst) nil) 
        ((endp (cdr lst)) (car lst))
        (t (c-last (cdr lst)))))

(defun c-remove (item lst)
  (cond ((endp lst) nil)
        ((equal (car lst) item) (c-remove item (cdr lst)))
        (t (cons (car lst) (c-remove item (cdr lst))))))

(defun rem-first (item lst)
  (cond ((endp lst) nil)
        ((equal (car lst) item) (cdr lst))
        (t (cons (car lst) (c-remove item (cdr lst))))))

(defun chop-end (lst)
  (if (or (endp lst) (= 1 (length lst)))
    nil
    (cons (car lst) (chop-end (cdr lst)))))


(defun sum (lst)
  (cond ((endp lst) 0) ; only when initial input is NIL
        ((endp (cdr lst)) (car lst))
        (t (sum (cons (+ (car lst) (cadr lst)) (cddr lst))))) )

(defun palindrome (lst)
  (cond ((endp lst) t)
        ((equal (car lst) (car (last lst))) (palindrome (cdr (chop-end lst))))))

(defun count-length (lst counter)
  (if (endp lst)
    counter
    (count-length (cdr lst) (+ counter 1))))
