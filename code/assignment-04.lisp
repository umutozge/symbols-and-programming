(defun insert-2nd (item lst)
  (cons (car lst) (cons item (cdr lst)))
  )

(defun replace-2nd (item lst)
  (cons (car lst) (cons item (cdr (cdr lst))))
  )

(defun swap (lst-of-two)
  (list (cadr lst-of-two) (car lst-of-two))
  )

(defun wrap-2 (lst)
  (list (list (car lst)) (list (cadr lst))))

(defun wrap-2 (lst)
  (cons (cons (car lst) nil) (cons (cons (cadr lst) nil) nil)))
;;; EOF
