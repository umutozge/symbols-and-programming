(defparameter *test* '(2 3 4 5))

(defun ranger (&key (start -1) (end 9) (step 1) (acc nil))
  (cond ((>= start end) (cons start acc))
        (t (ranger :acc (cons end acc)
                   :start start
                   :end (- end step)
                   :step step))))

(defun factorial (n &optional (acc 1))
  (if (zerop n)
    acc
    (factorial (- n 1) (* n acc))))

;;; Question 1

;; (lambda (x y) (if (> x y) x y)) 
;;
;; (lambda (x y) (or (zerop (rem x y)) (zerop (rem y x)))) 
;;
;; (lambda (lst) (float (/ (apply #'+ lst) (length lst))))
;;
;; (lambda (lst) (apply #'+ (mapcar #'factorial lst)))


;;; Question 2

;with length
(defun split (lst &optional first-half )
  (if (>= (length first-half) (length lst))
    (list first-half lst)
    (split (cdr lst) (append first-half (list (car lst))))))

;;; Question 3

;without length
(defun split (lst &optional first-half (track lst))  
  (if (endp track)
    (list first-half lst)
    (split (cdr lst) (append first-half (list (car lst))) (cddr track))))


;;; Question 4

(defun replace-if (&key test seq item)
  "replaces the elements in the seq that pass the test with item"
  (mapcar #'(lambda (x)
              (if (funcall test x)
                item
                x))
          seq))

;;; Question 5

(defun zip (lst1 lst2)
  (mapcar #'(lambda (x y) (list x y)) lst1 lst2))  

(defun trio (lst1 lst2 flst)
  (mapcar #'(lambda (x y z) (funcall z x y)) lst1 lst2 flst))


;;; Question 6

;; There are many ways to do this. Here is one way:

;; The zip we defined constructs pairs in a list. While this is fine for the 
;; first recursion, it will introduce unwanted bracketing in the later recursions
;; to avoid this we define a new function to use in all the recursive steps except
;; the first one, for which we will use zip.

(defun zip (lst1 lst2)
  (mapcar #'(lambda (x y) (list x y)) lst1 lst2))  

(defun distribute (lst1 lst2)
  (mapcar #'(lambda (x y) (append x (list y))) lst1 lst2))

(defun zipp (lst &optional (func #'zip))
  (if (endp (cdr lst))
    (car lst)
    (zipp 
      (cons 
        (funcall func (car lst) (cadr lst))
        (cddr lst))
      #'distribute)))


;;; EOF 
