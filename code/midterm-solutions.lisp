;; Question 1

; (car (car (cdr (car (cdr '(a (b (x)) d))))))

; (car (car (cdr (car (cdr '(a (b (x d))))))))

; (car (cdr (cdr (car (cdr '(a (b (d) x c)))))))

; (car (car (cdr (car (cdr (car (car '(((a (b (x c) d)))))))))))

;; Question 2

; w/o accumulator
(defun range_aux (n)
  (if (zerop n)
      nil
      (cons (- n 1) (range_aux (- n 1)))))

(defun range (n)
  (reverse (range_aux n)))

; with accumulator

(defun range (n &optional store)
  (if (zerop n)
      store
      (range (- n 1) (cons (- n 1) store))))

;; Question 3

(defun zeros (lst &optional (prec 1) (count 0))
  (if (endp lst)
      count
      (if (and (zerop (car lst)) (zerop prec))
          (zeros (cdr lst) (car lst) (+ count 1))
          (zeros (cdr lst) (car lst) count))))

;; Question 4

(defun colnext (n)
  (if (evenp n)
      (/ n 2)
      (+ (* 3 n) 1)))

(defun colmax (n &optional (maxx 1))
  (if (= n 1)
      maxx
      (let ((next (colnext n)))
        (colmax next (if (> next maxx) next maxx)))))

; note that when we divide by 2 it is needless to compare the resulting value
; to the current maximum; eliminating this inefficiency would cost you to have
; a more complicated code:

(defun colmax (n &optional (maxx 1))
  (if (= n 1)
      maxx
      (if (evenp n)
          (colmax (/ n 2) maxx)
          (let ((next (+ (* 3 n) 1)))
            (colmax next (if (> next maxx) next maxx)))))) 
