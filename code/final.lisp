;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions for Final Exam
;;

;; Q1

; version 1

(defun flatten (lst)
  (if lst 
    (if (consp (car lst))
      (append
        (flatten (car lst))
        (flatten (cdr lst)))
      (cons (car lst)
        (flatten (cdr lst))))))

; version 2

(defun flatten (lst &optional store)
  (if (endp lst)
    store
    (if (consp (car lst))
      (flatten (cdr lst) (flatten (car lst) store))
      (flatten (cdr lst) (append store (list (car lst)))))))

;; Q2

(defun mirror (tree) 
  (if tree 
    (list (car tree)  (mirror (caddr tree)) (mirror (cadr tree)))))

;; Q3

(defun rem-pos (pos lst)
  "remove the item at index pos from the list lst"
  (cond ((endp lst) nil)
        ((zerop pos) (cdr lst))
        (t (cons (car lst) (rem-pos (- pos 1) (cdr lst))))))


(defun shuffle (lst &optional store)
  (if (endp lst)
    store
    (let ((index (random (length lst))))
      (shuffle (rem-pos index lst) (cons (nth index lst) store)))))

;; Q4

(defun convert-notation (form)
  "change the notation from infix to prefix or prefix to infix"
  (if (atom form)
        form
    (list (convert-notation (cadr form))
          (convert-notation (car form))
          (convert-notation (caddr form)))))

;; Q5

; the simplest way would be the following -- which is almost a direct translation to LISP of the definition given in the question:

(defun collect (bt)
  (if bt 
    (append (collect (cadr bt))
            (cons (car bt) (collect (caddr bt))))))

; this is the implementation of the traversal method: 

(defun traverse (agenda &optional store)
  (if (endp agenda)
    store
    (let ((current (car agenda)))
      (cond ((null current)
             (traverse (cdr agenda) store))
            ((equal (car current) 'seen)
             (traverse (cdr agenda) (cons (cadr current) store)))
            (t
              (traverse
                (cons (cadr current)
                      (cons (list 'seen (car current))
                            (cons (caddr current) (cdr agenda))))
                store))))))

(defun collect (bt)
  (traverse (list bt)))

;; Q6

; iterative

(defun matches (search-item search-list)
  "return the times the search-item (a list) matches in search-list"
  (let ((win (make-list (length search-item)))
        (counter 0))
    (dotimes (i (length search-list) counter)
      (setf win (append
                  (cdr win)
                  (list (nth i search-list))))
      (if (equal win search-item)
        (incf counter)))))

; recursive

; with two functions -- handles empty pattern correctly

(defun prefix-p (pattern text)
  (if (null pattern) 
    t
    (if (equal (car pattern) (car text))
      (prefix-p (cdr pattern) (cdr text)))))

(defun matches (pattern text)
  (if (endp text)
    0
    (+ 
      (matches pattern (cdr text))
      (if (prefix-p pattern text) 1 0))))

; with a single function -- cannot handle empty pattern correctly

(defun matches (pattern lst &optional (backup pattern) inmatch)
  (cond ((endp pattern) 1) 
        ((endp lst) 0)
        ((equal (car pattern) (car lst))
         (if inmatch
           (matches (cdr pattern) (cdr lst) backup t)
           (+ (matches (cdr pattern) (cdr lst) backup t)
              (matches backup (cdr lst) backup nil))))
        (t (if inmatch
             0
             (matches backup (cdr lst) backup nil)))))
