;;
;; COGS 502 - Symbols and Programming - Fall 2018
;;
;; Sample solutions to Assignment 11 
;;


;; Q1

(defun split (lst &optional first-half)
  (if (>= (length first-half) (length lst))
    (list first-half lst)
    (split (cdr lst) (append first-half (list (car lst))))))

;; Q2

(defun split (lst &optional first-half (track lst))  
  (if (endp track)
    (list first-half lst)
    (split (cdr lst) (append first-half (list (car lst))) (cddr track))))

;; Q3

;; A list of pairs is called an association list; for each pair, the first element is called the "key"
;; and the second element is called the "value". There are some built-in functions to work with association lists
;; but for this exercise you need to use your own methods.  

;; The count records will be association lists, where pairs are kept in the order of their appearance in the input list.
;; You need a function to update the records you've been keeping as you see new elements in the input.

(defun update (item records)
  (if (endp records)
    (list (list item 1))
    (if (equal item (caar records))
      (cons 
        (list
          item
          (+ (cadar records) 1))
        (cdr records))
      (cons
        (car records)
        (update item (cdr records))))))

(defun summarize (lst &optional records)
  (if (endp lst)
    records
    (summarize (cdr lst) (update (car lst) records))))


;; Q4

(defun bubble (lst)
  (if (endp (cdr lst))
    lst
    (let ((a (car lst))
          (b (cadr lst)))
      (if (> a b)
        (cons b
              (bubble (cons a (cddr lst))))
        (cons a (bubble (cdr lst)))))))

(defun bubble-sort (lst &optional compare)
  (if (equal lst compare)
    lst
    (bubble-sort (bubble lst) lst)))

;; Q5

;;; Solution 1: 

;; First define an insertion function:

(defun insert (item lst &optional prefix store)
  "return a list of all the possible insertions of the item in lst
  (insert 'x '(p q)) ==> ((x p q) (p x q) (p q x))"
  (if (endp lst)
    (cons (append prefix (list item)) store)
    (insert
      item
      (cdr lst)
      (append prefix (list (car lst)))
      (cons (append prefix (cons item lst)) store))))

;; Now you can solve the problem by recursively inserting the current element to all the permutations of the cdr of the lst

(defun permute1 (lst)
  (cond ((endp lst) nil)
        ((endp (cdr lst)) (list lst))
        (t 
          (let (result)
            (dolist (i (permute1 (cdr lst)) result)
              (setf result (append result (insert (car lst) i))))))))

;; Solution 2:

;; A tail recursive solution that is more efficient than Solution 1:

(defun permute2 (lst &optional store)
  (cond
    ; you keep permutations in store 
    ; stop recursion if you discover
    ; that the first -- or any -- permutation
    ; in store has an equal length to lst
    ((= (length (car store)) (length lst))
         store)
    ; If the store is empty than this means you
    ; are recursing for the first time, put each
    ; element of lst to store in its own list, and
    ; call permute2 with this new store. These
    ; will be expanded to full permutations as you go along
    ((endp store)
     (permute2
            lst
            (let (result)
              (dolist (i lst result)
                (push (list i) result)))))
    ; this is the normal recursion step; for each
    ; list in store, expand it with an element from lst
    ; which is not already in that list, and call permute2 with
    ; this updated store
    (t
      (permute2
        lst
        (let (result)
          (dolist (i lst result)
            (dolist (j store)
              (if (not (member i j))
                (push (cons i j) result)))))))))


;; Solution 3:

;; A full iterative solution will be an exercise in the next assignment.

