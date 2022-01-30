
;;
;; Question
;;
;; Define a procedure \Verb+CHECK-F+ that takes a list \Verb+LST+ of three
;; element lists of numbers (triples) and a two place mathematical function
;; \Verb+FUNC+. The output of your procedure will be a list of \Verb+NIL+s and
;; \Verb+T+s in equal length with the input list. For a triple  \Verb+(X Y Z)+
;; in \Verb+LST+ if applying \Verb+FUNC+ to \Verb+X+ and \Verb+Y+ gives
;; \Verb+Z+, then there will be a \Verb+T+ in the output list, otherwise there
;; will be a \Verb+NIL+ in the output list.  


(defun f-check (&key triples function)
  (mapcar
    #'(lambda (triple)
        (let ((x (car triple))
              (y (cadr triple))
              (z (caddr triple)))
          (= (funcall function x y) z))
        )
    triples))

;;
;; Question
;;
;; Write a program that takes a sequence (i.e.\ a list) as input and returns
;; the list of its subsequences. A sequence is a subsequence of itself. 
;;



(defun list-subseqs (lst &optional store)
 (labels ((list-cdrs (lst &optional store)
            "return a list of lst and all its cdrs"
            (if lst
                (list-cdrs (cdr lst) (cons lst store) )
                (mapcar
                  #'reverse
                  store))))
   (if lst
       (list-subseqs
         (cdr lst)
         (append store (list-cdrs (reverse lst)))
         )
       store)))
