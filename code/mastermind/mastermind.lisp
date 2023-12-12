;;
;; COGS 502 -- Symbols and Programming
;;
;; Example program: MasterMind
;;


;; a global variable to keep the trial counts
(defparameter *trial-count* 0)

;;

;; PARSE-NUM
;; An auxiliary procedure to turn numbers into list of digits 
;;

(defun _parse-num (word n store)
  (if (zerop n)
      store
      (_parse-num
        word
        (- n 1)
        (cons
          (digit-char-p (aref word (- n 1)))
          store))))

(defun parse-num (num)
  "turn the given number to a list of digits"
  (let ((word (write-to-string num)))
    (_parse-num word (length word) nil)))

;; END of PARSE-NUM


;;
;; Auxiliary procedures for some boolean operations
;;

(defun flip-bool (lst)
  "Flips the boolean values in lst"
  (mapcar
    #'(lambda (x) (not x))
    lst))

(defun filter-with (bool-lst lst)
  "Removes those elms in lst that correspond to NIL in bool-lst"
  (remove-if #'null
             (mapcar
               #'(lambda (x y) (if y x))
               lst
               bool-lst)))

;; END of procedures for boolean operations


;; 
;; Various I/O procedures
;;

(defun pick-a-number ()
  "Picks a random number between 1000 and 9999"
  (+ 1000 (random 9000)))

(defun give-up ()
  (format t "Come again!"))

(defun get-guess ()
  (format t "Please enter your guess (q to give up): ")
  (finish-output)
  (incf *trial-count*)
  (read))

(defun greet ()
  (format t "~%***Welcome to MasterMind***~%~%"))

(defun success ()
  (format t "Well done! You did it in ~D trial~A!" *trial-count* (if (= 1 *trial-count*) "" "s")))

(defun print-result (lst)
  "Turns the star and plus count pair in lst to actual stars and pluses"
  (let ((star-count (car lst))
        (plus-count (cadr lst)))
    (format t "~{ ~S~} ~{ ~S~} ~%" (make-list star-count :initial-element '*) (make-list plus-count :initial-element '+)) ))

;; END of I/O procedures


;;
;; Procdures for computing star and plus counts
;;

(defun remove-once (elm lst)
  "Removes the first occurrence, if exists, of x from lst"
  (cond ((endp lst) nil)
        ((equal (car lst) elm) (cdr lst))
        (t (cons (car lst) (remove-once elm (cdr lst))))))

(defun count-pluses (target guess &optional (initial-length (length target))) 
  "Computes the plus count"
  (if (endp guess)
    (- initial-length (length target))
    (count-pluses
      (remove-once (car guess) target)
      (cdr guess)
      initial-length)))

(defun mark-stars (target guess)
  "Constructs a boolean list where T stands for exact matches"
  (mapcar
    #'(lambda (x y) (if (= x y) t))
    target
    guess))

(defun compute-match (target guess)
  "Returns the star-count plus-count pair on the basis of target and guess"
  (let* ((stars (mark-stars target guess))
         (new-target (filter-with (flip-bool stars) target))
         (new-guess (filter-with (flip-bool stars) guess)))
    (list
      (length (remove-if #'null stars))
      (count-pluses new-target new-guess))))

;; END of procedures for counting stars and pluses


(defun proc-input (target guess)
  "Initial handler for user input"
  (cond 
    ((equalp guess 'q) (give-up))
    ((= target guess) (success))
    (t
      (print-result (compute-match (parse-num target) (parse-num guess)))
      (proc-input target (get-guess)))))

(defun start ()
  "Main procedure of MasterMind"
  (setf *trial-count* 0)
  (setf *random-state* (make-random-state t))
  (let ((target (pick-a-number)))
    (greet)
    (proc-input target (get-guess))
    T
    ))
