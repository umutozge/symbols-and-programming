(defparameter spades  (format nil "~A" #.(code-char 9824)))
(defparameter hearts  (format nil "~A" #.(code-char 9829)))
(defparameter diamonds  (format nil "~A" #.(code-char 9830)))
(defparameter clubs  (format nil "~A" #.(code-char 9827)))

(defparameter suits (mapcar #'(lambda (x)
                                (format nil "~A" (code-char x)))
                            '(9824 9829 9830 9827)))


(defparameter deck (let ((store nil)
                         (types '("A" "K" "Q" "J" "10" "9" "8" "7" "6" "5" "4" "3" "2")))
                     (dolist (x suits store)
                       (dolist (y types)
                         (push (concatenate 'string x y) store)))))
