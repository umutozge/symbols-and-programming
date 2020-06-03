
(defparameter *grades*
  '((e842222 86) (e850421 98) (e790059 79) (e170139 45)
    (e917272 0) (e989199 75) (e877076 96) (e511096 83)
    (e386463 91) (e337777 90) (e861067 0) (e801835 70)
    (e493198 85) (e352336 82) (e243952 91) (e595538 47)
    (e304901 0) (e548145 70)))

(defun letter-grade (num-grade)
  (cond ((> num-grade 89) 'AA)
        ((> num-grade 84) 'BA)
        ((> num-grade 79) 'BB)
        ((> num-grade 74) 'CB)
        ((> num-grade 69) 'CC)
        ((> num-grade 64) 'DC)
        ((> num-grade 59) 'DD)
        ((> num-grade 54) 'FD)
        (t 'FF)))

(defun insert-letter (grade-entry)
  (let ((letter (letter-grade (cadr grade-entry)))
        (student (car grade-entry)))
    (cons student (list letter))))

(defun list-letter-grades (grades-list)
  (if grades-list
    (cons
      (insert-letter (car grades-list))
      (list-letter-grades (cdr grades-list)))))

(mapcar
  #'(lambda (x)
      (cons (car x)
            (list (if (>= (cadr x) 60) 'passed 'failed))))
  *grades*)

(defun class-mean (gl); ((stuno numgrade) ...)
  (let ((grades (remove-if #'zerop (mapcar #'cadr gl))))
    (float
      (/
        (reduce #'+ grades)
        (length grades)))))

(defun random-tosses (n)
  (mapcar
    #'(lambda (x) (random x))
    (make-list n :initial-element 2)))
