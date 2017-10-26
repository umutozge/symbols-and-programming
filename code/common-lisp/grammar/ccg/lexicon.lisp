(defmacro compose (f g)
  (let ((sym (gensym)))
	`(function (lambda (,sym) (funcall ,f (funcall ,g ,sym)) ))
	)
  )

(defmethod ccg-combine ((lcat cat) (rcat cat))
  (labels 
	((defun syn-domain (cat) 
	   (let ((syn (cat-syn cat))) 
		 (when (listp syn) (second syn))
		 )
	   )
	 (defun syn-range (cat)
	   (let ((syn (cat-syn cat)))
		 (if(listp syn)(third syn)syn))
	   )
	 (defun cat-equalp (lcat rcat)
	   (equalp lcat rcat)
	   )
	 (defun slash (cat)
	   (when (list (cat-syn cat)) (first (cat-syn cat)))
	   )
	 (defun ccg-apply (f a)
	   )
	 (defun ccg-compose (f g)
	   )
	 )
	(cond ((and (equal (syn-domain lcat) (cat-syn rcat)) (equal (slash lcat) 'fs)) 
			(make-instance 'cat 
						   :sem (funcall (cat-sem lcat) (cat-sem rcat)) 
						   :syn (syn-range lcat) 
						   :phon (list (cat-phon lcat) (cat-phon rcat)))
		   );forward apply
		  ((and (equal (cat-syn lcat) (syn-domain rcat)) (equal (slash rcat) 'bs))
			(make-instance 'cat 
						   :sem (funcall (cat-sem rcat) (cat-sem lcat)) 
						   :syn (syn-range rcat) 
						   :phon (list (cat-phon rcat) (cat-phon lcat)))
		   );backward apply
		  ((and (equal (syn-domain lcat) (syn-range rcat)) (equal (slash lcat) 'fs))
			(make-instance 'cat 
						   :sem (compose (cat-sem lcat) (cat-sem rcat)) 
						   :syn nil  
						   :phon (list (cat-phon lcat) (cat-phon rcat)))
		   );forward compose 
		  ((equal (syn-range rcat) (syn-domain rcat)) (ccg-compose rcat lcat));backward compose 
		  (t nil)
		  )
	
	)
  )

; (defun ccg-apply (lcat rcat &optional (direction 'forward))
;   (case direction
; 	(backward (ccg-apply rcat lcat))
; 	(forward
; 	 (when  (equal (syn-domain lcat) (cat-syn rcat))
; 			(make-instance 'cat :sem (funcall (cat-sem lcat) (cat-sem rcat)) :syn (syn-range lcat) :phon (list (cat-phon lcat) (cat-phon rcat)))
; 		   ))
; 	 )
;   )





;; Lexicon

(defclass cat ()
  ((phon :accessor cat-phon
		 :initform nil
		 :initarg :phon
		 )
  (syn   :accessor cat-syn
		 :initform nil
		 :initarg :syn
		 )
  (sem   :accessor cat-sem
		 :initform nil
		 :initarg :sem
		 )
  )
  )

(defmacro lex (lexid phon syn sem)
  	(list 'progn 
		  `(defvar ,lexid)
		  `(setf ,lexid (make-instance 'cat :phon ,phon :syn ,syn :sem ,sem)))
  )

(lex l-john		'john 'np 'john1)
(lex l-walks	'walks '(bs np s) #'(lambda (x) `(walks1 ,x)))
(lex l-the  	'the '(fs n np) #'(lambda (x) `(the1 ,x)))
(lex l-dog		'dog 'n 'dog1)



; (defstruct cat 
;   (phon nil)
;   (syn nil)
;   (sem nil))
; 
; (defvar l-the)
; (setf l-the (make-cat))
; (setf (cat-phon l-the) 'the)
; (setf (cat-syn l-the) '(fs n np))
; (setf (cat-sem l-the) #'(lambda (x) `(the1 ,x)))
; 
; (defvar l-dog)
; (setf l-dog (make-cat))
; (setf (cat-phon l-dog) 'dog)
; (setf (cat-syn l-dog) 'n)
; (setf (cat-sem l-dog) 'dog1)
; 
; (defmacro lexical (lex phon syn sem)
;   		  (list 'progn `(defvar ,lex)
; 		 `(setf ,lex (make-cat
; 				:phon ,phon 
; 				:syn ,syn
; 				:sem ,sem)))
; 		 )
; 
; (lexical l-john 'john 'np 'john1)
; (lexical l-walks 'walks '(bs np s) #'(lambda (x) `(walks1 ,x)))
