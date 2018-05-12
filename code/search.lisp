(defparameter *graph1* '((S A B C) (A S F) (C B) (B A)))

(defun extend-path (path &optional (graph *graph1*))
  (let ((neighbors (cdr (assoc (car path) graph))))
    (mapcar #'(lambda (node)
                (cons node path))
            (remove-if #'(lambda (node) (member node path)) 
                       neighbors))))

(defun search-path (start end graph
                          &key
                          (mode 'bf)
                          (agenda (list (list start))) 
                          result
                          (current (car agenda)))
  (cond ((endp agenda) result)
        ((equal end (car current))
         (search-path start end graph 
                      :agenda (cdr agenda)
                      :result (cons (reverse current) result)
                      :mode mode))
        (t
          (search-path start end graph 
                      :agenda (if (equal mode 'df) 
                                (append 
                                 (extend-path current graph) 
                                 (cdr agenda))
                                (append 
                                 (cdr agenda)
                                 (extend-path current graph)))
                      :result result
                      :mode mode))))

;; EOF
