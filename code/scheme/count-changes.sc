#lang racket

(define (count-changes amount denoms)
  (cond ((or (< amount 0) (null? denoms)) 0)
        ((= amount 0) 1)
        (else (+ (count-changes (- amount (first denoms)) denoms) (count-changes amount (rest denoms))))
  )
)

(count-changes 100 '(50 25 10 5 1))
