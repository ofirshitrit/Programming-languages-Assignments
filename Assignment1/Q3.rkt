#lang pl
; Q3) Create a data structure called `TaggedQueue`
(define-type TaggedQueue
  [EmptyTQ]
  [Enqueue Symbol Any TaggedQueue])


;Define a constructor for an empty TaggedQueue
(define emptyQT EmptyTQ)

;This function should accept an ID (symbol), a value (any type), and an existing `TaggedQueue
;returning a new `TaggedQueue` with the element added
(: enqueue : Symbol Any TaggedQueue -> TaggedQueue)
(define (enqueue ID value TQ)
  (Enqueue ID value TQ))


#|
The idea of this function is:
1.Go over (recursivelly) on the TaggedQueue
2.Each time checks if the currID is match to the one we looking for
3.If there a match we return the value
4.If we get to emptyTQ it means that the ID is not found and return #f
|#
  
(: search-queue : Symbol TaggedQueue -> (U Any #f))
(define (search-queue ID TQ)
  (cases TQ
    [(Enqueue currID currValue rest)
     (cond
       [(equal? ID currID) currValue]
       [else (search-queue ID rest)])]
    [EmptyTQ #f]))



#|
The idea of this function is:
1.Go over (recursivelly) on the TaggedQueue
2.Each time checks if the current TQ is TaggedQueue variant
3.If it is return the rest of the TQ
4.Else it means we have emptyTQ and it return #f
|#

(: dequeue-queue : TaggedQueue -> (U TaggedQueue #f))
(define (dequeue-queue TQ)
  (cases TQ
    [(Enqueue currID currValue rest) rest]
    [EmptyTQ #f]))




(test (EmptyTQ) => (EmptyTQ))
(test (Enqueue 'x 42 (EmptyTQ)) => (Enqueue 'x 42 (EmptyTQ)))
(test (search-queue 'x (Enqueue 'x 42 (EmptyTQ))) => 42)
(test (dequeue-queue (Enqueue 'x 42 (EmptyTQ))) => (EmptyTQ))
(test (dequeue-queue (EmptyTQ)) => #f)
         
    


  
