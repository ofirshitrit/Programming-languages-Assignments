#lang pl
; Q2.a) nested-list-depth
#|
The idea of this function is:
1. If the list is null than return 0 because the max depth is 0
2. Check if the list dose not contains sublists:
  - if not contains return 1
  - else return 0 
3. Check if the first element is list itself:
  - if not call the function with the rest of the elements
  - else find the max depth between the first element (list itself) and the rest of the list
|#
#|
( : nested-list-depth : (Listof Any) -> Number)
 (define (nested-list-depth list)
 (cond
   [(null? list) 0] ; if the list is empty, return 0 because the max depth is 0
   [(= (not-contains-sublist? list) 1) 1]  ; if the list has no sublists, return 1
   [(list? (first list))
     (max (+ 1 (nested-list-depth (first list))) (nested-list-depth (rest list)))] ; if the first element is a list, calculate its max depth recursively
   [else
     (nested-list-depth (rest list))]))
|#
#|
Helper function to nested-list-depth function
The idea of this function is:
1. If the list is null than return 1 because empty list doesn't contain sublist
2. Check if the first element is list itself: if it is return 0 , meaning that the list contains sublist
3. Check if the rest of the list contains sublists 
|#
#|

(: not-contains-sublist? : (Listof Any) -> Number)
(define (not-contains-sublist? lst)
  (cond
    [(null? lst) 1] ; empty list doesn't contain sublist
    [(list? (first lst)) 0] ; if the first element is a list, return 0
    [else (not-contains-sublist? (rest lst))])) ; check the rest of the list


(test (nested-list-depth '(1 (2 3) ((4)) (5 (6)))) => 3)
(test (nested-list-depth '(1 2 3)) => 1)
(test (nested-list-depth '() )=> 0)
(test (nested-list-depth '(1 (2 3) (a a))) => 2)
(test (nested-list-depth '(1 (2 3) ((4)) (5 (6)) ((4)) ((6))))=> 3)
(test (nested-list-depth '(a a a)) => 1)
(test (nested-list-depth '((((((()))))))) => 6)


;(test (not-contains-sublist? '(1 (2 3) ((4)) (5 (6)))) => 0)

;(test (not-contains-sublist? '(1)) => 1)

|#

; Q2.b) nested-list-depth
(: min&max-lists : (Listof (Listof Any)) -> (Listof (Listof Number)))
(define (min&max-lists lst)
  (map min&max-helper lst))

(: min&max-helper : (Listof Any) -> (Listof Number))
(define (min&max-helper inner-list)
  (cond
    [(null? inner-list) '()]
    [(contains-number? inner-list)
     (list (apply min (get-numbers inner-list '())) (apply max (get-numbers inner-list '())))]
    [else '()]))

(: contains-number? : (Listof Any) -> Boolean)
(define (contains-number? inner-list)
  (cond
    [(null? inner-list) #f]
    [(number? (first inner-list)) #t]
    [else (contains-number? (rest inner-list))]))

(: get-numbers : (Listof Any) (Listof Number) -> (Listof Number))
(define (get-numbers lst acc)
  (cond
    [(null? lst) acc]
    [(number? (first lst)) (get-numbers (rest lst) (cons (first lst) acc))]
    [else (get-numbers (rest lst) acc)]))







;(display (contains-number? '(a b c))) ; #f (no numbers)
;(display (contains-number? '(a 1 c))) ; #t (contains a number)


(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '((2) (4) (3))) => '((2 2) (4 4) (3 3)))
(test (min&max-lists '(() ())) => '(() ()))
(test (min&max-lists '((F B C) (1 4))) => '(() (1 4)))




