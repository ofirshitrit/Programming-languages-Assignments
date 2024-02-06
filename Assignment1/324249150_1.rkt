#lang pl



; Q1) create-fixed-length-lists

#|
The idea of this function is:
1. Get the n first element from the current list
2. Cons the new list (with the n first element) with the list that without the n first elemnts
3. And than return to the begining and do it recursively
|#
(: create-fixed-length-lists : (Listof Number) Integer -> (Listof (Listof Number)))
(define (create-fixed-length-lists list n)
(cond
  [(null? list)'()]
  [(>= 0 n) (error 'create-fixed-length-lists "bad syntax in ~s" n)]
  [else 
        (cons (get_n_first_elements list n) 
           (create-fixed-length-lists (drop_n_first_elements list n) n))]))



  

#|
The idea of this function is:
1. Get the first element from the current list
2. Cons the first element with the list that without the first elemnt
3. And than return to the begining and do it recursively
|#
(: get_n_first_elements : (Listof Number) Integer -> (Listof Number))
(define (get_n_first_elements list n)
(cond
  [(zero? n) '()]
  [(null? list) '()]
  [else
   (cons (first list) (get_n_first_elements(rest list)(- n 1)))]))


#|
The idea of this function is:
1. Get the current list without the first element 
2. And than return to the begining and do it recursively n times
|#
(: drop_n_first_elements : (Listof Number) Integer -> (Listof Number))
(define (drop_n_first_elements list n)
(cond
  [(zero? n) list]
  [(null? list) '()]
  [else(drop_n_first_elements(rest list)(- n 1))]))






; Tests

;(display (get_n_first_elements '(1 2 3 4 5 6 7 8 9) 3)) ; Expected to get: '(1 2 3)
;(display (drop_n_first_elements '(1 2 3 4 5 6 7 8 9) 3)); Expected to get: '(4 5 6 7 8 9)
;(display (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 3)) ; Expected to get: '('(1 2 3) '(4 5 6) '(7 8 9))
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 3))


;(display (get_n_first_elements '(1 2 3 4 5 6 7 8 9) 9)) ; Expected to get: '(1 2 3 4 5 6 7 8 9)
;(display (drop_n_first_elements '(1 2 3 4 5 6 7 8 9) 9)); Expected to get: '()
;(display (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 9)) ; Expected to get: '(1 2 3 4 5 6 7 8 9)
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 9))


;(display (get_n_first_elements '(1 2 3 4 5 6 7 8 9) 2)) ; Expected to get: '(1 2)
;(display (drop_n_first_elements '(1 2 3 4 5 6 7 8 9) 2)); Expected to get: '(3 4 5 6 7 8 9)
;(display (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 2)) ; Expected to get: '('(1 2) '(3 4) '(5 6) '(7 8) '(9))
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 2))



;(display (get_n_first_elements '() 2)) ; Expected to get: '()
;(display (drop_n_first_elements '() 2)); Expected to get: '(1 2 3 4 5 6 7 8 9)
;(display (create-fixed-length-lists '() 2)) ; Expected to get: '()
(test (create-fixed-length-lists '() 2))


(test (create-fixed-length-lists '(1 2 3) 0) =error> "bad syntax")



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

( : nested-list-depth : (Listof Any) -> Number)
 (define (nested-list-depth list)
 (cond
   [(null? list) 0] ; if the list is empty, return 0 because the max depth is 0
   [(= (not-contains-sublist? list) 1) 1]  ; if the list has no sublists, return 1
   [(list? (first list))
     (max (+ 1 (nested-list-depth (first list))) (nested-list-depth (rest list)))] ; if the first element is a list, calculate its max depth recursively
   [else
     (nested-list-depth (rest list))]))

#|
Helper function to nested-list-depth function
The idea of this function is:
1. If the list is null than return 1 because empty list doesn't contain sublist
2. Check if the first element is list itself: if it is return 0 , meaning that the list contains sublist
3. Check if the rest of the list contains sublists 
|#


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



; Q2.b) nested-list-depth

#|
Main function to find the minimum and maximum values in each inner list of a list of lists.
For each inner list:
1. If the list is empty, return an empty list.
2. If the inner list contains at least one number, replace it with a list containing the minimum and maximum values.
3. If the inner list doesn't contain any numbers, replace it with an empty list.
|# 
(: min&max-lists : (Listof (Listof Any)) -> (Listof (Listof Number)))
(define (min&max-lists lst)
  (map min&max-helper lst))

#|
Helper function to process each inner list and find its minimum and maximum values.
The idea of this function is:
1. If the inner list is empty, return an empty list.
2. If the inner list contains at least one number, replace it with a list containing the minimum and maximum values.
3. If the inner list doesn't contain any numbers, replace it with an empty list.
|# 
(: min&max-helper : (Listof Any) -> (Listof Number))
(define (min&max-helper inner-list)
  (cond
    [(null? inner-list) '()] ; If the inner list is empty, return an empty list
    [(contains-number? inner-list)
     (list (apply min (get-numbers inner-list '())) (apply max (get-numbers inner-list '())))] ; If it contains numbers, find min and max
    [else '()])) ; If it doesn't contain any numbers, replace it with an empty list

#|
Helper function to check if an inner list contains at least one number.
The idea of this function is:
1. If the inner list is empty, return #f because an empty list doesn't contain numbers.
2. If the first element of the inner list is a number, return #t.
3. Otherwise, recursively check the rest of the inner list.
|# 
(: contains-number? : (Listof Any) -> Boolean)
(define (contains-number? inner-list)
  (cond
    [(null? inner-list) #f] ; If the inner list is empty, return #f
    [(number? (first inner-list)) #t] ; If the first element is a number, return #t
    [else (contains-number? (rest inner-list))] ; Recursively check the rest of the inner list
    ))

#|
Helper function to extract numbers from an inner list.
The idea of this function is:
1. If the inner list is empty, return the accumulated list of numbers.
2. If the first element is a number, add it to the accumulator and continue recursively with the rest of the list.
3. If the first element is not a number, skip it and continue recursively.
|# 
(: get-numbers : (Listof Any) (Listof Number) -> (Listof Number))
(define (get-numbers lst acc)
  (cond
    [(null? lst) acc] ; If the inner list is empty, return the accumulated list of numbers
    [(number? (first lst)) (get-numbers (rest lst) (cons (first lst) acc))] ; If the first element is a number, add it to the accumulator
    [else (get-numbers (rest lst) acc)] ; If the first element is not a number, skip it and continue recursively
    ))




(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '((2) (4) (3))) => '((2 2) (4 4) (3 3)))
(test (min&max-lists '(() ())) => '(() ()))
(test (min&max-lists '((F B C) (1 4))) => '(() (1 4)))



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


;Q4) Design a BNF grammar

#|
<program> ::= { <statement> }

<statement> ::= <variable> "=" <expression>

<variable> ::= <identifier>
               
<expression> ::= <value> | <concatenation>
            
<concatenation> ::= <value> "++" <value>

<value> ::= <string> | <variable>

<string> ::= '"' <characters> '"'

<letter> ::= "a" | "b" | ... | "z" -> any lowercase alphabetic character

<digit> ::= "0" | "1" | ... | "9"

<other> ::= "+" | "(" | "%" | .... -> any char (that is not letter or digit) that is ASCII character

<characters>   ::= <character> | <characters>

<character> :: <letter> | <digit> | <other>

<identifier> ::= <letter> { <letter> | <digit> }
__________________________________________________________________________________________

Example 1: "x = "hello""

<program> -> <statement>

<statement> -> <variable> "=" <expression>

<variable> -> <identifier>
<expression> -> <value>

<identifier> -> <letter>
<value> -> <string>

<letter> -> x
<string> -> " <characters> "
" <characters> " => "hello"


Example 2: "z = x ++ y"

<program> -> <statement>

<statement> -> <variable> "=" <expression>

<variable> -> <identifier>
<expression> -> <concatenation>

<identifier> -> <letter>
<concatenation> -> <value> "++" <value>
<value> -> <string>
<value> -> <string>

<letter> -> z
<string> -> " <characters> "
<string> -> " <characters> "

<characters> -> <character>
<characters> -> <character>

<character> -> x
<character> -> y 



|#




