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
;(newline)

;(display (get_n_first_elements '(1 2 3 4 5 6 7 8 9) 9)) ; Expected to get: '(1 2 3 4 5 6 7 8 9)
;(display (drop_n_first_elements '(1 2 3 4 5 6 7 8 9) 9)); Expected to get: '()
;(display (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 9)) ; Expected to get: '(1 2 3 4 5 6 7 8 9)
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 9))
;(newline)

;(display (get_n_first_elements '(1 2 3 4 5 6 7 8 9) 2)) ; Expected to get: '(1 2)
;(display (drop_n_first_elements '(1 2 3 4 5 6 7 8 9) 2)); Expected to get: '(3 4 5 6 7 8 9)
;(display (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 2)) ; Expected to get: '('(1 2) '(3 4) '(5 6) '(7 8) '(9))
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 2))
;(newline)


;(display (get_n_first_elements '() 2)) ; Expected to get: '()
;(display (drop_n_first_elements '() 2)); Expected to get: '(1 2 3 4 5 6 7 8 9)
;(display (create-fixed-length-lists '() 2)) ; Expected to get: '()
(test (create-fixed-length-lists '() 2))
;(newline)

(test (create-fixed-length-lists '(1 2 3) 0) =error> "bad syntax")





