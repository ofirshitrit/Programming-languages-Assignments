#lang pl 02

;; q1
#|
IN: list of numbers
OUT: number r which is the sum of the squares of all of the numbers in the list
/////////////////////////////////////////////////////////////////////

|#
(: square (Number -> Number))
;; Computes the square of a given number.
(define (square x)
  (* x x))


(: sum-of-squares : (Listof Number) -> Number )
(define (sum-of-squares lst)
   (foldl + 0 (map square lst)))

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(5 1 1)) => 27)
(test (sum-of-squares '(1 1 1 1 1 1 1)) => 7)
(test (sum-of-squares '(-1 -2 -3)) => 14)
(test (sum-of-squares '(2.5 3.5 4.5)) => 38.75)
(test (sum-of-squares '(0)) => 0)

;; q2
#|
IN: a list of k numbers a_0, a_1 ..., a_(k-1)
OUT: number r which is the sum of the squares of all of the numbers in the list
/////////////////////////////////////////////////////////////////////

|#
;; Define createPolynomial function
(: createPolynomial : (Listof Number) -> (Number -> Number))

(define (createPolynomial coeffs)
  ;; Define helper function poly that calculates the polynomial value
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (cond
      ;; Base case: when there are no coefficients left
      ((null? argsL) accum)
      ;; Recursive case: multiply the coefficient with x raised to the power and add to the accumulator
      (else (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power)))))))
  
  ;; Define polyX function which takes a number x and returns the value of the polynomial
  (: polyX : Number -> Number)
  (define (polyX x)
    ;; Call the helper function poly with initial values
    (poly coeffs x 0 0))
  
  ;; Return the polyX function
  polyX)


;; Tests
(define p2345 (createPolynomial '(2 3 4 5))) 
(test (p2345 0) => 
 (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3)))) 

(test (p2345 4) => 
 (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3)))) 
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))

(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2)))) 
(define p_0 (createPolynomial '())) 
(test (p_0 4) => 0)