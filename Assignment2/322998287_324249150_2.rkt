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
a)
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

;b)
;; i)
#|
/////////////
BNF: 
    <PLANG> ::= {{poly AEs} {AEs}}  
    <AEs>   ::=  {AE AEs}|{AE}
    <AE>    ::=  <num> 
              |{+ AE AE} 
              |{-  AE AE} 
              |{*  AE AE] 
              |{/  AE AE}
|#

;; ii)

#|
/////////////
/////////////
parser  for PLANG
|#

(define-type PLANG 
    [Poly (Listof AE) (Listof AE)])

 
  (define-type AE 
    [Num  Number] 
    [Add  AE AE] 
    [Sub  AE AE] 
    [Mul  AE AE] 
    [Div  AE AE]) 

;; to convert s-expressions into AEs 
  (: parse-sexpr : Sexpr -> AE) 
  (define (parse-sexpr sexpr) 
    (match sexpr 
      [(number: n) (Num n)] 
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))] 
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
#|
parse a string containing a PLANG expression to a PLANG AST
we should get a string and parse it. for every string which is not of the form: {{poly C1 C2 ... Ck} {P1 P2 ... Pl}}
where all Ci and all Pj are valid AE expressions (and both k ≥ 1 and l ≥ 1) -> return an error

|#
(: parse : String -> PLANG)
(define (parse str)
(let ([code (string->sexpr str)])
       (match code
         [(list (list 'poly) (list x ...)) (error 'parse "at least one coefficient is required in ~s" code)]
         [(list (list 'poly  x ...) `()) (error 'parse "at least one point is required in ~s" code)]
         [(list (list 'poly x ...) (list y ...)) (Poly (map parse-sexpr x) (map parse-sexpr y))] ;;map call parse-sexpr for every element 
         [else (error 'parse "check youre syntax in ~s" code)])))

;;tests
(test (parse "{{poly 1 2 3} {1 2 3}}")=> (Poly (list (Num 1) (Num 2) (Num 3))  (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly 1} {1}}")=> (Poly (list (Num 1))  (list (Num 1))))
(test (parse "{{poly } {1 2} }")=error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }")=error> "parse: at least one point is required in ((poly 1 2) ())")
(test (parse "{{poly 1 2}}")=error> "parse: check youre syntax in ((poly 1 2))")
(test (parse "{{1 2}}")=error> "parse: check youre syntax in ((1 2))")
(test (parse "{{poly}}")=error> "parse: check youre syntax in ((poly))")
(test (parse "{{poly} {}}")=error> "parse: at least one coefficient is required in ((poly) ())")
(test (parse "{{1} {1}}")=error> "parse: check youre syntax in ((1) (1))")

;;evaluates AE expressions to numbers
(: eval : AE -> Number)
 (define (eval expr)
   (cases expr
     [(Num n) n]
     [(Add l r) (+ (eval l) (eval r))]
     [(Sub l r) (- (eval l) (eval r))]
     [(Mul l r) (* (eval l) (eval r))]
     [(Div l r) (/ (eval l) (eval r))]))

 (: eval-poly : PLANG -> (Listof Number) )
 (define (eval-poly p-expr)
   (cases p-expr
     ;;for very element in each list eval the AE expression and then, the outer map call createPolynomial with the
     ;;evaluated first list and with every elemnt from the second list
    [(Poly list1 list2) (map (createPolynomial (map eval list1)) (map eval list2))] ))

;; evaluate a PLANG program contained in a string
 (: run : String -> (Listof Number))
 (define (run str)
 (eval-poly (parse str)))

;;tests
(test (run "{{poly 1 2 3} {1 2 3}}")  => '(6 17 34)) 
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}")   => '(6 17 34)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}")  => '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}")  => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}")  => '(0 4 4)) 
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14))
(test (run "{{poly {> 4 2} {- 4 1}} {{- 8 4}}}") =error> "parse-sexpr: bad syntax in (> 4 2)" ) ;;invalid AE expression
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4))
;;invalid PLANG expression
(test (run "{{poly 1 2} {} }")=error> "parse: at least one point is required in ((poly 1 2) ())")
(test (run "{{poly 1 2}}")=error> "parse: check youre syntax in ((poly 1 2))")
(test (run "{{1 2}}")=error> "parse: check youre syntax in ((1 2))")
(test (run "{{poly}}")=error> "parse: check youre syntax in ((poly))")
(test (run "{{poly} {}}")=error> "parse: at least one coefficient is required in ((poly) ())")
(test (run "{{1} {1}}")=error> "parse: check youre syntax in ((1) (1))")


