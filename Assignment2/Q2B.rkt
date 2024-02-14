#lang pl 02
#| 
 The grammar: 
 <PLANG> ::= "{{" "poly" <AEs> "}{" <AEs> "}}"
 <AEs> ::=  <AE> | <AE> <AEs>
 <AE> ::= <num>
    |{+ <AE> <AE>}
    |{- <AE> <AE>}
    |{* <AE> <AE>}
    |{/ <AE> <AE>}
|# 

(define-type PLANG
  [Poly (Listof AE) (Listof AE)])

(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

(: parse-sexpr : Sexpr -> AE)
;; to convert s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> PLANG)
;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str)
  (let ([code (string->sexpr str)])
    (match code
      [`((poly ,coefficients ...) ,points ...)
       (if (null? coefficients)
           (error 'parse "at least one coefficient is required in ~s" code)
           (if (null? points)
               (error 'parse "at least one point is required in ~s" code)
               (Poly (map parse-sexpr coefficients) (map parse-sexpr points))))]
      [else (error 'parse "invalid syntax in ~s" code)])))



;; Tests

(test (parse "{{poly 1 2 3} {1 2 3}}")
      => (Poly ((Num 1) (Num 2) (Num 3)) ((Num 1) (Num 2) (Num 3))))

(test (parse "{{poly } {1 2} }")
      =error> "parse: at least one coefficient is required in ((poly) (1 2))")

(test (parse "{{poly 1 2} {} }")
      =error> "parse: at least one point is required in ((poly 1 2) ())")
