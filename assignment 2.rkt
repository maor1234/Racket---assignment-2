#lang pl 02


   
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares num)
  (: sum-of-squares-help : Number -> Number)
  (define (sum-of-squares-help number) (expt number 2))
  (foldl + 0 (map sum-of-squares-help num)))
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '(1 2)) => 5)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 2 3 4)) => 30)
(test (sum-of-squares '(0 0 0)) => 0)
(test (sum-of-squares '(1 1 1)) => 3)
(test (sum-of-squares '(0 0 1)) => 1)
(test (sum-of-squares '(2 2 2)) => 12)
(test (sum-of-squares '(3 1 2)) => 14)
(test (sum-of-squares '(5 3 4)) => 50)
(test (sum-of-squares '(8 5 4)) => 105)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '(2)) => 4)
(test (sum-of-squares '(1 2)) => 5)
(test (sum-of-squares '(1 2 3 4)) => 30)
(test (sum-of-squares '(1 2 3 4 5)) => 55)
(test (sum-of-squares '(1 2 3 4 5 6 7 8 9)) => 285)


(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if (null? argsL)
        accum
        (poly
         (rest argsL)
         x
         (+ 1 power)
         (+ (* (first argsL) (expt x power)) accum))))
  (: polyX : Number -> Number)
  (define (polyX x) (poly coeffs x 0 0))
  polyX)

(define p2345 (createPolynomial '(2 3 4 5)))
(test
 (p2345 0)
 =>
 (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(test
 (p2345 4)
 =>
 (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))
(test
 (p2345 11)
 =>
 (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))
(define p0000 (createPolynomial '(0 0 0 0)))
(test
 (p0000 5)
 =>
 (+
  (* 0 (expt 5 0))
  (* 0 (expt 5 1))
  (* 0 (expt 5 2))
  (* 0 (expt 5 3))
  (* 0 (expt 5 4))))
(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))
(define p111 (createPolynomial '(1 1 1)))
(test (p111 1) => (+ (* 1 (expt 1 0)) (* 1 (expt 1 1)) (* 1 (expt 1 2))))
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)
(define p123 (createPolynomial '(1 2 3)))
(test (p123 5) => (+ (* 1 (expt 5 0)) (* 2 (expt 5 1)) (* 3 (expt 5 2))))
(define p999 (createPolynomial '(9 9 9)))
(test (p999 0) => (+ (* 9 (expt 0 0)) (* 9 (expt 0 1)) (* 9 (expt 0 2))))
(define-type PLANG [Poly (Listof AE) (Listof AE)])
(define-type AE [Num Number] [Add AE AE] [Sub AE AE] [Mul AE AE] [Div AE AE])





(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sexpr)
  (match
      sexpr
    [(number: n) (Num n)]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
(: parse : String -> PLANG)
(define (parse str)
  (let ([code (string->sexpr str)])
    (match
        code
      [(cons (cons 'poly HelpList) (list '()))
       (error 'parse "at least one point is required in ~s" code)]
      [(cons (cons 'poly '()) (list (list x ...)))
       (error 'parse "at least one coefficient is required in ~s" code)]
      [(cons (cons 'poly HelpList) (list (list x ...)))
       (Poly (map parse-sexpr HelpList) (map parse-sexpr x))]
      [else (error 'parse "bad 'poly' syntax in ~s" code)])))
(test
 (parse "{{poly 1 2 3} {1 2 3}}")
 =>
 (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3))))
(test
 (parse "{{poly 1 2 3} {4 5 6}}")
 =>
 (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 4) (Num 5) (Num 6))))
(test
 (parse "{{poly 1 2 3} {4}}")
 =>
 (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 4))))
(test
 (parse "{{poly } {1 2} }")
 =error>
 "parse: at least one coefficient is required in ((poly) (1 2))")
(test
 (parse "{{poly 1 2} {} }")
 =error>
 "parse: at least one point is required in ((poly 1 2) ())")
(test
 (parse "{{poly 1 2 3 4 55 66 11 12 44 12 14} {} }")
 =error>
 "parse: at least one point is required in ((poly 1 2 3 4 55 66 11 12 44 12 14) ())")
(test
 (parse "{{poly } {1 2 3 4 5 6 7 8 9 } }")
 =error>
 "parse: at least one coefficient is required in ((poly) (1 2 3 4 5 6 7 8 9))")
(test
 (parse "{{poly } {} }")
 =error>
 "parse: at least one point is required in ((poly) ()")
(test
 (parse "{{poly 4/5 } {1/2 2/3 3}}")
 =>
 (Poly (list (Num 4/5)) (list (Num 1/2) (Num 2/3) (Num 3))))
(test
 (parse "{{poly 2 3} {4}}")
 =>
 (Poly (list (Num 2) (Num 3)) (list (Num 4))))
(test
 (parse "{{poly 1 1 0} {-1 3 3}}")
 =>
 (Poly (list (Num 1) (Num 1) (Num 0)) (list (Num -1) (Num 3) (Num 3))))
(test
 (parse "{{poly 4/5 } {1/2 2/3 3} {poly 1 2 4} {1 2}}")
 =error>
 "parse: bad 'poly' syntax in ((poly 4/5) (1/2 2/3 3) (poly 1 2 4) (1 2))")




(: eval : AE -> Number)
(define (eval expr)
  (cases
      expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]))
(: eval-poly : PLANG -> (Listof Number))
(define (eval-poly p-expr)
  (cases
      p-expr
    [(Poly X Y) (map (createPolynomial (map eval X)) (map eval Y))]))
(: run : String -> (Listof Number))
(define (run str) (eval-poly (parse str)))
(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}") => '(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}") => '(4/5 4/5 4/5))
(test (run "{{poly 2 3} {4}}") => '(14))
(test (run "{{poly 1 1 0} {-1 3 3}}") => '(0 4 4))
(test
 (run "{{poly } {}}")
 =error>
 "parse: at least one point is required in ((poly) ()")
(test
 (run "{{poly 1 } {}}")
 =error>
 "parse: at least one point is required in ((poly 1) ()")
(test
 (run "{{poly 4/5 } {1/2 2/3 3} {poly 1 2 4} {1 2}}")
 =error>
 "parse: bad 'poly' syntax in ((poly 4/5) (1/2 2/3 3) (poly 1 2 4) (1 2))")
   
