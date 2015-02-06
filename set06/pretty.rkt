;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;; set06 Q2 version2
;; pretty.rkt


(require "extras.rkt")
(require rackunit)
(provide expr-to-strings
         make-sum-exp
         sum-exp-exprs
         make-mult-exp
         mult-exp-exprs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct sum-exp (exprs))
(define-struct mult-exp (exprs))

;; An Expr is one of
;; -- Integer
;; -- (make-sum-exp NELOExpr)
;; -- (make-mult-exp NELOExpr)
;; Interpretation: a sum-up represents a sum and a mult-exp
;; represents a multiplication.
;; Template:
;; expr-fn : Expr -> ??
#;
(define (expr-fn e)
  (cond
    [(integer? e) ...]
    [(sum-exp? e) ...]
    [(mult-exp? e) ...]))


;; A LOExpr is one of
;; -- empty
;; -- (cons Expr LOExpr)

;; A NELOExpr is a non-empty LOExpr.
;; Template:
;; neloe-fn : neloe -> ??
#;
(define (neloe-fn neloe)
  (cond
    [(empty? neloe) ...]
    [else (... (first neloe)
               (neloe-fn (rest neloe)))]))
#;
(define (neloe-fn neloe)
  (cond
    [(empty? (rest neloe)) ...]
    [(else (... (first nelos)
                (neloe-fn (rest neloe))))]))
#;
(define (neloe-fn neloe)
  (... (first neloe)
       (eloe-fn2 (rest neloe))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expr-to-strings : Expr NonNegInt -> ListOfString
;; GIVEN: An expression and a width.
;; RETURNS: A representation of the expression as a sequence of lines,
;; with each line represented as a string of length not greater than 
;; width.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (expr-to-strings
    (make-sum-exp
     (list
      (make-mult-exp (list 22 3333 44))
      (make-mult-exp
       (list
        (make-sum-exp (list 66 67 68))
        (make-mult-exp (list 42 43))))
      (make-mult-exp (list 77 88))))
    100)
  (list "(+ (* 22 3333 44) (* (+ 66 67 68) (* 42 43)) (* 77 88))"))
  (check-error (expr-to-strings
                (make-sum-exp (list 123 11111111111111111)) 10)
               "not enough room")
  )
;; STRATEGY: Cases on e : Expr
(define (expr-to-strings e width)
  (one-expr-to-strings e width "" ""))


;; one-expr-to-strings : Expr PosInt String String -> ListOfString
;; GIVEN: an expression e, a width, the prefix string of this,
;; expression, the suffix of this expression.
;; RETURNS: A representation of the expression as a sequence of lines,
;; with each line represented as a string of length not greater than 
;; width.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (one-expr-to-strings (make-sum-exp (list 10 20 30 40)) 15 "" "")
   (list "(+ 10 20 30 40)"))
  (check-equal?
   (one-expr-to-strings (make-sum-exp (list 10 20 30 40)) 7 "" "")
   (list "(+ 10" "   20" "   30" "   40)")))
;; STRATEGY: Cases on e : Expr
(define (one-expr-to-strings e width prefix suffix)
  (cond
    [(fit-in-one-line? e (- width 
                            (string-length prefix)
                            (string-length suffix)))
     (list (expr-to-one-string e prefix suffix))]
    [else (one-expr-to-strings-helper e width prefix suffix)]))


;; one-expr-to-strings-helper 
;; : Expr PosInt String String -> ListOfString
;; GIVEN: An expression e, a width, the prefix string of e, the suffix
;; string of e.
;; WHERE: e cannot be represented as a one line string of length not
;; greater than width.
;; RETURNS: A representation of the expression as a sequence of lines,
;; with each line represented as a string of length not greater than 
;; width.
;; EXAMPLES/TESTS:
;; STRATEGY: Cases on e : Expr
(define (one-expr-to-strings-helper e width prefix suffix)
  (cond
    [(integer? e) (error "not enough room")]
    [else (op-expr-to-strings e width prefix suffix)]))
    

;; op-expr-to-strings : Expr PosInt String String -> ListOfString
;; GIVEN: an expression e, a width, the prefix string of this
;; expression, the suffix of this expression.
;; WHERE: e is a sum-exp or mult-exp.
;; RETURNS: A representation of the expression as a sequence of lines,
;; with each line represented as a string of length not greater than 
;; width.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (op-expr-to-strings (make-sum-exp (list 10 20 30 40)) 7 "" "")
   (list "(+ 10" "   20" "   30" "   40)"))
  (check-equal?
   (op-expr-to-strings (make-mult-exp (list 10 20 30 40)) 7 "" "")
   (list "(* 10" "   20" "   30" "   40)")))
;; STRATEGY: Structural Decomposition on e : Expr
(define (op-expr-to-strings e width prefix suffix)
  (cond
    [(sum-exp? e) (exprs-to-strings 
                   (sum-exp-exprs e)
                   width
                   (string-append prefix "(+ ")
                   (string-append suffix ")"))]
    [(mult-exp? e) (exprs-to-strings 
                    (mult-exp-exprs e)
                    width
                    (string-append prefix "(* ")
                    (string-append suffix ")"))]))

;; exprs-to-strings : NELOE PosInt String String -> ListOfString
;; GIVEN: a non empty list of expressions neloe, a width, the prefix 
;; string of these expressions, the suffix string of these 
;; expressions.
;; WHERE: neloe is the sub-expressions of a Expr(ie. sum-exp or 
;; mult-exp).
;; RETURNS: A representation of the expressions as a sequence of 
;; lines, with each line represented as a string of length not 
;; greater than width.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (exprs-to-strings (list 12 13) 7 "(+ " ")")
   (list "(+ 12" "   13)"))
  (check-equal?
   (exprs-to-strings (list 12) 7 "(+ " ")")
   (list "(+ 12)")))
;; STRATEGY: Structural Decompsition on neloe : NELOE
(define (exprs-to-strings neloe width prefix suffix)
  (if (= (length neloe) 1) 
      (one-expr-to-strings (first neloe)
                       width
                       prefix
                       suffix)
      (append (first-expr-to-strings (first neloe)
                                     width
                                     prefix)
              (rest-exprs-to-strings (rest neloe)
                                      width
                                      (make-string (string-length prefix)
                                                   #\ )
                                      suffix))))



;; first-expr-to-strings : Expr PosInt String -> ListOfString
;; GIVEN: the first sub-expression of an expression, a width, the 
;; prefix string of this sub-expression.
;; RETURNS: A representation of the expression as a sequence of
;; lines, with each line represented as a string of length not
;; greater than width.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (first-expr-to-strings 10 7 "(+ ")
   (list "(+ 10")))
;; STRATEGY: Function Composition
(define (first-expr-to-strings e width prefix)
  (one-expr-to-strings e width prefix ""))

;; rest-exprs-to-strings 
;;  : NELOE PosInt String String -> ListOfString
;; GIVEN: neloe is the sub-expressions of an expression except the 
;; first one, a width, the prefix string that should be on the left
;; of each sub-expression, a suffix string that should be on the right
;; of the last sub-expressions.
;; RETURNS: A representation of the expressions as a sequence of
;; lines, with each line represented as a string of length not
;; greater than width.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (rest-exprs-to-strings (list 12 13) 7 "   " ")")
   (list "   12" "   13)")))
;; STRATEGY: Structural Decomposition on neloe : NELOE
(define (rest-exprs-to-strings neloe width prefix suffix)
  (cond
    [(empty? (rest neloe)) 
     (one-expr-to-strings 
            (first neloe)
            width
            prefix
            suffix)]
    [else (append (one-expr-to-strings
                 (first neloe)
                 width
                 prefix
                 "")
                (rest-exprs-to-strings 
                 (rest neloe)
                 width
                 prefix
                 suffix))]))







;; fit-in-one-line? : Expr NonNegInt -> Boolean
;; GIVEN: An expression e, a width, 
;; RETURNS: true iff the expression can be represented as a string of
;; length not greater than width.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (fit-in-one-line? (make-sum-exp (list 1 2 3)) 15)
   true))
;; STRATEGY: Function Composition 
(define (fit-in-one-line? e width)
  (<= (expr-one-line-length e) width))

;; expr-one-line-legnth : Expr -> PosInt
;; GIVEN: a expression
;; RETURNS: the length of the representation of the expression as a 
;; string.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (expr-one-line-length 12)
   2)
  (check-equal?
   (expr-one-line-length (make-sum-exp (list 1 2)))
   7)
  (check-equal?
   (expr-one-line-length (make-mult-exp (list 1 2)))
   7)
  ;; debug
  (check-equal? (integer? 12) true)
  (check-equal? (string-length (number->string 12)) 2)
  )
;; STRATEGY: Cases on e : Expr
(define (expr-one-line-length e)
  (cond
    [(integer? e) (string-length (number->string e))]
    [(sum-exp? e) (non-int-expr-length e sum-exp-exprs)]
    [(mult-exp? e) (non-int-expr-length e mult-exp-exprs)]))

;; non-int-expr-length : Expr (Expr -> ListOfExpr) -> PosInt
;; GIVEN: an expression e, and its corresponding destructor.
;; WHERE: e is a sum-exp or mult-exp
;; RETURNS: the length of the representation of the expression as a 
;; string.
;; EXAMPLES/TESTS: in function expr-one-line-length.
;; STRATEGY: Structural Decomposition on e : Expr
(define (non-int-expr-length e d)
  (+ 3  ; (,),+ signs
     (length (d e)) ;blanks before each argument
     (foldr
      ; Expr PosInt -> PosInt
      ; GIVEN: an sub-expression sub-e of an expression e and the 
      ; length of the string representation of the rest 
      ; sub-expressions of e.
      ; RETURNS: the length of sub-expressions with sub-e included.
      (lambda(sub-e rest-length) 
        (+ (expr-one-line-length sub-e) rest-length))
      0
      (d e))))
  

;; expr-to-one-string : Expr String String -> String
;; GIVEN: an expression e.
;; RETURNS: the representation of e as a single line string.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (expr-to-one-string (make-sum-exp (list 1 2 3)) "" "")
   "(+ 1 2 3)")
  (check-equal?
   (expr-to-one-string (make-mult-exp (list 1 2 3)) "" "")
   "(* 1 2 3)")
  (check-equal?
   (expr-to-one-string 123 "" "")
   "123")
  ;; debug
  (check-equal? (mult-exp? (make-mult-exp (list 1 2 3))) true)
  (check-equal? 
   (mult-expr-to-one-string (make-mult-exp (list 1 2 3)) "" "")
   "(* 1 2 3)")
  )
;; STRATEGY: Structural Decomposition on e : Expr
(define (expr-to-one-string e prefix suffix)
  (cond
    [(number? e) (string-append prefix (number->string e) suffix)]
    [(sum-exp? e) (sum-expr-to-one-string e prefix suffix)]
    [(mult-exp? e) (mult-expr-to-one-string e prefix suffix)]))
    
;; sum-expr-to-one-string : Expr String String -> String
;; mult-expr-to-one-string : Expr String String -> String
;; GIVEN: an expression e, its prefix string, its suffix string.
;; WHERE: e is a Sum-Exp/Mult-Exp
;; RETURNS: a one line string representation of e with its prefix and
;; sufix.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (sum-expr-to-one-string (make-sum-exp (list 1 2)) "" "")
   "(+ 1 2)")
  (check-equal?
   (mult-expr-to-one-string (make-mult-exp (list 1 2)) "" "")
   "(* 1 2)"))
;; STRATEGY: Structural Decomposition on e : Expr
(define (sum-expr-to-one-string e prefix suffix)
  (string-append prefix
                 "(+"
                 (exprs-to-one-string (sum-exp-exprs e))
                 suffix
                 ")"))

(define (mult-expr-to-one-string e prefix suffix)
  (string-append prefix
                 "(*"
                 (exprs-to-one-string (mult-exp-exprs e))
                 ")"
                 suffix))
  

;; exprs-to-one-string : ListOfExpr -> String
;; GIVEN: a list of expressions sub-exps all of which are the 
;; sub-expressions of an expression.
;; RETURNS: the represention string of the sub-expressions each of 
;; which has a blank ahead.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (exprs-to-one-string (list 1))
   " 1")
  (check-equal?
   (exprs-to-one-string (list 1 (make-sum-exp (list 1 2))
                              (make-mult-exp (list 1 2))))
   " 1 (+ 1 2) (* 1 2)")
  ;;debug
  (check-equal?
   (exprs-to-one-string (list 1))
   " 1"))
;; STRATEGY: HOFC
(define (exprs-to-one-string loe)
  (foldl
   ; Expr String -> String
   ; GIVEN: an expression sub-exp and a string prev 
   ; RETURNS: a string the append rest to sub-exp's representation 
   ; string.
   (lambda(sub-exp prev)
     (string-append prev
                    " "
                    (expr-to-one-string sub-exp "" "")))
   ""
   loe))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE TESTS
;(define hw-example-1
;  (make-sum-exp (list 22 333 44)))
;
;(expr-to-strings hw-example-1 15)
;
;(expr-to-strings hw-example-1 10)
;
;(define (display-expr expr n)
;  (display-strings! (expr-to-strings expr n)))
;
;(display-expr hw-example-1 25)
;
;(display-expr hw-example-1 10)
;
;;(display-expr hw-example-1 5)
;
;(define  hw-example-2
;  (make-sum-exp
;   (list
;    (make-mult-exp (list 22 3333 44))
;    (make-mult-exp
;     (list
;      (make-sum-exp (list 66 67 68))
;      (make-mult-exp (list 42 43))))
;    (make-mult-exp (list 77 88)))))
;
;(display-expr hw-example-2 100)
;
;(display-expr hw-example-2 50)
; 
;(display-expr hw-example-2 20)
;
;(display-expr hw-example-2 15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;