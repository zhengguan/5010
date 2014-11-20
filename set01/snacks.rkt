;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snacks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide
 initial-machine
 machine-next-state
 machine-chocolates
 machine-carrots
 machine-bank)

(define-struct machine (chocolates carrots bank cash))
; A machine is a (make-machine PosInt PosInt PosInt PosInt)
; Interpretation:
; chocolates is the number of packages of chocolate bars in the machine
; carrots is the number of carrot sitcks in the machine
; bank is the amount of money in the machine's bank, in cents
; cash is the amount of money that is neither released nor purchased 
; anything, in cents
;(define (machine-fn m)
;  (...
;   (machine-chocolates m)
;   (machine-carrots m)
;   (machine-bank m)
;   (machine-cash m)))

; A CustomerInput is one of 
; -- a PosInt
; Interpretation: insert the specified number of cents
; -- "choolate"
; Interpretation: request a chocolate bar
; -- "carrots"
; Interpretation: request a package of carrot sticks
; -- "release"
; Interpretation: return all the coins that the customer has put in


;; initial-machine : NonNegInt NonNegInt -> Machine
;; GIVEN: the number of chocolate bars and the number of package of
;; carrot sticks
;; RETURNS: a machine loaded with the given number of chocolate bars
;; and carrot sticks, with an empty bank
;; EXAMPLES:
;; (initial-machine 10 10) = (make-machine 10 10 0 0)
;; STRATEGY: Function Composition
(define (initial-machine chocolates carrots)
  (make-machine chocolates carrots 0 0))

;; TESTS:
(begin-for-test
  (check-equal? (initial-machine 10 10) (make-machine 10 10 0 0)))

;; machine-next-state: Machine CustomerInput -> Machine
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's
;; input
;; EXAMPLES:
;; (machine-next-state (make-machine 10 10 0 200) 25)
;; = (make-machine 10 10 0 225)
;; (machine-next-state (make-machine 10 10 0 200) "chocolate")
;; = (make-machine 9 10 175  25)
;; (machine-next-state (make-machine 10 10 0 200) "carrots")
;; = (make-machine 10 9 70 130)
;; (machine-next-state (make-machine 10 10 0 200) "release")
;; = (make-machine 10 10 0 0)
;; STRATEGY: Cases on c : CustomerInput
(define (machine-next-state m ci)
  (cond
    [(and (integer? ci) (> ci 0)) (input-cents m ci)]
    [(string=? ci "chocolate") (input-chocolate m ci)]
    [(string=? ci "carrots") (input-carrots m ci)]
    [(string=? ci "release") (input-release m ci)]))

;; TESTS:
(begin-for-test
  (check-equal? (machine-next-state (make-machine 10 10 0 200) 25)
                (make-machine 10 10 0 225))
  (check-equal? (machine-next-state (make-machine 10 10 0 200) "chocolate")
                (make-machine 9 10 175 25))
  (check-equal? (machine-next-state (make-machine 10 10 0 200) "carrots")
                (make-machine 10 9 70 130))
  (check-equal? (machine-next-state (make-machine 10 10 0 200) "release")
                (make-machine 10 10 0 0))
  
;; bug
  (check-equal? (input-chocolate (make-machine 10 10 0 200) "chocolate")
                (make-machine 9 10 175 25))
  )




;; input-cents: Machine CustomerInput -> Machine
;; GIVEN: a machine state and a customer input which is the customer's input
;; money, in cents
;; WHERE: the customer input is an PosInt
;; RETURNS
;; RETURNS: the state of the machine that should follow the customer's
;; input
;; EXAMPLES:
;; (input-cents (initial-machine 10 10) 10)
;; = (make-machine 10 10 0 10)
;; STRATEGY: Structural Decomposition on ci : CustomerInput
(define (input-cents m ci)
  (make-machine 
   (machine-chocolates m)
   (machine-carrots m)
   (machine-bank m)
   (+ (machine-cash m) ci)))

;; TESTS:
(begin-for-test
  (check-equal? 
   (input-cents (initial-machine 10 10) 10)
   (make-machine 10 10 0 10)))

;; input-chocolate : Machine CustomerInput -> Machine
;; GIVEN: a machine state and a customer input
;; WHERE: the customer input is "chocolate"
;; RETURNS: the state of the machine that should follow the customer's
;; input
;; EXAMPLES:
;; (input-chocolate (make-machine 10 10 0 0))
;; = (make-machine 10 10 0 0)
;; (input-chocolate (make-machine 10 10 0 175))
;; = (make-machine 9 10 175 0)
;; STRATEGY: Structural Decomposition on m : Machine
(define (input-chocolate m ci)
  (if (can-buy-chocolate? m)
      (make-machine (- (machine-chocolates m) 1)
                    (machine-carrots m)
                    (+ (machine-bank m) 175)
                    (- (machine-cash m) 175))
      m))

;; TESTS:
(begin-for-test
  (check-equal? (input-chocolate (make-machine 10 10 0 0) "chocolate")
                (make-machine 10 10 0 0))
  (check-equal? (input-chocolate (make-machine 10 10 0 175) "chocolate")
                (make-machine 9 10 175 0)))



;; can-buy-chocolate? : Machine -> Boolean
;; GIVEN: a machine state
;; RETURNS: true iff the machine has some chocolate bars and the 
;; customer has offered enough money to buy one
;; EXAMPLE:
;; (can-buy-chocolate? (make-machine 10 10 0 175)) = true
;; STRATEGY: Structural Decomposition on m : Machine
(define (can-buy-chocolate? m)
  (and (> (machine-chocolates m) 0)
       (>= (machine-cash m) 175)))

;; TESTS:
(begin-for-test
  (check-equal? (can-buy-chocolate? (make-machine 10 10 0 175)) true))


;; input-carrots : Machine CustomerInput -> Machine
;; GIVEN: a machine state and a customer input
;; WHERE: the customer input is "carrots"
;; RETURNS: the state of the machine that should follow the customer's
;; input
;; EXAMPLES:
;; (input-carrots (make-machine 10 10 0 70) "carrots")
;; = (make-machine 10 9 70 0)
;; (input-carrots (make-machine 10 10 0 0) "carrots")
;; = (make-machine 10 10 0 0)
;; STRATEGY: Structural Decomposition on m : Machine
(define (input-carrots m ci)
  (if (can-buy-carrots? m)
      (make-machine (machine-chocolates m)
                    (- (machine-carrots m) 1)
                    (+ (machine-bank m) 70)
                    (- (machine-cash m) 70))
      m))
;; TESTS:
(begin-for-test
  (check-equal? (input-carrots (make-machine 10 10 0 70) "carrots")
                (make-machine 10 9 70 0))
  (check-equal? (input-carrots (make-machine 10 10 0 0) "carrots")
                (make-machine 10 10 0 0)))



;; can-buy-carrots? : Machine -> Boolean
;; GIVEN: a machine state
;; RETURNS: true iff the machine has some carrot sticks and the 
;; customer has offered enough money to buy one
;; EXAMPLES:
;; (can-buy-carrots? (make-machine 10 10 0 70)) = true
;; STRATEGY: Structural Decomposition on m : Machine
(define (can-buy-carrots? m)
  (and (> (machine-carrots m) 0)
       (>= (machine-cash m) 70)))

;; TESTS:
(begin-for-test
  (check-equal? (can-buy-carrots? (make-machine 10 10 0 70)) true))


;; input-release : Machine CustomerInput -> Machine
;; GIVEN: a machine state and a customer input
;; WHERE: the customer input is "release"
;; RETURNS: the state of the machine that should follow the customer's
;; input
;; EXAMPLES:
;; (input-release (make-machine 10 10 0 70) "release")
;; = (make-machine 10 10 0 0)

(define (input-release m ci)
  (make-machine 
   (machine-chocolates m)
   (machine-carrots m)
   (machine-bank m)
   0))

;; TESTS:
(begin-for-test
  (check-equal? (input-release (make-machine 10 10 0 70) "release")
                (make-machine 10 10 0 0)))

