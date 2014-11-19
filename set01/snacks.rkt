;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snacks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

#;(provide
 initial-machine
 machine-next-state
 machine-choolates
 machine-carrots
 machine-bank)

(define-struct machine (choolates carrots bank cash))
; A machine is a (make-machine PosInt PosInt PosInt PosInt)
; Interpretation:
; chocolates is the number of packages of chocolate bars in the machine
; carrots is the number of carrot sitcks in the machine
; bank is the amount of money in the machine's bank, in cents
; cash is the amount of money that is neither released nor purchased 
; anything, in cents
;(define (machine-fn m)
;  (...
;   (machine-choolates m)
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
;; (machine-next-state 
;; STRATEGY: Cases on c : CustomerInput
#;(define (machine-next-state m ci)
  (cond
    [(and (integer? ci) (> ci 0)) (input-cents m ci)]
    [(string=? "chocolate") (input-chocolate m ci)]
    [(string=? "carrots") (input-carrots m ci)]
    [(string=? "release") (input-release m ci)]))

;; TESTS:


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
   (machine-choolates m)
   (machine-carrots m)
   (machine-bank m)
   (+ (machine-cash m) ci)))

;; TESTS:
(begin-for-test
  (check-equal? 
   (input-cents (initial-machine 10 10) 10)
   (make-machine 10 10 0 10)))