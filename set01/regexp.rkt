;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname regexp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; regexp.rkt
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide
 initial-state
 next-state
 accepting-state?
 error-state?)

; A State is one of 
; --S
; --AB
; --CD
; --E
; --ER

(define S "start, expect to see: 'a','b','c','d','e'")
(define AB "expect to see: 'a','b','c','d','e'")
(define CD "expect to see: 'c','d','e'")
(define E "encounted a 'e', finished")
(define ER "error, user passed illegal key")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-state : Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;; of your machine. The given number is ignored.
;; EXAMPLES: (initial-state 1) = S
;; STRATEGY: 
(define (initial-state n)
  S)

;; TESTS:
(begin-for-test (check-equal? (initial-state 1)  S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-state : State KeyEvent -> State
;; GIVEN: a state of the machine and a key event.
;; RETURNS: the state that should follow the given key event. A
;; key event that is to be discarded should leave the state unchanged.
;; EXAMPLES:
;; (next-state S "a") = AB
;; (next-state AB "c") = CD
;; (next-state CD "e") = E
;; (next-state E "a") = E
;; (next-state ER "a") = ER
;; STRATEGY: Cases on s : State

(define (next-state s ke)
  (cond    
    [(state=? S s) (after-s s ke)]
    [(state=? AB s) (after-ab s ke)]
    [(state=? CD s) (after-cd s ke)]
    [(accepting-state? s) s]
    [(error-state? s) s]))

;; TESTS:
(begin-for-test
  (check-equal? (next-state S "a") AB)
  (check-equal? (next-state AB "c") CD)
  (check-equal? (next-state CD "e") E)
  (check-equal? (next-state E "a") E)
  (check-equal? (next-state ER "a") ER))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state
;; EXAMPLES: (accepting-state? E) = true
;; STRATEGY: Function Composition
(define (accepting-state? s)
  (state=? E s))

;; TESTS: 
(begin-for-test 
  (check-equal? (accepting-state? E) true))

;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the string seen so far does not match specified
;; regular expression and cannot be extended to do so.
;; EXAMPLES:
;; (error-state? ER) = true
;; STRATEGY: Function Composition
(define (error-state? s)
  (state=? ER s))

(begin-for-test
  (check-equal? (error-state? ER) true))

;; state=? : State State -> Boolean
;; RETURSN: true iff the given two states are equal
;; EXAMPLES: (state=? S S) = true
;; STRATEGY: Function Composition
(define (state=? s1 s2) (string=? s1 s2))

;; TESTS:
(begin-for-test 
  (check-equal? (state=? S S) true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; after-s : State KeyEvent -> State
;; after-ab : State KeyEvent -> State
;; GIVEN: a state of the machine and a key event
;; WHERE: the state is S or AB 
;; RETURNS: the state that should follow the given key event
;; EXAMPLES:
;; (after-s S "a") = AB
;; (after-s S "c") = CD
;; (after-s S "e") = E
;; (after-s S "left") = S
;; (after-s S "g") = ER
;; (after-ab AB "a") = AB
;; (after-ab AB "c") = CD
;; (after-ab AB "e") = E
;; (after-ab AB "left") = S
;; (after-ab AB "g") = ER
;; STRATEGY: Function Composition
(define (after-s s ke)
  (after-s-or-ab s ke))

(define (after-ab s ke)
  (after-s-or-ab s ke))

;; TESTS:
(begin-for-test
  (check-equal? (after-s S "a") AB)
  (check-equal? (after-s S "c") CD)
  (check-equal? (after-s S "e") E)
  (check-equal? (after-s S "left") S)
  (check-equal? (after-s S "g") ER)
  (check-equal? (after-ab AB "a") AB)
  (check-equal? (after-ab AB "c") CD)
  (check-equal? (after-ab AB "e") E)
  (check-equal? (after-ab AB "left") AB)
  (check-equal? (after-ab AB "g") ER))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; after-cd : State KeyEvent -> State
;; GIVEN: a state of the machine and a key event
;; WHERE: the state is CD
;; RETURNS: the state that should follow the given key event
;; EXAMPLES:
;; (after-cd CD "c") = CD
;; (after-cd CD "e") = E
;; (after-cd CD "left") = S
;; (after-cd CD "g") = ER
;; STRATEGY: Cases on KeyEvent
(define (after-cd s ke)
  (cond
    [(key-event-cd? ke) CD]
    [(key-event-e? ke) E]
    [(length>1? ke) s]
    [else ER]))

;; TESTS:
(begin-for-test
  (check-equal? (after-cd CD "c") CD)
  (check-equal? (after-cd CD "e") E)
  (check-equal? (after-cd CD "left") CD)
  (check-equal? (after-cd CD "g") ER))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; after-s-or-ab : State KeyEvent -> State
;; GIVEN: a state of the machine and a key event
;; WHERE: the state is S|AB 
;; RETURNS: the state that should follow the given key event
;; EXAMPLES: the same as after-a and after-ab
;; STRATEGY: Structural Decomposition on ke : KeyEvent
(define (after-s-or-ab s ke)
  (cond
    [(key-event-ab? ke) AB]
    [(key-event-cd? ke) CD]
    [(key-event-e? ke) E]
    [(length>1? ke) s]
    [else ER]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key-event-ab? : KeyEvent -> Boolean
;; key-event-cd? : KeyEvent -> Boolean
;; key-event-e? : KeyEvent -> Boolean
;; RETURNS: true iff the given event is 
;; "a" OR "b" for key-event-ab?
;; "c" OR "d" for key-event-cd?
;; "e" for key-event-e?
;; EXAMPLES:
;; (key-event-ab? "a") = true
;; (key-event-ab? "b") = true
;; (key-event-cd? "c") = true
;; (key-event-cd? "d") = true
;; (key-event-e? "e") = true
;; STRATEGY: Function Composition

(define (key-event-ab? ke)
  (or (string=? ke "a") (string=? ke "b")))

(define (key-event-cd? ke)
  (or (string=? ke "c") (string=? ke "d")))

(define (key-event-e? ke)
  (string=? ke "e"))

;; TESTS:
(begin-for-test
  (check-equal? (key-event-ab? "a") true)
  (check-equal? (key-event-ab? "b") true)
  (check-equal? (key-event-cd? "c") true)
  (check-equal? (key-event-cd? "d") true)
  (check-equal? (key-event-e? "e") true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; length>1? : KeyEvent -> Boolean
;; RETURN: true iff length of the given key event is greater than 1
;; EXAMPLES: (length>1? "left") = true
;; STRATEGY: Function Composition 
(define (length>1? ke)
  (> (string-length ke) 1))
    
;; TESTS:
(begin-for-test
  (check-equal? (length>1? "left") true))
    


    