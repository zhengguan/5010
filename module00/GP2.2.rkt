;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname GP2.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Guided Practice 2.2
(require "extras.rkt")
(require rackunit)


;; DATA DEFINITIONS

;; A Move is one of 
;; -- "betray"
;; -- "don't betray"

;; move-fn : Move -> ???
;; (define (move-fn m)
;;   (cond
;;     [(string=? m "betray") ...]
;;     [(string=? m "don't betray") ...]))

;;; outcome : Move Move -> Number
;;; GIVEN: the moves of player 1 and player 2
;;; RETURNS: the outcome for player 1
;;; EXAMPLES: (outcome "betray" "don't betray") = 0.
;;; see table above.

(define (outcome move1 move2)
  (cond
    [(string=? move1 "betray")
     (outcome-after-betray move2)]
    [(string=? move1 "don't betray")
     (outcome-after-dont-betray move2)]))

;; player1-betray : Move -> Integer
;; GIVEN: the move of the player2
;; RETURNS: the outcome if player1 moved "betray"
;; EXAMPLES:
;; (player1-betray "betray") = -3
;; (player1-betray "don't betray") = 0
;; STRATEGY: Structural Decomposition
(define (outcome-after-betray move2)
  (cond
     [(string=? move2 "betray") -3]
     [(string=? move2 "don't betray") 0]))

(check-equal? (outcome-after-betray "betray")  -3)
(check-equal? (outcome-after-betray "don't betray")  0)

;; player1-not-betray : Move -> Integer
;; GIVEN: the move of the player2
;; RETURNS: the outcome if player1 moved "don't betray"
;; EXAMPLES:
;; (player1-not-betray "betray") = -12
;; (player1-not-betray "don't betray") = -1
;; STRATEGY: Structural Decomposition
(define (outcome-after-dont-betray move2)
  (cond
     [(string=? move2 "betray") -12]
     [(string=? move2 "don't betray") -1]))  

(check-equal? (outcome-after-dont-betray "betray") -12)
(check-equal? (outcome-after-dont-betray "don't betray") -1)


;; TESTS:
(begin-for-test
  (check-equal? (outcome "betray" "betray") -3)
  (check-equal? (outcome "betray" "don't betray") 0)
  (check-equal? (outcome "don't betray" "betray") -12)
  (check-equal? (outcome "don't betray" "don't betray") -1))