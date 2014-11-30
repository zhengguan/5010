;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

;; run with (run 0)

(provide run)
(provide initial-world)
(provide world-x)
(provide world-y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; main : Any -> World
;; GIVEN: any value
;; EFFECT: runs the simulation, starting with the rectangle
;; in the center of the canvas
;; RETURNS: the final state of the world
(define (run any)
  (big-bang (make-world x y point selected?)
            (on-mouse world-after-mouse event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; dimensions of the rectangle
(define RECTANGLE-WIDTH 100)
(define RECTANGLE-HEIGHT 60)

;; dimensions of the circle
(define RADIUS 5)
(define CIRCLE-COLOR "red")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct point (x y))
;; A Point is a (make-point PosInt PosInt)
;; Interpretation:
;; x, y give the position of the down mouse.

;; Template:
;; point-fn : Point -> ??
;(define (point-fn p)
;  (... (point-x p) (point-y p)))

;; Examples:

(define-struct world (x y selected? point))
;; A World is a (make-world PosInt PosInt Boolean Point)
;; Interpretation:
;; x, y give the position of the rectangle.
;; selected? describes whether or not the reatangle is selected
;; point give the position of the down mouse if the rectangle 
;; is selected.

;; Template:
;; world-fn : World -> ??
;(define (world-fn w)
;  (... (world-x w) (world-y w) (world-selected? w) (world-point w)))

;; Examples:

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


