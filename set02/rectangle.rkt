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
(provide world-selected?)
(provide world-after-mouse-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; main : Any -> World
;; GIVEN: any value
;; EFFECT: runs the simulation, starting with the rectangle
;; in the center of the canvas
;; RETURNS: the final state of the world
(define (run any)
  (big-bang (initial-world 0)
            (on-draw world->scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; dimensions of the rectangle
(define RECTANGLE-WIDTH 100)
(define RECTANGLE-HEIGHT 60)
(define RECTANGLE-COLOR "green")

;; dimensions of the circle
(define CIRCLE-RADIUS 5)
(define CIRCLE-COLOR "red")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

(define-struct distance (x y))
;; A Distance is a (make-distance PosInt PosInt)
;; Interpretation:
;; x, y give the distance from the  down mouse to the center
;; of the rectangle.

;; Template:
;; distance-fn : Distance -> ??
;(define (distance-fn p)
;  (... (distance-x p) (distance-y p)))

;; Examples:

(define-struct world (x y selected? distance))
;; A World is a (make-world PosInt PosInt Boolean Distance)
;; Interpretation:
;; x, y give the position of the rectangle.
;; selected? describes whether or not the reatangle is selected
;; distance gives teh distance from the button down mouse to the
;; center of the rectangle if the rectangle is selected, otherwise
;; distance is meaningless

;; Template:
;; world-fn : World -> ??
;(define (world-fn w)
;  (... (world-x w) (world-y w) (world-selected? w) (world-distance w)))

;; Examples:
(define unselected-world-at-200-150
  (make-world 200 150 
              false (make-distance 0 0)))
(define selected-world-at-200-150
  (make-world 200 150
              true (make-distance 0 0)))
(define unselected-world-at-300-200
  (make-world 300 200
              false (make-distance 0 0)))
(define selected-world-at-300-200
  (make-world 300 200
              true (make-distance 0 0)))

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: the initial world. Ignore its arguments.
;; EXAMPLES:
;(initial-world 10) = unselected-world-at-200-150
;; STRATEGY: Function Composition
(define (initial-world any)
  (make-world (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)
                        false (make-distance 0 0)))

;; TESTS:
(begin-for-test
  (check-equal? (initial-world 10) 
                unselected-world-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event :  
;;   World Integer Integer MouseEvent -> World
;; RETURNS: the world that follows the given mouse event.
;; EXAMPLES: 
;; (world-after-mouse-event unselected-world-at-200-150 200 150 "button-down")
;; = selected-world-at-200-150
;; (world-after-mouse-event unselected-world-at-200-150 200 150 "drag")
;; = unselected-world-at-200-150
;; (world-after-mouse-event unselected-world-at-200-150 200 150 "button-up")
;; = unselected-world-at-200-150
;; STRATEGY: Cases on me : MouseEvent
(define (world-after-mouse-event w mx my me)
  (cond
    [(mouse=? me "button-down") (world-after-button-down w mx my)]
    [(mouse=? me "drag") (world-after-drag w mx my)]
    [(mouse=? me "button-up") (world-after-button-up w mx my)]
    [else w]))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-mouse-event unselected-world-at-200-150 200 150 "button-down")
   selected-world-at-200-150)
  (check-equal?
   (world-after-mouse-event unselected-world-at-200-150 200 150 "drag")
   unselected-world-at-200-150)
  (check-equal?
   (world-after-mouse-event unselected-world-at-200-150 200 150 "button-up")
   unselected-world-at-200-150)
  (check-equal?
   (world-after-mouse-event unselected-world-at-200-150 200 150 "move")
   unselected-world-at-200-150))



;; world-after-button-down : World PosInt PosInt -> World
;; GIVEN: a world and the coordinate of the mouse
;; RETURNS: the world that follows the mouse event "button-down".
;; EXAMPLES:
;; (world-after-button-down unselected-world-at-200-150 200 150)
;; = selected-world-at-200-150
;; (world-after-button-down unselected-world-at-200-150 300 250)
;; = unselected-world-at-200-150
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-button-down w x y)
  (if (in-rectangle? w x y)
      (make-world (world-x w) 
                  (world-y  w)
                  true
                  (make-distance 
                   (- (world-x w) x)
                   (- (world-y w)y)))
      w))

;; TESTS:
(begin-for-test
  (check-equal? 
   (world-after-button-down unselected-world-at-200-150 200 150)
   selected-world-at-200-150)
  (check-equal?
   (world-after-button-down unselected-world-at-200-150 300 250)
   unselected-world-at-200-150)
  
  ;debug
  (check-equal? (in-rectangle? unselected-world-at-200-150 200 150) true))

;; in-rectangle? : World PosInt PosInt -> Boolean
;; GIVEN: a world and the coordinate of a point
;; RETURNS: true iff the point in the rectangle of the given world
;; EXAMPLES: 
;; (in-rectangle? unselected-world-200-150 200 150) = true
;; (in-rectangle? unselected-world-200-150 350 250) = false
;; STRAGETY: Structural Decomposition on w : World
(define (in-rectangle? w x y)
  (and (<= (- (world-x w) (/ RECTANGLE-WIDTH 2))
           x
           (+ (world-x w) (/ RECTANGLE-WIDTH 2)))
       (<= (- (world-y w) (/ RECTANGLE-HEIGHT 2))
           y
           (+ (world-y w) (/ RECTANGLE-HEIGHT 2)))))

;; TESTS:
(begin-for-test
  (check-equal? (in-rectangle? unselected-world-at-200-150 200 150) true)
  (check-equal? (in-rectangle? unselected-world-at-200-150 350 250) false))

;; world-after-drag : World PosInt PosInt -> World
;; GIVEN: a world and the coordinate of the mouse
;; RETURNS: the world after the mouse event "drag"
;; EXAMPLES: 
;; (world-after-drag unselected-world-at-200-150 300 200) 
;; = unselected-world-at-200-150
;; (world-after-drag selected-world-at-200-150 300 200) 
;; = unselected-world-at-300-200
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-drag w x y)
  (if (world-selected? w)
      (make-world 
       (+ x (distance-x (world-distance w)))
       (+ y (distance-y (world-distance w)))
       (world-selected? w)
       (world-distance w))
      w))

;; TESTS:
(begin-for-test
   (check-equal? (world-after-drag unselected-world-at-200-150 300 200) 
                 unselected-world-at-200-150)
   (check-equal? (world-after-drag selected-world-at-200-150 300 200) 
                 selected-world-at-300-200))

;; world-after-button-up : World PosInt PosInt -> World
;; GIVEN: a world and the coordinate of the mouse
;; RETURNS: the world after the mouse event "button-up"
;; EXAMPLES: 
;; (world-after-button-up selected-world-at-200-150)
;; = (unselected-world-at-200-150)
;; (world-after-button-up unselected-world-at-200-150)
;; = (unselected-world-at-200-150)
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-button-up w x y)
  (if (world-selected? w)
      (make-world (world-x w) (world-y w)
                  false (make-distance 0 0))
      w))

;; TESTS:
(begin-for-test
  (check-equal? (world-after-button-up selected-world-at-200-150 200 150)
                unselected-world-at-200-150)
  (check-equal? (world-after-button-up unselected-world-at-200-150 200 150)
                unselected-world-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world->scene : World -> Scene
;; RETURNS: a Scene that potrays the given world.
;; EXAMPLES:
;; (world->scene unselected-world-at-200-150)
;; = (place-image (rectangle RECTANGLE-WIDTH 
;;                           RECTANGLE-HEIGHT "solid" RECTANGLE-COLOR)
;;                (world-x w)
;;                (world-y w)
;;                EMPTY-CANVAS)
;;(world->scene selected-world-at-200-150)
;;= (place-image (circle RADIUS "solid" CIRCLE-COLOR)
;;               200 150
;;               (place-image (rectangle RECTANGLE-WIDTH 
;;                           RECTANGLE-HEIGHT "outline" RECTANGLE-COLOR)
;;                200 150
;;                EMPTY-CANVAS))
;; STRATEGY: Structural Decomposition on w : World
(define (world->scene w)
  (if (world-selected? w)
      (place-image (circle CIRCLE-RADIUS "solid" CIRCLE-COLOR)
                   (circle-x w)
                   (circle-y w)
                   (place-image (rectangle 
                                 RECTANGLE-WIDTH 
                                 RECTANGLE-HEIGHT
                                 "outline"
                                 RECTANGLE-COLOR)
                                (world-x w)
                                (world-y w)
                                EMPTY-CANVAS))
      (place-image (rectangle 
                    RECTANGLE-WIDTH 
                    RECTANGLE-HEIGHT
                    "solid"
                    RECTANGLE-COLOR)
                   (world-x w)
                   (world-y w)
                   EMPTY-CANVAS)))
  
;; TESTS:
(begin-for-test
  (check-equal? (world->scene unselected-world-at-200-150)
                (place-image (rectangle 
                              RECTANGLE-WIDTH 
                              RECTANGLE-HEIGHT
                              "solid"
                              RECTANGLE-COLOR)
                             200 150
                             EMPTY-CANVAS))
  (check-equal? (world->scene selected-world-at-200-150)
                (place-image (circle CIRCLE-RADIUS "solid" CIRCLE-COLOR)
                             200 150
                             (place-image (rectangle 
                                           RECTANGLE-WIDTH 
                                           RECTANGLE-HEIGHT
                                           "outline"
                                           RECTANGLE-COLOR)
                                          200 150
                                          EMPTY-CANVAS))))



;; circle-x : World -> PosInt
;; Circle-y : World -> PosInt
;; RETURNS: the x,y coordinate of the circle in the given world
;; EXAMPLES:
;; (circle-x unselected-world-at-200-150) = 200
;; (circle-y unselected-world-at-200-150) = 150
;; STRATEGY: Structural Decomposition on w : World
(define (circle-x w)
  (- (world-x w)(distance-x (world-distance w))))

(define (circle-y w)
  (- (world-y w)(distance-y (world-distance w))))

;; TESTS:
(begin-for-test
  (check-equal? (circle-x selected-world-at-200-150) 200)
  (check-equal? (circle-y selected-world-at-200-150) 150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
