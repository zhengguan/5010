;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname balls-in-box) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A program that can produce balls which can be drag.

;; Start with (run 0)

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

(provide run
         initial-world
         world-after-key-event
         world-after-mouse-event
         world-balls
         ball-x-pos
         ball-y-pos
         ball-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION

;; run : Any -> World
;; GIVEN: An argument, which is ignored.
;; EFFECT: runs the world at tick rate of 0.25 secs/tick.
;; RETURNS: the final state of the world.
(define (run any)
  (big-bang (initial-world any)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; CANVAS
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

;; BALL
(define BALL-RADIUS 20)
(define BALL-COLOR "green")
(define SQUARE-OF-BALL-RADIUS (sqr BALL-RADIUS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONs

(define-struct ball (x-pos y-pos selected?))
;; A Ball is a (make-ball Integer Integer)
;; Interpretation:
;; x-pos and y-pos are coordinates of the ball.
;; selected? describes whether the ball is selected.
;; Template:
;; ball-fn : Ball -> ??
;;(define (ball-fn b)
;;  (... (ball-x-pos b) (ball-y-pos b)))
;; Examples:

;; A ListOfBalls(LOBall) is either
;; -- empty
;; -- (cons Ball LOBall)
;; Template:
;; loball-fn : LOBall -> ??
;;(define (loball-fn loball)
;;  (cond
;;    [(empty? loball) ...]
;;    [else (... (ball-fn (first loball))
;;               (loball-fn (rest loball)))]))
;; EXAMPLES:
(define unselected-ball-at-200-150 (make-ball 200 150 false))
(define selected-ball-at-200-150 (make-ball 200 150 true))

(define-struct world (balls x-coor y-coor))
;; A World is a (make-world LOBall Integer Integer)
;; Interpretation:
;; balls is the list of all balls in the canvas
;; x-coor and y-coor are mouse's coordinate when any ball, they are
;; meaningful only when any ball is selected.
;; Template:
;; world-fn : World -> ??
;;(define (world-fn w)
;;  (... 
;;   (world-balls w)
;;   (world-x-coor w)
;;   (world-y-coor w))
;; EXAMPLES:
(define world-with-one-ball 
  (make-world (cons unselected-ball-at-200-150 empty) 0 0))
(define world-with-one-ball-selected
  (make-world (cons selected-ball-at-200-150 empty) 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: An argument, which is ignored.
;; RETURNS: a world with no balls.
;; EXAMPLES: in TESTS
;; STRATEGY: Function Composition
(define (initial-world any)
  (make-world empty 0 0))

;; TESTS
(begin-for-test 
  (check-equal? (initial-world 10) (make-world empty 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a scene that potrays the given world.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on w : World
(define (world-to-scene w)
  (balls-to-scene (world-balls w)))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-to-scene(initial-world 10))
   EMPTY-CANVAS)
  (check-equal?
   (world-to-scene world-with-one-ball)
   (place-image (circle BALL-RADIUS "outline" BALL-COLOR)
                200 150
                EMPTY-CANVAS)))

;; balls-to-scene : LOBall -> Scene
;; RETURNS: a scene that potrays the given list of balls on 
;; EMPTY-CANVAS
;; EXAMPLES: in function world-to-scene's TESTS
;; STRATEGY: Structural Decomposition on balls : LOBall
(define (balls-to-scene balls)
  (cond
    [(empty? balls) EMPTY-CANVAS]
    [else (place-ball (first balls)
                      (balls-to-scene (rest balls)))]))

;; place-ball : Ball Scene -> Scene
;; GIVEN: a ball and a scene
;; RETURNS: a scene like the given one, but with the given ball 
;; painted on it.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Ball
(define (place-ball b s)
  (if (ball-selected? b)
      (place-image (circle BALL-RADIUS "solid" BALL-COLOR)
                   (ball-x-pos b) (ball-y-pos b) s)
      (place-image (circle BALL-RADIUS "outline" BALL-COLOR)
                   (ball-x-pos b) (ball-y-pos b) s)))
                   
                   

;; TESTS:
(begin-for-test
  (check-equal?
   (place-ball unselected-ball-at-200-150 EMPTY-CANVAS)
   (place-image (circle BALL-RADIUS "outline" BALL-COLOR)
                200 150 EMPTY-CANVAS))
  (check-equal?
   (place-ball selected-ball-at-200-150 EMPTY-CANVAS)
   (place-image (circle BALL-RADIUS "solid" BALL-COLOR)
                200 150 EMPTY-CANVAS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; RETURNS: the world that should follow the given world after the 
;; given key event.
;; EXAMPLES: in TESTS
;; STRATEGY: Cases on kev : KeyEvent
(define (world-after-key-event w kev)
  (if (key=? kev "n")
      (world-add-ball w)
      w))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-key-event (initial-world 10) "n")
   world-with-one-ball)
  (check-equal? 
   (world-after-key-event (initial-world 10) " ")
   (initial-world 10)))

;; world-add-ball : World -> World
;; RETURNS: a world like the given world, but with a new ball added at
;; the center.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on w : World
(define (world-add-ball w)
  (make-world 
   (cons (make-ball HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT false)
         (world-balls w))
   (world-x-coor w)
   (world-y-coor w)))
    

;; TESTS
(begin-for-test
  (check-equal?
   (world-add-ball (initial-world 10))
   world-with-one-ball))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: A world, the location of a mouse event, and the mouse event
;; itself.
;; RETURNS: the world that should follow the given world after the 
;; given mouse event at the given location.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-mouse-event w x y mev)
  (make-world (balls-after-mouse-event 
               (world-balls w) (world-x-coor w)
               (world-y-coor w) x y mev)
              x y))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-mouse-event world-with-one-ball 200 150 "button-down")
   (make-world (cons selected-ball-at-200-150 empty) 200 150)))


;; balls-after-mouse-event : 
;;   LOBall Integer Integer Integer Integer MouseEvent -> World
;; GIVEN: a list of balls, the coordinates of mouse's previous position
;; the location of a mouse event, and the mouse event itself.
;; RETURNS: the list of balls that should follow the given world after
;; the given mouse event at the given location.
;; EXAMPLES: in TESTS
;; STRATEGY: Strutural Decomposition on lob : LOBall
(define (balls-after-mouse-event lob x-coor y-coor x y mev)
  (cond
    [(empty? lob) empty]
    [else 
     (cons 
      (ball-after-mouse-event (first lob) x-coor y-coor x y mev)
      (balls-after-mouse-event (rest lob) x-coor y-coor x y mev))]))


;; TESTS:
(begin-for-test
  (check-equal?
   (balls-after-mouse-event
    (cons unselected-ball-at-200-150 empty)
    200 150 0 0 "button-down")
   (cons unselected-ball-at-200-150 empty))
  (check-equal?
   (balls-after-mouse-event
    (cons unselected-ball-at-200-150 empty)
    0 0 200 150 "button-down")
   (cons selected-ball-at-200-150 empty)))


;; ball-after-mouse-event : 
;;   Ball Integer Integer Integer Integer MouseEvent -> Ball
;; GIVEN: a ball, the coordinates of mouse's previous position,
;; the location of a mouse event, and the mouse event itself.
;; RETURNS: the ball that should follow the given world after the 
;; given mouse event at the given location.
;; EXAMPLES: in TESTS
;; STRATEGY: Cases on mev : MouseEvent
(define (ball-after-mouse-event b x-coor y-coor x y mev)
  (cond
    [(mouse=? mev "button-down") (ball-after-button-down b x y)]
    [(mouse=? mev "drag") (ball-after-drag b x-coor y-coor x y)]
    [(mouse=? mev "button-up") (ball-after-button-up b x y)]
    [else b]))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-after-mouse-event unselected-ball-at-200-150 0 0
                           200 150 "button-down")
   selected-ball-at-200-150)
  (check-equal?
   (ball-after-mouse-event unselected-ball-at-200-150 200 150
                           200 150 "drag")
   unselected-ball-at-200-150)
  (check-equal?
   (ball-after-mouse-event unselected-ball-at-200-150 0 0
                           200 150 "button-up")
   unselected-ball-at-200-150)
  (check-equal?
   (ball-after-mouse-event unselected-ball-at-200-150 0 0
                           200 150 "enter")
   unselected-ball-at-200-150))

;; ball-after-button-down : Ball Integer Integer -> Ball
;; GIVEN: a ball and the positon of a button down mouse event
;; RETURNS: the ball should follow the given one after button down 
;; at the given location
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Ball
(define (ball-after-button-down b x y)
  (if (in-ball? b x y)
      (make-ball (ball-x-pos b) (ball-y-pos b) true)
      b))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-after-button-down unselected-ball-at-200-150 0 0)
   unselected-ball-at-200-150))


;; in-ball? : Ball Integer Integer -> Boolean
;; GIVEN: a ball and the coordinates of a point
;; RETURNS: true iff the given point in the ball
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Ball
(define (in-ball? b x y)
  (<= (+ (sqr (- x (ball-x-pos b)))
         (sqr (- y (ball-y-pos b))))
      SQUARE-OF-BALL-RADIUS))

;; TESTS:
(begin-for-test
  (check-equal? (in-ball? unselected-ball-at-200-150 200 150) true))

;; ball-after-drag : Ball Integer Integer Integer Integer -> Ball
;; GIVEN: a ball, the coordinates of mouse's previous position and
;; the positon of a drag mouse event
;; RETURNS: the ball should follow the given one after drag at the
;; given location
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Ball
(define (ball-after-drag b x-coor y-coor x y)
  (if (ball-selected? b)
      (make-ball (+ (ball-x-pos b) (- x x-coor))
                 (+ (ball-y-pos b) (- y y-coor))
                 (ball-selected? b))
      b))


;; TESTS:
(begin-for-test
  (check-equal? 
   (ball-after-drag selected-ball-at-200-150 200 150 0 0)
   (make-ball 0 0 true)))

;; ball-after-button-up : Ball Integer Integer -> Ball
;; GIVEN: a ball and the positon of a drag mouse event
;; RETURNS: the ball should follow the given one after drag at the
;; given location
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Ball
(define (ball-after-button-up b x y)
  (if (ball-selected? b)
      (make-ball (ball-x-pos b) (ball-y-pos b) false)
      b))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-after-button-up selected-ball-at-200-150 200 150)
   unselected-ball-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
