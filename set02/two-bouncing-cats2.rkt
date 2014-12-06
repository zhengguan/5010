;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname two-bouncing-cats2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Two bouncing cat
;; like bouncing cat, but there are TWO cats. They are individually draggable
;; and have their own direction. But space pauses or unpauses the entire 
;; system.

;; bouncing cat.
;; like draggable cat, but can move in four directions. 
;; button-down to select, arrow key to change direction.

;; draggable cat.
;; like falling cat, but user can drag the cat with the mouse.
;; button-down to select, drag to move, button-up to release

;; falling cat.
;; A cat falls from the top of the scene.
;; The user can pause/unpause the cat with the space bar.

;; start with (main 0)

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

(provide initial-world
         world-after-tick
         world-after-mouse-event
         world-after-key-event
         world-cat1
         world-cat2
         world-paused?
         cat-x-pos
         cat-y-pos
         cat-north?
         cat-east?
         cat-south?
         cat-west?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION

;; main : Integer -> World
;; GIVEN the initial y-position of the cats
;; EFFECT: runs the simulation, starting with the cats falling
;; RETURNS: the final state of the world
(define (main y)
  (big-bang (initial-world y)
            (on-draw world-to-scene)
            (on-tick world-after-tick 0.5)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define CAT-IMAGE (bitmap "cat.png"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 8)

;; dimension of the canvas
(define CANVAS-WIDTH 450)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT1-X-COORD (/ CANVAS-WIDTH 3))
(define CAT2-X-COORD (* 2 CAT1-X-COORD))

;; dimension of the cat
(define HALF-CAT-WIDTH (/ (image-width CAT-IMAGE) 2))
(define HALF-CAT-HEIGHT (/ (image-height CAT-IMAGE) 2))

;; boundary of the cat
(define NORTH-BOUNDARY HALF-CAT-HEIGHT)
(define SOUTH-BOUNDARY (- CANVAS-HEIGHT HALF-CAT-HEIGHT))
(define WEST-BOUNDARY HALF-CAT-WIDTH)
(define EAST-BOUNDARY (- CANVAS-WIDTH HALF-CAT-HEIGHT))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONs

(define-struct world (cat1 cat2 paused?))
;; A World is a (make-world Cat Cat Boolean)
;; cat1 and cat2 are the two cats
;; paused? describes whether or not the world is paused

;; Template:
;; world-fn : World -> ?
;;(define (world-fn w)
;;  (... (world-cat1 w) (world-cat2 w) (world-paused? w)))

(define-struct cat (x-pos y-pos selected? direction))
;; A cat is a (make-cat Integer Integer Direction)
;; x-pos and y-pos is the coordinates of the cat
;; selected? describes whether or not the world is selected
;; direction is one of
;; --"north"
;; --"east"
;; --"south"
;; --"west"

;; Template:
;; cat-fn : Cat -> ?
;; (define (cat-fn c)
;;   (... (cat-x-pos c) (cat-y-pos c) (cat-selected? c)
;;        (cat-direction c)))

;; Examples:
(define selected-cat-toward-south-at-150-100 
  (make-cat 150 100 true "south"))
(define unselected-cat-toward-south-at-150-100 
  (make-cat 150 100 false "south"))
(define selected-cat-toward-south-at-300-100 
  (make-cat 300 100 true "south"))
(define unselected-cat-toward-south-at-300-100 
  (make-cat 300 100 false "south"))

(define paused-world1 
  (make-world
   selected-cat-toward-south-at-150-100
   unselected-cat-toward-south-at-300-100
   true))
(define paused-world2
  (make-world
   unselected-cat-toward-south-at-150-100
   unselected-cat-toward-south-at-300-100
   true))
(define unpaused-world1 
  (make-world
   selected-cat-toward-south-at-150-100
   unselected-cat-toward-south-at-300-100
   false))

;; key events
(define pause-key-event " ")
(define left-key-event "left")
(define right-key-event "right")
(define up-key-event "up")
(define down-key-event "down")
(define other-key-event "q")

;; mouse events
(define button-down-event "button-down")
(define drag-event "drag")
(define button-up-event "button-up")
(define other-event "enter")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Integer -> World
;; GIVEN: a y-coordinate
;; RETURNS: a world with two unselected cats, spaced evenly across the
;; canvas in the x-direction, and falling, and placed at the given y
;; coordinate.
;; EXAMPLES:
;; (initial-world 100)
;; = (make-world unselcted-cat-toward-south-at-150-100
;;               unselcted-cat-toward-south-at-300-100
;;               false)
;; STRATEGY: Function Composition

(define (initial-world y)
  (make-world (make-cat CAT1-X-COORD y false "south")
              (make-cat CAT2-X-COORD y false "south")
              false))

;; TESTS:
(begin-for-test
  (check-equal?
   (initial-world 100)
   (make-world unselected-cat-toward-south-at-150-100 
               unselected-cat-toward-south-at-300-100
               false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a scene that potrays the given world.
;; EXAMPLES: 
;; (world-to-scene paused-world1)
;; = (place-image 
;;    CAT-IMAGE 
;;    150 100
;;    (place-image 
;;     CAT-IMAGE
;;     300 100
;;     EMPTY-CANVAS))
;; STRATEGY: Structural Decomposition on w : World
(define (world-to-scene w)
  (place-cat 
   (world-cat1 w)
   (place-cat
    (world-cat2 w)
    EMPTY-CANVAS)))

;; TESTS: 
(begin-for-test
  (check-equal? (world-to-scene paused-world1)
                (place-image 
                 CAT-IMAGE 
                 150 100
                 (place-image 
                  CAT-IMAGE
                  300 100
                  EMPTY-CANVAS))))



;; place-cat : Cat Scene -> Scene
;; RETURNS: a scene like the given one, but with the given cat painted
;; on it.
;; EXAMPLES:
;; (place-cat unselected-cat-toward-south-at-150-100
;;            EMPTY-CANVAS)
;; = (place-image CAT-IMAGE 150 100 EMPTY-CANVAS)
;; STRATEGY: Structural Decomposition on c : Cat
(define (place-cat c s)
  (place-image CAT-IMAGE 
               (cat-x-pos c)
               (cat-y-pos c)
               s))

;; TESTS:
(begin-for-test
  (check-equal?
   (place-cat unselected-cat-toward-south-at-150-100 EMPTY-CANVAS)
   (place-image CAT-IMAGE 150 100 EMPTY-CANVAS)))

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; RETURNS: the world that should follows the given world after a tick.
;; EXAMPLES:
;; (world-after-tick unpaused-world1)
;; = (make-world selceted-cat-toward-south-at-150-100
;;               unselceted-cat-toward-south-at-300-100
;;               (make-cat 300 (+ 100 CATSPEED) false "south")
;;               false)
;; (world-after-tick paused-world1)
;; = paused-world1
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world (cat-after-tick (world-cat1 w))
                  (cat-after-tick (world-cat2 w))
                  (world-paused? w))))

;; TESTS:
(begin-for-test
  (check-equal? (world-after-tick unpaused-world1)
                (make-world selected-cat-toward-south-at-150-100
                            (make-cat 300 (+ 100 CATSPEED) false "south")
                            false))
  (check-equal? (world-after-tick paused-world1)
                paused-world1))

;; cat-after-tick : Cat -> Cat
;; RETURNS: the cat that should follows the given cat after a tick
;; EXAMPLES:
;; (cat-after-tick (make-cat 150 100 true "north"))
;; = (make-cat 150 100 true "north")
;; (cat-after-tick (make-cat 150 100 false "north"))
;; = (make-cat 150 (- 100 CATSPEED) false "north")
;; (cat-after-tick (make-cat 150 100 false "east"))
;; = (make-cat (+ 150 CATSPEED) 100 false "east")
;; (cat-after-tick (make-cat 150 100 false "south"))
;; = (make-cat 150 (+ 100 CATSPEED) false "south")
;; (cat-after-tick (make-cat 150 100 false "west"))
;; = (make-cat (- 150 CATSPEED 100) false "west")
;; STRATEGY: Function Composition
(define (cat-after-tick c)
  (cond
    [(cat-selected? c) c]
    [(cat-north? c) (cat-toward-north-after-tick c)]
    [(cat-east? c) (cat-toward-east-after-tick c)]
    [(cat-south? c) (cat-toward-south-after-tick c)]
    [(cat-west? c) (cat-toward-west-after-tick c)]))

;; TESTS:

(begin-for-test
  (check-equal? 
   (cat-after-tick (make-cat 150 100 true "north"))
   (make-cat 150 100 true "north"))
  (check-equal? 
   (cat-after-tick (make-cat 150 100 false "north"))
   (make-cat 150 (- 100 CATSPEED) false "north"))
  (check-equal? 
   (cat-after-tick (make-cat 150 100 false "east"))
   (make-cat (+ 150 CATSPEED) 100 false "east"))
  (check-equal? 
   (cat-after-tick (make-cat 150 100 false "south"))
   (make-cat 150 (+ 100 CATSPEED) false "south"))
  (check-equal? 
   (cat-after-tick (make-cat 150 100 false "west"))
   (make-cat (- 150 CATSPEED) 100 false "west")))


;; cat-north? : Cat -> Boolean
;; cat-east? : Cat -> Boolean
;; cat-south? : Cat -> Boolean
;; cat-west? : Cat -> Boolean
;; GIVEN: a Cat c
;; RETURNS: true iff c is travelling in the specified derection.
;; EXAMPLES:
;; (cat-north? unselected-cat-toward-south-at-150-100) = false
;; (cat-east? unselected-cat-toward-south-at-150-100) = false
;; (cat-south? unselected-cat-toward-south-at-150-100) = true
;; (cat-west? unselected-cat-toward-south-at-150-100) = false
;; STRATEGY: Structural Decomposition on c : Cat
(define (cat-north? c)
  (string=? (cat-direction c) "north"))

(define (cat-east? c)
  (string=? (cat-direction c) "east"))

(define (cat-south? c)
  (string=? (cat-direction c) "south"))

(define (cat-west? c)
  (string=? (cat-direction c) "west"))

;; TESTS:
(begin-for-test
  (check-equal? (cat-north? unselected-cat-toward-south-at-150-100) false)
  (check-equal? (cat-east? unselected-cat-toward-south-at-150-100) false)
  (check-equal? (cat-south? unselected-cat-toward-south-at-150-100) true)
  (check-equal? (cat-west? unselected-cat-toward-south-at-150-100) false))

;; cat-toward-north-after-tick : Cat -> Cat
;; cat-toward-east-after-tick : Cat -> Cat
;; cat-toward-south-after-tick : Cat -> Cat
;; cat-toward-west-after-tick : Cat -> Cat
;; GIVEN: a cat that travelling in the specified direction.
;; RETURNS: the cat the should follow the given cat after a tick
;; EXAMPLES:
;; (cat-toward-north-after-tick (make-cat 150 100 false "north"))
;; = (make-cat 150 (- 100 CATSPEED) false "north")
;; (cat-toward-north-after-tick (make-cat 150 NORTH-BOUNDARY false "north"))
;; = (make-cat 150 NORTH-BOUNDARY false "south")
;; (cat-toward-east-after-tick (make-cat 150 100 false "east"))
;; = (make-cat (+ 150 CATSPEED) 100 false "east")
;; (cat-toward-east-after-tick (make-cat 150 EAST-BOUNDARY false "east"))
;; = (make-cat EAST-BOUNDARY 100 false "west")
;; (cat-toward-south-after-tick (make-cat 150 100 false "south"))
;; = (make-cat 150 (+ 100 CATSPEED) false "south")
;; (cat-toward-south-after-tick (make-cat 150 SOUTH-BOUNDARY false "south"))
;; = (make-cat 150 SOUTH-BOUNDARY false "north")
;; (cat-toward-west-after-tick (make-cat 150 100 false "west"))
;; = (make-cat (- 150 CATSPEED) 100 false "west")
;; (cat-toward-west-after-tick (make-cat WEST-BOUNDARY 100 false "west"))
;; = (make-cat WEST-BOUNDARY 100 false "east")

;; STRATEGY: Structural Decomposition on c : Cat
(define (cat-toward-north-after-tick c)
  (if (out-of-north-boundary-next-tick c)
      (make-cat (cat-x-pos c) NORTH-BOUNDARY 
                (cat-selected? c) "south")
      (make-cat (cat-x-pos c) (- (cat-y-pos c) CATSPEED)
                (cat-selected? c) (cat-direction c))))

(define (cat-toward-east-after-tick c)
  (if (out-of-east-boundary-next-tick c)
      (make-cat EAST-BOUNDARY (cat-y-pos c)
                (cat-selected? c) "west")
      (make-cat (+ (cat-x-pos c) CATSPEED) (cat-y-pos c)
                (cat-selected? c) (cat-direction c))))

(define (cat-toward-south-after-tick c)
  (if (out-of-south-boundary-next-tick c)
      (make-cat (cat-x-pos c) SOUTH-BOUNDARY 
                (cat-selected? c) "north")
      (make-cat (cat-x-pos c) (+ (cat-y-pos c) CATSPEED)
                (cat-selected? c) (cat-direction c))))

(define (cat-toward-west-after-tick c)
  (if (out-of-west-boundary-next-tick c)
      (make-cat WEST-BOUNDARY (cat-y-pos c)
                (cat-selected? c) "east")
      (make-cat (- (cat-x-pos c) CATSPEED) (cat-y-pos c) 
                (cat-selected? c) (cat-direction c))))
      
;; TESTS:
(begin-for-test
  (check-equal?
   (cat-toward-north-after-tick (make-cat 150 100 false "north"))
   (make-cat 150 (- 100 CATSPEED) false "north"))
  (check-equal?
   (cat-toward-north-after-tick (make-cat 150 NORTH-BOUNDARY false "north"))
   (make-cat 150 NORTH-BOUNDARY false "south"))
  (check-equal?
   (cat-toward-east-after-tick (make-cat 150 100 false "east"))
   (make-cat (+ 150 CATSPEED) 100 false "east"))
  (check-equal?
   (cat-toward-east-after-tick (make-cat EAST-BOUNDARY 100 false "east"))
   (make-cat EAST-BOUNDARY 100 false "west"))
  (check-equal?
   (cat-toward-south-after-tick (make-cat 150 100 false "south"))
   (make-cat 150 (+ 100 CATSPEED) false "south"))
  (check-equal?
   (cat-toward-south-after-tick (make-cat 150 SOUTH-BOUNDARY false "south"))
   (make-cat 150 SOUTH-BOUNDARY false "north"))
  (check-equal?
   (cat-toward-west-after-tick (make-cat 150 100 false "west"))
   (make-cat (- 150 CATSPEED) 100 false "west"))
  (check-equal?
   (cat-toward-west-after-tick (make-cat WEST-BOUNDARY 100 false "west"))
   (make-cat WEST-BOUNDARY 100 false "east"))
  
  ;debug
  (check-equal?
   (out-of-west-boundary-next-tick (make-cat WEST-BOUNDARY 100 false "west"))
   true)
  )

;; out-of-north-boundary-next-tick : Cat -> Boolean
;; out-of-east-boundary-next-tick : Cat -> Boolean
;; out-of-south-boundary-next-tick : Cat -> Boolean
;; out-of-west-boundary-next-tick : Cat -> Boolean
;; GIVEN: a cat toward the specified boundary
;; RETRUSN: true iff the cat will out of boundary after next tick
;; EXAMPLES:
;; (out-of-north-boundary-next-tick (make-cat 150 100 false "north")) = false
;; (out-of-east-boundary-next-tick (make-cat 150 100 false "east")) = false
;; (out-of-south-boundary-next-tick (make-cat 150 100 false "south")) = false
;; (out-of-west-boundary-next-tick (make-cat 150 100 false "west")) = false
;; STRATEGY: Structural Decomposition on c : Cat
(define (out-of-north-boundary-next-tick c)
  (< (- (cat-y-pos c) CATSPEED) NORTH-BOUNDARY))

(define (out-of-east-boundary-next-tick c)
  (> (+ (cat-x-pos c) CATSPEED) EAST-BOUNDARY))

(define (out-of-south-boundary-next-tick c)
  (> (+ (cat-y-pos c) CATSPEED) SOUTH-BOUNDARY))

(define (out-of-west-boundary-next-tick c)
  (< (- (cat-x-pos c) CATSPEED) WEST-BOUNDARY))

;; TESTS:
(begin-for-test
  (check-equal? 
   (out-of-north-boundary-next-tick (make-cat 150 100 false "north")) false)
  (check-equal? 
   (out-of-east-boundary-next-tick (make-cat 150 100 false "east")) false)
  (check-equal? 
   (out-of-south-boundary-next-tick (make-cat 150 100 false "south")) false)
  (check-equal? 
   (out-of-west-boundary-next-tick (make-cat 150 100 false "west")) false))
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; RETURNS: the world that follows the given key event
;; EXAMPLES:
;; (world-after-key-event unpaused-world1 pause-key-event)
;; = (make-world selected-cat-toward-south-at-150-100 
;;               unselected-cat-toward-south-at-300-100
;;               true)
;; (world-after-key-event unpaused-world1 left-key-event)
;; = (make-world (make-cat 150 100 true "west")
;;               unselected-cat-toward-south-at-300-100
;;               false)
;; (world-after-key-event unpaused-world1 right-key-event)
;; = (make-world (make-cat 150 100 true "east")
;;               unselected-cat-toward-south-at-300-100
;;               false)
;; (world-after-key-event unpaused-world1 up-key-event)
;; = (make-world (make-cat 150 100 true "north")
;;               unselected-cat-toward-south-at-300-100
;;               false)
;; (world-after-key-event unpaused-world1 down-key-event)
;; = (make-world (make-cat 150 100 true "south")
;;               unselected-cat-toward-south-at-300-100
;;               false)
;; (world-after-key-event unpaused-world1 other-key-event)
;; = (make-world selected-cat-toward-south-at-150-100 
;;               unselected-cat-toward-south-at-300-100
;;               false)
;; STRATEGY: Cases on kev : KeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev pause-key-event) (world-with-paused-toggled w)]
    [(key=? kev left-key-event) (world-after-left-key-event w)]
    [(key=? kev right-key-event) (world-after-right-key-event w)]
    [(key=? kev up-key-event) (world-after-up-key-event w)]
    [(key=? kev down-key-event) (world-after-down-key-event w)]
    [else w]))

;; TESTS:
(begin-for-test
  (check-equal? 
   (world-after-key-event unpaused-world1 pause-key-event)
   (make-world selected-cat-toward-south-at-150-100 
               unselected-cat-toward-south-at-300-100
               true))
  (check-equal? 
   (world-after-key-event unpaused-world1 left-key-event)
   (make-world (make-cat 150 100 true "west")
               unselected-cat-toward-south-at-300-100
               false))
  (check-equal? 
   (world-after-key-event unpaused-world1 right-key-event)
   (make-world (make-cat 150 100 true "east")
               unselected-cat-toward-south-at-300-100
               false))
  (check-equal? 
   (world-after-key-event unpaused-world1 up-key-event)
   (make-world (make-cat 150 100 true "north")
               unselected-cat-toward-south-at-300-100
               false))
  (check-equal? 
   (world-after-key-event unpaused-world1 down-key-event)
   (make-world (make-cat 150 100 true "south")
               unselected-cat-toward-south-at-300-100
               false))
  (check-equal? 
   (world-after-key-event unpaused-world1 other-key-event)
   (make-world selected-cat-toward-south-at-150-100 
               unselected-cat-toward-south-at-300-100
               false)))

;; world-with-paused-toggled : World -> World
;; RETURNS: a world just like the given one, but with paused? toggled.
;; EXAMPLES:
;; STRATEGY: Structural Decomposition on w : World
(define (world-with-paused-toggled w)
  (make-world (world-cat1 w) (world-cat2 w) (not (world-paused? w))))

;; world-after-left-key-event : World -> World
;; world-after-right-key-event : World -> World
;; world-after-up-key-event : World -> World
;; world-after-down-key-event : World -> World
;; RETURNS: the world that follows the given world after the specified
;; key event.
;; EXAMPLES:
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-left-key-event w)
  (make-world (cat-after-left-key-event (world-cat1 w))
              (cat-after-left-key-event (world-cat2 w))
              (world-paused? w)))

(define (world-after-right-key-event w)
  (make-world (cat-after-right-key-event (world-cat1 w))
              (cat-after-right-key-event (world-cat2 w))
              (world-paused? w)))

(define (world-after-up-key-event w)
  (make-world (cat-after-up-key-event (world-cat1 w))
              (cat-after-up-key-event (world-cat2 w))
              (world-paused? w)))

(define (world-after-down-key-event w)
  (make-world (cat-after-down-key-event (world-cat1 w))
              (cat-after-down-key-event (world-cat2 w))
              (world-paused? w)))

;; cat-after-left-key-event : Cat -> Cat
;; cat-after-right-key-event : Cat -> Cat
;; cat-after-up-key-event : Cat -> Cat
;; cat-after-down-key-event : Cat -> Cat
;; RETURNS: the cat follows the given cat after the specified 
;; key event
;; EXAMPLES:
;; STRATEGY: Structural Decomposition on c : Cat
(define (cat-after-left-key-event c)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c) (cat-y-pos c)
                (cat-selected? c) "west")
      c))

(define (cat-after-right-key-event c)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c) (cat-y-pos c)
                (cat-selected? c) "east")
      c))

(define (cat-after-up-key-event c)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c) (cat-y-pos c)
                (cat-selected? c) "north")
      c))

(define (cat-after-down-key-event c)
  (if (cat-selected? c)
      (make-cat (cat-x-pos c) (cat-y-pos c)
                (cat-selected? c) "south")
      c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; RETURNS: the world that follows the given mouse event.
;; EXAMPLES:
;; (world-after-mouse-event paused-world2 150 100 button-down-event)
;; = (make-world selected-cat-toward-south-at-150-100 
;;               unselected-cat-toward-south-at-300-100
;;               true)
;; (world-after-mouse-event paused-world1 150 200 drag-event)
;; = (make-world (make-cat 150 200 true "south")
;;               unselected-cat-toward-south-at-300-100
;;               true)
;; (world-after-mouse-event 
;;  (make-world (make-cat 150 200 true "south")
;;              unselected-cat-toward-south-at-300-100 true)
;;  150 200 button-up-event)
;; = (make-world (make-cat 150 200 false "south")
;;               unselected-cat-toward-south-at-300-100
;;               true)
;; (world-after-mouse-event pauesd-world2 150 100 other-event)
;; = paused-world2
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-mouse-event w x-pos y-pos mev)
  (make-world (cat-after-mouse-event (world-cat1 w) x-pos y-pos mev)
              (cat-after-mouse-event (world-cat2 w) x-pos y-pos mev)
              (world-paused? w)))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-mouse-event paused-world2 150 100 button-down-event)
   (make-world selected-cat-toward-south-at-150-100 
               unselected-cat-toward-south-at-300-100
               true))
  (check-equal?
   (world-after-mouse-event paused-world1 150 200 drag-event)
   (make-world (make-cat 150 200 true "south")
               unselected-cat-toward-south-at-300-100
               true))
  (check-equal?
   (world-after-mouse-event 
    (make-world (make-cat 150 200 true "south")
                unselected-cat-toward-south-at-300-100 true)
    150 200 button-up-event)
   (make-world (make-cat 150 200 false "south")
               unselected-cat-toward-south-at-300-100
               true))
  (check-equal?
   (world-after-mouse-event paused-world2 150 100 other-event)
   paused-world2))
                          


;; cat-after-mouse-event : Cat Integer Integer MouseEvent -> Cat
;; RETURNS: the cat that follows the given mouse event.
;; EXAMPLES:
;; STRATEGY: Cases on mev : MouseEvent
(define (cat-after-mouse-event c x-pos y-pos mev)
  (cond
    [(mouse=? mev "button-down") (cat-after-button-down c x-pos y-pos)]
    [(mouse=? mev "drag") (cat-after-drag c x-pos y-pos)]
    [(mouse=? mev "button-up") (cat-after-button-up c x-pos y-pos)]
    [else c]))

;; cat-after-button-down : Cat Integer Integer -> Cat
;; GIVEN: a cat and a button down mouse event's coordinate
;; RETURNS: the cat after mouse event
;; EXAMPLES:
;; STRATEGY: Structural Decomposition on c : Cat
(define (cat-after-button-down c x-pos y-pos)
  (if (in-cat? c x-pos y-pos)
      (make-cat (cat-x-pos c)
                (cat-y-pos c)
                true
                (cat-direction c))
      c))

;; in-cat? : Cat Integer Integer -> Cat
;; GIEVN: a cat an a point's coordinates
;; RETURNS: true iff the point is within the cat
;; EXAMPLES:
;; (in-cat? (make-cat 150 100 false "south") 150 100) = true
;; STRATEGY: Structural on c : Cat
(define (in-cat? c x-pos y-pos)
  (and (<= (- (cat-x-pos c) HALF-CAT-WIDTH)
           x-pos
           (+ (cat-x-pos c) HALF-CAT-WIDTH))
       (<= (- (cat-y-pos c) HALF-CAT-HEIGHT)
           y-pos
           (+ (cat-y-pos c) HALF-CAT-HEIGHT))))

(begin-for-test
  (check-equal? (in-cat? (make-cat 150 100 false "south") 150 100) true))
;; cat-after-drag : Cat Integer Integer -> Cat
;; GIVEN: a cat and a drag mouse event's coordinate
;; RETURNS: the cat after mouse event
;; EXAMPLES:
;; STRATEGY: Structural Decomposition on c : Cat
(define (cat-after-drag c x-pos y-pos)
  (if (cat-selected? c)
      (make-cat x-pos y-pos
                (cat-selected? c)
                (cat-direction c))      
      c))



;; bound-in-x-boundary : Integer -> Integer
;; bound-in-y-boundary : Integer -> Integer
;; GIVEN: a x/y coordinate
;; RETURNS: the given coordinate iff it is in the corresponding boundary,
;; else return the boundary it crosses
;; EXAMPLES:
;; (bound-in-x-boundary 100) = 100
;; (bound-in-x-boundary (- WEST-BOUNDARY 10)) = WEST-BOUNDARY
;; (bound-in-x-boundary (+ EAST-BOUNDARY 10)) = EAST-BOUNDARY
;; (bound-in-y-boundary 100) = 100
;; (bound-in-y-boundary (- NORTH-BOUNDARY 10)) = NORTH-BOUNDARY
;; (bound-in-y-boundary (+ SOUTH-BOUNDARY 10)) = SOUTH-BOUNDARY
;; STRATEGY: cases on x/y : Integer
(define (bound-in-x-boundary x)
  (cond
    [(< x WEST-BOUNDARY) WEST-BOUNDARY]
    [(> x EAST-BOUNDARY) EAST-BOUNDARY]
    [else x]))

(define (bound-in-y-boundary y)
  (cond
    [(< y NORTH-BOUNDARY) NORTH-BOUNDARY]
    [(> y SOUTH-BOUNDARY) SOUTH-BOUNDARY]
    [else y]))

;; TESTS:
(begin-for-test
  (check-equal? (bound-in-x-boundary 100) 100)
  (check-equal? (bound-in-x-boundary (- WEST-BOUNDARY 10)) WEST-BOUNDARY)
  (check-equal? (bound-in-x-boundary (+ EAST-BOUNDARY 10)) EAST-BOUNDARY)
  (check-equal? (bound-in-y-boundary 100) 100)
  (check-equal? (bound-in-y-boundary (- NORTH-BOUNDARY 10)) NORTH-BOUNDARY)
  (check-equal? (bound-in-y-boundary (+ SOUTH-BOUNDARY 10)) SOUTH-BOUNDARY))

;; cat-after-button-up : Cat Interger Interger -> Cat
;; GIVEN: a cat and a button up mouse event's coordinates
;; RETURNS: the following cat
;; EXAMPLES:
;; STRATEGY: Structural Decomposition on c : Cat
(define (cat-after-button-up c x-pos y-pos)
  (if (cat-selected? c)
      (make-cat (bound-in-x-boundary x-pos)
                (bound-in-y-boundary y-pos)
                false
                (cat-direction c))
      c))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
