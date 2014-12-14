;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname two-bouncing-cats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Two bouncing cats.
;; like bouncing cats, but there are TWO cats. They are individually
;; draggable. But space pauses or unpauses the entire system.

;; bouncing cat
;; like draggable cat, but the bouncing cat can move in all four 
;; directions. button-down to select, arrow keys to change direction

;; draggable cat
;; like falling cat, but user can drag the cat with the mouse.
;; button-down to select, drag to move, button-up to release.

;; falling cat
;; A cat falls from the top of the scene.

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION

;; main : Integer -> World
;; GIVEN: a y-coordinate
;; EFFECT: runs the simulation, starting with two unselected 
;; cats, spaced evenly across the canvas in the x-direction
;; and placed at the given y-coordinate.
;; RETURNS: the final state of the world
(define (main y)
  (big-bang (initial-world y)
            (on-tick world-after-tick 0.5)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define CAT-IMAGE (bitmap "cat.png"))

;; how fast the cat falls, in pixels/tick
(define CATSPEED 8)

;; dimensions of the canvas
(define CANVAS-WIDTH 450)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CAT1-X-COORD (/ CANVAS-WIDTH 3))
(define CAT2-X-COORD (* 2 CAT1-X-COORD))

;; dimensions of the cat
(define HALF-CAT-WIDTH  (/ (image-width  CAT-IMAGE) 2))
(define HALF-CAT-HEIGHT (/ (image-height CAT-IMAGE) 2))

;; cat's boundary
(define NORTH-BOUNDARY (+ 0 HALF-CAT-HEIGHT))
(define SOUTH-BOUNDARY (- CANVAS-HEIGHT HALF-CAT-HEIGHT))
(define WEST-BOUNDARY (+ 0 HALF-CAT-WIDTH))
(define EAST-BOUNDARY (- CANVAS-WIDTH HALF-CAT-WIDTH))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct world (cat1 cat2 paused?))
;; A World is a (make-world Cat Cat Boolean)
;; cat1 and cat2 are the two cats
;; paused? describes whether or not the world is paused

;; template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (world-cat1 w) (world-cat2 w) (world-paused? w)))


(define-struct cat (x-pos y-pos selected? direction))
;; A Cat is a (make-cat Number Number Boolean Direction)
;; Interpretation: 
;; x-pos, y-pos give the position of the cat. 
;; selected? describes whether or not the cat is selected.
;; direction is one of:
;; --"north"
;; --"east"
;; --"south"
;; --"west"

;; template:

;; direction-fn: Direction -> ??
;;(define (direction-fn d)
;;  (cond
;;    [(string=? d "north") ...]
;;    [(string=? d "east") ...]
;;    [(string=? d "south") ...]
;;    [(string=? d "west") ...]))

;; cat-fn : Cat -> ??
;(define (cat-fn c)
; (... (cat-x-pos w) (cat-y-pos w) (cat-selected? w) (cat-direction w)))

;; examples of cats, for testing
(define selected-cat1-at-20 (make-cat CAT1-X-COORD 20 true "south"))
(define unselected-cat1-at-20 (make-cat CAT1-X-COORD 20 false "south"))

(define selected-cat1-at-28 (make-cat CAT1-X-COORD 28 true "south"))
(define unselected-cat1-at-28 (make-cat CAT1-X-COORD 28 false "south"))

(define selected-cat2-at-35 (make-cat CAT2-X-COORD 35 true "south"))
(define unselected-cat2-at-35 (make-cat CAT2-X-COORD 35 false "south"))

;; examples of worlds, for testing

(define paused-world-at-20
  (make-world
    unselected-cat1-at-20
    selected-cat2-at-35
    true))

(define unpaused-world-at-20
  (make-world
    unselected-cat1-at-20
    selected-cat2-at-35
    false))

;; in an unpaused world, the unselected cat falls, but the selected
;; cat stays pinned to the mouse.
(define unpaused-world-at-20-after-tick
  (make-world
    unselected-cat1-at-28
    selected-cat2-at-35
    false))
  

;; examples KeyEvents for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   


;; example MouseEvents for testing:
(define button-down-event "button-down")
(define drag-event "drag")
(define button-up-event "button-up")
(define other-event "enter")

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow w after a tick.
;; STRATEGY: structural decomposition on w : World
(define (world-after-tick w)
  (if (world-paused? w)
    w
    (make-world
      (cat-after-tick (world-cat1 w))
      (cat-after-tick (world-cat2 w))
      (world-paused? w))))



;; cat-after-tick : Cat -> Cat
;; GIVEN: a cat c
;; RETURNS: the state of the given cat after a tick if it were in an
;; unpaused world.

;; examples: 
;; cat selected
;; (cat-after-tick selected-cat1-at-20) = selected-cat1-at-20
;; cat paused:
;; (cat-after-tick unselected-cat1-at-20) = unselected-cat-at-28

;; STRATEGY: structural decomposition on c : Cat

(define (cat-after-tick c)
 #;(cond
   [(cat-selected? c) c]
   [(cat-north? c) (cat-after-tick-toward-north c)]
   [(cat-east? c) (cat-after-tick-toward-east c)]
   [(cat-south? c) (cat-after-tick-toward-south c)]
   [(cat-west? c) (cat-after-tick-toward-west c)])
  (cat-after-tick-helper (cat-x-pos c) (cat-y-pos c)
                         (cat-selected? c) (cat-direction c)))

;; tests: tests follow help function.

;; cat-after-tick-helper : Number Number Boolean -> Cat
;; GIVEN: a position and a value for selected?
;; RETURNS: the cat that should follow one in the given position in an
;; unpaused world 
;; STRATEGY: Cases on direction : Direction
(define (cat-after-tick-helper x-pos y-pos selected? direction)
  (cond 
    [selected? (make-cat x-pos y-pos selected? direction)]
    [(string=? direction "north") 
     (cat-after-tick-toward-north x-pos y-pos selected? direction)]
    [(string=? direction "east") 
     (cat-after-tick-toward-east x-pos y-pos selected? direction)]
    [(string=? direction "south")
     (cat-after-tick-toward-south x-pos y-pos selected? direction)]
    [(string=? direction "west")
     (cat-after-tick-toward-west x-pos y-pos selected? direction)]))

;; tests:
(begin-for-test
  ;; cat selected
  (check-equal?
    (cat-after-tick selected-cat1-at-20)
    selected-cat1-at-20
    "selected cat shouldn't move")

  ;; cat unselected
  (check-equal? 
    (cat-after-tick unselected-cat1-at-20)
    unselected-cat1-at-28
    "unselected cat should fall CATSPEED pixels and remain unselected"))

;; cat-after-tick-toward-north Integer Integer Boolean Direction
;; cat-after-tick-toward-east Integer Integer Boolean Direction
;; cat-after-tick-toward-south Integer Integer Boolean Direction
;; cat-after-tick-toward-west Integer Integer Boolean Direction
;; GIVEN: the position of a cat, a value for selected?, the direction
;; of the cat
;; RETURNS: the cat in next tick of the specified direction
;; EXAMPLES: 
;; STRATEGY: Function Composition
(define (cat-after-tick-toward-north x-pos y-pos selected? direction)
  (if (< (- y-pos CATSPEED) NORTH-BOUNDARY)
      (make-cat x-pos NORTH-BOUNDARY selected? "south")
      (make-cat x-pos (- y-pos CATSPEED) selected? direction)))

(define (cat-after-tick-toward-east x-pos y-pos selected? direction)
  (if (> (+ x-pos CATSPEED) EAST-BOUNDARY)
      (make-cat EAST-BOUNDARY y-pos selected? "west")
      (make-cat (+ x-pos CATSPEED) y-pos  selected? direction)))

(define (cat-after-tick-toward-south x-pos y-pos selected? direction)
  (if (> (+ y-pos CATSPEED) SOUTH-BOUNDARY)
      (make-cat x-pos SOUTH-BOUNDARY selected? "north")
      (make-cat x-pos (+ y-pos CATSPEED) selected? direction)))

(define (cat-after-tick-toward-west x-pos y-pos selected? direction)
  (if (< (- x-pos CATSPEED) WEST-BOUNDARY)
      (make-cat WEST-BOUNDARY y-pos selected? "east")
      (make-cat (- x-pos CATSPEED) y-pos selected? direction)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cat-north? : Cat -> Boolean
;; cat-east? : Cat -> Boolean
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene paused-world-at-20) should return a canvas with
;; two cats, one at (150,20) and one at (300,28)
;;          
;; STRATEGY: structural decomposition w : World
(define (world-to-scene w)
  (place-cat
    (world-cat1 w)
    (place-cat
      (world-cat2 w)
      EMPTY-CANVAS)))

(define image-of-paused-world-at-20
  (place-image CAT-IMAGE 150 20
    (place-image CAT-IMAGE 300 35
      EMPTY-CANVAS)))

(begin-for-test
  (check-equal?
    (world-to-scene paused-world-at-20)
    image-of-paused-world-at-20))

;; place-cat : Cat Scene -> Scene
;; RETURNS: a scene like the given one, but with the given cat painted
;; on it.
(define (place-cat c s)
  (place-image
    CAT-IMAGE
    (cat-x-pos c) (cat-y-pos c)
    s))

;; tests

;;; check this visually to make sure it's what you want
(define image-at-20 (place-image CAT-IMAGE CAT1-X-COORD 20 EMPTY-CANVAS))

;;; note: these only test whether world-to-scene calls place-image properly.
;;; it doesn't check to see whether image-at-20 is the right image!
(begin-for-test
 (check-equal? 
   (place-cat selected-cat1-at-20 EMPTY-CANVAS)
   image-at-20
   "(place-cat selected-cat1-at-20 EMPTY-CANVAS) returned unexpected image or value")

 (check-equal?
   (place-cat unselected-cat1-at-20 EMPTY-CANVAS)   
   image-at-20
   "(place-cat unselected-ca1t-at-20 EMPTY-CANVAS) returned unexpected image or value"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on kev : KeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [(key=? kev "left")
     (world-after-left-key-event w)]
    [(key=? kev "right")
     (world-after-right-key-event w)]
    [(key=? kev "up")
     (world-after-up-key-event w)]
    [(key=? kev "down")
     (world-after-down-key-event w)]
    [else w]))

;; world-with-paused-toggled : World -> World
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: structural decomposition on w : World
(define (world-with-paused-toggled w)
  (make-world
   (world-cat1 w)
   (world-cat2 w)
   (not (world-paused? w))))


;; for world-after-key-event, we need 4 tests: a paused world, and an
;; unpaused world, and a pause-key-event and a non-pause key event.

(begin-for-test
  (check-equal?
    (world-after-key-event paused-world-at-20 pause-key-event)
    unpaused-world-at-20
    "after pause key, a paused world should become unpaused")

  (check-equal?
    (world-after-key-event unpaused-world-at-20 pause-key-event)
    paused-world-at-20
    "after pause key, an unpaused world should become paused")

  (check-equal?
    (world-after-key-event paused-world-at-20 non-pause-key-event)
    paused-world-at-20
    "after a non-pause key, a paused world should be unchanged")

  (check-equal?
    (world-after-key-event unpaused-world-at-20 non-pause-key-event)
    unpaused-world-at-20
    "after a non-pause key, an unpaused world should be unchanged"))

;; world-after-left-key-event : World -> World
;; world-after-right-key-event : World -> World
;; world-after-up-key-event : World -> World
;; world-after-down-key-event : World -> World
;; RETURNS: the world that should folow the specified key event
;; EXAMPLES:
;; (world-after-left-key-event paused-world-at-20)
;; = (make-world unselected-cat1-at-20 
;;               (make-cat CAT2-X-COORD 35 false "west")
;;               true)
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-left-key-event w)
  (make-world 
   (cat-after-left-key-event (world-cat1 w))
   (cat-after-left-key-event (world-cat2 w))
   (world-paused? w)))

(define (world-after-right-key-event w)
  (make-world 
   (cat-after-right-key-event (world-cat1 w))
   (cat-after-right-key-event (world-cat2 w))
   (world-paused? w)))

(define (world-after-up-key-event w)
  (make-world 
   (cat-after-up-key-event (world-cat1 w))
   (cat-after-up-key-event (world-cat2 w))
   (world-paused? w)))

(define (world-after-down-key-event w)
  (make-world 
   (cat-after-down-key-event (world-cat1 w))
   (cat-after-down-key-event (world-cat2 w))
   (world-paused? w)))

;; TESTS:

;; cat-after-left-key-event : Cat -> Cat
;; cat-after-right-key-event : Cat -> Cat
;; cat-after-up-key-event : Cat -> Cat
;; cat-after-down-key-event : Cat -> Cat
;; GIVEN: a cat
;; RETURNS: the cat after the specified key event
;; EXAMPLES:
;; (cat-after-left-key-event selected-cat1-at-20)
;; = (make-cat CAT1-X-COORD 20 true "west")
;; (cat-after-left-key-event unselected-cat1-at-20 )
;; = unselected-cat1-at-20
;; (cat-after-right-key-event selected-cat1-at-20)
;; = (make-cat CAT1-X-COORD 20 true "east")
;; (cat-after-right-key-event unselected-cat1-at-20 )
;; = unselected-cat1-at-20
;; (cat-after-up-key-event selected-cat1-at-20)
;; = (make-cat CAT1-X-COORD 20 true "north")
;; (cat-after-up-key-event unselected-cat1-at-20 )
;; = unselected-cat1-at-20
;; (cat-after-down-key-event selected-cat1-at-20)
;; = (make-cat CAT1-X-COORD 20 true "south")
;; (cat-after-down-key-event unselected-cat1-at-20 )
;; = unselected-cat1-at-20
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


;; TESTS:
(begin-for-test
  (check-equal? (cat-after-left-key-event selected-cat1-at-20)
                (make-cat CAT1-X-COORD 20 true "west"))
  (check-equal? (cat-after-left-key-event unselected-cat1-at-20 )
                unselected-cat1-at-20)
  (check-equal? (cat-after-right-key-event selected-cat1-at-20)
                (make-cat CAT1-X-COORD 20 true "east"))
  (check-equal? (cat-after-right-key-event unselected-cat1-at-20 )
                unselected-cat1-at-20)
  (check-equal? (cat-after-up-key-event selected-cat1-at-20)
                (make-cat CAT1-X-COORD 20 true "north"))
  (check-equal? (cat-after-up-key-event unselected-cat1-at-20 )
                unselected-cat1-at-20)
  (check-equal? (cat-after-down-key-event selected-cat1-at-20)
                (make-cat CAT1-X-COORD 20 true "south"))
  (check-equal? (cat-after-down-key-event unselected-cat1-at-20 )
                unselected-cat1-at-20))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Number Number MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
(define (world-after-mouse-event w mx my mev)
  (make-world
    (cat-after-mouse-event (world-cat1 w) mx my mev)
    (cat-after-mouse-event (world-cat2 w) mx my mev)
    (world-paused? w)))



;; cat-after-mouse-event : Cat Number Number MouseEvent -> Cat
;; GIVEN: a cat and a description of a mouse event
;; RETURNS: the cat that should follow the given mouse event
;; examples:  See slide on life cycle of dragged cat
;; strategy: struct decomp on mouse events
(define (cat-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev "button-down") (cat-after-button-down c mx my)]
    [(mouse=? mev "drag") (cat-after-drag c mx my)]
    [(mouse=? mev "button-up") (cat-after-button-up c mx my)]
    [else c]))

;; how many tests do we need here?
;; 3 mouse events (+ a test for the else clause)
;; cats selected or unselected  (do we need to worry about being
;; paused?)
;; event inside cat or not.

#;(begin-for-test

  ;; button-down:

  ;; button-down inside cat1
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        false)
      (+ CAT1-X-COORD 5) 15    ;; a coordinate inside cat1
      "button-down")
    (make-world
      selected-cat1-at-20
      unselected-cat2-at-35
      false)
    "button down inside cat1 should select it but didn't")


  ;; button-down inside cat2
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        false)
      (+ CAT2-X-COORD 5) 15    ;; a coordinate inside cat2
      "button-down")
    (make-world
      unselected-cat1-at-20
      selected-cat2-at-35
      false)
    "button down inside cat2 should select it but didn't")

  ;; button-down not inside any cat
  (check-equal?
    (world-after-mouse-event 
      (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        false)
      (+ CAT1-X-COORD 5) 115    ;; a coordinate not inside cat1 or cat2
      "button-down")
    (make-world
      unselected-cat1-at-20
      unselected-cat2-at-35
      false)
    "button down outside any cat should leave world unchanged, but didn't")

  ;; Question: is it possible to do a button-down on an
  ;; already-selected cat?  Is it possible to do a button-down on a
  ;; world in which ANY cat is selected?

  ;; tests for drag

  ;; don't care about paused, care only about which cat is selected. 

  ;; no cats selected: drag should not change anything
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        false)
    "drag with no cat selected didn't leave world unchanged")
    
  ;; cat1 selected
  (check-equal?
    (world-after-mouse-event
      (make-world
        selected-cat1-at-20
        unselected-cat2-at-35
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
      (make-cat (+ CAT1-X-COORD 100) 15 true "south")
      unselected-cat2-at-35
      false)
    "drag when cat1 is selected should just move cat1, but didn't")

  ;; cat2 selected
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat2-at-35
        selected-cat1-at-20
        false)
      (+ CAT1-X-COORD 100) 15    ;; a large motion
      "drag")
    (make-world
      unselected-cat2-at-35
      (make-cat (+ CAT1-X-COORD 100) 15 true "south")
      false)
    "drag when cat2 is selected should just move cat2, but didn't")

  ;; Question: is it possible to have both cat1 and cat2 selected?  If
  ;; so, what happens when they are both selected?

  ;; tests for button-up

  ;; button-up always unselects both cats

  ;; unselect cat1
  (check-equal?
    (world-after-mouse-event
      (make-world
        selected-cat2-at-35
        unselected-cat1-at-20
        true)
      (+ CAT1-X-COORD 100) 15    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat2-at-35
        unselected-cat1-at-20
        true)
    "button-up failed to unselect cat1")



  ;; unselect cat2
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-20
        selected-cat2-at-35
        true)
      (+ CAT1-X-COORD 100) 15    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        true)
    "button-up failed to unselect cat2")
  
  ;; unselect cat2
  (check-equal?
    (world-after-mouse-event
      (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        true)
      (+ CAT1-X-COORD 100) 15    ;; arbitrary location
      "button-up")
    (make-world
        unselected-cat1-at-20
        unselected-cat2-at-35
        true)
    "button-up with two unselected cats failed.")



  

  ;; tests for other mouse events

  (check-equal?
    (world-after-mouse-event unpaused-world-at-20 
      (+ CAT1-X-COORD 100) 15    ;; arbitrary coordinate
      "move")
    unpaused-world-at-20
    "other mouse events should leave the world unchanged, but didn't")

  )

;; helper functions:

;; cat-after-button-down : Cat Number Number -> Cat
;; RETURNS: the cat following a button-down at the given location.
;; STRATEGY: struct decomp on cat
(define (cat-after-button-down c x y)
  (if (in-cat? c x y)
      (make-cat (cat-x-pos c) (cat-y-pos c) true (cat-direction c))
      c))

;; cat-after-drag : Cat Number Number -> Cat
;; RETURNS: the cat following a drag at the given location
;; STRATEGY: struct decomp on cat
(define (cat-after-drag c x y)
  (if (cat-selected? c)
      (make-cat x y true (cat-direction c))
      c))

;; bound-in-x-boundary : Integer -> Integer
;; bound-in-y-boundary : Integer -> Integer
;; GIVEN: the x/y coordinate of the mouse in drag mouse event
;; RETURNS: the cat's x/y coordinate
;; EXAMPLES: 
;; STRATEGY: Cases on x/y : Integer
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


;; cat-after-button-up : Cat Number Number -> Cat
;; RETURNS: the cat following a button-up at the given location
;; STRATEGY: struct decomp on cat
(define (cat-after-button-up c x y)
  (if (cat-selected? c)
      (make-cat (bound-in-x-boundary (cat-x-pos c))
                (bound-in-y-boundary (cat-y-pos c))
                false (cat-direction c))
      c))
  

;; in-cat? : Cat Number Number -> Cat
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given cat.
;; EXAMPLES: see tests below
;; STRATEGY: structural decomposition on c : Cat
(define (in-cat? c x y)
  (and
    (<= 
      (- (cat-x-pos c) HALF-CAT-WIDTH)
      x
      (+ (cat-x-pos c) HALF-CAT-WIDTH))
    (<= 
      (- (cat-y-pos c) HALF-CAT-HEIGHT)
      y
      (+ (cat-y-pos c) HALF-CAT-HEIGHT))))

(begin-for-test
  
  ;; inside the cat
  (check-equal?
    (in-cat? unselected-cat1-at-20 (+ CAT1-X-COORD 5) 15)
    true
    "test of in-cat? with nearby point")

  (check-equal?
    (in-cat? unselected-cat1-at-20 
      (+ CAT1-X-COORD 100) 15)    ;; a coordinate not inside the cat
    false
    "test of in-cat? with distant point")

  )

;; discussion question: are these tests sufficient to test in-cat?

;; initial-world : Number -> World
;; RETURNS: a world with two unselected cats at the given y coordinate
(define (initial-world y)
  (make-world
    (make-cat CAT1-X-COORD y false "south")
    (make-cat CAT2-X-COORD y false "south")
    false))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;