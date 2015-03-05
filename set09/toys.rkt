;; set 09
;; toys.rkt

#lang racket

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)      

(provide World%
         SquareToy%
         CircleToy%
         make-world
         run
         make-square-toy
         make-circle-toy
         World<%>
         Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS 
  (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; A Direction is on of
;; -- "right"
;; -- "left"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; World%     -- a class that satisfies the World<%> interface (shown below).
;; SquareToy% -- a class that satisfies the Toy<%> interface
;; CircleToy% -- a class that satisfies the Toy<%> interface

;; make-world : PosInt -> World%
;; RETURNS: a world with a target, but no toys, and in which any
;; toys created in the future will travel at the given speed (in pixels/tick).
(define (make-world speed)
  (new World% [s speed]))

;; run : PosNum PosInt -> World%
;; GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;; creates and runs a world.  Returns the final state of the world.
(define (run frame-rate speed)
  (big-bang (make-world speed)
            (on-tick 
             (lambda(w) (send w on-tick))
             frame-rate)
            (on-draw
             (lambda(w) (send w on-draw)))
            (on-key
             (lambda(w kev) (send w on-key kev)))
            (on-mouse
             (lambda(w x y mev) (send w on-mouse x y mev)))))

;; make-square-toy : PosInt PosInt PosInt -> SquareToy%
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;; travelling right at the given speed.
(define (make-square-toy x y speed)
  (new SquareToy% [x x] [y y] [s speed]))


;; make-circle-toy : PosInt PosInt -> CircleToy%
;; GIVEN: an x and a y position
;; RETURNS: an object represeenting a circle toy at the given position.
(define (make-circle-toy x y)
  (new CircleToy% [x x] [y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interfaces:

(define World<%>
  (interface ()

    ;; -> World<%>
    ;; Returns the World<%> that should follow this one after a tick
    on-tick                             

    ;; Integer Integer MouseEvent -> World<%>
    ;; Returns the World<%> that should follow this one after the
    ;; given MouseEvent
    on-mouse

    ;; KeyEvent -> World<%>
    ;; Returns the World<%> that should follow this one after the
    ;; given KeyEvent
    on-key

    ;; -> Scene
    ;; Returns a Scene depicting this world
    ;; on it.
    on-draw 
    
    ;; -> Integer
    ;; RETURN: the x and y coordinates of the target
    target-x
    target-y

    ;; -> Boolean
    ;; Is the target selected?
    target-selected?

    ;; -> ListOfToy<%>
    get-toys

))

(define Toy<%> 
  (interface ()

    ;; -> Toy<%>
    ;; returns the Toy that should follow this one after a tick
    on-tick                             

    
    ;; Scene -> Scene
    ;; Returns a Scene like the given one, but with this toy drawn
    ;; on it.
    add-to-scene

    ;; -> Int
    toy-x
    toy-y

    ;; -> ColorString
    ;; returns the current color of this toy
    toy-color

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASSES

;; A World% is a 
;; (new World% [s PosNum] [target Target%] [toys ListOf<Toy%>])
;; RETPRESENTS: A world state.
(define World%
  (class* object% (World<%>)
    (init-field s                     ; toys' travel speed
                [target (new Target%  ; Target circle
                             [x HALF-CANVAS-WIDTH] 
                             [y HALF-CANVAS-HEIGHT])]
                [toys empty])  ; Toys
    
    
    
    (super-new)
                
    ;; -> World%
    ;; Returns the World% that should follow this one after a tick
    (define/public (on-tick)
      (new World% 
           [s s]
           [target target]
           [toys (map
                  (lambda(elt) (send elt on-tick))
                  toys)]))

    ;; Integer Integer MouseEvent -> World<%>
    ;; Returns the World<%> that should follow this one after the
    ;; given MouseEvent
    ;; DETAIL: toys are not responsive to mouse event.
    (define/public (on-mouse x y mev)
      (new World% 
           [s s]
           [target (send target on-mouse x y mev)]
           [toys toys]))

    ;; KeyEvent -> World<%>
    ;; Returns the World<%> that should follow this one after the
    ;; given KeyEvent.
    ;; STRATEGY: Cases on kev : KeyEvent
    (define/public (on-key kev)
      (cond
       [(key=? kev "s") 
        (new World% 
             [s s]
             [target target]
             [toys(cons (new SquareToy% 
                             [x (send this target-x)]
                             [y (send this target-y)]
                             [s s])
                        toys)])]
       [(key=? kev "c") 
        (new World% 
             [s s]
             [target target]
             [toys (cons (new CircleToy%
                              [x (send this target-x)]
                              [y (send this target-y)])
                         toys)])]
       [else this]))

    ;; -> Scene
    ;; Returns a Scene depicting this world
    ;; on it.
    (define/public (on-draw)
      (local
        ((define scene-with-target (send target add-to-scene EMPTY-CANVAS)))
        (foldr
         ; Toy% Scene -> Scene
         ; RETURN: A scene like the given one, but with the given toy
         ; depicted on it.
         (lambda(elt s) (send elt add-to-scene s))
         scene-with-target
         toys)))
    
    ;; -> Integer
    ;; RETURN: the x or y coordinates of the target
    (define/public (target-x)
      (send target get-x))
    (define/public (target-y)
      (send target get-y))

    ;; -> Boolean
    ;; Is the target selected?
    (define/public (target-selected?)
      (send target get-selected?))

    ;; -> ListOfToy<%>
    ;; RETURNS: the list of toys of this.
    (define/public (get-toys)
      toys)
    
    ))



;; A Target% is a 
;; (new Target% [x NonNegInt] [y NonNegInt] [s? Boolean] 
;;      [x-dist Integer] [y-dist Integer])
;; REPRESENTS: A target circle
(define Target%
  (class* object% ()
    (init-field x  ; x position
                y  ; y position
                [s? false]) ; selected state
    (init-field [x-dist 0]  ; the x-dist from mouse's location to this.
                [y-dist 0]) ; the y-dist from mouse's location to this.
    
    (field [r 10]        ; radius
           [m "outline"] ; mode
           [c "black"])  ; color string
    (field [IMG (circle r m c)])
    
    (super-new)
    
    ;; Integer Integer MouseEvent -> Target%
    ;; Returns the Target% that should follow this one after the
    ;; given MouseEvent
    ;; STRATEGY: Cases on mev : MouseEvent
    (define/public (on-mouse x-pos y-pos mev)
      (cond
        [(mouse=? mev "button-down")  
         (target-after-button-down x-pos y-pos)]
        [(mouse=? mev "button-up")
         (target-after-button-up x-pos y-pos)]
        [(mouse=? mev "drag")
         (target-after-drag x-pos y-pos)]
        [else this]))
    
    ;; target-after-button-down: Integer Integer -> Target%
    ;; GIVEN: the location of a mouse event.
    ;; RETURNS: the target that should follow this one after a 
    ;; button-down at the given location.
    (define/public (target-after-button-down x-pos y-pos)
      (if (send this in-target? x-pos y-pos)
          (new Target% [x x] [y y] [s? true] [x-dist (- x x-pos)]
               [y-dist (- y y-pos)])
          this))
    
    ;; target-after-button-up: Integer Integer -> Target%
    ;; GIVEN: the location of a mouse event.
    ;; RETURNS: the target that should follow this one after a 
    ;; button-down at the given location.
    (define/public (target-after-button-up x-pos y-pos)
      (new Target% [x x] [y y] [s? false]))
    
    ;; target-after-drag: Integer Integer -> Target%
    ;; GIVEN: the location of a mouse event.
    ;; RETURNS: the target that should follow this one after a 
    ;; button-down at the given location.
    (define/public (target-after-drag x-pos y-pos)
      (if s?
          (new Target%
               [x (+ x-pos x-dist)]
               [y (+ y-pos y-dist)]
               [s? true]
               [x-dist x-dist]
               [y-dist y-dist])
          this))
    
    
    
    ;; in-target? : Integer Integer -> Boolean
    ;; GIVEN: the coordinates of a position.
    ;; RETURNS: true iff the position is in this.
    (define/public (in-target? x-pos y-pos)
      (<= (+ (sqr (- x x-pos))
             (sqr (- y y-pos)))
          (sqr r)))

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: A Scene like the given one, but with this target 
    ;; depicted on it.
    (define/public (add-to-scene scene0)
      (place-image IMG x y scene0))
    
    
    
    ;; -> Integer
    ;; RETURNS: the x or y coordinates.
    (define/public (get-x)
      x)
    (define/public (get-y)
      y)
    
    ;; -> Boolean
    ;; RETURNS: True iff this is selected.
    (define/public (get-selected?)
      s?)
    
    ;; target-x-dist -> Integer
    ;; target-y-dist -> Integer
    ;; RETURNS: the distant from mouse location to this target center.
    (define/public (get-x-dist)
      x-dist)
    
    (define/public (get-y-dist)
      y-dist)
    
    ))

;; A CircleToy% is a
;; (new CircleToy% [x NonNegInt] [y NonNegInt] 
;;                 [color String] [ticks PosInt])
;; REPRESENTS: A circle toy
(define CircleToy%
  (class* object% (Toy<%>)
    (init-field x      ; Center's x position
                y)      ; Center's y position
    (init-field [c "green"]) ; Color
    (init-field [ticks 5])   ; Ticks until color change
    
    (field [r 5]        ; radius
           [m "solid"]) ; mode
    (field [IMG (circle r m c)])
    
    (super-new)
    
    ;; -> Toy%
    ;; returns the Toy that should follow this one after a tick
    (define/public (on-tick)
      (if (zero? ticks)
          (new CircleToy% 
               [x x]
               [y y] 
               [c (send this color-toggle c)]
               [ticks 5])
          (new CircleToy% 
               [x x]
               [y y] 
               [c c]
               [ticks (- ticks 1)])))
    
    ;; color-goggle : String -> String
    ;; GIVEN: a color string.
    ;; RETURNS: the toggled color string("green" -> "red", 
    ;; "red" -> "green")
    (define/public (color-toggle c0)
      (cond
        [(string=? c0 "green") "red"]
        [(string=? c0 "red") "green"]))
      

    ;; Scene -> Scene
    ;; Returns a Scene like the given one, but with this toy drawn
    ;; on it.
    (define/public (add-to-scene scene0)
      (place-image IMG x y scene0))
    

    ;; -> Int
    ;; RETURNS: the x's or y's coordinate of toy's center.
    (define/public (toy-x)
      x)
      
    (define/public (toy-y)
      y)

    ;; -> ColorString
    ;; returns the current color of this toy
    (define/public (toy-color)
      c)
    
    ;; -> Number
    ;; RETURNS: the toys's ticks
    (define/public (toy-ticks)
      ticks)
    
    ))


;; A SquareToy% is a 
;; (new SquareToy% [x NonNegInt] [y NonNegInt] 
;;      [d String][s PosInt)
;; RETPRESENTS: A square toy.
(define SquareToy%
  (class* object% (Toy<%>)
    (init-field x           ; Center's x coordinate
                y           ; Center's y coordinate
                s           ; speed
                [d "right"]); Direction "right" or "left"
                
    
    (field [m "solid"] ; mode
           [c "blue"]  ; Color string
           [l 40])     ; Side length

    (field [IMG (square l m c)]
           [LEFT-BORDER (/ l 2)]
           [RIGHT-BORDER (- CANVAS-WIDTH (/ l 2))])
    
    (super-new)
    
    
    ;; -> Toy%
    ;; returns the Toy that should follow this one after a tick
    (define/public (on-tick)
      (local ((define next-x-coor (send this next-x)))
        (cond
          [(send this out-of-left-border (send this next-x))
           (new SquareToy% [x LEFT-BORDER] [y y] [d "right"] [s s])]
          [(send this out-of-right-border (send this next-x))
           (new SquareToy% [x RIGHT-BORDER] [y y] [d "left"] [s s])]
          [else 
           (new SquareToy% [x next-x-coor] [y y] [d d] [s s])])))
     
    
    
    ;; next-x : -> Integer
    ;; RETURNS: this toy's x coordinate in next tick if it won't reach
    ;; the edge of canvas.
    ;; STRATEGY: Cases on d : Direction
    (define/public (next-x)
      (cond
        [(string=? d "right") (+ x s)]
        [(string=? d "left") (- x s)]))
    
    ;; out-of-left-border : Integer -> Boolean
    ;; out-of-right-border : Integer -> Boolean
    ;; GIVEN: the x coordinate of a position.
    ;; RETURNS: true iff the position will out of canvas's left/right
    ;; edge.
    (define/public (out-of-left-border x-coor)
      (< x-coor LEFT-BORDER))
    
    (define/public (out-of-right-border x-coor)
      (> x-coor RIGHT-BORDER))
         


    
    ;; Scene -> Scene
    ;; Returns a Scene like the given one, but with this toy drawn
    ;; on it.
    (define/public (add-to-scene scene0)
      (place-image IMG x y scene0))

    ;; -> Int
    ;; RETURNS: the x's or y's coordinate of the toy's center.
    (define/public (toy-x)
      x) 
    
    (define/public (toy-y)
      y)

    ;; -> ColorString
    ;; returns the current color of this toy
    (define/public (toy-color)
      c)
    
    ;; -> Direction
    ;; RETURNS: this toy's direction
    (define/public (toy-direction)
      d)
    
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS: 

(define (square-toy-equal? st1 st2)
  (and
   (= (send st1 toy-x)
      (send st2 toy-x))
   (= (send st1 toy-y)
      (send st2 toy-y))
   (string=? (send st1 toy-color)
             (send st2 toy-color))
   (string=? (send st1 toy-direction)
             (send st2 toy-direction))))

(define (circle-toy-equal? ct1 ct2)
  (and
   (= (send ct1 toy-x)
      (send ct2 toy-x))
   (= (send ct1 toy-y)
      (send ct2 toy-y))
   (string=? (send ct1 toy-color)
             (send ct2 toy-color))
   (= (send ct1 toy-ticks)
      (send ct2 toy-ticks))))

(define (target-equal? t1 t2)
  (and
   (= (send t1 get-x)
      (send t2 get-x))
   (= (send t1 get-y)
      (send t2 get-y))
   (equal? (send t1 get-selected?)
           (send t2 get-selected?))
   (= (send t1 get-x-dist)
      (send t2 get-x-dist))
   (= (send t1 get-y-dist)
      (send t2 get-y-dist))))

;; Tests for CircleToy%
(begin-for-test
  (local
    ((define circle-toy-200-250-green-0
       (new CircleToy%
            [x 200]
            [y 250]
            [ticks 0]))
     (define circle-toy-200-250-red-5
       (new CircleToy%
            [x 200]
            [y 250]
            [c "red"]
            [ticks 5])))
    (check circle-toy-equal?
           (send circle-toy-200-250-green-0 on-tick)
           circle-toy-200-250-red-5)))
       
;; Tests for SquareToy%
(begin-for-test
  (local
    
    ;; tests for on-tick
    ((define square-toy-200-250-right-3
       (new SquareToy% 
            [x 200]
            [y 250]
            [s 3]))
     (define square-toy-203-250-right-3
       (new SquareToy%
            [x 203]
            [y 250]
            [s 3])))
    (check square-toy-equal?
           (send square-toy-200-250-right-3
                 on-tick)
           square-toy-203-250-right-3)
    ; reaches right border
    (check square-toy-equal?
           (send (new SquareToy% 
                      [x 380]
                      [y 250]
                      [s 3])
                 on-tick)
           (new SquareToy%
                [x 380]
                [y 250]
                [d "left"]
                [s 3]))
    ; reaches left border
    (check square-toy-equal?
           (send (new SquareToy% 
                      [x 20]
                      [y 250]
                      [d "left"]
                      [s 3])
                 on-tick)
           (new SquareToy%
                [x 20]
                [y 250]
                [d "right"]
                [s 3]))
    ))

;; Tests for Target%
(begin-for-test
  ; tests for on-mouse
  (check target-equal?
         (send(new Target%
                   [x 200]
                   [y 250])
              on-mouse
              195
              245
              "button-down")
         (new Target%
              [x 200]
              [y 250]
              [s? true]
              [x-dist 5]
              [y-dist 5]))
  (check target-equal?
         (send(new Target%
                   [x 200]
                   [y 250]
                   [s? true]
                   [x-dist 20]
                   [y-dist 20])
              on-mouse
              250
              300
              "drag")
         (new Target%
              [x 270]
              [y 320]
              [s? true]
              [x-dist 20]
              [y-dist 20]))
  (check target-equal?
         (send(new Target%
                   [x 200]
                   [y 250]
                   [s? true]
                   [x-dist 20]
                   [y-dist 20])
              on-mouse
              250
              300
              "button-up")
         (new Target%
              [x 200]
              [y 250]
              [s? false]
              [x-dist 0]
              [y-dist 0])))
  
  


