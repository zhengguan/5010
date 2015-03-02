;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; set 07 Q2

;; robot.rkt

(require "extras.rkt")
(require rackunit)
(require "sets.rkt")
(provide path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Position is a (list PosInt PosInt)
;; (x y) represents the position at position x, y.
;; Note: this is not to be confused with the built-in data type Posn.

;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated
;; direction. 

;; A Direction is one of
;; -- "north" ↑
;; -- "east"  →
;; -- "south" ↓
;; -- "west"  ←

;; A Plan is a ListOf<Move>
;; WHERE: the list does not contain two consecutive moves in the same
;; direction. 

;; A Predecessor is a (list Position Position)
;; Interpretation: the second position in Predecessor is the first 
;; position's predecessor(the first position reachable from the second 
;; position).

(define blocks1 (list (list 5 4) (list 5 6)
                      (list 4 5) (list 6 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION DEFINITIONS

;; path : Position Position ListOf<Position> -> Maybe<Plan>>
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. the target position that robot is supposed to reach
;; 3. A list of the blocks on the board
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; EXAMPLES/TESTS: at the end of this file.
#;(begin-for-test
  (check-equal?
   (path (list 1 1)  (list 5 5) blocks1)
   false)
  #;(check-equal?
   (path (list 1 1)  (list 5 5) 
         (list (list 5 4) (list 5 6)
               (list 4 5) (list 6 5)))
   (list (list "south" 5) (list "east" 4)
         (list "north" 1))))
;; STRATEGY:
#;(define (path start end blocks)
  (local
    ((define x-border (+ 1 (apply max (map first blocks))))
     (define y-border (+ 1 (apply max (map (lambda(p) (first (rest p)))
                                           blocks))))
    ; Position PositionSet PositionSet -> Maybe<Plan>
    ; GIVEN: a Position and two PositionSets
    ; WHERE: current is the current position , reached is the set of
    ; position that is reachable, prev-path is the list of
    ; positions from start to current.
    ; STRATEGY: General Recursion
    ; TERMINATION ARGUMENT: the number of posiitons that are not in
    ; reached.     
     (define (path-loop current reached prev-path)
       (local ((define reachable-positions (reachable current))
               (define diff (set-diff reachable-positions reached))
               (define new-reached (append diff reached)))
         (cond
           [(empty? diff) false]
           [(my-member? end diff) (cons end prev-path)]
           [else 
            (local ((define part-path (positions-to-position diff new-reached)))
              (if (false? part-path)
                  false
                  (append part-path prev-path)))])))
     ; Position -> PositionSet
     ; RETURNS: the set of positions that are reachable from the given
     ; position.
     (define (reachable p)
       (local ((define x (first p))
               (define y (first (rest p))))
         (append (reachable-north x y)
                 (reachable-east x y)
                 (reachable-south x y)
                 (reachable-east x y))))
     ; NonNegInt -> PositinoSet
     ; GIVEN: the coordinates of a position
     ; RETURNS: the set of positions that are reachable from p in the
     ; given specified direction.
     (define (reachable-north x y)
       (local ((define p (list x y)))
         (if (or (my-member? p blocks)
                 (y-out-of-border? y))
             empty
             (cons p (reachable-north x (- y 1))))))
     (define (reachable-east x y)
       (local ((define p (list x y)))
         (if (or (my-member? p blocks)
                 (x-out-of-border? x))
             empty
             (cons p (reachable-east (+ x 1) y)))))
     (define (reachable-south x y)
       (local ((define p (list x y)))
         (if (or (my-member? p blocks)
                 (y-out-of-border? x))
             empty
             (cons p (reachable-south x (+ y 1))))))
     (define (reachable-west x y)
       (local ((define p (list x y)))
         (if (or (my-member? p blocks)
                 (x-out-of-border? x))
             empty
             (cons p (reachable-west (- x 1) y)))))
     ; NonNegInt -> Boolean
     ; GIVEN: the x/y coordinate of a position
     ; RETURNS: true iff it is out of border.(the north and west 
     ; border are 1, the south and east border are the greatest y
     ; and x coordinate of positions in blocks plus 1.
     ; STRATEGY: Function Composition 
     (define (x-out-of-border? x)
       (not (<= 1 x x-border)))
     (define (y-out-of-border? y)
       (not (<= 1 y y-border)))                              
     
     ; PositionSet PositionSet -> Maybe<Plan>
     ; GIVEN: a set of positions ps and a set of positions that are 
     ; already reached .
     ; RETURNS: a plan if there exists from one element of the ps to end.
     ; STRATEGY: Structural Decomposition on ps : PositionSet
     (define (positions-to-position ps reached)
       (local ((define first-elt (first ps)))
         (cond
           [(empty? ps) false]
           [(my-member? first-elt reached)
            (positions-to-position (rest ps) reached)]
           [else
            (local
              ((define maybe-path
                 (path-loop first-elt reached (list first-elt))))
              (if(false? maybe-path)
                 (positions-to-position (rest ps) reached)
                 maybe-path))]))))
    (path-loop start (list start) (list start))))




(define (path start end blocks)
  (local 
    ((define x-border 
        (+ 1 (apply max (map
                         first
                         (cons start (cons end blocks))))))
     (define y-border 
       (+ 1 (apply max (map 
                        (lambda(p)(first (rest p)))
                        (cons start (cons end blocks))))))
     ; PositionSet PositinoSet ListOf<Predecessor> -> Plan     
     (define (path-loop waiting explored predecessors)
       (cond
         [(empty? waiting) false]
         [else 
          (local ((define first-elt (first waiting))
                  (define reachable-positions 
                    (reachable first-elt))
                  (define diff 
                    (set-diff
                     (set-diff reachable-positions
                               explored)
                     waiting)))            
            (cond
              [(empty? diff)
               (path-loop (rest waiting)
                          (cons first-elt explored)
                          predecessors)]
              [(my-member? end diff)
               (produce-plan start end
                             (cons (list end first-elt)
                                   predecessors))]
              [else (path-loop (append (rest waiting)
                                       diff)
                               (cons first-elt explored)
                               (append predecessors
                                       (map 
                                        (lambda(p) (list p first-elt))
                                        diff)))]))]))
;(define blocks empty)
;(define x-border 2)
;(define y-border 3)
     ; Position -> PositionSet
     ; RETURNS: the set of positions that are reachable from the given
     ; position.
     (define (reachable p)
       (local ((define x (first p))
               (define y (first (rest p))))
         (append (reachable-north x (- y 1))
                 (reachable-east (+ x 1) y)
                 (reachable-south x (+ y 1))
                 (reachable-west (- x 1) y))))
     ; NonNegInt -> PositinoSet
     ; GIVEN: the coordinates of a position
     ; RETURNS: the set of positions that are reachable from p in the
     ; given specified direction.
     (define (reachable-north x y)
       (local ((define p (list x y)))
         (if (or (my-member? p blocks)
                 (y-out-of-border? y))
             empty
             (cons p (reachable-north x (- y 1))))))
     (define (reachable-east x y)
       (local ((define p (list x y)))
         (if (or (my-member? p blocks)
                 (x-out-of-border? x))
             empty
             (cons p (reachable-east (+ x 1) y)))))
     (define (reachable-south x y)
       (local ((define p (list x y)))
         (if (or (my-member? p blocks)
                 (y-out-of-border? y))
             empty
             (cons p (reachable-south x (+ y 1))))))
     (define (reachable-west x y)
       (local ((define p (list x y)))
         (if (or (my-member? p blocks)
                 (x-out-of-border? x))
             empty
             (cons p (reachable-west (- x 1) y)))))
     ; NonNegInt -> Boolean
     ; GIVEN: the x/y coordinate of a position
     ; RETURNS: true iff it is out of border.(the north and west 
     ; border are 1, the south and east border are the greatest y
     ; and x coordinate of positions in blocks plus 1.
     ; STRATEGY: Function Composition 
     (define (x-out-of-border? x)
       (not (<= 1 x x-border)))
     (define (y-out-of-border? y)
       (not (<= 1 y y-border))))
    (if(equal? start end)
       empty
       (path-loop (list start) empty empty))))


;; produce-plan : Position Position ListOf<Predecessor>
;; GIVEN: two positions start and end, a list of predecessor.
;; RETURNS: a plan from the start to end.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (produce-plan (list 1 1) (list 5 5)
                 (list (list (list 5 5) (list 5 4))
                       (list (list 5 4) (list 3 4))
                       (list (list 3 4) (list 3 1))
                       (list (list 3 1) (list 1 1))))
   (list (list "east" 2) (list "south" 3) 
         (list "east" 2) (list "south" 1))))
;; STRATEGY: Function Composition
(define (produce-plan start end predecessors)
  (simplify-plan 
   (path-to-plan
    (produce-path start end predecessors))))

;; simplify-plan : ListOf<Move> -> ListOf<Move>
;; GIVEN: a list of moves.
;; RETURNS: the simplified plan of the given one.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (simplify-plan (list (list "north" 1) (list "north" 2)))
   (list (list "north" 3)))
  (check-equal?
   (simplify-plan (list (list "north" 1) (list "south" 3)))
   (list (list "south" 2)))
  (check-equal?
   (simplify-plan (list (list "north" 1) (list "east" 2) 
                        (list "south" 3)))
   (list (list "north" 1) (list "east" 2) 
         (list "south" 3))))
;; STRATEGY: General Recursion 
;; TERMINATION ARGUMENT: the length of plan.
(define (simplify-plan plan)
  (cond
    [(empty? (rest plan)) plan]
    [else
     (local ((define first-move (first plan))
             (define second-move (first (rest plan))))
       (if (same-line? first-move second-move)
           (simplify-plan
            (cons (combine-move first-move second-move)
                  (rest (rest plan))))
           (cons (first plan) (simplify-plan (rest plan)))))]))

;; same-line? : Move Move -> Boolean
;; GIVEN: two moves.
;; RETURNS: true iff the given two moves' directions are along the
;; same line.
;; EXAMPLES/TEST:
(begin-for-test
  (check-equal?
   (same-line? (list "south" 1) (list "south" 2))
   true)
  (check-equal?
   (same-line? (list "south" 1) (list "north" 2))
   true)
  (check-equal?
   (same-line? (list "east" 1) (list "east" 2))
   true)
  (check-equal?
   (same-line? (list "east" 1) (list "west" 2))
   true)
  (check-equal?
   (same-line? (list "north" 1) (list "east" 1))
   false)
  (check-equal?
   (same-line? (list "south" 1) (list "west" 1))
   false))
;; STRATEGY: Structural Decomposition on Move
(define (same-line? m1 m2)
  (local ((define direction1 (first m1))
          (define direction2 (first m2)))
    (cond
      [(and(or(string=? direction1 "south")(string=? direction1 "north"))
           (or(string=? direction2 "south")(string=? direction2 "north")))
       true]
      [(and(or(string=? direction1 "west")(string=? direction1 "east"))
           (or(string=? direction2 "west")(string=? direction2 "east")))
       true]
      [else false])))


;; combine-move : Move Move -> Move
;; GIVEN: two moves.
;; WHERE: their directions are along the same line.
;; RETURNS: the combined move.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (combine-move (list "north" 1) (list "north" 2))
   (list "north" 3))
  (check-equal?
   (combine-move (list "north" 1) (list "south" 3))
   (list "south" 2))
  (check-equal?
   (combine-move (list "east" 1) (list "east" 1))
   (list "east" 2))
  (check-equal?
   (combine-move (list "west" 1) (list "west" 1))
   (list "west" 2))
  (check-equal?
   (combine-move (list "east" 2) (list "west"1))
   (list "east" 1)))
;; STRATEGY: Structural Decomposition on Move
(define (combine-move m1 m2)
  (local ((define direction1 (first m1))
          (define direction2 (first m2))
          (define distance1 (first (rest m1)))
          (define distance2 (first (rest m2))))
    (cond
      [(string=? direction1 direction2)
       (list direction1 (+ distance1 distance2))]
      [(> distance1 distance2)
       (list direction1 (- distance1 distance2))]
      [(> distance2 distance1)
       (list direction2 (- distance2 distance1))])))
  
   
                  

;; path-to-plan : PositionSet -> ListOf<Move>
;; GIVEN: the list of positions of a path.
;; RETURNS: the corresponding plan.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (path-to-plan (list (list 1 1 ) (list 3 1) (list 3 4)
                       (list 5 4) (list 5 5)))
   (list (list "east" 2) (list "south" 3)
         (list "east" 2) (list "south" 1))))
;; STRATEGY: General Recursion
;; TERMINATION ARGUMENT: the length of ps
(define (path-to-plan ps)
  (cond
    [(empty? (rest ps)) empty]
    [else 
     (local ((define from (first ps))
             (define to (first (rest ps))))
       (cons (positions-to-move from to)
             (path-to-plan (rest ps))))]))


;; positions-to-move : Position Position -> Move
;; GIVEN: two Positions.
;; RETURNS: the move from the first positions to the second position.
;; EXAMPLES/TESTS:
(begin-for-test
 (check-equal?
  (positions-to-move (list 2 2) (list 2 1))
  (list "north" 1))
 (check-equal?
  (positions-to-move (list 2 2) (list 2 3))
  (list "south" 1))
 (check-equal?
  (positions-to-move (list 2 2) (list 1 2))
  (list "west" 1))
 (check-equal?
  (positions-to-move (list 2 2) (list 3 2))
  (list "east" 1)))
;; STRATEGY: Structural Decomposition on Position
(define (positions-to-move from to)
  (distance-to-move 
   (- (first to) (first from))
   (- (first (rest to)) 
      (first (rest from)))))

;; distance-to-move : Integer Integer -> Move
;; GIVEN: the x/y distances of two positions.
;; RETURNS: the corrresponding move from one position to another.
;; EXAMPLES/TESTS: see function positions-to-move.
;; STRATEGY: Cases on Integer
(define (distance-to-move x-dist y-dist)
  (cond
    [(and (zero? x-dist) (negative? y-dist))
     (list "north" (abs y-dist))]
    [(and (zero? x-dist) (positive? y-dist))
     (list "south" y-dist)]
    [(and (zero? y-dist) (negative? x-dist))
     (list "west" (abs x-dist))]
    [(and (zero? y-dist) (positive? x-dist))
     (list "east" x-dist)]))


;; produce-path : Position Position ListOf<Predecessor>
;; GIVEN: two positions start and end, a list of predecessor.
;; RETURNS: a path from the start to end.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (produce-path (list 1 1) (list 5 5)
                 (list (list (list 5 5) (list 5 4))
                       #;(list (list 4 4) (list 3 4))
                       (list (list 5 4) (list 3 4))
                       (list (list 3 4) (list 3 1))
                       (list (list 3 1) (list 1 1))))
   (list (list 1 1) (list 3 1) (list 3 4) (list 5 4) (list 5 5))))
;; STRATEGY: General Recursion
;; TERMINATION ARGUMENT: the path from start to end.
(define (produce-path start end predecessors)
  (local    
    ((define (produce-path-loop end)
       (local 
         ((define predecessor (search-predecessor end predecessors)))
         (cond 
           [(equal? predecessor start)
            (cons start empty)]
           [else (cons predecessor 
                       (produce-path-loop predecessor))]))))
    (reverse (cons end (produce-path-loop end)))))

;; search-predecessor : Position ListOf<Predecessor>
;; GIVEN: a Position and a list of predecessor.
;; RETURNS: the given position's predecessor.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (search-predecessor (list 5 4)
                       (list (list (list 5 5) (list 5 4))
                             #;(list (list 4 4) (list 3 4))
                             (list (list 5 4) (list 3 4))
                             (list (list 3 4) (list 3 1))
                             (list (list 3 1) (list 1 1))))
   (list 3 4)))
;; STRATEGY: Structural Decomposition on ListOf<Predecessor> 
;; and Predecessor.
(define (search-predecessor position predecessors)
  (local
    ((define first-pred (first predecessors)))
    (cond
      [(equal? position (first first-pred))
       (first (rest first-pred))]
      [else (search-predecessor position (rest predecessors))])))
    
    
;; set-diff : Set Set -> Set
;; GIVEN: two sets
;; RETURNS: the difference between the first set and the second set.
;; STRATEGY:HOFC
(define (set-diff s1 s2)
  (filter
   (lambda(elt) (not (my-member? elt s2)))
   s1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS
#; (path (list 1 1) (list 1 1) empty)
#;(path (list 1 1)  (list 5 5) 
      (list (list 5 4) (list 5 6)
            (list 4 5) (list 6 5)))
#;(path (list 1 1) (list 1 2) empty) 
#;(path (list 1 1) (list 4 2) 
        (list (list 4 1) (list 3 2)
              (list 2 3)))
#; (path (list 1 1) (list 3 2)
         (list (list 2 1) (list 2 2)
               (list 2 3) (list 3 3)))
               
