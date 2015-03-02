;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname obstacles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; set 07 Q1

;; obstacles.rkt
(require "extras.rkt")
(require rackunit)
(require "sets.rkt")
(provide position-set-equal?
         obstacle?
         blocks-to-obstacles)



(define-binary-check (check-set-equal? s1 s2)
  (set-equal? s1 s2))

(define-binary-check (check-set-set-equal? ss1 ss2)
  (set-set-equal? ss1 ss2))


;; set-set-equal? : SetSet SetSet -> Boolean
;; RETRUSN: true iff the given two SetSet equal
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (set-set-equal?
    (list (list 1 2 3) 
          (list 4 5 6))
    (list (list 6 5 4)
          (list 3 2 1)))
   true)
  (check-equal?
   (set-set-equal?
    (list (list 1 2 3) 
          (list 4 5 6))
    (list (list 6 5 4)
          (list 3 2 1)
          empty))
   false))
;; STRATEGY: HOFC
(define (set-set-equal? ss1 ss2)
  (and (set-set-contain? ss1 ss2)
       (set-set-contain? ss2 ss1)))

;; set-set-contain? : SetSet SetSet -> Boolean
;; GIVEN: two SetSets ss1 and ss2.
;; RETURNS: true iff ss1 contains ss2.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (set-set-contain?
    (list (list 1 2 3) 
          (list 4 5 6))
    (list (list 6 5 4)
          (list 3 2 1)))
   true)
  (check-equal?
   (set-set-contain?
    (list (list 1 2 3) 
          (list 4 5 6))
    (list (list 6 5 4)
          (list 3 2 1)
          empty))
   false))
(define (set-set-contain? ss1 ss2)
  (andmap
   ; SetOf<X> -> Boolean
   ; GIVEN: a set
   ; RETURNS: true iff ss1 contains a set equals to the given set.
   (lambda(s2)
     (ormap
      (lambda(s1) (set-equal? s2 s1))
      ss1))
   ss2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Position is a (list PosInt PosInt)
;; (x y) represents the position x, y.
;; Note: this is not to be confused with the built-in data type Posn.

;; A PositionSet is a list of positions without duplication.

;; A PositionSetSet is a list of PositionSets without duplication,
;; that is, no two position-sets denote the same set of positions.





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION DEFINITIONS

;; position-set-equal? : PositionSet PositionSet -> Boolean
;; GIVEN: two PositionSets
;; RETURNS: true iff they denote the same set of positions.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (position-set-equal? (list (list 1 2) (list 1 3))
                        (list (list 1 3) (list 1 2)))
   true)
  (check-equal?
   (position-set-equal? (list (list 1 2) (list 1 3))
                        (list (list 1 3) (list 1 2) (list 1 4)))
   false))
;; STRATEGY: Function Composition
#;(define (position-set-equal? ps1 ps2)
  (and (position-set-contain? ps1 ps2)
       (position-set-contain? ps2 ps1)))

(define (position-set-equal? ps1 ps2)
  (set-equal? ps1 ps2))



;; obstacle? : PositionSet -> Boolean
;; GIVEN: a PositionSet
;; RETURNS: true iff the set of positions would be an obstacle if they
;; were all occupied and all other positions were vacant.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (obstacle? (list (list 1 2) (list 2 3) (list 3 2) (list 3 4)))
   true)
  (check-equal?
   (obstacle? (list (list 1 2) (list 2 3) (list 3 2) 
                    (list 3 4) (list 4 4)))
   false))
;; STRATEGY: Function Composition
(define (obstacle? ps)
  (andmap
   (lambda(p) (contain-adjacent-position? ps p))
   ps))

;; contain-adjacent-position? : PositionSet Position -> Boolean
;; GIVEN: a PositionSet ps, a Position p.
;; RETURNS: true iff ps contains a Position that is adjacent to p.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (contain-adjacent-position? (list (list 1 2) (list 2 3) 
                                     (list 3 2) (list 3 4))
                               (list 1 2))
   true)
  (check-equal?
   (contain-adjacent-position? (list (list 1 2) (list 2 3) 
                                     (list 3 2) (list 3 4))
                               (list 3 3))
   false))
;; STRATEGY: HOFC
(define (contain-adjacent-position? ps p)
  (ormap
   ; Position -> Boolean
   ; GIVEN: a position elt
   ; RETURNS: true iff elt is adjacent to p
   (lambda(elt) (position-adjacent? elt p))
   ps))

;; position-adjacent? : Position Position -> Boolean
;; GIVEN: two Positions
;; RETURNS: true iff the giventwo Positions are adjacent.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (position-adjacent? (list 1 2) (list 2 3))
   true)
  (check-equal?
   (position-adjacent? (list 1 2) (list 1 3))
   false))
;; STRATEGY: Structural Decomposition on Position
(define (position-adjacent? p1 p2)
  (and (integer-adjacent? (first p1) (first p2))
       (integer-adjacent? (first (rest p1))
                          (first (rest p2)))))

;; integer-adjacent? : Integer Integer -> Boolean
;; GIVEN: two integers.
;; RETURNS: true iff the given two integers are adjacent.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (integer-adjacent? 1 2)
   true)
  (check-equal?
   (integer-adjacent? 1 3)
   false))
;; STRATEGY: Function Composition
(define (integer-adjacent? i1 i2)
  (local ((define dif (- i1 i2)))
    (or (= dif 1) (= dif -1))))


;; blocks-to-obstacles : PositionSet -> PositionSetSet
;; GIVEN: the set of occupied positions on some chessboard
;; RETURNS: the set of obstacles on that chessboard.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-set-set-equal?
   (blocks-to-obstacles (list (list 1 2) (list 1 3) (list 2 3)
                              (list 3 2) (list 3 4) (list 4 1)
                              (list 4 4)))
   (list (list (list 1 2) (list 2 3)
               (list 3 2) (list 3 4) (list 4 1))
         (list (list 1 3))
         (list (list 4 4)))))
;; STRATEGY: Function Composition
(define (blocks-to-obstacles ps)
  (blocks-to-obstacles-loop ps empty))


;; blocks-to-obstacles-loop 
;; : PositionSet PositionSetSet -> PositionSetSet
;; GIVEN: a set of occupied positions on some chessboard, a set of
;; positions that have been added to obstacles ob.
;; RETURNS: a set of obstacles contain positions that have not been
;; added to ob.
;; EXAMPLES/TESTS: see function blocks-to-obstacles
(begin-for-test
  (check-set-set-equal?
   (blocks-to-obstacles-loop (list (list 1 2) (list 1 3) (list 3 2)  
                                   (list 2 3) (list 3 4) (list 4 1)
                                   (list 4 4))
                             empty)
   (list (list (list 1 2) (list 2 3)
               (list 3 2) (list 3 4) (list 4 1))
         (list (list 1 3))
         (list (list 4 4)))))
;; STRATEGY: General Recursion 
;; HALTING MEASURE: the length of blocks.
(define (blocks-to-obstacles-loop blocks already-added)
  (cond
    [(empty? blocks) empty]
    ; if already added to obstacle, then the obstacle that contains 
    ; this block is already calculated.
    [(my-member? (first blocks) already-added) 
     (blocks-to-obstacles-loop (rest blocks) already-added)]
    [else 
     (local ((define obstacle 
               (block-to-obstacle (first blocks) (rest blocks))))
       (cons obstacle (blocks-to-obstacles-loop
                       (rest blocks)
                       (append obstacle already-added))))]))
;; TERMINATION ARGUMENT: 
;; At the recursive call, blocks is 1 less than original.


;; block-to-obstacle : Position PositionSet -> PositionSet
;; GIVEN: a occupied Position p on chessboard and a set of position 
;; on that chessboard.
;; RETURNS: the obstacle that contains p.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-set-equal?
   (block-to-obstacle (list 1 2) empty)
   (list (list 1 2)))
  (check-set-equal?
   (block-to-obstacle (list 1 2) (list (list 2 1)))
   (list (list 2 1) (list 1 2)))
  (check-set-equal?
   (block-to-obstacle (list 3 4)
                      (list (list 1 2) (list 1 3) (list 3 2)  
                            (list 2 3) (list 4 1) (list 4 4)))
   (list (list 1 2) (list 3 2) (list 3 4)
         (list 2 3) (list 4 1))))
;; STRATEGY: Function Composition
(define (block-to-obstacle p blocks)
  (local 
    ; PositinSet -> PositionSet
    ; GIVEN: a PositionSet positions
    ; RETURNS: the set of positions in blocks that are adjacent to
    ; positions in positions.
    ((define (positions-to-adjacents positions)
            (foldr
             set-union
             empty
             (map
              (lambda(position) 
                (filter 
                 (lambda(block) 
                   (position-adjacent? position block))
                 blocks))
              positions)))
     ; PositionSet PositionSet -> PositionSet
     ; GIVEN: the positions had been added to obstacle most recently 
     ; and the temporary obstacle.
     ; RETURNS: the final obstacle.
     ; STRATEGY: General Recursion
     ; HALTING MEASURE: the number of positions that are in blocks and
     ; not in obstacle.
     (define (positions-to-obstacle newst obstacle)
       ; Why we only need to calculate adjacents of newst? What's the 
       ; meaning of newst? What's the meaning of obstacle?
       ; obstacle contains positions 
       ; whose adjacents had been 
       ; calculated and these adjacents are either in newst or in 
       ; obstacle. 
       (local ((define adjacents (positions-to-adjacents newst))
               (define new-obstacle (append newst obstacle))
               (define diff (set-diff adjacents new-obstacle)))
         (if (empty? diff)
             new-obstacle
             (positions-to-obstacle diff
                                     new-obstacle)))))
    (positions-to-obstacle (list p) empty )))
        

;; set-diff : Set Set -> Set
;; RETURNS: the difference between the first set and the second set.
;; EXAMPLES/TESTS: 
(begin-for-test
  (check-set-equal?
   (set-diff
    (list 1 2 3)
    (list 4 5 6))
   (list 1 2 3))
  (check-set-equal?
   (set-diff
    (list 1 2 3)
    (list 3 4 5))
   (list 1 2)))
;; STRATEGY: HOFC
(define (set-diff set1 set2)
  (filter
    (lambda (x) (not (my-member? x set2)))
    set1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;