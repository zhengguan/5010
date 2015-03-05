;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname river) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;; set 08
;; Q3
;; river.rkt
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(provide list-to-pitchers
         pitchers-to-list
         pitchers-after-moves
         make-move
         move-src
         move-tgt
         solution
         ;; added for Q3
         make-fill
         make-dump
         move?
         fill?
         dump?
         fill-pitcher
         dump-pitcher)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Pitcher is a (list PosInt NonNegInt)
;; Interpretation:
;; the PosInt is the Pitcher's capacity, the NonNegInt is the 
;; Pitcher's content.

;; A Pitchers is a ListOf<Pitcher>.

;; PitchersExternalRep ::= ((capacity1 contents1)
;;                          (capacity2 contents2)
;;                          ...
;;                          (capacity_n contents_n))
;; WHERE:  n >=1, and for each i, 0 <= contents_i <= capacity_i
;; INTERPRETATION: the list of pitchers, from 1 to n, with their 
;; contents and capacity
;; EXAMPLE: ((10 5) (8 7)) is a list of two pitchers.  The first has
;; capacity 10 and currently holds 5; the second has capacity 8 and
;; currently holds 7.


(define-struct move (src tgt))
(define-struct fill (i))
(define-struct dump (i))

;; A Move is one of
;; -- (make-move i j)    --pour the contents of pitcher i into pitcher j
;; -- (make-fill i)      --fill pitcher i from the river
;; -- (make-dump i)      --dump the contents of pitcher i into the river.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION DEFINITIONS

;; list-to-pitchers : PitchersExternalRep -> PitchersInternalRep
;; RETURNS: your internal representation of the given input.
(define (list-to-pitchers per) per)

;; pitchers-to-list : PitchersInternalRep -> PitchersExternalRep
;; GIVEN: an internal representation of a set of pitchers
;; RETURNS: a PitchersExternalRep that represents them.
(define (pitchers-to-list pir) pir)

;; pitcher-capacity : Pitcher -> PosInt
;; pitcher-content : Pitcher -> NonNegInt
;; RETURNS: the given pitcher's capacity/content.
;; EXAMPLES/TETSS:
(begin-for-test
  (check-equal?
   (pitcher-capacity (list 1 2))
   1)
  (check-equal?
   (pitcher-content (list 1 2))
   2))
;; STRATEGY: Structural Decomposition on p : Pitcher
(define (pitcher-capacity p)
  (first p))
(define (pitcher-content p)
  (first (rest p)))

;; pitchers-after-moves
;; : PitchersInternalRep ListOf<Move> -> PitchersInternalRep
;; GIVEN: An internal representation of a set of pitchers, and a sequence
;; of moves
;; WHERE: every move refers only to pitchers that are in the set of pitchers.
;; RETURNS: the internal representation of the set of pitchers that should
;; result after executing the given list of moves, in order, on the given
;; set of pitchers.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (pitchers-after-moves
    (list (list 8 8) (list 5 0) (list 3 0))
    (list (make-move 1 2) (make-move 2 3) (make-move 3 1)
          (make-move 2 3) (make-move 1 2) (make-move 2 3)))
   (list (list 8 1) (list 5 4) (list 3 3))))
;; STRATEGY: HOFC
(define (pitchers-after-moves pitchers moves)
  (foldl
   ; Move Pitchers -> Pitchers
   ; GIVEN: a list of Pitchers and a Move.
   ; RETURNS: the internal representation of the set of pitchers that
   ; should result after executing the given move.
   (lambda(elt old-pitchers) 
     (pitchers-after-move old-pitchers elt))
   pitchers
   moves))
                  
;; pitchers-after-move :
;; : PitchersInternalRep Move -> PitchersInternalRep
;; GIVEN: An internal representation of a set of pitchers, and a move.
;; WHERE: every move refers only to pitchers that are in the set of pitchers.
;; RETURNS: the internal representation of the set of pitchers that should
;; result after executing the given move.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (pitchers-after-move 
    (list (list 8 8) (list 5 0) (list 3 0))
    (make-move 1 2))
   (list (list 8 3) (list 5 5) (list 3 0)))
  (check-equal?
   (pitchers-after-move 
    (list (list 8 3) (list 5 5) (list 3 0))
    (make-move 2 3))
   (list (list 8 3) (list 5 2) (list 3 3))))
;; STRATEGY: Structural Decomposition on move : Move
(define (pitchers-after-move pitchers move)
  (local((define new-pitchers
           (pour (get-pitcher pitchers (move-src move))
                 (get-pitcher pitchers (move-tgt move))))
         (define new-src (first new-pitchers))
         (define new-dst (first (rest new-pitchers))))
    (set-pitcher
     (set-pitcher pitchers (move-src move) new-src)
     (move-tgt move) new-dst)))

;; pour : Pitcher Pitcher -> (list Pitcher Pitcher)
;; GIVEN: two pitchers src dst.
;; RETURNS: the list of two pitchers that should result after pour
;; from src to dst.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (pour (list 10 10) (list 8 0))
   (list (list 10 2) (list 8 8)))
  (check-equal?
   (pour (list 10 2) (list 8 0))
   (list (list 10 0) (list 8 2)))
  (check-equal?
   (pour (list 10 5) (list 8 5))
   (list (list 10 2) (list 8 8)))
  ;; tests added for Q3
  (check-equal?
   (pour (list 0 8) (list 8 0))
   (list (list 0 8) (list 8 8)))
  (check-equal?
   (pour (list 8 8) (list 0 8))
   (list (list 8 0) (list 0 8)))
  )
;; STRATEGY: Structural Decomposition on Pitcher
(define (pour src dst)
  (cond
    [(river? src) (list src (fill-pitcher dst))]
    [(river? dst) (list (dump-pitcher src) dst)]
    [else
     (local 
       ((define pour-out (pitcher-content src))
        (define pour-in (pitcher-rest-capacity dst))
        (define actual-pour-amount (min pour-out pour-in)))
       (list (pitcher-pour-out src actual-pour-amount)
             (pitcher-pour-in dst actual-pour-amount)))]))

;; fill-pitcher : Pitcher -> Pitcher
;; dump-pitcher : Pitcher -> Pitcher
;; GIVEN: a Pitcher.
;; RETURNS: the given Pitcher filled/dumped.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (fill-pitcher (list 8 4))
   (list 8 8))
  (check-equal?
   (dump-pitcher (list 8 4))
   (list 8 0)))
;; STRATEGY: Structural Decomposition on pitcher : Pitcher
(define (fill-pitcher pitcher)
  (local ((define capacity (pitcher-capacity pitcher)))
    (list capacity capacity)))

(define (dump-pitcher pitcher)
  (list (pitcher-capacity pitcher) 0))

;; pitcher-rest-capacity : Pitcher -> NonNegInt
;; GIVEN: A Pitcher.
;; RETURNS: Its capacity avaliable.
;; EXAMPLES:
(begin-for-test
  #;(check-equal?
   (pitcher-rest-capacity (list 0 8)) ; river
   8)
  (check-equal?
   (pitcher-rest-capacity (list 8 4))
   4))
;; STRATEGY: Structural Decomposition on pitcher : Pitcher
(define (pitcher-rest-capacity pitcher)
  (- (pitcher-capacity pitcher)
     (pitcher-content pitcher)))


;; pitcher-pour-in : Pitcher NonNegInt -> Pitcher
;; GIVEN: A Pitcher and the pour in amount pour-in-amount.
;; WHERE: pour-in-amount is no greater than pitcher's capacity 
;; avaliable.
;; RETURNS: the given Pitcher after pour in pour-in-amount.
;; EXAMPLES/TESTS:
(begin-for-test
  #;(check-equal?
   (pitcher-pour-in (list 0 8) 8)
   (list 0 8))
  (check-equal?
   (pitcher-pour-in (list 8 4) 4)
   (list 8 8)))
;; STRATEGY: Structural Decomposition on pitcher : Pitcher
(define (pitcher-pour-in pitcher pour-in-amount)
  (list (pitcher-capacity pitcher)
        (+ (pitcher-content pitcher)
           pour-in-amount)))

;; pitcher-pour-out : Pitcher NonNegInt -> Pitcher
;; GIVEN: A Pitcher and the pour-out amounnt pour-out-amount.
;; WHERE: pour-out-amount is no greater than pitcher's content.
;; RETURNS: the given Pitcher after pour out pour-out-amount.
;; EXAMPLES/TESTS:
(begin-for-test
  #;(check-equal?
   (pitcher-pour-out (list 0 8) 8)
   (list 0 8))
  (check-equal?
   (pitcher-pour-out (list 8 8) 5)
   (list 8 3)))
;; STRATEGY: Structural Decomposition on pitcher : Pitcher
(define (pitcher-pour-out pitcher pour-out-amount)
  (list (pitcher-capacity pitcher)
        (- (pitcher-content pitcher) 
           pour-out-amount)))

;; river? : Pitcher -> Boolean                
;; RETURNS: true iff the given pitcher represents river.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (river? (list 0 8))
   true)
  (check-equal?
   (river? (list 8 8))
   false))
;; STRATEGY: Structural Decomposition on pitcher : Pitcher
(define (river? pitcher)
  (= (pitcher-capacity pitcher) 0))
           
;; get-pitcher : Pitchers PosInt -> Pitchers
;; GIVEN: a Pitchers, an index.
;; WHERE: the length of Pitchers is not less than index.
;; RETURNS: the pitcher with the given index in the given Pitchers.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (get-pitcher
    (list (list 8 0) (list 5 0) (list 3 0))
    1)
   (list 8 0))
  (check-equal?
   (get-pitcher
    (list (list 8 0) (list 5 0) (list 3 0))
    3)
   (list 3 0)))
;; STRATEGY: Structural Decomposition on pitchers : Pitchers
(define (get-pitcher pitchers index)
  (cond
    [(= 1 index) (first pitchers)]
    [else (get-pitcher (rest pitchers) (- index 1))]))
    
;; set-pitcher : Pitchers PosInt Pitcher -> Pitchers
;; GIVEN: a Pitchers, an index, a Pitcher.
;; WHERE: the length of Pitchers is not less than index.
;; RETURNS: a Pitchers like the given one but with the Pitchers of
;; the given index replaced with by given Pitcher.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (set-pitcher
    (list (list 8 0) (list 5 0) (list 3 0))
    1
    (list 8 3))
   (list (list 8 3) (list 5 0) (list 3 0)))
  (check-equal?
   (set-pitcher
    (list (list 8 3) (list 5 0) (list 3 0))
    2
    (list 5 5))
   (list (list 8 3) (list 5 5) (list 3 0))))  
;; STRATEGY: Structural Decomposition on pitchers : Pitchers
(define (set-pitcher pitchers index new-pitcher)
  (cond
    [(= 1 index) (cons new-pitcher (rest pitchers))]
    [else 
     (cons (first pitchers)
           (set-pitcher (rest pitchers) (- index 1) new-pitcher))]))


;; make-move : PosInt PosInt -> Move
;; WHERE: the two arguments are not equal
;; RETURNS: a move with the given numbers as its source and target,
;; respectively. 

;; move-src : Move -> PosInt
;; move-tgt : Move -> PosInt
;; RETURNS: the pitcher numbers of the source or target of the move.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Q2

;; solution : NEListOf<PosInt> PosInt -> Maybe<ListOf<Move>>
;; GIVEN: a list of the capacities of the pitchers and the goal amount
;; RETURNS: a sequence of moves which, when executed from left to right,
;; results in one pitcher (not necessarily the first pitcher) containing
;; the goal amount.  Returns false if no such sequence exists.
;; EXAMPLES/TESTS:
;; STRATEGY: Structural Decomposition on capacities : NEListOf<PosInt>
(define (solution capacities goal)
  (local ((define moves
            (generalized-solution 
             (capacities-to-pitchers capacities)
             empty
             goal
             (min 8 
                  (inexact->exact (floor (/ 15 (log (length capacities)))))))))
    (if (false? moves)
        false
        (standardize-moves moves))))


;; standardize-moves : ListOf<Move> -> ListOf<Move>
;; GIVEN: a list of moves.
;; WHERE: in each move, 1 represents river, and each pitcher's index
;; is 1 greater than its actual index.
;; RETURNS: a list of the actual moves.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (standardize-moves (list (make-move 1 3) (make-move 3 2) 
                            (make-move 2 1) (make-move 2 3)))
   (list (make-fill 2) (make-move 2 1) 
         (make-dump 1) (make-move 1 2))))
;; STRATEGY: HOFC
(define (standardize-moves moves)
  (map
   ; Move -> Move
   ; GIVEN: a move.
   ; WHERE: In its src and dst, 1 represents river and other pitchers'
   ; index is 1 greater than its actual index.
   ; RETURNS: the actual move.
   (lambda(elt)
     (local ((define src (- (move-src elt) 1))
             (define tgt (- (move-tgt elt) 1)))
       (cond
         [(zero? src) (make-fill tgt)]
         [(zero? tgt) (make-dump src)]
         [else (make-move src tgt)])))
   moves))
                           

;; capacities-to-pitchers : NEListOf<PosInt> -> Pitchers
;; GIVEN: a list of the capacities of the pitchers.
;; RETURN: the corresponding Pitchers, insert a Pitcher represents 
;; river at the beginning. (with 0 capacity and max value of 
;; capacities as content).
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (capacities-to-pitchers (list 10 7 3))
   (list (list 0 10) (list 10 0) (list 7 0) (list 3 0)))
  (check-equal?
   (capacities-to-pitchers (list 10))
   (list (list 0 10) (list 10 0))))
;; STRATEGY: HOFC
(define (capacities-to-pitchers capacities)
  (cons (list 0 (apply max capacities))
        (map
         (lambda(elt) (list elt 0))
         capacities)))
                           

;; generalized-solution 
;; : Pitchers Moves PosInt NonNegInt -> Maybe<ListOf<Move>>
;; GIVEN: a Pitchers, a list of moves, the goal amount content, the 
;; the number of steps left.
;; WHERE: moves lead to the current pitchers from the original
;; RETURNS: a sequence of moves which, when executed from left to right,
;; results in one pitcher (not necessarily the first pitcher) containing
;; the goal amount.  Returns false if no such sequence exists.
;; EXAMPLES/TESTS:   
;; STRATEGY: General Recursion
;; TERMINATION ARGUMENT: the value of steps
(define (generalized-solution pitchers moves goal steps)
  (cond    
    [(satisfied? pitchers goal) moves]
    [(zero? steps) false]
    [else
     (local ((define possible-moves (next-moves pitchers)))
       (first-success
        (lambda(elt) 
          (generalized-solution 
           (pitchers-after-move pitchers elt)
           (append moves (list elt))
           goal
           (- steps 1)))
        possible-moves))]))

;; satisfied? : Pitchers PosInt -> Boolean
;; GIVEN: A Pitchers and a goal amount.
;; WHERE: the first element in Pitchers represents river(i.e. 
;; capacity = 0).
;; RETURNS: true iff there is one pitcher's content equals to the goal
;; amount.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (satisfied?
    (list (list 0 8) (list 8 0) (list 5 4) (list 3 3))
    8)
   false)
  (check-equal?
   (satisfied?
    (list (list 0 8) (list 8 1) (list 5 4) (list 3 3))
    4)
   true))
;; STRATEGY: HOFC
(define (satisfied? pitchers goal)
  (ormap
   ; Pitcher -> Boolean
   ; RETURNS: true iff the given pitcher's content equals to goal.
   (lambda(elt) (= (pitcher-content elt) goal))
   (rest pitchers)))

;; next-moves : Pitchers -> ListOf<Move>
;; GIVEN: a Pitchers.
;; RETURNS: a list of possible moves(pouring from a non-empty pitcher 
;; to a non-full pitcher).
;; EXAMPLES/TESTS:
(begin-for-test
  (check set-equal?
   (next-moves (list (list 8 3) (list 5 5) (list 3 0)))
   (list (make-move 1 3) (make-move 2 3) (make-move 2 1))))
;; STRATEGY: HOFC
(define (next-moves pitchers)
  (local 
    ((define non-empty-pitchers 
       (filter-return-index
        ; Pitcher -> Boolean
        ; RETURNS: true iff the given pitcher is not empty.
        (lambda(elt) (not (pitcher-empty? elt)))
        pitchers))
     (define non-full-pitchers
       (filter-return-index
        ; Pitcher -> Boolean
        ; RETURNS: true iff the given pitcher is not full.
        (lambda(elt) (not (pitcher-full? elt)))
        pitchers)))
    (produce-moves non-empty-pitchers non-full-pitchers)))

;; pitcher-empty? : Pitcher -> Boolean
;; pitcher-full? : Pitcher -> Boolean
;; GIVEN: A Pitcher.
;; RETURNS: true iff the given pitcher is empty/full.(The pitcher 
;; represents river is neither empty nor full)
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (pitcher-empty? (list 0 8))
   false)
  (check-equal?
   (pitcher-empty? (list 8 0))
   true)
  (check-equal?
   (pitcher-empty? (list 8 5))
   false)
  (check-equal?
   (pitcher-full? (list 0 8))
   false)
  (check-equal?
   (pitcher-full? (list 8 8))
   true)
  (check-equal?
   (pitcher-full? (list 8 5))
   false))
;; STRATEGY: Structural Decompotion on pitcher : Pitcher
(define (pitcher-empty? pitcher)
  (if (river? pitcher)
      false
      (= (pitcher-content pitcher) 0)))

(define (pitcher-full? pitcher)
  (if (river? pitcher)
      false
      (= (pitcher-capacity pitcher) 
         (pitcher-content pitcher))))
  

;; produce-moves : ListOf<PosInt> ListOf<PosInt> -> ListOf<Move>
;; GIVEN: two lists of positive integers.
;; WHERE: the first list is the indexes of non-empty pitchers, 
;; the second list is the indexes of non-full pitchers.
;; RETURNS: the list of possible moves.
;; EXAMPLES/TESTS:
(begin-for-test
  (check 
   set-equal?
   (produce-moves (list 1 2 3) (list 1 2))
   (list (make-move 1 2) (make-move 2 1) (make-move 3 1)
         (make-move 3 2)))
  (check
   set-equal?
   (produce-moves empty empty)
   empty)
  (check
   set-equal?
   (produce-moves (list 1) (list 1))
   empty))
;; STRATEGY: HOFC
(define (produce-moves indexes-non-empty indexes-non-full)
  (foldl
   ; PosInt ListOf<Move> -> ListOf<Move>
   ; GIVEN: the index of a non-empty pitcher and the rest of possible
   ; moves.
   ; RETURNS: the list of possible moves with moves from the given 
   ; pitcher included.
   (lambda(non-emp-elt rest-moves)
     (append
      (foldl 
       ; PosInt ListOf<Move> -> ListOf<Move>
       ; GIVEN: the index of a non-full pitcher non-full-elt and the
       ; rest of moves that pour from non-emp-elt.
       ; RETURN: the list of moves that pour from non-emp-elt.
       (lambda(non-full-elt rest-moves) 
         (if (= non-emp-elt non-full-elt)
             rest-moves
             (cons (make-move non-emp-elt non-full-elt)
                   rest-moves)))
       empty
       indexes-non-full)
      rest-moves))
   empty
   indexes-non-empty))

;; filter-return-index : ListOf<X> (X -> Boolean) -> ListOf<PosInt>
;; GIVEN: A list of X and a predicate f.
;; RETURNS: A list of indexes of Xs who satisfy f.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (filter-return-index
    even?
    (list 1 2 3 4 5 6))
   (list 2 4 6))
  (check-equal?
   (filter-return-index
    odd?
    (list 1 2 3 4 5 6))
   (list 1 3 5))
  (check-equal?
   (filter-return-index
    odd?
    empty)
   empty))
;; STRATEGY: Structural Decomposition on lox : ListOf<X>
(define (filter-return-index f lox)
  (local
    ; ListOf<X> PosInt -> ListOf<PosInt>
    ; GIVEN: a list of X and the index of first element in the list.
    ; RETURNS: the indexes of Xs who satisfy f.
    ((define (filter-return-index-loop rest-lox start-index)
       (cond
         [(empty? rest-lox) empty]
         [(f (first rest-lox))
          (cons start-index
                (filter-return-index-loop
                 (rest rest-lox)
                 (+ start-index 1)))]
         [else (filter-return-index-loop
                (rest rest-lox)
                (+ start-index 1))])))
    (filter-return-index-loop lox 1)))


;; first-success : (X -> Maybe<Y>) ListOf<X> -> Maybe<Y>
;; GIVEN: a list of Xs.
;; RETURNS: the first non-false value of (f X).
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (first-success 
    (lambda(x) (if(even? x) (* 2 x) false))
    (list 1 3 5 7 8))
   16)
  (check-equal?
   (first-success 
    (lambda(x) (if(even? x) (* 2 x) false))
    (list 1 3 5 7))
   false)
  (check-equal?
   (first-success 
    (lambda(x) (if(even? x) (* 2 x) false))
    empty)
   false))
;; STRATEGY: Structural Decomposition on lox : ListOf<X>
(define (first-success f lox)
  (cond
    [(empty? lox) false]
    [else (local
            ((define firstval (f (first lox))))
            (if (false? firstval)
                (first-success f (rest lox))
                firstval))]))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; satisfied-pitchers : ListOf<PosInt> PosInt -> ListOf<Pitchers>
;; GIVEN: a list of the capacities of the pitchers and the goal 
;; amount.
;; RETURNS: the Pitchers that satisfied the goal if possible,
;; other wise return false(Pitchers with 0 capacity represents river).
;; STRATEGY: Function Composition
(define (satisfied-pitchers capacities goal)
  (pitchers-after-moves
   (capacities-to-pitchers capacities)
   (generalized-solution 
    (capacities-to-pitchers capacities)
    empty
    goal
    10)))

;; TESTS:
#;(satisfied-pitchers (list 7 3) 2)
#;(satisfied-pitchers (list 2 5 10) 1)
#;(solution (list 8 5 3) 4)
#;(solution (list 8 5 3 16 32 65 128) 4)
#;(solution (list 10 7 3) 5)
#;(solution (list 10 7 3) 18)
#;(solution (list 7 3) 2)
#;(solution (list 2 5 10) 1)

