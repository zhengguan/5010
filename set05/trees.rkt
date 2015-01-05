;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Set 05

(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANT DEFINITIONS

;; CANVAS
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; NODE
(define NODE-SIZE 20)
(define HALF-NODE-SIZE (/ NODE-SIZE 2))
(define NEW-ROOT-NODE (make-node (make-posn 200 10) empty false))

;; LINE between node and its sons
(define LINE-COLOR "blue")

;; LINE on the left edge of new son
(define EDGE-LIE-COLOR "red")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS


;; (define-struct posn (x y))
;; A Posn is a (make-posn x y)
;; Interpretation:
;; x, y are the coordinates of a point
;; Template:
;; posn-fn : Posn -> ??
;; (define (posn-fn p)
;;   (... (posn-x p) (posn-y p)))
;; Examples:
(define posn-100-100 (make-posn 100 100))
(define posn-150-150 (make-posn 150 150))
(define posn-200-200 (make-posn 200 200))
(define posn-200-10 (make-posn 200 10))

(define-struct node (to-center to-sons to-selected?))
;; A Node is a (make-node Posn ListOf<Node> Boolean)
;; Interpretation:
;; to-center is the center point of a node.
;; to-sons is a list of sons of the given node.
;; to-selected? describes whether the node is selected
;; Template:
;; node-fn : Node -> ??
;; (define (node-fn n)
;;   (... (node-to-center n)
;;        (lon-fn (node-to-sons n))
;;        (node-to-selected? n)))

;; A ListOf<Node> (LoN) is either:
;; -- empty
;; -- (cons node LoN)
;; Template:
;; lon-fn : LoN -> ??
;; (define (lon-fn lon)
;;   (cond
;;     [(empty? lon) empty]
;;     [else (... (node-fn (first lon))
;;                (lon-fn (rest lon)))]))
;; Examples:
(define node-100-100-emp-f (make-node posn-100-100 empty false))
(define node-100-100-emp-t (make-node posn-100-100 empty true))
(define node-200-200-emp-f (make-node posn-200-200 empty false))
(define node-200-200-emp-t (make-node posn-200-200 empty true))
(define node-100-100-1son-f
  (make-node posn-100-100 (list node-200-200-emp-f) false))
(define node-100-100-1son-t
  (make-node posn-100-100 (list node-200-200-emp-f) true))
(define node-150-150-1son-t
  (make-node posn-150-150 (list node-200-200-emp-f) true))
(define node-200-10-emp-f (make-node posn-200-10 empty false))

(define-struct world (to-roots))
;; A World is a (make-world LoN)
;; Template:
;; world-fn : World -> ??
;; (define (world-fn w)
;;   (... (lon-fn (world-to-roots w))))
;; Examples:
(define world-empty (make-world empty))
(define world-1node (make-world (list node-200-10-emp-f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world. The given value is ignored.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (initial-world 10)
   (make-world empty)))

;; STRATEGY: Function Composition
(define (initial-world any)
  (make-world empty))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world. The given is ignored.
;; STRATEGY: Function Composition
#;(define (run any)
  (big-bang (initial-world any)
            (on-mouse world-after-mouse-event)
            (on-key world-after-key-event)
            (to-draw world-to-scene)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a World, a location, and a MouseEvent
;; RETURNS: the state of the world as it shoud be following the given
;; mouse event at that location.
;; EXAMPLES/TESTS:

;; STRATEGY: Structural Decomposition on w : World
(define (world-after-mouse-event w x y mev)
    (make-world (lon-after-mouse-event (world-to-roots w) x y mev)))

;; lon-after-mouse-event : LoN Integer Integer MouseEvent -> LoN
;; GIVEN: list of all the root nodes in a world, a mouse event's 
;; location, a mouse event itself.
;; RETURNS: a list of nodes as it should be following the given mouse
;; event at that location.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (lon-after-mouse-event (list node-100-100-emp-f)
                          100 100 "button-down")
   (list node-100-100-emp-t)))

;; STRATEGY: HOFC
(define (lon-after-mouse-event lon x y mev)
  (map
   (lambda(n) (node-after-mouse-event n x y mev))
   lon))

;; node-after-mouse-event : Node Integer Integer MouseEvent -> Node
;; GIVEN: a node, a mouse event's position, and the mouse event 
;; itself.
;; RETURNS: the given one after the given mouse event happened 
;; at the given location.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (node-after-mouse-event node-100-100-emp-f 100 100 "button-down")
   node-100-100-emp-t)
  (check-equal?
   (node-after-mouse-event node-100-100-emp-t 100 100 "button-up")
   node-100-100-emp-f)
  (check-equal?
   (node-after-mouse-event node-100-100-emp-t 200 200 "drag")
   node-200-200-emp-t)
  (check-equal?
   (node-after-mouse-event node-100-100-emp-t 100 100 "enter")
   node-100-100-emp-t))

;; STRATEGY: Cases on mev : MouseEvent
(define (node-after-mouse-event n x y mev)
  (cond
    [(mouse=? mev "button-down") (node-after-button-down n x y)]
    [(mouse=? mev "button-up") (node-after-button-up n x y)]
    [(mouse=? mev "drag") (node-after-drag n x y)]
    [else n]))
;;   (... (node-to-center n)
;;        (lon-fn (node-to-sons n))
;;        (node-to-selected? n)))


;; node-after-button-down : Node Integer Integer -> Node
;; GIVEN: a node and the position of a button-down mouse event.
;; RETURNS: the given node after button-down at the given location.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (node-after-button-down node-100-100-emp-f 100 100)
   node-100-100-emp-t)
  (check-equal?
   (node-after-button-down node-100-100-emp-f 200 200)
   node-100-100-emp-f)
  (check-equal?
   (node-after-button-down node-100-100-1son-f  100 100)
   node-100-100-1son-t))

;; STRATEGY: Structural Decomposition on n : Node
(define (node-after-button-down n x y)
  (make-node (node-to-center n)
             (map 
              (lambda(son) (node-after-button-down son x y))
              (node-to-sons n))
             (if (in-node? n x y)
                 true
                 (node-to-selected? n))))


;; in-node? : Node Integer Integer -> Boolean
;; GIVEN: a node and the coordinates of a point    
;; RETURNS: true iff the given point is located in the given node.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (in-node? node-100-100-emp-f 100 100)
   true))
;; STRATEGY: Structural Decomposition on n : Node
(define (in-node? n x y)
  (in-node?-helper (node-to-center n) x y))

;; in-node?-helper : Posn Integer Integer -> Boolean
;; GIVEN: the center of a node, and the coordiantes of a point.
;; RETURNS: true iff the given point is located in the node.
;; STRATEGY: Structural Decomposition on p : Posn
(define (in-node?-helper p x y)
  (and (<= (- (posn-x p) HALF-NODE-SIZE)
           x
           (+ (posn-x p) HALF-NODE-SIZE))
       (<= (- (posn-y p) HALF-NODE-SIZE)
           y
           (+ (posn-y p) HALF-NODE-SIZE))))



;; node-after-button-up : Node Integer Integer -> Node
;; GIVEN: a node and the position of a button-up mouse event.
;; RETURNS: the given node after the button-up mouse event at the 
;; given location.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (node-after-button-up node-100-100-emp-t 100 100)
   node-100-100-emp-f)
  (check-equal?
   (node-after-button-up node-100-100-emp-f 100 100)
   node-100-100-emp-f)
  (check-equal?
   (node-after-button-up node-100-100-1son-t  100 100)
   node-100-100-1son-f))
   
;; STRATEGY: Structural Decomposition on n : Node
(define (node-after-button-up n x y)
  (make-node (node-to-center n)
             (map 
              (lambda(son) (node-after-button-up son x y))
              (node-to-sons n))
             (if (node-to-selected? n)
                 false
                 (node-to-selected? n))))



;; node-after-drag : Node Integer Integer -> Node
;; GIVEN: a node and the position of drag mouse event.
;; RETURNS: the given after the drag mouse event at the given 
;; position.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (node-after-drag node-100-100-emp-t 200 200)
   node-200-200-emp-t)
  (check-equal?
   (node-after-drag node-100-100-emp-f 200 200)
   node-100-100-emp-f)
  (check-equal?
   (node-after-drag node-100-100-1son-f 150 150)
     node-100-100-1son-f))
;; STRATEGY: Structural Decomposition on n : Node   
(define (node-after-drag n x y)
  (if (node-to-selected? n)
      (make-node (make-posn x y)
                 (move-nodes (node-to-sons n) 
                             (distance (node-to-center n) x y))
                 (node-to-selected? n))
      (make-node (node-to-center n)
                 (map 
                  (lambda(son) (node-after-drag son x y))
                  (node-to-sons n))
                 (node-to-selected? n))))

;; distance : Posn Integer Integer -> Posn
;; GIVEN: a point and the coordinates of a position
;; RETURNS: the distance from the point to the given position.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (distance posn-100-100 200 200)
   posn-100-100))
;; STRATEGY: Structural Decomposition on p : Posn
(define (distance p x y)
  (make-posn (- x (posn-x p))
             (- y (posn-y p))))
   

;; move-nodes : LoN Posn -> LoN
;; GIVEN: a list of nodes and a distance expressed as posn
;; RETURNS: a list of nodes after the given list of nodes moving the 
;; given distance.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (move-nodes
    (list node-100-100-emp-f)
    posn-100-100)
   (list node-200-200-emp-f)))
;; STRATEGY: HOFC
(define (move-nodes lon dist)
  (map
   (lambda(n) (move-node n dist))
   lon))

;; move-node : Node Posn -> Node
;; GIVEN: a node and a distance expressed as posn
;; RETURNS: the given node after moving the given distance.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (move-node node-100-100-emp-f posn-100-100)
   node-200-200-emp-f))
;; STRATEGY: Structural Decomposition on n : Node
(define (move-node n p)
  (make-node (add-posn (node-to-center n) p)
             (move-nodes (node-to-sons n) p)
             (node-to-selected? n)))

;; add-posn : Posn Posn -> Posn
;; RETURNS: a posn whose x,y coordinates is the sum of the two given 
;; posn's x,y coordinates.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (add-posn posn-100-100 posn-100-100)
   posn-200-200))
;; STRATEGY: Structural Decomposition on p2 : Posn
(define (add-posn p1 p2)
  (add-posn-helper p1 (posn-x p2) (posn-y p2)))

;; add-posn-helper : Posn Integer Integer -> Posn
;; RETURNS: a posn like the given one but with its x coordiante 
;; adds the first given integer and its y coordinate adds the second
;; integer.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (add-posn-helper posn-100-100 100 100)
   posn-200-200))
;; STRATEGY: Structural Decomopsitoin on p1 : Posn
(define (add-posn-helper p x y)
  (make-posn (+ (posn-x p) x)
             (+ (posn-y p) y)))
                
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world and a key event
;; RETURNS: the state of the world as it should be following the given
;; key event.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (world-after-key-event world-empty "t")
   world-1node)
  (check-equal?
   (world-after-key-event world-empty "n")
   world-empty)
  (check-equal?
   (world-after-key-event 
    (make-world (list (make-node posn-200-10 empty true))) "d")
   world-empty)
  (check-equal?
   (world-after-key-event world-1node "u")
   world-empty)
  (check-equal?
   (world-after-key-event world-empty " ")
   world-empty))
;; STRATEGY: Structural Decomposition on w : World
(define (world-after-key-event w kev)
  (make-world 
   (nodes-after-key-event (world-to-roots w) kev)))

;; nodes-after-key-event : LoN KeyEvent -> Node
;; GIVEN: the list of nodes of a world and a key event.
;; RETURNS: a list of nodes that should follow the given list of nodes
;; after the key event happened.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (nodes-after-key-event empty "t")
   (list node-200-10-emp-f))
  (check-equal?
   (nodes-after-key-event 
    (list (make-node posn-200-10 empty true)) 
    "n")
   (list (make-node posn-200-10
                    (make-node (make-posn 200 70) 
                               empty true)
                    true)))
  (check-equal?
   (nodes-after-key-event
    (list (make-node posn-200-10 empty true))
    "d")
   empty)
  (check-equal?
   (nodes-after-key-event
    (list (make-node posn-200-10 empty true))
    "u")
   empty))
;; STRATEGY: Cases on kev : KeyEvent
(define (nodes-after-key-event lon kev)
  (cond
    [(key=? kev "t") (cons NEW-ROOT-NODE lon)]
    [(key=? kev "n") 
     (nodes-add-son lon)]
    [(key=? kev "d") 
     (nodes-delete-selected lon)]
    [(key=? kev "u") 
     (nodes-delete-upper-half lon)]
    [else lon]))
            

;; nodes-add-son : LoN -> LoN
;; GIVEN: a list of nodes 
;; RETURNS: a list of nodes like the given one, but with all selected
;; node(s) added a new son.
;; STRATEGY: HOFC
(define (nodes-add-son lon)
  (map
   (node-add-son n)
   lon))

;; node-add-son : Node -> Node
;; RETURNS: add a new son to the given node and each node of its 
;; subtree if it is selected, otherwise returns the original one.
;; STRATEGY: Structural Decomposition on n : Node
(define (node-add-son n)
  (if (and (node-to-selected? n) (enough-room? n))
      (make-node (node-to-center n)
                 (cons (next-son n) (nodes-add-son (node-to-sons n)))
                 (node-to-selected? n))
      (make-node (node-to-center n)
                 (nodes-add-son (node-to-sons n))
                 (node-to-selected? n))))


;; enough-room? : Node -> Boolean
;; RETURNS: true iff the given node has enough room for a new son.
;; STRATEGY: Structural Decomposition on n : Node
(define (enough-room? n)
  ;(enough-room?-helper (node-to-sons n)))
  (and (>= (next-son-x n) 0)
       (<= (next-son-y n) CANVAS-HEIGHT)))

;; next-son-x : Node -> Integer 
;; RETURNS: the next son's x position of the given node

;; next-son-y : Node -> Integer
;; RETURNS: the next son's y position of the given node.
;; STRATEGY: Structural Decomposition on n : Node
(define (next-son-y n)
  (next-son-y-helper (node-to-center n)))

;; next-son-y-helper : Posn -> Integer
;; GIVEN: the center position of a node
;; RETURNS: the next son's y position of the node
;; STRATEGY: Structural Decomposition on p : Posn
(define (next-son-y-helper p)
  (+ (posn-y p) (* 3 NODE-SIZE)))



;; enough-room? : LoN -> Boolean
;; GIVEN: the sons of a node
;; RETURNS: true iff the node has enough room for a new son.
;; STRATEGY: Structural Decomposition on lon : LoN
#;(define (enough-room? lon)
  (if (empty? lon)))



;; next-son : Node -> Node


;; 



;; nodes-delete-selected : LoN -> LoN
;; GIVEN: a list of nodes 
;; RETURNS: a list of nodes like the given one, but with all selected 
;; node(s) and its whole subtree.

;; nodes-delete-upper-half : LoN -> LoN
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene

  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;