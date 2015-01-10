;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Set 05

(require "extras.rkt")
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(provide initial-world
         run
         world-after-mouse-event
         world-after-key-event
         world-to-roots
         node-to-center
         node-to-sons
         node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANT DEFINITIONS

;; CANVAS
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 400)
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; NODE
(define NODE-SIZE 20)
(define HALF-NODE-SIZE (/ NODE-SIZE 2))
(define NODE-UNSELECTED (square 20 "outline" "green"))
(define NODE-SELECTED (square 20 "solid" "green"))
(define NODE-NO-ROOM (square 20 "solid" "red"))

;; LINE between node and its sons
(define LINE-COLOR "blue")

;; LINE on the left edge of new son
(define EDGE-LINE-COLOR "maroon")

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
;; Examples:
(define NEW-ROOT-NODE (make-node (make-posn 200 10) empty false))


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
(define node-100-100-2sons-f
  (make-node posn-100-100 
             (list node-200-200-emp-f node-150-150-1son-t)
             false))



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
(define (run any)
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
(begin-for-test
  (check-equal?
   (world-after-mouse-event (initial-world 10) 100 100 "button-down")
   (initial-world 10)))
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
                    (list (make-node (make-posn 200 70) 
                                     empty false))
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
(begin-for-test
  (check-equal?
   (nodes-add-son  (list node-100-100-emp-t node-200-200-emp-f))
   (list (make-node posn-100-100 
              (list (make-node (make-posn 100 (+ 100 (* 3 NODE-SIZE)))
                               empty
                               false))
              true)
         node-200-200-emp-f)))
    
;; STRATEGY: HOFC
(define (nodes-add-son lon)
  (map
   node-add-son
   lon))

;; node-add-son : Node -> Node
;; RETURNS: add a new son to the given node and each node of its 
;; subtree if it is selected, otherwise returns the original one.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (node-add-son  node-100-100-emp-f)
    node-100-100-emp-f))
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
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (enough-room? node-100-100-emp-f)
   true))
;; STRATEGY: Structural Decomposition on n : Node
(define (enough-room? n)
  ;(enough-room?-helper (node-to-sons n)))
  (and (>= (next-son-x n) 0)
       (<= (next-son-y n) CANVAS-HEIGHT)))

;; next-son-x : Node -> Integer 
;; RETURNS: the next son's x position of the given node
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (next-son-x
    (make-node posn-200-10
               (list (make-node (make-posn 200 70) 
                                empty false))
               true))
    160))
;; STRATEGY: Structural Decomposition on n : Node
(define (next-son-x n)
  (next-son-x-helper (node-to-center n) 
                     (node-to-sons n)
                     (node-to-selected? n)))

;; next-son-x-helper : Posn LoN Boolean -> Node
;; GIVEN: the center point, sons and selected state of a node.
;; RETURNS: its next son's x position.
;; STRATEGY: Structural Decomposition on p : Posn
(define (next-son-x-helper p lon selected?)
  (if (empty? lon)
      (posn-x p)
      (first-son-x lon)))

;; first-son-x : LoN -> Integer
;; GIVEN: a list of nodes which is the sons of a node
;; WHERE: the list if not empty
;; RETURNS:  the x coordinate of the node's next new son.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (first-son-x (list node-100-100-emp-f))
   60))
;; STRATEGY: Structural Decomposition on lon : LoN
(define (first-son-x lon)
  (- (node-x (first lon)) (* 2 NODE-SIZE)))

;; node-x : Node -> Integer
;; RETURNS: the given node's x coordinate.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal? 
   (node-x node-100-100-emp-f)
   100))
;; STRATEGY: Structural Decomposition on n : Node
(define (node-x n)
  (posn-x (node-to-center n)))

;; next-son-y : Node -> Integer
;; RETURNS: the next son's y position of the given node.
(begin-for-test
  (check-equal? 
   (next-son-y node-100-100-emp-f)
   160))
;; STRATEGY: Structural Decomposition on n : Node
(define (next-son-y n)
  (next-son-y-helper (node-to-center n)))

;; next-son-y-helper : Posn -> Integer
;; GIVEN: the center position of a node
;; RETURNS: the next son's y position of the node
;; STRATEGY: Structural Decomposition on p : Posn
(define (next-son-y-helper p)
  (+ (posn-y p) (* 3 NODE-SIZE)))



;; next-son : Node -> Node
;; RETURNS: the next new son of the give nndoe.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal? 
   (next-son node-100-100-emp-f)
   (make-node (make-posn 100 160)
              empty
              false)))
;; STRATEGY: Structural Decomposition on n : Node
(define (next-son n)
  (make-node
   (make-posn (next-son-x n) (next-son-y n))
   empty
   false))


;; 



;; nodes-delete-selected : LoN -> LoN
;; GIVEN: a list of nodes 
;; RETURNS: a list of nodes like the given one, but with all selected 
;; node(s) and each node's selected children removed.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (nodes-delete-selected 
    (list node-100-100-1son-t node-100-100-2sons-f))
   (list(make-node posn-100-100 
                   (list node-200-200-emp-f)
                   false))))
;; STRATEGY: HOFC
(define (nodes-delete-selected lon)
  (foldr
   ; Node LoN -> LoN
   ; GIVEN: a node and the rest of the answer
   ; RETURNS: rest of the answer iff the given is selected, otherwise
   ; remove the selected children along with its subtree of the given
   ; given node, then cons it to the rest of the answer.
   (lambda(n ans-rest) 
     (if(node-to-selected? n)
        ans-rest
        (cons (make-node (node-to-center n)
                         (nodes-delete-selected (node-to-sons n))
                         (node-to-selected? n))
              ans-rest)))
   empty
   lon))

;; nodes-delete-upper-half : LoN -> LoN
;; GIVEN: a list of nodes.
;; RETURNS: a list of nodes like the given one, but with all node(s) 
;; whose center is in the upper half of the canvas removed (its whole
;; children are also removed.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (nodes-delete-upper-half   
    (list node-150-150-1son-t
          (make-node (make-posn 300 300)
                     (list node-100-100-2sons-f node-100-100-1son-f)
                     false)))
   (list (make-node (make-posn 300 300)
                    empty
                    false))))
;; STRATEGY: HOFC
(define (nodes-delete-upper-half lon)
  (foldr 
   ; Node LoN -> LoN
   ; GIVEN: a node and the rest of the answer
   ; RETURNS: the rest of the answer iff the given node is in the 
   ; upper half of the canvas, other remove all the children of the 
   ; given node who is located in the upper half of the canvas and 
   ; cons it to the rest of the answer.
   (lambda(n ans-rest) 
     (if (in-upper-half? n)
         ans-rest
         (cons (make-node (node-to-center n)
                          (nodes-delete-upper-half (node-to-sons n))
                          (node-to-selected? n))
               ans-rest)))
   empty
   lon))
  

;; in-upper-half? : Node -> Boolean
;; RETURNS: ture iff the given node is in the upper half of the canvas
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (in-upper-half? node-100-100-emp-f)
   true))
;; STRATEGY: Structural Decomposition on n : Node
(define (in-upper-half? n)
  (posn-in-upper-half (node-to-center n)))


;; posn-in-upper-half : Posn -> Boolean
;; RETURNS: true iff the given posn is in the upper half of the canvas
(begin-for-test
  (check-equal?
   (posn-in-upper-half posn-100-100)
   true))
;; STRATEGY: Structural Decomposition on p : Posn
(define (posn-in-upper-half p)
  (<= (posn-y p) HALF-CANVAS-HEIGHT))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; GIVEN: a world state
;; RETURNS: the corresponding Scene
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (world-to-scene (initial-world 10))
   EMPTY-CANVAS))
;; STRATEGY: Structural Decomposition on w : World
(define (world-to-scene w)
  (add-root-nodes-to-scene (world-to-roots w)
                      EMPTY-CANVAS))

;; add-root-nodes-to-scene : LoN Scene -> Scene
;; GIVEN: the list of root nodes of a world and a Scene.
;; RETURNS: a scene like the given one but with the given list of 
;; nodes painted on it.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (add-root-nodes-to-scene (list node-100-100-emp-f) EMPTY-CANVAS)
   (place-image NODE-UNSELECTED 100 100 EMPTY-CANVAS)))
;; STRATEGY: HOFC
(define (add-root-nodes-to-scene lon s)
  (foldr
   add-root-node-to-scene
   s
   lon))

;; add-root-node-to-scene : Node Scene -> Scene
;; GIVEN: a node and a Scene.
;; RETURNS: a Scene like the given one, but with the given node painted
;; on it.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal? 
   (add-root-node-to-scene node-100-100-emp-f EMPTY-CANVAS)
   (place-image NODE-UNSELECTED 100 100 EMPTY-CANVAS))
  (check-equal? 
   (add-root-node-to-scene node-100-100-emp-t EMPTY-CANVAS)
   (scene+line (place-image NODE-SELECTED 100 100 EMPTY-CANVAS)
               90 0 90 CANVAS-HEIGHT EDGE-LINE-COLOR))
  (check-equal? 
   (add-root-node-to-scene (make-node posn-100-100
                                      (list (make-node 
                                             (make-posn 20 20)
                                             empty
                                             false))
                                      true)
                           EMPTY-CANVAS)
   #;(scene+line (place-image NODE-NO-ROOM
                            100 100
                            (place-image NODE-UNSELECTED 20 20 EMPTY-CANVAS))
               20 20 100 100 LINE-COLOR)
   (place-image NODE-UNSELECTED
                20 20
                (scene+line 
                 (place-image NODE-NO-ROOM 100 100 EMPTY-CANVAS)
                 100 100 20 20 LINE-COLOR))))
;; STRATEGY: Structural Decomposition on n : Node
(define (add-root-node-to-scene n s)
  (if (node-to-selected? n) 
      (if (enough-room? n)
          (add-sons-to-scene (node-to-sons n) 
                             (node-to-center n)
                             (place-son-border 
                              (next-son-left-border n)
                              (place-node NODE-SELECTED 
                                          (node-to-center n)
                                          s)))
          (add-sons-to-scene (node-to-sons n) (node-to-center n)
                             (place-node NODE-NO-ROOM 
                                         (node-to-center n) s)))
      (add-sons-to-scene (node-to-sons n) (node-to-center n)
                         (place-node NODE-UNSELECTED
                                     (node-to-center n) s))))
     
  

      
;; place-node : Image Posn Scene -> Scene
;; GIVEN: the image of a node, a position and a Scene.
;; RETURNS: a scene like the givne , but with the givne image
;; placed onto it with its center at the given position.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (place-node NODE-SELECTED posn-100-100 EMPTY-CANVAS)
   (place-image NODE-SELECTED 100 100 EMPTY-CANVAS)))
;; STRATEGY: Structural Decomposition on p : Posn
(define (place-node node-image p scene-image)
  (place-image node-image (posn-x p) (posn-y p) scene-image))
  
;; next-son-left-border : Node -> Integer
;; RETURNS: the left border of the given node's next new son.
;; EXAMPLES:
(begin-for-test
  (check-equal?
   (next-son-left-border node-100-100-emp-f)
   90))
;; STRATEGY: Function Composition 
(define (next-son-left-border n)
  (- (next-son-x n) HALF-NODE-SIZE))


;; place-son-border : Integer Scene -> Scene
;; RETURNS: an Scene like the given one, but with a vertical line 
;; displayed on it, whose x position is the given integer.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (place-son-border 100 EMPTY-CANVAS)
   (scene+line EMPTY-CANVAS 100 0 100 CANVAS-HEIGHT EDGE-LINE-COLOR)))
;; STRATEGY: Function Composition 
(define (place-son-border i s)
  (scene+line s i 0 i CANVAS-HEIGHT EDGE-LINE-COLOR))



;; add-sons-to-scene : LoN Posn Scene -> Scene
;; GIVEN: the list of sons of a node, the center point of the node,
;; a scene.
;; RETURNS: a scene like the given one with the given list of sons 
;; potrayed on it.
;; EXAMPLES/TESTS:
(begin-for-test
  (add-sons-to-scene (list node-200-200-emp-f) 
                     posn-100-100 EMPTY-CANVAS)
  (scene+line (place-image NODE-UNSELECTED 100 100 EMPTY-CANVAS)
              100 100 200 200 LINE-COLOR))
;; STRATEGY: HOFC
(define (add-sons-to-scene los p s)
  (foldr
   ; Node Scene -> Scene
   ; GIVEN: a son of a node, a scene
   ; RETURNS: a scene like the given one, but with the given son 
   ; potrayed on it.
   (lambda(son s)
     (add-root-node-to-scene son 
                              (place-branch-line p 
                                                 (node-to-center son)
                                                 s)))
   s
   los))


;; place-branch-line : Posn Posn Scene -> Scene
;; GIVEN: the position of two points and a scene.
;; RETURNS: a scene like the given one, but with a blue line between 
;; the two given positions potrayed on it.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (place-branch-line posn-100-100 posn-200-200 EMPTY-CANVAS)
   (scene+line EMPTY-CANVAS 100 100 200 200 LINE-COLOR)))
;; STRATEGY: Structural Decomposition on start : Posn
(define (place-branch-line start end s)
  (place-branch-line-helper (posn-x start) (posn-y start) end s))

;; place-branch-line-helper : Integer Integer Posn Scene -> Scene
;; GIVEN: the coordinates of a position, a position, a scene
;; RETURNS: a scene like the given, but with a blue line between
;; the two position
;; STRATEGY:
(define (place-branch-line-helper x y p s)
  (scene+line s x y (posn-x p) (posn-y p) LINE-COLOR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;