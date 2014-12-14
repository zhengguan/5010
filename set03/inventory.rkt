;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname inventory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A set of funciont for manipulating the inventory of a bookstore,
;; represented as a list of books.

(require rackunit)
(require "extras.rkt")
(provide inventory-potential-profit
         inventory-total-volume
         price-for-line-item
         fillable-now?
         days-til-fillable
         price-for-order
         inventory-after-order
         increase-prices
         make-book
         make-line-item
         reorder-present?
         make-empty-reorder
         make-reorder)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONs

;; A MaybeInteger is one of:
;; -- Integer
;; -- false



(define-struct book (isbn title author publisher unit-price 
                          unit-cost copies reorder cuft))
;; A Book is a (make-book Integer String String String NonNegInt 
;;                        NonNegInt NonNegInt ReorderStatus Number)
;; Intepretation:
;; isbn is the book's "international standard book number"
;; title is the book's title
;; author is the name of the book's author
;; publisher is the book's publisher
;; unit-price is book's selling price, in USD*100
;; unit-cost is the cost of the book to the bookstore in USD*100
;; copies is the number of copies on hand.
;; reorder is the book's reorder status
;; cuft is the volume taken up by one unit of this item, in cubic feet
;; Template:
;; book-fn: Book -> ??
;;(define (book-fn b)
;;  (...
;;   (book-isbn b)
;;   (book-title b)
;;   (book-author b)
;;   (book-publisher b)
;;   (book-unit-price b)
;;   (book-unit-cost b)
;;   (book-copies b)
;;   (book-reorder b)
;;   (book-cuft b)))

(define-struct reorder (days copies))
;; A Reorder-Status is a (make-reorder PosInt PosInt)
;; Interpretation:
;; days is the number of days until the next shipment 
;; copies is the number of copies in the next shipment, copies is 0
;; means there is no reorder
;; Template:
;; reorder-fn : ReorderStatus -> ??
;;(define (reorder-fn rs)
;;  (.. (reorder-days rs)
;;      (reorder-copies rs)))

;; Examples:
(define empty-reorder (make-reorder 0 0))
(define reorder-status1 (make-reorder 7 10))
(define reorder-status2 (make-reorder 7 0))

(define htdp
  (make-book 
   0262062186 "How to design programs" 
   "Matthias Felleisen" "The MIT Press"
   8075 5000 100 reorder-status1 0.1))
(define htdp2
  (make-book 
   0262062186 "How to design programs" 
   "Matthias Felleisen" "The MIT Press"
   8075 5000 0 reorder-status1 0.1))
(define tls 
  (make-book
   0262560992 "The little schemer" "Daniel P. Friedman"
   "The MIT Press" 3071 2000 100 reorder-status2 0.08))
(define sicp 
  (make-book
   0070004846 "Structure and Interpretation of Computer Programs"
   "Harold Abelson" "McGraw-Hill Science/Engineering/Math" 3688
   2655 10 reorder-status1 0.09))
(define book-dont-know 
  (make-book
   0000000000 "Don't know"
   "Don't know" "Don't know" 3688
   2655 10 reorder-status1 0.09))
                       


;; An Inventory is either
;; -- empty
;; -- (cons Book Inventory)
;; Template:
;; inventory-fn : Inventiry -> ??
;;(define (inventory-fn inv)
;;  (cond
;;    [(empty? inv) ...]
;;    [else (... (book-fn (first inv))
;;               (inventory-fn (rest inv)))]))
;; Examples:
(define inventory1 (cons htdp (cons tls (cons sicp empty))))
(define inventory2 
  (list 
   (make-book 
    0262062186 "How to design programs" 
    "Matthias Felleisen" "The MIT Press"
    8075 5000 0 reorder-status1 0.1)
   (make-book
    0262560992 "The little schemer" "Daniel P. Friedman"
    "The MIT Press" 3071 2000 0 reorder-status2 0.08)
   (make-book
    0070004846 "Structure and Interpretation of Computer Programs"
    "Harold Abelson" "McGraw-Hill Science/Engineering/Math" 3688
    2655 0 reorder-status1 0.09)))


(define-struct line-item (isbn quantity))
;; An Item is a (make-line-item Number PosInt)
;; isbn and quantity is the ISBN and the number of that book in order
;; Template:
;; line-item-fn : LineItem -> ??
;;(define (line-item-fn i)
;;  (...
;;   (line-item-isbn i)
;;   (line-item-quantity i)))
;; Examples:
(define line-item1 (make-line-item 0262062186 10))
(define line-item2 (make-line-item 0262560992 10))
(define sicp-line-item-fillable (make-line-item 0070004846 10))
(define sicp-line-item-unfillable (make-line-item 0070004846 30))


;; An Order is either
;; -- empty
;; -- (cons LineItem Order)
;; Template:
;; order-fn : Order -> ??
;;(define (order-fn o)
;;  (cond
;;    [(empty? o) ...]
;;    [else (... (line-item-fn (first o))
;;               (order-fn (rest o)))]))
;; Examples:
(define order1 (list (make-line-item 0262062186 100)
                     (make-line-item 0262560992 100)
                     (make-line-item 0070004846 10)))
(define order2 (list (make-line-item 0262062186 100)
                     (make-line-item 0262560992 100)
                     (make-line-item 0070004846 100)))
(define order-with-non-exist-line-item
  (list (make-line-item 0262062186 100)
        (make-line-item 0262560992 100)
        (make-line-item 0000000000 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reorder-present? : ReorderStatus -> Boolean
;; RETURNS: true iff the given ReorderStatus shows a pending re-order
;; EXAMPLES: in TESTS
;; STRATEGY: Strucutral Decomposition on rs : ReorderStatus
(define (reorder-present? rs)
  (> (reorder-copies rs) 0))

(begin-for-test
  (check-equal? (reorder-present? reorder-status1) true)
  (check-equal? (reorder-present? reorder-status2) false))

;; make-empty-reorder : Any -> ReorderStatus
;; Ignores its argument
;; RETURNS: a ReorderStatus showing no pending re-order
;; EXAMPLES: in TESTS
(define (make-empty-reorder any)
  (make-reorder 0 0))

;; TESTS:
(begin-for-test
  (check-equal? (make-empty-reorder 10) (make-reorder 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inventory-potential-profit : Inventory -> Integer
;; GIVEN: an inventory
;; RETURNS: the total profit, in USD*100, for all the items in stock
;; EXAMPLES: in TESTS 
;; STRATEGY: Structural Decomposition on inv : Inventory
(define (inventory-potential-profit inv)
  (cond
    [(empty? inv) 0]
    [else (+ (book-potential-profit (first inv))
             (inventory-potential-profit (rest inv)))]))

(begin-for-test 
  (check-equal? (inventory-potential-profit inventory1) 424930))
                
;; book-potential-profit : Book -> Integer
;; GIVEN: a book
;; RETURNS: the profit, in USD*100, if this kind of book is sold
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Book
(define (book-potential-profit b)
  (*(- (book-unit-price b)
       (book-unit-cost b))
   (book-copies b)))

;; TESTS:
(begin-for-test
  (check-equal? (book-potential-profit htdp) 307500))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inventory-total-volume : Inventory -> Integer
;; GIVEN: an inventory
;; REUTRNS: the total volume needed to store all the books in the 
;; inventory, in cubic feet
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on inv : Inventory
(define (inventory-total-volume inv)
  (cond
    [(empty? inv) 0]
    [else (+ (book-total-cuft (first inv))
             (inventory-total-volume (rest inv)))]))

;; TESTS:
(begin-for-test
  (check-equal? (inventory-total-volume inventory1) 18.9))

;; book-total-cuft : Book -> Number
;; GIVEN: a book
;; RETURNS: the total volume needed to store that kind of books in the
;; inventory, in cubic feet
;; EXAMPLES:
;; STRATEGY: Structural Decomposition on b : Book
(define (book-total-cuft b)
  (* (book-copies b) (book-cuft b)))

(begin-for-test
  (check-equal? (book-total-cuft htdp) 10))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; price-for-line-item : Inventory LineItem -> MaybeInteger
;; GIVEN: an inventory and a line item
;; RETURNS: the price for that line item(the quanlity times the unit
;; price for that item). Returns false if that isbn does not exist in
;; the inventory.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on li : LineItem
(define (price-for-line-item inv li)
  (price-for-line-item-helper 
   inv 
   (line-item-isbn li)
   (line-item-quantity li)))

;; price-for-line-item-helper : Inventory Integer PosInt -> MaybeInteger
;; GIVEN: an inventory, a book's isbn, the quantity of the book
;; RETURNS: the total price. Returns false if that isbn does not exist in
;; the inventory.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on inv : Inventory
(define (price-for-line-item-helper inv isbn n)
  (cond
    [(empty? inv) false]
    [else (if (same-isbn? (first inv) isbn)
              (price-for-books (first inv) n)
              (price-for-line-item-helper (rest inv) isbn n))]))
;; TESTS:
(begin-for-test
  (check-equal? (price-for-line-item inventory1 line-item2) 30710)
  (check-equal? (price-for-line-item empty line-item2) false))

;; same-isbn? : Book Integer -> Boolean
;; GIVEN: a book and an ISBN 
;; RETURNS: true iff the book's ISBN as the given ISBN
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Book
(define (same-isbn? b isbn)
  (= (book-isbn b) isbn))

;; TESTS:
(begin-for-test
  (check-equal? (same-isbn? tls 0262560992) true))

;; price-for-books : Book PosInt -> Integer
;; GIVEN: a book and the number of the book
;; RETURNS: the price for the books
;; EXAMPLES: in TESTS
;; STRATEGY: Structuran Decomposition on b : Book
(define (price-for-books b n)
  (* (book-unit-price b) n))

;; TESTS:
(begin-for-test
  (check-equal? (price-for-books htdp 10) 80750))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fillable-now? : Order Inventory -> Boolean
;; GIVEN: an order and an inventory
;; RETURNS: true iff there are enough copies of each book on hand to
;; fill the order. If the order contains a book that is not in the 
;; inventory, then the order is not fillable.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on o : Order
(define (fillable-now? o inv)
  (cond
    [(empty? o) true]
    [else (if (line-item-fillable-now? inv (first o))
              (fillable-now? (rest o) inv)
              false)]))

;; TESTS:
(begin-for-test
  (check-equal? (fillable-now? order1 inventory1) true)
  (check-equal? (fillable-now? order2 inventory1) false))

;; line-item-fillable-now? : Inventory LineItem -> Boolean
;; GIVEN: an inventory and a line item
;; RETURNS: true iff there are enough books for this item
;; EXAMPLES: in helper function's TESTS
;; STRATEGY: Structural Decomposition on li : LineItem
(define (line-item-fillable-now? inv li)
  (line-item-fillable-now?-helper inv (line-item-isbn li) 
                                  (line-item-quantity li)))

;; line-item-fillable-now?-helper : Inventory Number PosInt -> Boolean
;; GIVEN: an inventory, an book's ISBN, the number of that book ordered
;; RETURNS: true iff there are enough book in the inventory
;; EXAMPLES: in TESTS
;; STRATEGY: Structural DEcomposition on inv : Inventory
(define (line-item-fillable-now?-helper inv isbn n)
  (cond
    [(empty? inv) false]
    [else (if (same-isbn? (first inv) isbn)
              (book-fillable-now? (first inv) n)
              (line-item-fillable-now?-helper (rest inv) isbn n))]))

;; TESTS:
(begin-for-test
  (check-equal? 
   (line-item-fillable-now? inventory1 (make-line-item 0070004846 10)) true)
  (check-equal?
   (line-item-fillable-now? inventory1 (make-line-item 0070004846 100)) false)
  (check-equal?
   (line-item-fillable-now? inventory1 (make-line-item 1270004846 100)) false))
         
;; book-fillable-now? : Book PosInt -> Boolean
;; GIVEN: a book and the number of books required
;; RETURNS: true iff there are enough book
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Book
(define (book-fillable-now? b n)
  (>= (book-copies b) n))

;; TESTS:
(begin-for-test
  (check-equal? (book-fillable-now? sicp 10) true)
  (check-equal? (book-fillable-now? sicp 100) false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; days-til-fillable : Order Inventory -> MaybeInteger
;; GIVEN: an order an a inventory
;; RETURSN: the number of days until the order is fillable, assuming 
;; all the shipments come in on times. Returns false if there won't be
;; enough copies of some book, even aftere the next shipment of that 
;; book comes in.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on o : Order
(define (days-til-fillable o inv)
  (cond
    [(empty? o) 0]
    [else (max-maybe-integer
           (days-til-line-item-fillable (first o) inv)
           (days-til-fillable (rest o) inv))]))

;; TESTS:
(begin-for-test
  (check-equal? (days-til-fillable order1 inventory1) 0)
  (check-equal? (days-til-fillable order2 inventory1) false))

;; max-maybe-integer : MaybeInteger MaybeInteger -> MaybeInteger
;; GIVEN: two maybe integers
;; RETURNS: false if either one of the given integers is false or both
;; otherwise returns the greater of the two integers
;; EXAMPLES: in TESTS
;; STRATEGY: Function Composition
(define (max-maybe-integer m1 m2)
  (if (or (false? m1) (false? m2))
      false
      (max m1 m2)))

;; TESTS:
(begin-for-test
  (check-equal? (max-maybe-integer false 1) false)
  (check-equal? (max-maybe-integer 1 3) 3))

;; days-til-line-item-fillable : LineItem Inventory -> MaybeInteger
;; GIVEN: a line item and an inventory 
;; RETURNS: the number of days until the order is fillable. Returns
;; false if the line item is not fillable.
;; EXAMPLES: in helper function's TESTS
;; STRATEGY: Structural Decomposition on li : LineItem
(define (days-til-line-item-fillable li inv)
  (days-til-line-item-fillable-helper
   (line-item-isbn li) 
   (line-item-quantity li)
   inv))

;; days-til-line-item-fillable-helper : 
;;   Integer PosInt Inventory -> MaybeInteger
;; GIVEN: ISBN of a book, the required number of the book and an 
;; inventory
;; RETURNS: the number of days unitl the order is fillable. Returns
;; false if there won't be enough copies of that book even after the
;; next shipment.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on inv : Inventory
(define (days-til-line-item-fillable-helper isbn n inv)
  (cond
    [(empty? inv) false]
    [else (if (same-isbn? (first inv) isbn)
              (days-til-book-fillable (first inv) n)
              (days-til-line-item-fillable-helper
               isbn n (rest inv)))]))

;; TESTS:
(begin-for-test
  (check-equal? (days-til-line-item-fillable
                 (make-line-item 0000000000 10)
                 inventory1)
                false)
  (check-equal? (days-til-line-item-fillable
                 sicp-line-item-fillable inventory1) 0)
  (check-equal? (days-til-line-item-fillable
                 sicp-line-item-unfillable inventory1) false))

;; days-til-book-fillable : Book PosInt -> MaybeInteger
;; GIVEN: a book and the number of the required book
;; RETURNS: the number days until that book is fillable. Returns
;; false if there won't be enough copies of that book even after the
;; next shipment.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Books
(define (days-til-book-fillable b n)
  (if (book-fillable-now? b n)
      0
      (days-til-book-fillable-with-reorder
       (book-copies b) (book-reorder b) n)))

;; TESTS:
(begin-for-test
  (check-equal? (days-til-book-fillable sicp 10) 0)
  (check-equal? (days-til-book-fillable sicp 20) 7)
  (check-equal? (days-til-book-fillable sicp 30) false))

;; days-til-book-fillable-with-reorder : 
;;   NonNegInt ReorderStatus PosInt -> MaybeInteger
;; GIVEN: the number of a book's copies on hand, the book's reorder
;; status and the number of required book
;; WHERE: the number of the book's copies on hand is less than 
;; requeired.
;; RETURNS: the number days until that book is fillable. Returns
;; false if there won't be enough copies of that book even after the
;; next shipment.
;; EXAMPLES: in function days-til-book-fillable's TESTS
;; STRATEGY: Structural Decomposition on rs : ReorderStatus
(define (days-til-book-fillable-with-reorder copies rs n)
  (if (>= (+ copies (reorder-copies rs)) n)
      (reorder-days rs)
      false))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; price-for-order : Inventory Order -> NonNegInteger
;; RETURNS: the total price for the given order, in USD*100. The price
;; does not depend on whether any particular line item is in stock. 
;; Line items for an ISBN that is not in the inventory count as 0.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on o : Order
(define (price-for-order inv o)
  (cond
    [(empty? o) 0]
    [else (add-maybe-integer 
           (price-for-line-item inv (first o))
           (price-for-order inv (rest o)))]))

;; TESTS:
(begin-for-test
  (check-equal? (price-for-order inventory1 order1) 1151480)
  (check-equal? 
   (price-for-order inventory1 order-with-non-exist-line-item)
   1114600))

;; add-maybe-integer : MaybeInteger MaybeInteger -> Integer
;; GIVEN: two maybe integers
;; RETURNS: the sum of the two maybe integers, false is counted as 0
;; EXAMPLES: in TESTS
;; STRATEGY: Function Composition
(define (add-maybe-integer m1 m2)
  (+ (false-to-0 m1) (false-to-0 m2)))

(begin-for-test
  (check-equal? (add-maybe-integer false 10) 10)
  (check-equal? (add-maybe-integer 10 false) 10)
  (check-equal? (add-maybe-integer 10 10) 20))

;; false-to-0 : MaybeInteger -> Integer
;; RETURNS: 0 iff given false, otherwise returns the given number
;; EXAMPLES:
;; STRATEGY: Funciont Composition
(define (false-to-0 m)
  (if (false? m)
      0
      m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inventory-after-order : Inventory Order -> Inventory
;; GIVEN: an inventory and an order
;; WHERE: the order is fillable now
;; RETURNS: the inventory after the order has benn filled.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on o : Order
(define (inventory-after-order inv o)
  (cond
    [(empty? o) inv]
    [else (inventory-after-order
           (inventory-after-line-item inv (first o))
           (rest o))]))

;; TESTS:
(begin-for-test
  (check-equal? (inventory-after-order inventory1 order1) inventory2))

;; inventory-after-line-item : Inventory LineItem -> Inventory
;; GIVEN: an inventory and an line item
;; WHERE: the line item is fillable now
;; RETURNS: the inventory after the line item has been filled.
;; EXAMPLES: in function inventory-after-order's TESTS
;; STRATEGY: Structural Decomposition on li : LineItem
(define (inventory-after-line-item inv li)
  (inventory-after-line-item-helper 
   inv
   (line-item-isbn li)
   (line-item-quantity li)))

;; inventory-after-line-item : Inventory Number PosInt -> Inventory
;; GIVEN: an inventory, an ISBN of a book and the required number of 
;; that book
;; WHERE: there is enough book copies
;; RETURNS: the inventory after the required number of books has been
;; filled
;; EXAMPLES: in function inventory-after-order's TESTS
;; STRATEGY: Structural Decomposition on inv : Inventory
;; 
#;(define (inventory-after-line-item-helper inv isbn n)
    (cond
      [(empty? inv) empty] ; This line will never be executed
      [else (if (same-isbn? (first inv) isbn)
                (cons (book-after-order (first inv) n) (rest inv))
                (cons (first inv)
                      (inventory-after-line-item-helper 
                       (rest inv) isbn n)))]))

(define (inventory-after-line-item-helper inv isbn n)
  (if (same-isbn? (first inv) isbn)
      (cons (book-after-order (first inv) n) (rest inv))
      (cons (first inv)
            (inventory-after-line-item-helper (rest inv) isbn n))))


;; book-after-order : Book PosInt -> Book
;; GIVEN: a book and the number of ordered copies of that book
;; WHERE: there are enough copies on hand for the order
;; RETURNS: the book after order has been filled
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Book
(define (book-after-order b n)
  (make-book 
   (book-isbn b)
   (book-title b)
   (book-author b)
   (book-publisher b)
   (book-unit-price b)
   (book-unit-cost b)
   (- (book-copies b) n)
   (book-reorder b)
   (book-cuft b)))

;; TESTS:
(begin-for-test
  (check-equal? (book-after-order htdp 100) htdp2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; increase-prices : Inventory String Real -> Inventory
;; GIVEN: an inventory, a publisher, and a percentage
;; RETURNS: an inventory like the original, except that all items by 
;; that publisher have their prices increased by the specified 
;; percentage.
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on inv : Inventory
(define (increase-prices inv publisher percentage)
  (cond
    [(empty? inv) empty]
    [else (if (same-publisher? (first inv) publisher)
              (cons (book-increase-price (first inv) percentage)
                    (increase-prices
                     (rest inv)
                     publisher
                     percentage))
              (cons (first inv)
                    (increase-prices 
                     (rest inv) 
                     publisher
                     percentage)))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (increase-prices inventory1 "The MIT Press" 10)
   (list 
    (make-book 
     0262062186 "How to design programs" 
     "Matthias Felleisen" "The MIT Press"
     8882 5000 100 reorder-status1 0.1)
    (make-book
     0262560992 "The little schemer" "Daniel P. Friedman"
     "The MIT Press" 3378 2000 100 reorder-status2 0.08)
    sicp))
  
  ;; debug
  (check-equal?
   (book-increase-price sicp 10)
    (make-book
     0070004846 "Structure and Interpretation of Computer Programs"
     "Harold Abelson" "McGraw-Hill Science/Engineering/Math" 4057
     2655 10 reorder-status1 0.09)))

;; same-publisher? : Book String -> Boolean
;; GIVEN: a book and a publisher
;; RETURNS: true iff the book is published by that publisher
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Book
(define (same-publisher? b p)
  (string=? (book-publisher b) p))

;; TESTS:
(begin-for-test
  (check-equal?
   (same-publisher? htdp "The MIT Press") true))



;; book-increase-price : Book Real -> Book
;; GIVEN: a book and a percentage
;; RETURNS: a book like the original, except its unit price increased
;; by that percentage
;; EXAMPLES: in TESTS
;; STRATEGY: Structural Decomposition on b : Book
(define (book-increase-price b p)
  (make-book 
   (book-isbn b)
   (book-title b)
   (book-author b)
   (book-publisher b)
   (round (* (book-unit-price b) (+ 1 (/ p 100))))
   (book-unit-cost b)
   (book-copies b)
   (book-reorder b)
   (book-cuft b)))

;; TESTS:
(begin-for-test
  (check-equal?
   (book-increase-price htdp 10)
   (make-book 
     0262062186 "How to design programs" 
     "Matthias Felleisen" "The MIT Press"
     8882 5000 100 reorder-status1 0.1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
