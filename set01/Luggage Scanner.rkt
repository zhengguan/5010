;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Luggage Scanner|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; "Luggage Scanner.rkt"
; Solution for the Luggage Scanner problem
(require "extras.rkt")
(require rackunit)
(define-struct suitcase (length width height))

;; A suitcase is a (make-suitcase PosReal PosReal PosReal)
;; INTERPRETATION:
;; length, width, height are the dimensions of the suitcase
;; in cm.

;; TEMPLATE:
;; suitcaes-fn : Suitcase -> ??
(define (suitcase-fn sc)
  (...(suitcase-length sc)
      (suitcase-width sc)
      (suitcase-heigth sc)))

;;EXAMPLE:
(define suitcase1 (make-suitcase 12 35 24))

(define-struct scanner (width height))
;; A scanner is a (make-scanner PosReal PosReal)
;; Interpretation:
;; width and heigth are the width and height of the
;; intake in cm.

;; Template:
;; scanner-fn : Scanner -> ??
(define (scanner-fn s)
  (... (scanner-width s)
       (scanner-height s)))

; suitcase-fits-scanner : Suitcase Scanner -> Boolean
; GIVEN: a Suitcase and a Scanner
; RETURNS: true iff the suitcase  can fit through the scanner.

; EXAMPLES:
; (suitcase-fits-scanner 
;  (make-suitcase 24 12 11)
;  (make-scanner 15 18))
; = true
; (suitcase-fits-scanner 
;  (make-suitcase 24 12 21)
;  (make-scanner 15 18))
; = false
; (suitcase-fits-scanner
;  (make-suitcase 24 21 12)
;  (make-scanner 15 18))
; = false
;; STRATEGY: Structural Decomposition on sc : Suitcase
(define (suitcase-fits-scanner sc s)
  (face-fits-scanner
   (min
    (suitcase-length sc)
    (suitcase-width sc)
    (suitcase-height sc))
   (second-smallest
    (suitcase-length sc)
    (suitcase-width sc)
    (suitcase-height sc))
   s))

; TESTS:
(begin-for-test
  (check-equal? 
   (suitcase-fits-scanner
    (make-suitcase 24 12 11)
    (make-scanner 15 18))
   true
   "suitcase (make-suitcase 24 12 11) \
could fits into scanner (make-scanner 15 18)")                                
  
  (check-equal?
   (suitcase-fits-scanner
    (make-suitcase 24 12 21)
    (make-scanner 15 18))
   false
   "suitcase (make-suitcase 24 12 21) \
could not fits into scanner (make-scanner 15 18)")
  
  (check-equal?
   (suitcase-fits-scanner
    (make-suitcase 24 21 12)
    (make-scanner 15 18))
   false
   "suitcase (make-suitcase 24 21 12) \
could not fits into scanner (make-scanner 15 18)"))

;; face-fits-scanner : PosReal PosReal Scanner -> Boolean
;; GIVEN: the dimensions of a suitcase face and a Scanner
;; RETURNS: true iff that face will fit into the scanner's mouth
; (face-fits-scanner 12 11 (make-scanner 15 18))
; = true
; (face-fits-scanner 11 12 (make-scanner 15 18))
; = true
; (face-fits-scanner 12 19 (make-scanner 15 18))
; = false
; (face-fits-scanner 12 11 (make-scanner 21 18))
; = true
; (face-fits-scanner 12 11 (make-scanner 15 18))
; = true
;; STRATEGY: Structural Decomposition on s : Scanner
(define (face-fits-scanner h w s)
  (rectangle-fits? h w
                   (scanner-width s)
                   (scanner-height s)))

(begin-for-test
  (check-equal? 
   (face-fits-scanner 
    12 
    11
    (make-scanner 15 18))
   true
   "face 12*11 could fit into scanner (make-scanner 15 18)")
  
  (check-equal? 
   (face-fits-scanner
    11 
    12
    (make-scanner 15 18))
   true
   "face 11*12 could fit into scanner (make-scanner 15 18)")
  
  (check-equal?
   (face-fits-scanner
    12
    19
    (make-scanner 15 18))
   false
   "face 12*19 could not fit into scanner (make-scanner 15 18)")
  
  (check-equal?
   (face-fits-scanner
    12
    11
    (make-scanner 21 18))
   true
   "face 12*11 could not fit into scanner (make-scanner 21 18)")
  
  (check-equal?
   (face-fits-scanner
    12
    11
    (make-scanner 15 18))
   true
   "face 12*11 could fit into scanner (make-scanner 15 18)"))

;; rectangle-fits? : PosReal PosReal PosReal PosReal -> Boolean
;; GIVEN: the height and width of two rectangle.
;; RETURNS: true iff the first rectangle will fit in the second 
;; rectangle, either horizontally or vertically.
;; EXAMPLES:
;; (rectangle-fits? 11 20 13 23) = true
;; (rectangle-fits? 20 11 13 23) = true
;; (rectangle-fits? 11 20 13 19) = false
;; (rectangle-fits? 20 11 13 19) = false
;; STRATEGY: function composition
(define (rectangle-fits? h1 w1 h2 w2)
  (or
   (and (< h1 h2) (< w1 w2))
   (and (< h1 w2) (< w1 h2))))
;; BUG: (rectangle-fits? 10 20 10 20)



;; ANSWER: (rectangle-fits 10 20 9 21)
(begin-for-test
  (check-equal? (rectangle-fits? 11 20 13 23) true)
  (check-equal? (rectangle-fits? 20 11 13 23) true)
  (check-equal? (rectangle-fits? 11 20 13 19) false)
  (check-equal? (rectangle-fits? 20 11 13 19) false)
  (check-equal? (rectangle-fits? 20 11 19 13) false)
  (check-equal? (rectangle-fits? 3 6 4 5) false))

;; second-smallest : PosReal PosReal PosReal -> PosReal
;; GIVEN: 3 positive reals
;; RETURNS: the second-smallest element
;; EXAMPLES: See tests below.
;; STRATEGY: function composition

;(define (second-smallest a b c)
;  (cond
;    [(and (<= a b) (<= a c))
;     (if (<= b c)
;         b
;         c)]
;    [(and (<= b c) (<= b a))
;     (if (<= a c)
;         a
;         c)]
;    [(and (<= c a) (<= c b))
;     (if (<= a b)
;         a
;         b)]))

(define (second-smallest a b c)
  (- (+ a (+ b c))
     (+ (max a b c)
        (min a b c))))

(begin-for-test
  (check-equal? (second-smallest 1 2 3) 2)
  (check-equal? (second-smallest 1 3 2) 2)
  (check-equal? (second-smallest 2 3 1) 2)
  (check-equal? (second-smallest 2 1 3) 2)
  (check-equal? (second-smallest 3 2 1) 2)
  (check-equal? (second-smallest 3 1 2) 2))

;; ANSWER: They are insufficient.
