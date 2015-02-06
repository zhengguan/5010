;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;; set 06 Q1
;; outlines.rkt
(require "extras.rkt")
(require rackunit)
(provide
 nested-rep?
 nested-to-flat)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATE DEFINITIONS

;; An Sexp is one of the following
;; -- a String
;; -- a NonNegInt
;; -- a ListOfSexp

;; A ListOfSexp is one of
;; -- empty
;; (cons Sexp ListOfSexp)

;; A NestedRep is a ListOfSection
;; Template : ListOfSection -> ??
#;
(define (los-fn los)
  (cond
    [(empty? los) ...]
    [else (... (section-fn (first los))
               (los-fn (rest los)))]))
;; Examples:
(define nr1 '(("The first section"
               ("A subsection with no subsections")
               ("Another subsection"
                ("This is a subsection of 1.2")
                ("This is another subsection of 1.2"))
               ("The last subsection of 1"))
              ("Another section"
               ("More stuff")
               ("Still more stuff"))))


;; A Section is one of the following
;; -- (cons String empty)
;; -- (cons String NonEmpListOfSection)
;; Template: 
;; section-fn : Section -> ??
#;
(define (section-fn s)
  (... (first s)
       (if (empty? (rest s))
           (... (rest s))
           (nelos-fn (rest s)))))
;; EXAMPLES: 
(define section1 '("The first section"
                    ("A subsection with no subsections")
                    ("Another subsection"
                     ("This is a subsection of 1.2")
                     ("This is another subsection of 1.2"))
                    ("The last subsection of 1")))

;; A NonEmpListOfSection is one of 
;; -- (cons Section empty)
;; -- (cons Section NonEmptyListOfSection)
;; Template:
;; nelos : NonEmpListOfSection -> ??
#;
(define (nelos-fn nelos)
  (... (section-fn (first nelos))
       (if (empty? (rest nelos))
           (... (rest nelos))
           (nelos-fn (rest nelos)))))
                   


;; A Line is (list ListOfPosInt String)
;; Template : Line -> ??
#;
(define (line-fn l)
  (... (lopi-fn (first l))
       (string-fn (first (rest l)))))

;; A FlatRep is a ListOfLine
;; Template : ListOfLine -> ??
#;
(define (lol-fn lol)
  (cond
    [(empty? lol) ...]
    [else (... (line-fn (first lol))
               (lol-fn (rest lol)))]))
;; EXAMPLES:
(define fr1 '(((1) "The first section")
              ((1 1) "A subsection with no subsections")
              ((1 2) "Another subsection")
              ((1 2 1) "This is a subsection of 1.2")
              ((1 2 2) "This is another subsection of 1.2")
              ((1 3) "The last subsection of 1")
              ((2) "Another section")
              ((2 1) "More stuff")
              ((2 2) "Still more stuff")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION DEFINITIONS

;; nested-rep? : Sexp -> Boolean
;; GIVEN: an Sexp
;; RETURNS: true iff it is the nested representation of some outline.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (nested-rep? nr1)
   true))
;; STRATEGY: HOFC
(define (nested-rep? s)
  (if (not (list? s))
      false
      (andmap section? s)))

;; section? : Sexp -> Boolean
;; GIVEN: an Sexp
;; RETURNS: true iff it is a Section
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (section? section1)
   true)
  (check-equal?
   (section? '("hello" ()))
   false))
;; STRATEGY: Structural Decomposition on s : Section
(define (section? s)
  (if (or (not(list? s)) (empty? s))
      false
      (and (string? (first s))
           (if (empty? (rest s))
               true
               (nelos? (rest s))))))

;; nelos? : Sexp -> Boolean
;; GIVEN: a Sexp
;; WHERE: it is not an empty list.
;; RETURNS: true iff it is a NonEmpListOfSection.
;; EXAMPLES: 
(begin-for-test
  (check-equal?
   (nelos? '(("hello" ())))
   false)
  (check-equal?
   (nelos? '(("hello")))
   true))
;; STRATEGY: HOFC
#;
(define (nelos? nelos)
  (and (section? (first nelos))
       (if (empty? (rest nelos))
           true
           (nelos? (rest nelos)))))

(define (nelos? nelos)
  (andmap section? nelos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nested-to-flat : NestedRep -> FlatRep
;; GIVEN: the representation of an outline as a nested list.
;; RETURNS: the flat representation of the outline.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (nested-to-flat
    nr1)
   fr1))
;; STRATEGY: Function Composition
(define (nested-to-flat nr)
  (sections-to-flat nr empty 1))



;; section-to-flat : Section ListOfPosInt PosInt -> ListOfLine
;; GIVEN: a section s, a list of positive integer lopi, a positive 
;; integer pi.
;; WHERE: lopi is the section number of s's upper section us, n is the
;; the sequence number of s in us's subsection list.
;; RETURNS: the flat representation of s.
;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal?
   (section-to-flat '("The first section") empty 1)
   '(((1) "The first section")))
  (check-equal?
   (section-to-flat '("The first section"
                      ("A subsection with no subsections"))
                    empty 
                    1)
   '(((1) "The first section")
     ((1 1) "A subsection with no subsections"))))
   
;; STRATEGY: Structural Decomposition on s : Section.
(define (section-to-flat s lopi n)
  (cons (list (make-section-number lopi n) (first s))
          (if (empty? (rest s))
              (rest s)
              (sections-to-flat (rest s)
                                (make-section-number lopi n)
                                1))))

;; sections-to-flat : ListOfSection PosInt ListOfPosInt -> ListOfLine
;; GIVEN: losub is part of the list of the subsections of a section s,
;; upper-section-number is the section number of s, n is the order
;; number of losub in s's subsection list.
;; RETURN: the flat representation of losub.
;; STRATEGY: Structural Decomposition on losub : ListOfSection
(define (sections-to-flat losub upper-section-number n)
  (cond
    [(empty? losub) empty]
    [else (append (section-to-flat (first losub) 
                                   upper-section-number n)
                  (sections-to-flat (rest losub) 
                                    upper-section-number (+ n 1)))]))


;; make-section-number : ListOfPosInt PosInt -> ListOfPosInt
;; GIVEN: a section number of a section s's upper section us and the 
;; sequence number of s in us's subsections.
;; RETURNS: the section number of s
;; EXAMPLE/TESTS:
(begin-for-test
  (check-equal?
   (make-section-number (list 1 1) 1)
   (list 1 1 1)))
;; STRATEGY: Function Composition
(define (make-section-number lopi pi)
  (append lopi (list pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; flat-rep? : Sexp -> Boolean


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; flat-to-nest : FlatRep -> NestedRep
;; GIVEN: the representation of an outline as a flat-list.
;; RETURNS: the nested-list representation of the outline.
;; EXAMPLES/TESTS:
#;(begin-for-test
  (check-equal?
   (nested-to-flat
    fr1)
   nr1))
;; STRATEGY: 