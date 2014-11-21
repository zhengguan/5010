;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname GP1.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct spider (nlegs volume))
(define-struct elephant (volume))
(define-struct boa (length grith volume))
(define-struct armadillo (length volume appetite))

;; A ZooAnimal is one of:
;; -- (make-spider NonNegInt PosReal)
;; -- (make-elephant PosReal)
;; -- (make-boa PosReal PosReal PosReal)
;; -- (make-armadillo PosReal PosReal PosReal)

;; INTERPRETATOIN:
;; (make-spider n vol) -- a spider with n lengs and 
;;                        volume vol
;; (make-elephant vol) -- an elephant with volume vol
;; (make-boa l g vol)  -- a boa constrictor with:
;;                         length l cm
;;                         girth g cm
;;                         volume vol cm^3
;; (make-armadillo l vol app) -- an armadillo with:
;;                                length l cm
;;                                volume vol cm^3
;;                                eats app cm^3/day of ants
;; NOTE: volume refers to the volume of the container
;; in which the animal is to be shipped.

; animal-fn : ZooAnimal -> ??
;(define (animal-fn a)
;  (cond
;    [(spider? a)
;     (..
;      (spider-nlegs a)
;      (spider-volume a))]
;    [(elephant? a)
;     (..
;      (elephant-volume a))]
;    [(boa? a)
;     (..
;      (boa-length a)
;      (boa-grith a)
;      (boa-volume a))]
;    [(armadillo? a)
;     (..
;      (armadillo-length a)
;      (armadillo-volume a)
;      (armadillo-appetite a))]))

(define spider1 (make-spider 7 10.0))
(define elephant1 (make-elephant 1.5e6))
(define boa1 (make-boa 105.6 8.2 2000))
(define armadillo1 (make-armadillo 56 3000 125))

     