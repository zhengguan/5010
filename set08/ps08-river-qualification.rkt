;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps07-qualification) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(require "river.rkt")
(check-location "08" "river.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(begin-for-test
  ;; this only tests to see if required functions were provided. 
  ;; This does not test correctness at all.
  (check-provided list-to-pitchers)
  (check-provided pitchers-to-list)
  (check-provided pitchers-after-moves)
  (check-provided make-move)
  (check-provided move-src)
  (check-provided move-tgt)
  (check-provided make-fill)
  (check-provided make-dump)
  (check-provided move?)
  (check-provided fill?)
  (check-provided dump?)
  (check-provided fill-pitcher)
  (check-provided dump-pitcher)
  (check-provided solution)
  (check-provided (solution (list 8 5 3) 4))
  (check-provided (solution (list 8 5 3 16 32 65 128) 4))
  (check-provided (solution (list 10 7 3) 5))
  (check-provided (solution (list 10 7 3) 18))
  (check-provided (solution (list 7 3) 2))
  (check-provided (solution (list 2 5 10) 1))
  )
