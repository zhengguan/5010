;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide 
   make-editor
   editor-pre
   editor-post
   edit)

(define-struct editor [pre post])
; Editor is a (make-editor String String)
; Interpretation: 
; (make-editor pre post) means the texst in editor is
; (string-append pre post) with cursor displayed between pre and post
; Template:
;(define (editor-fn ed)
;  (...
;   (editor-pre ed)
;   (editor-post ed)))

;; edit : Editor KeyEvent -> Editor
;; GIVEN: an editor and a keyevent
;; RETURNS: a new editor with the keyevent happened on the editor
;; EXAMPLES:
;; (edit (make-editor "hello" " world") "left")
;; = (make-editor "hell" "o world")
;; (edit (make-editor "hello" " world") ",")
;; = (make-editor "hello," " world")
;; STRATEGY: Cases on ke : KeyEvent
(define (edit ed ke)
  (if (> (string-length ke) 1)
      (move-cursor ed ke)
      (single-letter ed ke)))

;; TESTS:
(begin-for-test
  (check-equal?
   (edit (make-editor "hello" " world") "left")
   (make-editor "hell" "o world"))
  (check-equal?
   (edit (make-editor "hello" " world") ",")
   (make-editor "hello," " world")))


;; move-cursor : Editor KeyEvent -> Editor
;; GIVEN: an editor and a keyevent
;; WHERE: the keyevent is "left" or "right"
;; RETURNS: the editor after the event happened
;; EXAMPLES:
;; (move-cursor (make-editor "hello" " world") "left")
;; = (make-editor "hell" "o world")
;; (move-cursor (make-editor "hello" " world") "right")
;; = (make-editor "hello " "world")
;; STRATEGY: cases on ke : KeyEvent
(define (move-cursor ed ke)
  (cond
    [(string=? "left" ke) 
     (cursor-move-left ed)]
    [(string=? "right" ke)
     (cursor-move-right ed)]))

;; TESTS:
(begin-for-test
  (check-equal? (move-cursor (make-editor "hello" " world") "left")
                (make-editor "hell" "o world"))
  (check-equal? (move-cursor (make-editor "hello" " world") "right")
                (make-editor "hello " "world")))


;; cursor-move-left : Editor -> Editor
;; cursor-move-right : Editor -> Editor
;; RETURNS: the editor after the cursor moves
;; EXAMPLES:
;; (cursor-move-left (make-editor "hello" " world"))
;; = (make-editor "hell" "o world")
;; (cursor-move-left (make-editor "" " world"))
;; = (make-editor "" " world")
;; (cursor-move-right (make-editor "hello" " world"))
;; = (make-editor "hello " "world")
;; (cursor-move-right (make-editor "hello" ""))
;; = (make-editor "hello" "")
;; STRATEGY: Structural Decomposition on ed : Editor
(define (cursor-move-left ed)
  (if(not (cursor-at-begin ed))
     (make-editor (string-remove-last (editor-pre ed))
                  (string-append (string-last (editor-pre ed))
                                 (editor-post ed)))
     ed))

(define (cursor-move-right ed)
  (if(not(cursor-at-end ed))
     (make-editor (string-append (editor-pre ed)
                                 (string-first (editor-post ed)))
                  (string-remove-first (editor-post ed)))
     ed))

;; TESTS:
(begin-for-test
  (check-equal? (cursor-move-left (make-editor "hello" " world"))
                (make-editor "hell" "o world"))
  (check-equal? (cursor-move-left (make-editor "" " world"))
                (make-editor "" " world"))
  (check-equal? (cursor-move-right (make-editor "hello" " world"))
                (make-editor "hello " "world"))
  (check-equal? (cursor-move-right (make-editor "hello" ""))
                (make-editor "hello" "")))



;; cursor-at-begin : Editor -> Boolean
;; cursor-at-end : Editor -> Boolean
;; RETURNS: true iff the cursor of the given editor at the begin/end
;; EXAMPLES:
;; (cursor-at-begin (make-editor "" "world")) = true
;; (cursor-at-end (make-editor "hello" "")) = true
;; STRATEGY: Structural Decomposition on ed : Editor
(define (cursor-at-begin ed)
  (zero?(string-length (editor-pre ed))))

(define (cursor-at-end ed)
  (zero?(string-length(editor-post ed))))

;; TESTS:
(begin-for-test
  (check-equal? (cursor-at-begin (make-editor "" "world"))  true)
  (check-equal? (cursor-at-end (make-editor "hello" "")) true))



;; string-first : String -> 1String
;; string-last : String -> 1String
;; WHERE: the given string should be a non-empty string
;; RETURNS: the first/last 1-letter string from the given string
;; EXAMPLE:
;; (string-first "hello") = "h"
;; (string-last "hello") = "o"
;; Strategy: Function Composition
(define (string-first s)
  (string-ith s 0))

(define (string-last s)
  (string-ith s (- (string-length s) 1)))

;; TESTS:
(begin-for-test
  (check-equal? (string-first "hello") "h")
  (check-equal? (string-last "hello") "o"))


;; string-remove-first : String -> String
;; string-remove-last : String -> String
;; WHERE: the given string should be a non-empty string
;; RETURNS: the string after the given string's first/last letter removed
;; EXAMPLE:
;; (string-remove-first "hello") = "ello"
;; (string-remove-last "hello") = "hell"
;; STRATEGY: Function Composition
(define (string-remove-first s)
  (substring s 1))

(define (string-remove-last s)
  (substring s 0 (- (string-length s) 1)))

;; TESTS:
(begin-for-test
  (check-equal? (string-remove-first "hello")  "ello")
  (check-equal? (string-remove-last "hello")  "hell"))

;; single-letter : Editor KeyEvent -> Editor
;; GIVEN: an Editor and a KeyEvent
;; WHERE: the KeyEvent should be a 1 letter KeyEvent
;; RETURNS: the editor after the keyevent happened
;; EXAMPLES:
;; (single-letter (make-editor "hello" " world") "\b")
;; = (make-editor "hell" " world")
;; (single-letter (make-editor "" " world") "\b")
;; = (make-editor "" " world")
;; (single-letter (make-editor "hello" " world") ",")
;; = (make-editor "hello," " world")
;; STRATEGY: Cases on ke : KeyEvent

(define (single-letter ed ke)
  (if(string=? ke "\b") 
     (if(not(cursor-at-begin ed))
        (make-editor (string-remove-last (editor-pre ed))
                     (editor-post ed))
        ed)
     (make-editor (string-append (editor-pre ed) ke)
                  (editor-post ed))))

;; TESTS:
(begin-for-test
  (check-equal? (single-letter (make-editor "hello" " world") "\b")
                (make-editor "hell" " world"))
  (check-equal? (single-letter (make-editor "" " world") "\b")
                (make-editor "" " world"))
  (check-equal? (single-letter (make-editor "hello" " world") ",")
                (make-editor "hello," " world")))