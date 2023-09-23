;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname htdp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(circle 10 "solid" "red")

(define (y x) (* x x))

(define red-circle (circle 5 "solid" "red"))

(define (picture-of-dot height)
  (cond
    [(<= height 60)
     (place-image red-circle 50 height (empty-scene 100 60))]
    [else
     (place-image red-circle 50 60 (empty-scene 100 60))]))

(define someth "12")

(define (toNum in)
  (cond
    ((string? in) (string->number in))
    ((number? in) in)
    (#true 1)
    (#false 0)
    (else 0)))

(define (factorial n)
  (cond
    ((= n 0) 1)
    (else
     (* n (factorial (- n 1))))))

(define (combine n p)
  (/ (factorial n) (* (factorial (- n p)) (factorial p))))

(define (power m n)
  (cond
    ((= n 0) 1)
    (else
     (* m (power m (- n 1))))))

(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))

(distance-to-origin 3 4)

(define (string-insert s i)
  (string-append (substring s 0 i)
                 "_"
                 (substring s i)))
 
(string-insert "helloworld" 5)
