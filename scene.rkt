#lang racket
(require "intersections.rkt")
(require "math.rkt")

(provide get-scene-objects)

;; spheres: (intersection-func center radius color)
(define *sphere-0* `(,intersect-sphere #(-3 -3 8) 2 (1 1 1)))
(define *sphere-1* `(,intersect-sphere #(2 -3 5) 2 (1 1 1)))

;; triangles ccw (intersection-func v0 v1 v2 normal color)
(define *back-wall-0* `(,intersect-triangle #(-5 -5 10) #(5 -5 10) #(5 5 10) #(0 0 -1) (1 1 1)))
(define *back-wall-1* `(,intersect-triangle #(-5 -5 10) #(5 5 10) #(-5 5 10) #(0 0 -1) (1 1 1)))
(define *floor-0* `(,intersect-triangle #(-5 -5 0) #(5 -5 0) #(5 -5 10) #(0 1 0) (1 1 1)))
(define *floor-1* `(,intersect-triangle #(-5 -5 0) #(5 -5 10) #(-5 -5 10) #(0 1 0) (1 1 1)))
(define *right-wall-0* `(,intersect-triangle #(5 -5 0) #(5 5 0) #(5 5 10) #(-1 0 0) (0 1 0)))
(define *right-wall-1* `(,intersect-triangle #(5 -5 0) #(5 5 10) #(5 -5 10) #(-1 0 0) (0 1 0)))
(define *left-wall-0* `(,intersect-triangle #(-5 -5 0) #(-5 -5 10) #(-5 5 10) #(1 0 0) (1 0 0)))
(define *left-wall-1* `(,intersect-triangle #(-5 -5 0) #(-5 5 10) #(-5 5 0) #(1 0 0) (1 0 0)))
(define *ceiling-0* `(,intersect-triangle #(-5 5 0) #(-5 5 10) #(5 5 10) #(0 -1 0) (1 1 1)))
(define *ceiling-1* `(,intersect-triangle #(-5 5 0) #(5 5 10) #(5 5 0) #(0 -1 0) (1 1 1)))
;(define *light-0* `(,intersect-triangle #(-1 4.9999 4) #(-1 4.99 6) #(1 4.99 6) #(0 1 0) (1 1 1)))
;(define *light-1* `(,intersect-triangle #(-1 4.9999 4) #(1 4.99 6) #(1 4.99 4) #(0 1 0) (1 1 1)))

;; ceiling quad light
(provide get-rand-light-point)
(define light-min #(-1 4.9999 4))
(define light-max #(1 4.9999 6))
(define (get-rand-light-point)
  (for/vector #:length 3
    ((a (in-vector light-min))
     (b (in-vector light-max)))
    (+ a (* (- b a) (random)))))

(define *objects* `(,*back-wall-0* ,*back-wall-1* ,*floor-0* ,*floor-1* ,*right-wall-0* ,*right-wall-1* ,*left-wall-0* ,*left-wall-1* ,*ceiling-0* ,*ceiling-1* ,*sphere-0* ,*sphere-1*))
(define (get-scene-objects)
  *objects*)