#lang racket

(provide vec-add)
(provide vec-sub)
(provide vec-mul)
(provide vec-norm)
(provide vec-dist)
(provide vec-dist-sqr)
(provide vec-dot)
(provide vec-cross)

(define (vec-add v0 v1)
  (for/vector #:length (vector-length v0)
    ((a (in-vector v0))
     (b (in-vector v1)))
    (+ a b)))

(define (vec-sub v0 v1)
  (for/vector #:length (vector-length v0)
    ((a (in-vector v0))
     (b (in-vector v1)))
    (- a b)))

(define (vec-mul v s)
  (for/vector #:length (vector-length v)
    ((a (in-vector v)))
    (* a s)))

(define (vec-norm v)
  (vec-mul v (/ 1 (sqrt (for/sum ((a (in-vector v))) (* a a))))))

(define (vec-dist v0 v1)
  (let ((v (vec-sub v1 v0)))
    (sqrt (for/sum ((a (in-vector v))) (* a a)))))

(define (vec-dist-sqr v0 v1)
  (let ((v (vec-sub v1 v0)))
    (for/sum ((a (in-vector v))) (* a a))))
  
(define (vec-dot v0 v1)
  (for/fold ((sum 0))
    ((a (in-vector v0))
     (v (in-vector v1)))
    (+ sum (* a v))))

;; note: 3d vectors only
(define (vec-cross v0 v1)
  (vector (- (* (vector-ref v0 1) (vector-ref v1 2)) (* (vector-ref v0 2) (vector-ref v1 1)))
          (- (* (vector-ref v0 2) (vector-ref v1 0)) (* (vector-ref v0 0) (vector-ref v1 2)))
          (- (* (vector-ref v0 0) (vector-ref v1 1)) (* (vector-ref v0 1) (vector-ref v1 0)))))