#lang racket
(require "math.rkt")

(provide *no-intersection*)

(provide intersect-sphere)
(provide intersect-triangle)

(define *no-intersection* '(0 0 0 0 0))

;; ray-sphere intersection where sphere is (intersection-func center radius color)
(define (intersect-sphere ro rd sphere)
  (let* ((rts (vec-sub (second sphere) ro))
         (tca (vec-dot rts rd)))
    (if (< tca 0)
        *no-intersection*
        (let ((dsq (- (vec-dot rts rts) (* tca tca)))
              (rsq (* (third sphere) (third sphere)))) ; TODO: precompute sphere's squared radius
         (if (> dsq rsq)
             *no-intersection*
             (let* ((thc (sqrt (- rsq dsq)))
                    (ta (- tca thc)) ; first intersection point
                    (tb (+ tca thc)) ; second intersection point
                    (t (if (> ta 0) ta tb))
                    (ipos (vec-add ro (vec-mul rd t)))
                    (inorm (vec-norm (vec-sub ipos (second sphere)))))
               `(1 ,(fourth sphere) ,ipos ,inorm))))))) ; (status color position normal)

;; ray-triangle intersection where triangle is (intersection-func v0 v1 v2 normal color)
(define (intersect-triangle ro rd triangle)
  (let* ((n (vec-cross (vec-sub (fourth triangle) (second triangle)) (vec-sub (third triangle) (second triangle))))
         (ndr (vec-dot n rd)))
    (if (> ndr -0.0001)
        *no-intersection* ; only hit front facing tris
        (let* ((d (vec-dot n (second triangle)))
               (t (/ (vec-dot n (vec-sub (second triangle) ro)) ndr)))
          (if (< t 0)
              *no-intersection* ; triangle is behind camera
              (let* ((ipos (vec-add ro (vec-mul rd t)))
                     (v0v1 (vec-sub (third triangle) (second triangle)))
                     (v0p (vec-sub ipos (second triangle)))
                     (ed0 (vec-dot n (vec-cross v0p v0v1))))
                (if (< ed0 -0.0001) ; inside-out test edge0
                    *no-intersection*
                    (let* ((v1p (vec-sub ipos (third triangle)))
                           (v1v2 (vec-sub (fourth triangle) (third triangle)))
                           (ed1 (vec-dot n (vec-cross v1p v1v2))))
                      (if (< ed1 -0.0001) ; inside-out test edge1
                          *no-intersection*
                          (let* ((v2p (vec-sub ipos (fourth triangle)))
                                 (v2v0 (vec-sub (second triangle) (fourth triangle)))
                                 (ed2 (vec-dot n (vec-cross v2p v2v0))))
                            (if (< ed2 -0.0001) ; inside-out test edge2
                                *no-intersection*
                                `(1 ,(sixth triangle) ,ipos ,(vec-norm n))))))))))))) ; (status color position normal)
                     