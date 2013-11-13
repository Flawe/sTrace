#lang racket
(require "math.rkt")

(provide phong)
(provide ggx)

(define *ambient* #(0.1 0.1 0.1))
(define *brightness* 1)
(define *gamma* 2.2)
(define *exposure* 0)
(define *light-intensity* 100)

;; brightness, exposure and gamma
(define (finalize-color col)
  (vec-expt (vec-mul (vec-mul col *brightness*) (expt 2 *exposure*)) (/ 1 *gamma*)))

; http://graphicrants.blogspot.co.uk/2013/08/specular-brdf-reference.html

;; phong
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *phong-spec-pow* 8)
(define (phong l v n ldist-sqr col)
  (vec-saturate (finalize-color (vec-mul (vec-mul (vec-add (vec-add *ambient* 
                                                                    (vec-mul col (max 0 (vec-dot n l))))
                                                           (vec-mul #(1 1 1) (expt (max 0 (vec-dot (vec-reflect l n) v)) *phong-spec-pow*))) *light-intensity*) (/ 1 ldist-sqr)))))


;; ggx
;; http://www.gamedev.net/topic/638197-cook-torrance-brdf-general/#entry5028381
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fresnel-schlick f0 vdh)
  (+ f0 (* (- 1 f0) (expt (- 1 (max vdh 0)) 5))))

(define (ndf-ggx a ndh)
  (let ((a2 (* a a)))
    (/ a2 (* pi (expt (+ (* (* ndh ndh) (- a2 1)) 1) 2)))))

(define (geometry-ggx a ndv)
  (let ((a2 (* a a)))
    (/ (* 2 ndv) (+ ndv (sqrt (+ a2 (* (- 1 a2) (expt ndv 2))))))))

(define *f0* 0.04)
(define *a* 0.8)
(define (ggx l v n ldist-sqr col)
  (let ((ndl (vec-dot n l)))
    (if (<= ndl 0)
        #(0 0 0)
        (let* ((h (vec-norm (vec-add l v)))
               (ndh (vec-dot n h))
               (ndv (vec-dot n v))
               (vdh (vec-dot v h))
               (f (fresnel-schlick *f0* vdh))
               (d (ndf-ggx *a* ndh))
               (g (geometry-ggx *a* ndv))
               (denom (/ 1 (* 4 ndl ndv)))
               (spec (vec-mul #(1 1 1) (* f d g denom)))
               (diff (vec-mul col (/ 1 pi))))
          (vec-saturate (finalize-color (vec-mul (vec-mul (vec-mul (vec-add diff spec) ndl) *light-intensity*) (/ 1 ldist-sqr))))))))