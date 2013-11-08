#lang racket
(require racket/gui/base)
(require "gui.rkt")
(require "intersections.rkt")
(require "math.rkt")
(require "scene.rkt")

(define *max-ray-steps* 10)
(define *bg-col* '(0 0 0))
(define *img-width* 256)
(define *img-height* 256)
(define *pixel-width* (/ 1 *img-width*))
(define *pixel-height* (/ 1 *img-height*))
(define *half-pixel-width* (/ *pixel-width* 2))
(define *half-pixel-height* (/ *pixel-height* 2))

(define *cam-pos* #(0 0 -8.5))
(define *cam-dir* #(0 0 1))
(define *cam-up* #(0 1 0))
(define *cam-right* #(1 0 0))
(define *cam-fov* (degrees->radians 30.0))

(define *point-light* #(0 3 5))

; we use this argb bitmap to render onto the gui canvas
(define img (make-bitmap *img-width* *img-height*))


;; recursively subdivide the unit square for sub-pixel sampling
(define *square-splits* '((-0.25 -0.25) (0.25 -0.25) (-0.25 0.25) (0.25 0.25)))
(define (get-sample-pos-rec sidx w h pos)
  (if (= sidx 0)
      pos ; TODO: apply jitter
      (let* ((quadrant (modulo (- sidx 1) 4))
             (split (list-ref *square-splits* quadrant))
             (new-pos `(,(+ (first pos) (* (first split) w))
                        ,(+ (second pos) (* (second split) h)))))
        (get-sample-pos-rec (exact-floor (/ (- sidx 1) 4)) (/ w 2) (/ h 2) new-pos))))
      
(define (get-sample-pos sidx)
  (get-sample-pos-rec (+ sidx 1) *pixel-width* *pixel-height* '(0 0)))

;; returns a ray direction for the image plane coordinate
(define (get-screen-ray x y pass)
  (let* ((sample-pos (get-sample-pos pass))
         (nx (+ (/ x *img-width*) (first sample-pos) *half-pixel-width* -0.5))
         (ny (- (- 1 (+ (/ y *img-height*) (second sample-pos) *half-pixel-height*)) 0.5))
         (d (/ 0.5 (tan *cam-fov*)))
         (rd (vec-add (vec-mul *cam-dir* d) (vec-add (vec-mul *cam-right* nx) (vec-mul *cam-up* ny)))))
    (vec-norm rd)))

;; calls the relevant intersection function for all objects in the scene
;; and builds a list of intersection results
(define (traverse-scene ro rd)
  (second (foldl (lambda (intersection closest) ; closest contains (dist-sqr intersection)
                   (if (> (first intersection) 0)
                       (let ((dist (vec-dist-sqr ro (third intersection))))
                         (if (< dist (first closest))
                             `(,dist ,intersection)
                             closest))
                       closest))
                 `(9999999 ,*no-intersection*)
                 (map (lambda (object)
                        ((first object) ro rd object)) ;; evaluate the object's intersection function
                      (get-scene-objects)))))

;; traces a ray through the scene and returns a color
(define (trace ro rd step)
  (if (> step *max-ray-steps*)
      *bg-col*
      (let* ((result (traverse-scene ro rd))
             (status (first result))
             (color (second result))
             (pos (third result))
             (norm (fourth result)))
        (if (> status 0)
            (let* ((lvec (vec-norm (vec-sub *point-light* pos)))
                   (ndl (max 0 (vec-dot norm lvec)))
                   (vcol (list->vector color)))
              (vector->list (vec-add (vec-mul (vec-mul vcol 0.9) ndl) (vec-mul vcol 0.1))))
            *bg-col*))))

;; spawns rays through all pixels and collects the results
(define (render-pixels index pixels pass)
  (if (< index 0)
      pixels
      (render-pixels (- index 1)
                     (cons (trace *cam-pos*
                                  (get-screen-ray (modulo index *img-width*) (quotient index *img-width*) pass)
                                  1)
                           pixels)
                     pass)))

;; render the image pixels starting with last one (width * height - 1).
;; note that the results will be prepended do the list
(define (render-image pass)
    (render-pixels (- (* *img-width* *img-height*) 1) null pass))

;; converts the pixel data list into an array of bytes and adds a dummy alpha channel needed
;; for rendering onto the canvas
(define (pixels->bytes pixels spp)
  (list->bytes (foldr (lambda (pixel byte-list)
                        (cons 255 (foldr (lambda (c b)
                                           (cons (exact-round (* (/ c spp) 255)) b))
                                         byte-list
                                         pixel)))
                      null
                      pixels)))


(define *frame* (init-gui *img-width* *img-height* img))
(send *frame* create-status-line)

(define (do-async-render pixels spp)
  (if (not (send *frame* is-initialized?))
      null
      (begin
        (send *frame* set-status-text (~a "Samples per pixel: " spp))
        (let ((new-pixels (if (eq? pixels null)
                              (render-image spp)
                              (map (lambda (p0 p1) ; accumulate samples
                                     (map (lambda (c0 c1)
                                            (+ c0 c1))
                                          p0
                                          p1))
                                   pixels
                                   (render-image spp)))))
          (send img set-argb-pixels 0 0 *img-width* *img-height* (pixels->bytes new-pixels (+ 1 spp)))
          (send *frame* refresh)
          (sleep 0)
          (do-async-render new-pixels (+ 1 spp))))))

(define (async-render)
  (begin
    (do-async-render null 0)
    (displayln "Render thread finished...")))

(define *render-thread* (thread async-render))
      
