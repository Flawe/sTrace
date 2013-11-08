#lang racket/gui
(require racket/gui/base)

(provide init-gui)


(define my-frame%
  (class frame%
    (define init #t)
    
    (define/public (is-initialized?)
      init)
    
    (define/augment on-close
      (lambda ()
        (set! init #f)))
    
    (super-new)))

(define (init-gui w h img)
  (let* ((frame (new my-frame%
                     (label "sTrace")
                     (width w)
                     (height (+ 64 h))
                     (style (cons 'no-resize-border null))))
         (canvas (new canvas%
                      (parent frame)
                      (paint-callback (lambda (canvas dc)
                                        (send dc draw-bitmap img 0 0)))))
         (btn-save (new button%
                        (parent frame)
                        (label "Save")
                        (callback (lambda (b e)
                                    (send frame set-cursor (make-object cursor% 'watch))
                                    (send img save-file "img.png" 'png 100)
                                    (send frame set-cursor #f))))))
    (send frame show #t)
    frame))