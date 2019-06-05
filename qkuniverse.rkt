#lang typed/racket/base
(require typed/2htdp/universe)
(require typed/2htdp/image)
(require "world.rkt")

(provide (all-from-out "world.rkt")
         current-screen-width
         current-screen-height
         current-text-size
         current-screen-scale
         current-screen-mass-logscale
         start)

(: current-screen-width (Parameterof Nonnegative-Real))
(define current-screen-width (make-parameter 640))
(: current-screen-height (Parameterof Nonnegative-Real))
(define current-screen-height (make-parameter 480))

(: current-text-size (Parameterof Positive-Byte))
(define current-text-size (make-parameter 10))

(: current-screen-scale (Parameterof Flonum))
(define current-screen-scale (make-parameter 1.0))

(: current-screen-mass-logscale (Parameterof Flonum))
(define current-screen-mass-logscale (make-parameter 1.0))

(: draw-world (-> world Image))
(define (draw-world w)
  (for/fold : Image
      ([screen : Image (empty-scene (current-screen-width)
                                    (current-screen-height))])
      ([(_ [obj : object]) (world-objects w)])
    (place-image
     (draw-object obj)
     (* (object-x obj) (current-screen-scale))
     (* (object-y obj) (current-screen-scale))
     screen)))

(: draw-object (-> object Image))
(define (draw-object obj)
  (overlay
   (circle (assert (* (assert (log (object-m obj)) real?)
                      (current-screen-mass-logscale))
                   positive?)
           "outline" "black")
   (text (symbol->string (object-id obj))
         (current-text-size)
         "black")))

(: start (-> world world))
(define (start w)
  (big-bang w : world
            (on-tick (λ ([w : world])
                       (update-world w)))
            (to-draw draw-world)))
