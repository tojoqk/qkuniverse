#lang racket/base
(require racket/contract)
(require 2htdp/universe)
(require 2htdp/image)
(require "world.rkt")

(provide (all-from-out "world.rkt")
         (contract-out
          [current-screen-width (parameter/c (and/c real? (not/c negative?)))]
          [current-screen-height (parameter/c (and/c real? (not/c negative?)))]
          [current-text-size (parameter/c (and/c byte? positive?))]
          [current-screen-scale (parameter/c flonum?)]
          [current-screen-mass-logscale (parameter/c flonum?)]
          [start (-> world? world?)]))

(define current-screen-width (make-parameter 640))
(define current-screen-height (make-parameter 480))
(define current-text-size (make-parameter 10))
(define current-screen-scale (make-parameter 1.0))
(define current-screen-mass-logscale (make-parameter 1.0))

(define (draw-world w)
  (for/fold ([screen (empty-scene (current-screen-width)
                                  (current-screen-height))])
            ([(_ obj) (world-objects w)])
    (place-image
     (draw-object obj)
     (* (object-x obj) (current-screen-scale))
     (* (object-y obj) (current-screen-scale))
     screen)))

(define (draw-object obj)
  (overlay
   (circle (* (log (object-m obj))
              (current-screen-mass-logscale))
           "outline" "black")
   (text (symbol->string (object-id obj))
         (current-text-size)
         "black")))

(define (start w)
  (big-bang w
            (on-tick update-world)
            (to-draw draw-world)))
