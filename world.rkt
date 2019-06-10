#lang typed/racket
(require math/flonum)

(provide (except-out (struct-out particle)
                     particle-position
                     %make-particle
                     particle-velocity)
         make-particle particle-x particle-y
         (except-out (struct-out world)
                     %make-world)
         world-particle
         make-world
         update-world
         current-gravitational-constant
         current-dt)

(struct world ([particles : (Immutable-HashTable Symbol particle)])
  #:constructor-name %make-world)

(struct particle ([id : Symbol]
                [position : FlVector]
                [velocity : FlVector]
                [m : Flonum])
    #:constructor-name %make-particle)

(: make-particle (-> Symbol
                   #:x Flonum
                   #:y Flonum
                   [#:z Flonum]
                   #:vx Flonum
                   #:vy Flonum
                   [#:vz Flonum]
                   #:m Flonum
                   particle))
(define (make-particle id #:x x #:y y #:z [z 0.0] #:vx vx #:vy vy #:vz [vz 0.0] #:m m)
  (%make-particle id
                (flvector x y z)
                (flvector vx vy vz)
                m))

(: particle-x (-> particle Flonum))
(define (particle-x p)
  (flvector-ref (particle-position p) 0))

(: particle-y (-> particle Flonum))
(define (particle-y p)
  (flvector-ref (particle-position p) 1))

(: particle-z (-> particle Flonum))
(define (particle-z p)
  (flvector-ref (particle-position p) 2))

(: make-world (-> (Listof particle) world))
(define (make-world ps)
  (%make-world
   (for/hash : (Immutable-HashTable Symbol particle)
       ([p : particle ps])
     (values (particle-id p) p))))

(: update-world (-> world world))
(define (update-world w)
  (let ([dt (current-dt)])
    (update-position (update-velocity w dt) dt)))

(: current-gravitational-constant (Parameterof Flonum))
(define current-gravitational-constant (make-parameter 6.67408e-11))

(: current-dt (Parameterof Flonum))
(define current-dt (make-parameter 0.2))

(: update-velocity (-> world Flonum world))
(define (update-velocity w dt)
  (define ps (world-particles w))
  (struct-copy
   world w
   [particles
    (cond
      [(= (hash-count ps) 1) ps]
      [else
       (for*/hash : (Immutable-HashTable Symbol particle)
           ([(id1 p1) ps]
            [(id2 p2) ps]
            #:unless (eq? id1 id2))
         (let* ([pos1 (particle-position p1)]
                [pos2 (particle-position p2)]
                [r (distance pos1 pos2)]
                [g (fl/ (fl* (current-gravitational-constant) (particle-m p2))
                        (fl* r r))]
                [u (flvector-scale (flvector- pos2 pos1)
                                   (fl/ 1.0 r))])
           (values id1
                   (struct-copy
                    particle p1
                    [velocity (flvector+ (particle-velocity p1)
                                         (flvector-scale u (fl* dt g)))]))))])]))

(: distance (-> FlVector FlVector Flonum))
(define (distance f1 f2)
  (flsqrt
   (flvector-sum
    (flvector-sqr
     (flvector- f1 f2)))))

(: update-position (-> world Flonum world))
(define (update-position w dt)
  (struct-copy
   world w
   [particles
    (for/hash : (Immutable-HashTable Symbol particle)
        ([(id p) (world-particles w)])
      (values id
              (struct-copy
               particle p
               [position
                (flvector+ (particle-position p)
                           (flvector-scale (particle-velocity p)
                                           dt))])))]))

(: world-particle (-> world Symbol particle))
(define (world-particle w id)
  (hash-ref (world-particles w) id))
