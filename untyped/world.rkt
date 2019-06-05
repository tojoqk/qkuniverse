#lang /racket
(require math/flonum)

(provide object?
         make-object object-x object-y object-m
         world?
         make-world world-objects world-object
         update-world
         current-gravitational-constant
         current-dt)

(struct world ([objects : (Immutable-HashTable Symbol object)])
  #:constructor-name %make-world)

(struct object ([id : Symbol]
                [position : FlVector]
                [velocity : FlVector]
                [m : Flonum])
    #:constructor-name %make-object)

(: make-object (-> Symbol
                   #:x Flonum
                   #:y Flonum
                   [#:z Flonum]
                   #:vx Flonum
                   #:vy Flonum
                   [#:vz Flonum]
                   #:m Flonum
                   object))
(define (make-object id #:x x #:y y #:z [z 0.0] #:vx vx #:vy vy #:vz [vz 0.0] #:m m)
  (%make-object id
                (flvector x y z)
                (flvector vx vy vz)
                m))

(: object-x (-> object Flonum))
(define (object-x obj)
  (flvector-ref (object-position obj) 0))

(: object-y (-> object Flonum))
(define (object-y obj)
  (flvector-ref (object-position obj) 1))

(: make-world (-> (Listof object) world))
(define (make-world objs)
  (%make-world
   (for/hash : (Immutable-HashTable Symbol object)
       ([obj : object objs])
     (values (object-id obj) obj))))

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
  (define objs (world-objects w))
  (struct-copy
   world w
   [objects
    (cond
      [(= (hash-count objs) 1) objs]
      [else
       (for*/hash : (Immutable-HashTable Symbol object)
           ([(id1 obj1) objs]
            [(id2 obj2) objs]
            #:unless (eq? id1 id2))
         (let* ([pos1 (object-position obj1)]
                [pos2 (object-position obj2)]
                [r (distance pos1 pos2)]
                [g (fl/ (fl* (current-gravitational-constant) (object-m obj2))
                        (fl* r r))]
                [u (flvector-scale (flvector- pos2 pos1)
                                   (fl/ 1.0 r))])
           (values id1
                   (struct-copy
                    object obj1
                    [velocity (flvector+ (object-velocity obj1)
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
   [objects
    (for/hash : (Immutable-HashTable Symbol object)
        ([(id obj) (world-objects w)])
      (values id
              (struct-copy
               object obj
               [position
                (flvector+ (object-position obj)
                           (flvector-scale (object-velocity obj)
                                           dt))])))]))

(: world-object (-> world Symbol object))
(define (world-object w id)
  (hash-ref (world-objects w) id))
