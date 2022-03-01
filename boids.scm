;; boids.scm - Boids algorithm implemented in BiwaScheme.
;; Copyright (C) 2022 Robert Coffey
;; Released under the GPLv3.

;; config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BOID-COLOR "lightgreen")
(define BOID-COUNT 2)

(define CANVAS-WIDTH 800)
(define CANVAS-HEIGHT 600)

(define NEIGHBOR-DIST 100)

(define SPEED-LIMIT 1)

(define SLEEP-TIME (/ 1 60))

(define WALL-DIST 64)
(define WALL-ACCEL 16)

;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

;; vec2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define vec2 cons)
(define vec2-x car) (define vec2-x-set! set-car!)
(define vec2-y cdr) (define vec2-y-set! set-cdr!)

(define (vec2-len x y)
  (sqrt (+ (* x x) (* y y))))

(define (vec2-norm x y)
  (let ((len (vec2-len x y)))
    (cons (/ x len) (/ y len))))

;; boid ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%make-boid id x y vx vy) (vector id x y vx vy))

(define (make-boid id)
  (define x (random-integer CANVAS-WIDTH))
  (define y (random-integer CANVAS-HEIGHT))
  (define vx (- (random-integer 30) 15))
  (define vy (- (random-integer 30) 15))
  (%make-boid id x y vx vy))

(define (boid-id boid) (vector-ref boid 0))

(define (boid-x boid) (vector-ref boid 1))
(define (boid-x-set! boid e) (vector-set! boid 1 e))

(define (boid-y boid) (vector-ref boid 2))
(define (boid-y-set! boid e) (vector-set! boid 2 e))

(define (boid-vx boid) (vector-ref boid 3))
(define (boid-vx-set! boid e) (vector-set! boid 3 e))

(define (boid-vy boid) (vector-ref boid 4))
(define (boid-vy-set! boid e) (vector-set! boid 4 e))

(define (boid-speed boid)
  (vec2-len (boid-vx boid) (boid-vy boid)))

(define (boid-move! boid)
  (boid-x-set! boid (+ (boid-x boid) (boid-vx boid)))
  (boid-y-set! boid (+ (boid-y boid) (boid-vy boid))))

(define (boid-accel! boid ax ay)
  (boid-vx-set! boid (+ (boid-vx boid) ax))
  (boid-vy-set! boid (+ (boid-vy boid) ay)))

(define (boid-limit! boid)
  (when (> (boid-speed boid) SPEED-LIMIT)
    (let ((norm (vec2-norm (boid-vx boid) (boid-vy boid))))
      (boid-vx-set! boid (* (vec2-x norm) SPEED-LIMIT))
      (boid-vy-set! boid (* (vec2-y norm) SPEED-LIMIT)))))

(define (neighbors? b1 b2)
  (and (not (= (boid-id b1) (boid-id b2)))
       (<= (vec2-len (- (boid-x b2) (boid-x b1))
                     (- (boid-y b2) (boid-y b1)))
           NEIGHBOR-DIST)))

(define (get-neighbors boid boids)
  (let loop ((lst boids))
    (cond ((null? lst) '())
          ((neighbors? boid (car lst)) (cons (car lst) (loop (cdr lst))))
          (else (loop (cdr lst))))))

(define (avoid-walls! boid)
  (define ax 0)
  (define ay 0)
  (cond ((< (boid-x boid) WALL-DIST) (set! ax WALL-ACCEL))
        ((< (- CANVAS-WIDTH (boid-x boid)) WALL-DIST)
         (set! ax (- WALL-ACCEL))))
  (cond ((< (boid-y boid) WALL-DIST) (set! ay WALL-ACCEL))
        ((< (- CANVAS-HEIGHT (boid-y boid)) WALL-DIST)
         (set! ay (- WALL-ACCEL))))
  (boid-accel! boid ax ay))

;; draw ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ctx)

(define (draw-line x1 y1 x2 y2 color)
  (js-set! ctx "strokeStyle" color)
  (js-invoke ctx "beginPath")
  (js-invoke ctx "moveTo" x1 y1)
  (js-invoke ctx "lineTo" x2 y2)
  (js-invoke ctx "stroke"))

;; TODO: Replace with drawing triangles.
(define (draw-boid boid)
  (draw-line (boid-x boid) (boid-y boid)
             (+ (boid-x boid) (boid-vx boid))
             (+ (boid-y boid) (boid-vy boid))
             BOID-COLOR))

;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main)
  (define canvas (js-ref (getelem "canvas") "0"))
  (js-set! canvas "width" CANVAS-WIDTH)
  (js-set! canvas "height" CANVAS-HEIGHT)
  (set! ctx (js-invoke canvas "getContext" "2d"))
  (define boids
    (let loop ((i 0))
      (if (= i BOID-COUNT)
          '()
          (cons (make-boid i) (loop (1+ i))))))
  (do ((i 0 (+ i 1))) (#f)
    (for-each (lambda (boid)
                (let ((neighbors (get-neighbors boid boids)))
                  (null? neighbors)
                  )
                (avoid-walls! boid)
                )
              boids)
    (for-each boid-limit! boids)
    (for-each boid-move! boids)
    (js-invoke ctx "clearRect" 0 0 CANVAS-WIDTH CANVAS-HEIGHT)
    (for-each draw-boid boids)
    (sleep SLEEP-TIME)))

(main)
