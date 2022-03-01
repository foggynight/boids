(define BOID-COLOR "lightgreen")
(define BOID-COUNT 20)

(define CANVAS-WIDTH 512)
(define CANVAS-HEIGHT 512)

(define SLEEP-TIME (/ 1 60))

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))

(define (%make-boid x y vx vy) (vector x y vx vy))

(define (boid-x boid) (vector-ref boid 0))
(define (boid-x-set! boid e) (vector-set! boid 0 e))

(define (boid-y boid) (vector-ref boid 1))
(define (boid-y-set! boid e) (vector-set! boid 1 e))

(define (boid-vx boid) (vector-ref boid 2))
(define (boid-vx-set! boid e) (vector-set! boid 2 e))

(define (boid-vy boid) (vector-ref boid 3))
(define (boid-vy-set! boid e) (vector-set! boid 3 e))

(define (boid-accel! boid ax ay)
  (boid-vx-set! boid (+ (boid-vx boid) ax))
  (boid-vy-set! boid (+ (boid-vy boid) ay)))

(define (boid-move! boid)
  (boid-x-set! boid (+ (boid-x boid) (boid-vx boid)))
  (boid-y-set! boid (+ (boid-y boid) (boid-vy boid))))

(define (make-boid)
  (define x (random-integer CANVAS-WIDTH))
  (define y (random-integer CANVAS-HEIGHT))
  (define vx (- (random-integer 30) 15))
  (define vy (- (random-integer 30) 15))
  (%make-boid x y vx vy))

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

(define (main)
  (define canvas (js-ref (getelem "canvas") "0"))
  (js-set! canvas "width" CANVAS-WIDTH)
  (js-set! canvas "height" CANVAS-HEIGHT)
  (set! ctx (js-invoke canvas "getContext" "2d"))
  (define boids
    (let loop ((i 0))
      (if (= i BOID-COUNT)
          '()
          (cons (make-boid) (loop (1+ i))))))
  (do ((i 0 (+ i 1))) (#f)
    (for-each boid-move! boids)
    (js-invoke ctx "clearRect" 0 0 CANVAS-WIDTH CANVAS-HEIGHT)
    (for-each draw-boid boids)
    (sleep SLEEP-TIME)))

(main)
