;;;
;; --- boids.lisp ---
;;
;; Boids algorithm implemented in Common Lisp using the SDL2 library.
;;
;; -- References --
;; 1. Flocks, Herds, and Schools: A Distributed Behavioral Model, Craig
;;    Reynolds, https://www.cs.toronto.edu/~dt/siggraph97-course/cwr87/
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the GPLv2 license
;;;

(require :sdl2)

;;; CONFIG SECTION -------------------------------------------------------------

;;; -- GENERAL --
;; Number of boids to simulate.
(defparameter *boid-count* 100)
;; Maximum speed of the boids.
(defparameter *boid-speed-limit* 10)
;; Screen width and height.
(defparameter *sw* 1280)
(defparameter *sh* 720)

;;; -- BOID GEOMETRY --
;; Radius of the circle containing a boid.
(defparameter *boid-radius* 8)
;; Angle between the two vectors pointing from the center of a boid to its left
;; and right tail points.
(defparameter *boid-tail-angle* 60)

;;; -- BOID FOV --
;; Radius of the boids' FOV.
(defparameter *boid-fov-radius* 100)
;; Maximum angle of the boids' FOV measured as the difference from the forward
;; angle of a given boid.
(defparameter *boid-fov-max-angle* 120)

;;; -- BOID AVOIDANCE --
;; Distance from an obstacle indicating a boid should attempt to avoid colliding
;; with it.
(defparameter *boid-avoidance-distance* *boid-fov-radius*)
;; Magnitude with which a boid can accelerate away from an obstacle.
(defparameter *boid-avoidance-acceleration* 1.0)

;;; -- BOID ALIGNMENT --
;; Magnitude with which a boid can accelerate to align with its neighbors.
(defparameter *boid-alignment-acceleration* 0.05)

;;; -- BOID COHESION --
;; Magnitude with which a boid can accelerate to cohere with its neighbors.
(defparameter *boid-cohesion-acceleration* 0.005)

;;; -- BOID SEPARATION --
;; Distance from another boid indicating a boid should attempt to separate
;; itself from it.
(defparameter *boid-separation-distance* 32)
;; Magnitude with which a boid can accelerate to separate from its neighbors.
(defparameter *boid-separation-acceleration* 0.05)

;;; UTILITY SECTION ------------------------------------------------------------

;; Convert radians to degrees.
(defun degrees (radians)
  (* (/ radians pi) 180.0))

;; Convert degrees to radians.
(defun radians (degrees)
  (* (/ degrees 180.0) pi))

;; Perform vector addition on two x-y vectors, creating a new vector.
(defun vec2-add (v0 v1)
  `(,(+ (car v0) (car v1)) ,(+ (cadr v0) (cadr v1))))

;; Perform vector subtraction on two x-y vectors, creating a new vector.
(defun vec2-sub (v0 v1)
  `(,(- (car v0) (car v1)) ,(- (cadr v0) (cadr v1))))

;; Perform scalar multiplication on an x-y vector, creating a new vector.
(defun vec2-mul (v c)
  `(,(* (car v) c) ,(* (cadr v) c)))

;; Perform scalar division on an x-y vector, creating a new vector.
(defun vec2-div (v c)
  `(,(/ (car v) c) ,(/ (cadr v) c)))

;; TODO Update vec2-length and vec2-angle to take a vector parameter rather than
;; the two individual components.

;; Determine the length of an x-y vector.
(defun vec2-length (x y)
  (sqrt (+ (expt x 2) (expt y 2))))

;; Convert an x-y vector into an angle in degrees relative to the positive
;; x-axis with a positive rotation.
(defun vec2-to-angle (x y)
  (let* ((angle 0))
    (if (= x 0)
        (cond ((> y 0) (setq angle 90))
              ((< y 0) (setq angle 270))
              (t (setq angle 0)))
        (progn (setq angle (degrees (atan (/ y x))))
               ;; Adjust angle for correct quadrant
               (cond ((< x 0) (setq angle (+ angle 180)))
                     ((< y 0) (setq angle (+ angle 360))))))
    angle))

;;; BOID SECTION ---------------------------------------------------------------

;; Boid class representing a boid in the world. Boid's have a position and
;; velocity, both with an x and y component.
(defclass boid ()
  ((x :accessor boid-x)
   (y :accessor boid-y)
   (dx :accessor boid-dx)
   (dy :accessor boid-dy)))

;; Get the position of a boid represented by a list containing the x-y
;; components.
(defmethod boid-position ((object boid))
  `(,(boid-x object) ,(boid-y object)))

;; Get the velocity of a boid represented by a list containing the x-y
;; components.
(defmethod boid-velocity ((object boid))
  `(,(boid-dx object) ,(boid-dy object)))

;; Determine the angle of a boid based on its velocity.
(defmethod boid-angle ((object boid))
  (vec2-to-angle (boid-dx object) (boid-dy object)))

;; Accelerate a boid to avoid the edges of the world.
(defmethod boid-avoid-edges ((object boid))
  (let ((x (boid-x object))
        (y (boid-y object)))
    (if (<= x *boid-avoidance-distance*)
        (setf (boid-dx object) (+ (boid-dx object) *boid-avoidance-acceleration*)))
    (if (>= x (- *sw* *boid-avoidance-distance*))
        (setf (boid-dx object) (- (boid-dx object) *boid-avoidance-acceleration*)))
    (if (<= y *boid-avoidance-distance*)
        (setf (boid-dy object) (+ (boid-dy object) *boid-avoidance-acceleration*)))
    (if (>= y (- *sh* *boid-avoidance-distance*))
        (setf (boid-dy object) (- (boid-dy object) *boid-avoidance-acceleration*)))))

;; Get a list of the neighbors of a boid.
;; TODO Ignore self -- Currently counts self as neighbor
;; TODO Check if target is within view angle of the boid
(defmethod boid-get-neighbors ((object boid) boid-list)
  (if boid-list
      (let* ((target (car boid-list))
             (distance (vec2-length (- (boid-x target) (boid-x object))
                                    (- (boid-y target) (boid-y object)))))
        (if (<= distance *boid-fov-radius*)
            (cons target (boid-get-neighbors object (cdr boid-list)))
            (boid-get-neighbors object (cdr boid-list))))
      nil))

;; Gradually align a boid's velocity vector with the mean average velocity
;; vector of its neighbors.
(defmethod boid-align-with-neighbors ((object boid) neighbor-list)
  (let* ((mean-vel (vec2-div (reduce #'vec2-add (map 'list #'boid-velocity neighbor-list))
                             (length neighbor-list)))
         (delta-vel (vec2-sub mean-vel (boid-velocity object))))
    (setf (boid-dx object) (+ (boid-dx object)
                              (* *boid-alignment-acceleration* (car delta-vel))))
    (setf (boid-dy object) (+ (boid-dy object)
                              (* *boid-alignment-acceleration* (cadr delta-vel))))))

;; Gradually accelerate a boid's velocity vector towards the average position of
;; its neighbors.
(defmethod boid-cohere-with-neighbors ((object boid) neighbor-list)
  (let* ((mean-pos (vec2-div (reduce #'vec2-add (map 'list #'boid-position neighbor-list))
                             (length neighbor-list)))
         (delta-pos (vec2-sub mean-pos (boid-position object))))
    (setf (boid-dx object) (+ (boid-dx object)
                              (* *boid-cohesion-acceleration* (car delta-pos))))
    (setf (boid-dy object) (+ (boid-dy object)
                              (* *boid-cohesion-acceleration* (cadr delta-pos))))))

;; Maintain distance between a boid and its neighbors by applying accelerations
;; to its velocity vector pointing away from neighbors should they be too close.
(defmethod boid-separate-from-neighbors ((object boid) neighbor-list)
  (when neighbor-list
    (let* ((delta-pos (vec2-sub (boid-position (car neighbor-list))
                                (boid-position object)))
           (delta-x (car delta-pos))
           (delta-y (cadr delta-pos)))
      (when (< (vec2-length (car delta-pos) (cadr delta-pos)) *boid-separation-distance*)
        (unless (= delta-x 0)
          (setf (boid-dx object) (- (boid-dx object)
                                    (/ *boid-separation-acceleration* delta-x))))
        (unless (= delta-y 0)
          (setf (boid-dy object) (- (boid-dy object)
                                    (/ *boid-separation-acceleration* delta-y))))))
    (boid-separate-from-neighbors object (cdr neighbor-list))))

;; Limit the speed of a boid.
(defmethod boid-limit-speed ((object boid))
  (let ((speed (vec2-length (boid-dx object) (boid-dy object))))
    (when (> speed *boid-speed-limit*)
      (setf (boid-dx object) (* (boid-dx object) (/ *boid-speed-limit* speed)))
      (setf (boid-dy object) (* (boid-dy object) (/ *boid-speed-limit* speed))))))

;; Update the position of a boid based on its velocity.
(defmethod boid-update-pos ((object boid))
  (setf (boid-x object) (+ (boid-x object) (boid-dx object)))
  (setf (boid-y object) (+ (boid-y object) (boid-dy object))))

;; Initialize a list of boids with random parameters.
(defun boid-init (boid-count)
  (let ((boid nil)
        (boid-list nil))
    (dotimes (i boid-count)
      (setq boid (make-instance 'boid))
      (setf (boid-x boid) (random *sw*))
      (setf (boid-y boid) (random *sh*))
      ;; Random velocity with components in the range:
      ;; { v:real | -1 <= v <= 1 and v * 10 == floor(v * 10) }
      ;; TODO Replace magic numbers with global variables
      (setf (boid-dx boid) (- (random 21) 10))
      (setf (boid-dy boid) (- (random 21) 10))
      (setq boid-list (cons boid boid-list)))
    boid-list))

;; Limit the speeds of a list of boids.
(defun boid-list-limit-speed (boid-list)
  (unless (eq boid-list nil)
    (boid-limit-speed (car boid-list))
    (boid-list-limit-speed (cdr boid-list))))

;; Update the positions of a list of boids.
(defun boid-list-update-pos (boid-list)
  (unless (eq boid-list nil)
    (boid-update-pos (car boid-list))
    (boid-list-update-pos (cdr boid-list))))

;;; RENDER SECTION -------------------------------------------------------------

;; Clear the screen.
(defun render-clear (ren)
  (sdl2:set-render-draw-color ren 0 0 0 255)
  (sdl2:render-clear ren))

;; Draw a boid.
(defun render-draw-boid (ren boid)
  (let* ((x (boid-x boid))
         (y (boid-y boid))
         (angle (vec2-to-angle (boid-dx boid) (boid-dy boid)))
         (forward-x (floor (+ x (* *boid-radius* (cos (radians angle))))))
         (forward-y (floor (+ y (* *boid-radius* (sin (radians angle))))))
         (left-x (floor (+ x (* *boid-radius* (cos (radians (- angle (- 180 (/ *boid-tail-angle* 2)))))))))
         (left-y (floor (+ y (* *boid-radius* (sin (radians (- angle (- 180 (/ *boid-tail-angle* 2)))))))))
         (right-x (floor (+ x (* *boid-radius* (cos (radians (+ angle (- 180 (/ *boid-tail-angle* 2)))))))))
         (right-y (floor (+ y (* *boid-radius* (sin (radians (+ angle (- 180 (/ *boid-tail-angle* 2))))))))))
    (sdl2:set-render-draw-color ren 0 255 0 255)
    (sdl2:render-draw-line ren forward-x forward-y left-x left-y)
    (sdl2:render-draw-line ren forward-x forward-y right-x right-y)
    (sdl2:render-draw-line ren left-x left-y right-x right-y)))

;; Draw a list of boids.
(defun render-draw-boid-list (ren boid-list)
  (when boid-list
    (render-draw-boid ren (car boid-list))
    (render-draw-boid-list ren (cdr boid-list))))

;;; MAIN SECTION ---------------------------------------------------------------

(defun main ()
  (let ((boid-list (boid-init *boid-count*)))
    (sdl2:with-init (:everything)
      (sdl2:with-window (win :title "boids" :w *sw* :h *sh* :flags '(:shown))
        (sdl2:with-renderer (ren win :flags '(:accelerated))
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym key)
             (when (sdl2:scancode= (sdl2:scancode-value key) :scancode-escape)
               (sdl2:push-event :quit)))
            (:idle
             ()
             (render-clear ren)
             (render-draw-boid-list ren boid-list)
             (sdl2:render-present ren)
             (dolist (boid boid-list)
               (boid-avoid-edges boid)
               (let ((neighbor-list (boid-get-neighbors boid boid-list)))
                 (when neighbor-list
                   (boid-align-with-neighbors boid neighbor-list)
                   (boid-cohere-with-neighbors boid neighbor-list)
                   (boid-separate-from-neighbors boid neighbor-list))))
             (boid-list-limit-speed boid-list)
             (boid-list-update-pos boid-list)
             (sdl2:delay 7)) ; 7 ms ~= 1000 ms / 144 fps
            (:quit () t)))))))
