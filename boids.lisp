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

;; Number of boids to simulate.
(defparameter *boid-count* 32)

;; Radius of the circle containing a boid.
(defparameter *boid-radius* 16)
;; Angle between the two vectors pointing from the center of a boid to its left
;; and right tail points.
(defparameter *boid-tail-angle* 60)

;; Screen width and height.
(defparameter *sw* 1280)
(defparameter *sh* 720)

;;; UTILITY SECTION ------------------------------------------------------------

;; Convert radians to degrees.
(defun degrees (radians)
  (* (/ radians pi) 180.0))

;; Convert degrees to radians.
(defun radians (degrees)
  (* (/ degrees 180.0) pi))

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

;; Determine the angle of a boid based on its velocity.
(defmethod boid-angle ((object boid))
  (vec2-to-angle (boid-dx object) (boid-dy object)))

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
      (setf (boid-dx boid) (/ (- (random 21) 10) 10.0))
      (setf (boid-dy boid) (/ (- (random 21) 10) 10.0))
      (setq boid-list (cons boid boid-list)))
    boid-list))

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
  (unless (eq boid-list nil)
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
             (boid-list-update-pos boid-list)
             (sdl2:delay 7)) ; 7 ms ~= 1000 ms / 144 fps
            (:quit () t)))))))
