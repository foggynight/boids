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

;; Number of boids to simulate
(defparameter *boid-count* 32)

;; Screen width and height
(defparameter *sw* 1280)
(defparameter *sh* 720)

;;; UTILITY SECTION ------------------------------------------------------------

;; Convert radians to degrees
(defun degrees (radians)
  (* (/ radians pi) 180.0))

;; Convert degrees to radians
(defun radians (degrees)
  (* (/ degrees 180.0) pi))

;;; BOID SECTION ---------------------------------------------------------------

;; @TODO Write a comment for this
(defclass boid ()
  ((x :accessor boid-x)
   (y :accessor boid-y)
   (dx :accessor boid-dx)
   (dy :accessor boid-dy)))

;; Determine the angle of a boid based on its velocity, the angle is measured in
;; degrees relative to the right facing x-axis with a clockwise rotation
(defmethod boid-angle ((object boid))
  (let* ((dx (boid-dx object))
         (dy (boid-dy object))
         (angle 0))
    (if (= dx 0)
        (cond ((> dy 0) 90)
              ((< dy 0) 180)
              (t (setq angle 0)))
        (progn (setq angle (degrees (atan (/ dy dx))))
               ;; Adjust angle for correct quadrant
               (cond ((< dx 0) (setq angle (+ angle 180)))
                     ((< dy 0) (setq angle (+ angle 360))))))
    angle))

;; Initialize a list of boids with random parameters
(defun boid-init (boid-count)
  (let ((boid nil)
        (boid-list nil))
    (dotimes (i boid-count)
      (setq boid (make-instance 'boid))
      (setf (boid-x boid) (random *sw*))
      (setf (boid-y boid) (random *sh*))
      ;; @TODO Figure out how to handle initial velocity
      (setf (boid-dx boid) (- (random 9) 4))
      (setf (boid-dy boid) (- (random 9) 4))
      (setq boid-list (cons boid boid-list)))
    boid-list))

;;; RENDER SECTION -------------------------------------------------------------

;; Clear the screen
(defun render-clear (ren)
  (sdl2:set-render-draw-color ren 0 0 0 255)
  (sdl2:render-clear ren))

;; Draw the boids
(defun render-draw-boids (ren boid-list)
  (unless (eq boid-list nil)
    (let* ((boid (car boid-list))
           (x (boid-x boid))
           (y (boid-y boid))
           (angle (boid-angle boid)))
      (sdl2:set-render-draw-color ren 255 255 255 255)
      (sdl2:render-draw-line ren
                             x
                             y
                             (floor (+ x (* 10 (cos (radians angle)))))
                             (floor (+ y (* 10 (sin (radians angle))))))
      (sdl2:set-render-draw-color ren 0 255 0 255)
      (sdl2:render-draw-point ren x y)
      (render-draw-boids ren (cdr boid-list)))))

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
             (render-draw-boids ren boid-list)
             (sdl2:render-present ren)
             (sdl2:delay 5))
            (:quit () t)))))))
