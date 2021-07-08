;;; PACKAGE SECTION ------------------------------------------------------------

(require :sdl2)

;;; CONFIG SECTION -------------------------------------------------------------

;;; -- GENERAL --
(defparameter *boid-count* 30
  "Number of boids to simulate.")
(defparameter *boid-speed-limit* 10
  "Maximum speed of the boids.")
(defparameter *screen-width* 1280
  "Screen width in pixels.")
(defparameter *screen-height* 720
  "Screen height in pixels.")

;;; -- BOID GEOMETRY --
(defparameter *boid-radius* 8
  "Radius of the circle containing a boid.")
(defparameter *boid-tail-angle* 60
  "Angle between the two vectors pointing from the center of a boid to its left
and right tail points.")

;;; -- BOID FOV --
(defparameter *boid-fov-radius* 64
  "Radius of the boids' FOV.")

;;; -- BOID AVOIDANCE --
(defparameter *boid-avoidance-distance* 128
  "Distance from an obstacle indicating a boid should attempt to avoid colliding
with it.")
(defparameter *boid-avoidance-acceleration* 0.1
  "Magnitude with which a boid can accelerate away from an obstacle.")

;;; -- BOID ALIGNMENT --
(defparameter *boid-alignment-acceleration* 0.01
  "Magnitude with which a boid can accelerate to align with its neighbors.")

;;; -- BOID COHESION --
(defparameter *boid-cohesion-acceleration* 0.0005
  "Magnitude with which a boid can accelerate to cohere with its neighbors.")

;;; -- BOID SEPARATION --
(defparameter *boid-separation-distance* 16
  "Distance from another boid indicating a boid should attempt to separate
itself from it.")
(defparameter *boid-separation-acceleration* 0.001
  "Magnitude with which a boid can accelerate to separate from its neighbors.")

;;; UTILITY SECTION ------------------------------------------------------------

(defun degrees (radians)
  "Convert radians to degrees."
  (* (/ radians pi) 180.0))

(defun radians (degrees)
  "Convert degrees to radians."
  (* (/ degrees 180.0) pi))

(defun vec2-add (v0 v1)
  "Perform vector addition on two x-y vectors, creating a new vector."
  `(,(+ (car v0) (car v1)) ,(+ (cadr v0) (cadr v1))))

(defun vec2-sub (v0 v1)
  "Perform vector subtraction on two x-y vectors, creating a new vector."
  `(,(- (car v0) (car v1)) ,(- (cadr v0) (cadr v1))))

(defun vec2-mul (v c)
  "Perform scalar multiplication on an x-y vector, creating a new vector."
  `(,(* (car v) c) ,(* (cadr v) c)))

(defun vec2-div (v c)
  "Perform scalar division on an x-y vector, creating a new vector."
  `(,(/ (car v) c) ,(/ (cadr v) c)))

(defun vec2-eq (v0 v1)
  "Check if two x-y vectors are equal."
  (and (= (car v0)
          (car v1))
       (= (cadr v0)
          (cadr v1))))

(defun vec2-length (v)
  "Determine the length of an x-y vector."
  (sqrt (+ (expt (car v) 2) (expt (cadr v) 2))))

(defun vec2-to-angle (v)
  "Convert an x-y vector into an angle in degrees relative to the positive
x-axis with a positive rotation."
  (let ((x (car v))
        (y (cadr v))
        (angle 0))
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

(defclass boid ()
  ((x :accessor boid-x)
   (y :accessor boid-y)
   (dx :accessor boid-dx)
   (dy :accessor boid-dy))
  (:documentation
   "Boid class representing a boid in the world. Boid's have a position and
velocity, both with an x and y component."))

(defmethod boid-position ((object boid))
  "Get the position of a boid represented by a list containing the x-y
components."
  `(,(boid-x object) ,(boid-y object)))

(defmethod boid-velocity ((object boid))
  "Get the velocity of a boid represented by a list containing the x-y
components."
  `(,(boid-dx object) ,(boid-dy object)))

(defmethod boid-angle ((object boid))
  "Determine the angle of a boid based on its velocity."
  (vec2-to-angle (boid-velocity object)))

(defmethod boid-push ((object boid) dv)
  "Apply an acceleration to the velocity of a boid."
  (setf (boid-dx object) (+ (boid-dx object) (car dv)))
  (setf (boid-dy object) (+ (boid-dy object) (cadr dv))))

(defmethod boid-avoid-edges ((object boid))
  "Accelerate a boid to avoid the edges of the world."
  (let ((x (boid-x object))
        (y (boid-y object)))
    (if (<= x *boid-avoidance-distance*)
        (setf (boid-dx object) (+ (boid-dx object) *boid-avoidance-acceleration*)))
    (if (>= x (- *screen-width* *boid-avoidance-distance*))
        (setf (boid-dx object) (- (boid-dx object) *boid-avoidance-acceleration*)))
    (if (<= y *boid-avoidance-distance*)
        (setf (boid-dy object) (+ (boid-dy object) *boid-avoidance-acceleration*)))
    (if (>= y (- *screen-height* *boid-avoidance-distance*))
        (setf (boid-dy object) (- (boid-dy object) *boid-avoidance-acceleration*)))))

(defmethod boid-get-neighbors ((object boid) boid-list)
  "Get a list of the neighbors of a boid."
  (let ((neighbor-list nil))
    (dolist (target boid-list)
      (when (<= (vec2-length (vec2-sub (boid-position target)
                                       (boid-position object)))
                *boid-fov-radius*)
        (setq neighbor-list (cons target neighbor-list))))
    (remove-if (lambda (target) (and (vec2-eq (boid-position target)
                                              (boid-position object))
                                     (vec2-eq (boid-velocity target)
                                              (boid-velocity object))))
               neighbor-list
               :count 1)))

(defmethod boid-align-with-neighbors ((object boid) neighbor-list)
  "Gradually align a boid's velocity vector with the mean average velocity
vector of its neighbors."
  (let* ((mean-vel (vec2-div (reduce #'vec2-add (map 'list #'boid-velocity neighbor-list))
                             (length neighbor-list)))
         (delta-vel (vec2-sub mean-vel (boid-velocity object))))
    (boid-push object (vec2-mul delta-vel *boid-alignment-acceleration*))))

(defmethod boid-cohere-with-neighbors ((object boid) neighbor-list)
  "Gradually accelerate a boid's velocity vector towards the average position of
its neighbors."
  (let* ((mean-pos (vec2-div (reduce #'vec2-add (map 'list #'boid-position neighbor-list))
                             (length neighbor-list)))
         (delta-pos (vec2-sub mean-pos (boid-position object))))
    (boid-push object (vec2-mul delta-pos *boid-cohesion-acceleration*))))

(defmethod boid-separate-from-neighbors ((object boid) neighbor-list)
  "Maintain distance between a boid and its neighbors by applying accelerations
to its velocity vector pointing away from neighbors should they be too close."
  (let ((move-vector '(0 0)))
    (dolist (neighbor neighbor-list)
      (let ((delta-pos (vec2-sub (boid-position object) (boid-position neighbor))))
        (when (<= (vec2-length delta-pos) *boid-separation-distance*)
          (setq move-vector (vec2-add move-vector delta-pos)))))
    (boid-push object (vec2-mul move-vector *boid-separation-acceleration*))))

(defmethod boid-limit-speed ((object boid))
  "Limit the speed of a boid."
  (let ((speed (vec2-length (boid-velocity object))))
    (when (> speed *boid-speed-limit*)
      (let ((new-speed (vec2-mul (boid-velocity object)
                                 (/ *boid-speed-limit* speed))))
        (setf (boid-dx object) (car new-speed))
        (setf (boid-dy object) (cadr new-speed))))))

(defmethod boid-update-pos ((object boid))
  "Update the position of a boid based on its velocity."
  (let ((new-position (vec2-add (boid-position object)
                                (boid-velocity object))))
    (setf (boid-x object) (car new-position))
    (setf (boid-y object) (cadr new-position))))

(defun boid-init (boid-count)
  "Initialize a list containing a boid-count number of boids with random
parameters. Specifically, boid positions are within the screen dimensions and
boid velocities are within the boid speed limit in a random direction."
  (let ((boid nil)
        (boid-list nil))
    (dotimes (i boid-count)
      (setq boid (make-instance 'boid))
      (setf (boid-x boid) (random *screen-width*))
      (setf (boid-y boid) (random *screen-height*))
      (let ((random-cap (sqrt *boid-speed-limit*)))
        (setf (boid-dx boid) (* (random random-cap)
                                (- (* (random 2) 2) 1)))
        (setf (boid-dy boid) (* (random random-cap)
                                (- (* (random 2) 2) 1)))
        (setq boid-list (cons boid boid-list))))
    boid-list))

(defun boid-list-limit-speed (boid-list)
  "Limit the speeds of a list of boids."
  (unless (eq boid-list nil)
    (boid-limit-speed (car boid-list))
    (boid-list-limit-speed (cdr boid-list))))

(defun boid-list-update-pos (boid-list)
  "Update the positions of a list of boids."
  (unless (eq boid-list nil)
    (boid-update-pos (car boid-list))
    (boid-list-update-pos (cdr boid-list))))

(defun boid-list-update (boid-list)
  "Update the velocities of a list of boids based on the boid various behaviors,
and then update their positions."
  (dolist (boid boid-list)
    (boid-avoid-edges boid)
    (let ((neighbor-list (boid-get-neighbors boid boid-list)))
      (when neighbor-list
        (boid-align-with-neighbors boid neighbor-list)
        (boid-cohere-with-neighbors boid neighbor-list)
        (boid-separate-from-neighbors boid neighbor-list))))
  (boid-list-limit-speed boid-list)
  (boid-list-update-pos boid-list))

;;; SCREEN SECTION -------------------------------------------------------------

(defmacro screen-update-size (win)
  `(multiple-value-bind (screen-width screen-height)
       (sdl2:get-window-size ,win)
     (setq *screen-width* screen-width)
     (setq *screen-height* screen-height)))

(defun render-clear (ren)
  "Clear the screen."
  (sdl2:set-render-draw-color ren 0 0 0 255)
  (sdl2:render-clear ren))

(defun render-draw-boid (ren boid)
  "Draw a boid."
  (let* ((x (boid-x boid))
         (y (boid-y boid))
         (angle (vec2-to-angle (boid-velocity boid)))
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

(defun render-draw-boid-list (ren boid-list)
  "Draw a list of boids."
  (when boid-list
    (render-draw-boid ren (car boid-list))
    (render-draw-boid-list ren (cdr boid-list))))

;;; MAIN SECTION ---------------------------------------------------------------
;;
;; The boid list is not created until within the event loop because otherwise
;; the initial positions of the boids would be based on the original window
;; size, as opposed to the window size after the initial window events (such as
;; the initial resize in a window manager).

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "boids"
                           :w *screen-width*
                           :h *screen-height*
                           :flags '(:resizable
                                    :shown))
      (let ((boid-list nil))
        (sdl2:with-renderer (ren win :flags '(:accelerated))
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym key)
             (cond ((sdl2:scancode= (sdl2:scancode-value key) :scancode-r)
                    (setq boid-list (boid-init *boid-count*)))
                   ((sdl2:scancode= (sdl2:scancode-value key) :scancode-q)
                    (sdl2:push-event :quit))))
            (:windowevent
             ()
             (screen-update-size win))
            (:idle
             ()
             (unless boid-list
               (setq boid-list (boid-init *boid-count*)))
             (render-clear ren)
             (render-draw-boid-list ren boid-list)
             (sdl2:render-present ren)
             (boid-list-update boid-list)
             (sdl2:delay 7)) ; 7 ms ~= 1000 ms / 144 fps
            (:quit () t)))))))
