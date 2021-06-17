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

;; Screen width and height
(defparameter *sw* 100)
(defparameter *sh* 100)

;; Clear the screen
(defun render-clear (ren)
  (sdl2:set-render-draw-color ren 0 0 0 255)
  (sdl2:render-clear ren))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "boids" :w *sw* :h *sh* :flags '(:shown))
      (sdl2:with-renderer (ren win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keydown
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
           ()
           (render-clear ren)
           (sdl2:render-present ren)
           (sdl2:delay 5))
          (:quit () t))))))
