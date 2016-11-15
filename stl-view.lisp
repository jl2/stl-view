;;;; stl-view.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:stl-view)

(declaim (optimize (speed 3) (safety 3) (size 0) (debug 3)))
(defstruct rvector
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(declaim (optimize (speed 3) (safety 1) (debug 3)))

(defmacro each-point-to-vector (op p1 p2)
  `(make-rvector :x (funcall ,op (point-x ,p1) (point-x ,p2))
                 :y (funcall ,op (point-y ,p1) (point-y ,p2))
                 :z (funcall ,op (point-z ,p1) (point-z ,p2))))

(defmacro each-vector-to-vector (op p1 p2)
  `(make-rvector :x (funcall ,op (rvector-x ,p1) (rvector-x ,p2))
                 :y (funcall ,op (rvector-y ,p1) (rvector-y ,p2))
                 :z (funcall ,op (rvector-z ,p1) (rvector-z ,p2))))

(defun vscale (k v)
  (declare (type single-float k)
           (type rvector v))
  (make-rvector :x (* k (rvector-x v))
                :y (* k (rvector-y v))
                :z (* k (rvector-z v))))

(defun psub (p1 p2)
  (declare (type point p1 p2))
  (each-point-to-vector #'- p1 p2))

(defun vsub (v1 v2)
  (declare (type rvector v1 v2))
  (each-vector-to-vector #'- v1 v2))

(defun vadd (v1 v2)
  (declare (type rvector v1 v2))
  (each-vector-to-vector #'+ v1 v2))

(defun pvadd (p v)
  (declare (type stl:point p)
           (type rvector v))
  (make-point :x (+ (point-x p) (rvector-x v))
              :y (+ (point-y p) (rvector-y v))
              :z (+ (point-z p) (rvector-z v))))

(defun dot (v1 v2)
  (declare (type rvector v1 v2))
  (+ (* (rvector-x v1) (rvector-x v2))
     (* (rvector-y v1) (rvector-y v2))
     (* (rvector-z v1) (rvector-z v2))))

(defun cross (v1 v2)
  (declare (type rvector v1 v2))
  (let ((v1x (rvector-x v1))
        (v1y (rvector-y v1))
        (v1z (rvector-z v1))
        (v2x (rvector-x v2))
        (v2y (rvector-y v2))
        (v2z (rvector-z v2)))
    (make-rvector :x (- (* v1y v2z) (* v1z v2y))
                 :y (- (* v1z v2x) (* v1x v2z))
                 :z (- (* v1x v2y) (* v1y v2x)))))

(defun vlength (vect)
  (declare (type rvector vect))
  (let ((xl (rvector-x vect))
        (yl (rvector-y vect))
        (zl (rvector-z vect)))
    (sqrt (+ (* xl xl) (* yl yl) (* zl zl)))))

(defun normalize (vect)
  (declare (type rvector vect))
  (let ((ilen (/ 1.0 (vlength vect))))
    (vscale ilen vect)))

(defparameter *fps* 60)

(defun render-stl (triangles scale-factor x-rotation y-rotation z-rotation)
  "Used OpenGL to display the grid."
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:clear :color-buffer :depth-buffer)
  ;; Clear the background
  ;; Actual drawing goes here.  In this case, just a line.
  (gl:push-matrix)

  (gl:scale scale-factor scale-factor scale-factor)
  (gl:rotate x-rotation 1 0 0)
  (gl:rotate y-rotation 0 1 0)
  (gl:rotate z-rotation 0 0 1)
  (gl:material :front :diffuse #(0.0 0.5 0.0 1.0))
  
  ;; TODO: Use "modern" OpenGL
  (gl:with-primitives :triangles
;;    (gl:color 0 1 0)
    (loop
       for tri across triangles
       do
         (let* ((pt1 (stl:triangle-pt1 tri))
                (pt2 (stl:triangle-pt2 tri))
                (pt3 (stl:triangle-pt3 tri))
                (normal (normalize (cross (psub pt1 pt2) (psub pt1 pt3) ))))
           (gl:normal (rvector-x normal) (sin (* pi (rvector-y normal))) (rvector-z normal))
           (gl:vertex (stl:point-x pt1)
                      (stl:point-y pt1)
                      (stl:point-z pt1))
           (gl:vertex (stl:point-x pt2)
                      (stl:point-y pt2)
                      (stl:point-z pt2))
           (gl:vertex (stl:point-x pt3)
                      (stl:point-y pt3)
                      (stl:point-z pt3)))))
  
  (gl:pop-matrix))

(defun show-stl (triangles
                 &key
                   (width 800)
                   (height 800)
                   (fps 30)
                   (delay 20))
  (let ((scale-factor 1.0f0)
        (x-rotation 0.0f0)
        (y-rotation 0.0f0)
        (z-rotation 0.0f0)
        (prev-tick 0) ;; prev-tick is the previous value of sdl-system-ticks when the board was updated
        (paused nil)) ;; paused is t when paused, nil when not
    (sdl:with-init
        ()
      ;; Setup the window and view
      (sdl:window width height
                  :opengl t
                  :opengl-attributes '((:sdl-gl-depth-size   24)
                                       (:sdl-gl-doublebuffer 1)))
      (setf (sdl:frame-rate) fps)
      
      (gl:viewport 0 0 width height)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 50 (/ height width) 1.0 5000.0)
      (glu:look-at 12 12 12
                   0 0 0
                   0 1 0)

      (gl:clear-color 0 0 0 0)
      (gl:shade-model :smooth)
      (gl:cull-face :back)
      (gl:polygon-mode :front :fill)
      (gl:draw-buffer :back)

;;       (gl:material :front :ambient-and-diffuse #(0.2 1.0 0.2 0.2))
      (gl:light :light0 :position #(0.0f0 8.0f0 0.0f0 1.0f0))
      (gl:light :light0 :diffuse #(1.0f0 1.0f0 1.0f0 1.0f0))
      (gl:light :light1 :position #(8.0f0 8.0f0 8.0f0 1.0f0))
      (gl:light :light1 :diffuse #(1.0f0 1.0f0 1.0f0 1.0f0))
      (gl:light :light2 :position #(0.0f0 0.0f0 0.0f0 1.0f0))
      (gl:light :light2 :diffuse #(1.0f0 1.0f0 1.0f0 1.0f0))
      (gl:enable :cull-face :depth-test
                 :lighting :light0 :light1 :light2
                 )

      (gl:clear :color-buffer :depth-buffer)
      
      ;; Draw the initial board
      (render-stl triangles scale-factor x-rotation y-rotation z-rotation)
      (sdl:update-display)

      ;; Handle events
      (sdl:with-events ()
        (:quit-event () t)
        (:mouse-button-up-event
         (:button button :state state :x x :y y)
         (when (= button 4)
           (incf scale-factor 0.5))
         (when (= button 5)
           (setf scale-factor (max 1.0 (- scale-factor 0.5)))))
        
        (:key-down-event
         ()
         ;; quit
         (when (or (sdl:key-down-p :sdl-key-q) (sdl:key-down-p :sdl-key-escape))
           (sdl:push-quit-event))

         ;; Pause/unpause
         (when (sdl:key-down-p :sdl-key-p)
           (if paused
               (setf paused nil)
               (setf paused t))))

        (:video-expose-event () (sdl:update-display))

        (:idle
         ;; Check if it's time to update
         (when (> (- (sdl:system-ticks) prev-tick) delay)
           (setf prev-tick (sdl:system-ticks))

           ;; Only update the board while not paused
           (when (not paused)
             (incf y-rotation (/ (* 2 pi) 2)))

             ;; Clear the screen and redraw
           (gl:clear :color-buffer :depth-buffer)
           (render-stl triangles scale-factor x-rotation y-rotation z-rotation)
           (sdl:update-display)))))))
