;;
;; file: shapes.cl
;; author: cyrus harmon
;;

(in-package :ch-image-drawing)

(defgeneric in-clip-region (clip-region y x))

(defmethod in-clip-region ((clip-rect clip-rect) y x)
  (and (and (>= y (y1 clip-rect))
            (< y (y2 clip-rect)))
       (and (>= x (x1 clip-rect))
            (< x (x2 clip-rect)))))

(defmethod clip-set-pixel ((img image) y x value)
  (when (in-clip-region (clip-region img) y x)
    (set-pixel img y x value)))

(defgeneric horiz-line (img y x0 x1 value))

(defmethod horiz-line ((img image) y x0 x1 value)
  (declare (type fixnum y x0 x1))
  (loop for x fixnum from x0 to x1
     do (clip-set-pixel img y x value)))

(defgeneric vert-line (img y0 y1 x value))

(defmethod vert-line ((img image) y0 y1 x value)
  (declare (type fixnum y0 y1 x))
  (loop for y fixnum from y0 to y1
     do (clip-set-pixel img y x value)))

(defgeneric draw-line (img y0 x0 y1 x1 value))

(defmethod draw-line ((img image) y0 x0 y1 x1 value)
  (declare (type fixnum y0 x0 y1 x1))
  (let ((dx (- x1 x0))
        (dy (- y1 y0)))
    (declare (type fixnum dx dy))
    (let ((absdx (abs dx))
          (absdy (abs dy)))
      (declare (type fixnum absdx absdy))
      (let ((xstep (if (minusp dx) -1 1))
            (ystep (if (minusp dy) -1 1)))
        (if (>= absdx absdy)
            (let ((d (- (* 2 absdy) absdx))
                  (incr-e (* 2 absdy))
                  (incr-ne (* 2 (- absdy absdx)))
                  (x x0)
                  (y y0))
              (declare (type fixnum d incr-e incr-ne x y))
              (clip-set-pixel img y x value)
              (dotimes (i absdx)
                (cond
                  ((<= d 0)
                   (incf d incr-e)
                   (incf x xstep))
                  (t
                   (incf d incr-ne)
                   (incf x xstep)
                   (incf y ystep)))
                (clip-set-pixel img y x value)))
            (let ((d (- (* 2 absdy) absdx))
                  (incr-n (* 2 absdx))
                  (incr-ne (* 2 (- absdx absdy)))
                  (x x0)
                  (y y0))
              (declare (type fixnum d incr-n incr-ne x y))
              (clip-set-pixel img y x value)
              (dotimes (i absdy)
                (cond
                  ((<= d 0)
                   (incf d incr-n)
                   (incf y ystep))
                  (t
                   (incf d incr-ne)
                   (incf y ystep)
                   (incf x xstep)))
                (clip-set-pixel img y x value))))))))
              
(defgeneric draw-circle (img center-y center-x radius value))

(defmethod draw-circle ((img image) center-y center-x radius value)
  "draws a circle centered at (x, y) with radius r on a image."
  (declare (type fixnum center-y center-x radius))
  (flet ((circle-points (y x value)
           (clip-set-pixel img (+ center-y y) (+ center-x x) value) 
           (clip-set-pixel img (+ center-y x) (+ center-x y) value) 
           (clip-set-pixel img (- center-y x) (+ center-x y) value) 
           (clip-set-pixel img (- center-y y) (+ center-x x) value) 
           (clip-set-pixel img (- center-y y) (- center-x x) value) 
           (clip-set-pixel img (- center-y x) (- center-x y) value) 
           (clip-set-pixel img (+ center-y x) (- center-x y) value) 
           (clip-set-pixel img (+ center-y y) (- center-x x) value)))
    (let ((x 0)
          (y radius)
          (d (- 1 radius))
          (delta-e 3)
          (delta-se (+ (* -2 radius) 5)))
      (declare (type fixnum x y d delta-e delta-se))
      (circle-points y x value)
      (do () ((>= x y))
        (if (< d 0)
            (progn
              (incf d delta-e)
              (incf delta-e 2)
              (incf delta-se 2))
            (progn
              (incf d delta-se)
              (incf delta-e 2)
              (incf delta-se 4)
              (decf y)))
        (incf x)
        (circle-points y x value)))))
        
(defgeneric fill-circle (img center-y center-x radius value))

(defmethod fill-circle ((img image) center-y center-x radius value)
  "draws a filled circle centered at (x, y) with radius r on a image."
  (declare (type fixnum center-y center-x radius))
  (flet ((circle-lines (y x value)
           (horiz-line img (- center-y y) (- center-x x) (+ center-x x) value)
           (horiz-line img (- center-y x) (- center-x y) (+ center-x y) value)
           (horiz-line img (+ center-y y) (- center-x x) (+ center-x x) value)
           (horiz-line img (+ center-y x) (- center-x y) (+ center-x y) value)))
    (let ((x 0)
          (y radius)
          (d (- 1 radius))
          (delta-e 3)
          (delta-se (+ (* -2 radius) 5)))
      (declare (type fixnum x y d delta-e delta-se))
      (circle-lines y x value)
      (do () ((>= x y))
        (if (< d 0)
            (progn
              (incf d delta-e)
              (incf delta-e 2)
              (incf delta-se 2))
            (progn
              (incf d delta-se)
              (incf delta-e 2)
              (incf delta-se 4)
              (decf y)))
        (incf x)
        (circle-lines y x value)))))
                
(defgeneric draw-rectangle (img y0 x0 y1 x1 value))

(defmethod draw-rectangle ((img image) y0 x0 y1 x1 value)
  (horiz-line img y0 x0 x1 value)
  (vert-line img y0 y1 x0 value)
  (vert-line img y0 y1 x1 value)
  (horiz-line img y1 x0 x1 value))

(defgeneric fill-rectangle (img y0 x0 y1 x1 value))

(defmethod fill-rectangle ((img image) y0 x0 y1 x1 value)
  (loop for y from y0 to y1
     do (horiz-line img y x0 x1 value)))

(defgeneric fill-image (img value))
(defmethod fill-image (img value)
  (fill-rectangle img
                  0 0 
                  (1- (image-height img))
                  (1- (image-width img))
                  value))

(defgeneric draw-triangle (img y0 x0 y1 x1 y2 x2 value))

(defmethod draw-triangle ((img image) y0 x0 y1 x1 y2 x2 value)
  (draw-line img y0 x0 y1 x1 value)
  (draw-line img y1 x1 y2 x2 value)
  (draw-line img y2 x2 y0 x0 value))

(defgeneric draw-polygon (img points value))

(defmethod draw-polygon ((img image) points value)
  (loop for p on points
     do (let ((p1 (car p))
              (p2 (cadr p)))
          (when (and (consp p1) (consp p2))
            (draw-line img (car p1) (cdr p1) (car p2) (cdr p2) value)))))