
(in-package :ch-image-test)


(defparameter *dark-blue* '(255 5 5 80))

(defun ch-image-example-1 ()
  (let* ((width 600)
         (height 400)
         (img (make-instance 'argb-8888-image :width width :height height)))
    (ch-image:fill-rectangle img 0 0 (1- height) (1- width) *dark-blue*)
    (ch-image:draw-triangle img 10 300 50 280 60 330 (list 255 255 255 20))
    (ch-image:write-jpeg-file "example1.jpeg" img)))

(ch-image-example-1)

