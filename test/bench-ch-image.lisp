
(in-package :ch-image-test)

(defun image-bench-1 ()
  (let* ((width 1024)
         (height 1024)
         (img (make-instance 'argb-8888-image :width width :height height)))
    (dotimes (i height)
      (dotimes (j width)
	(set-argb-values img i j 255 127 63 63)))))

(defparameter *test-image-1* (make-instance 'argb-8888-image :height 1024 :width 1024))

(defun image-bench-2 (img)
  (multiple-value-bind (height width)
      (ch-image::image-dim img)
    (dotimes (i height)
      (dotimes (j width)
        (set-argb-values img i j 255 127 63 63)))))

