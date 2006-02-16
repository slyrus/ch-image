
(in-package :ch-image-test)

(defun test-argb-image-text ()
  (destructuring-bind (height width) (list 400 300)
    (let ((img (make-instance 'argb-8888-image :width width :height height)))
      (let ((context (ch-image::make-text-context)))
        (ch-image::set-font context :arial)
        (ch-image::draw-char img context #\1 20 20)
        (ch-image::draw-string img context "Cyrus" 60 20))
      img)))
