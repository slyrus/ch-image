
(in-package :ch-image-test)

(defun test-argb-image-text ()
  (destructuring-bind (height width) (list 400 300)
    (let ((img (make-instance 'argb-8888-image :width width :height height)))
      (let ((context (ch-image::make-text-context)))
        (ch-image::set-font context :times 24)
        (ch-image::draw-string img context "This is some text!" 72 24)

        (ch-image::set-font context :futura 48)
        (ch-image::draw-string img context "Futura!" 150 24)
        
        (ch-image::set-font context :gill-sans 48)
        (ch-image::draw-string img context "Gill!" 200 24)
        
        (ch-image::set-font context :monaco 48)
        (ch-image::draw-string img context "Monaco!" 250 24))
    
      img))))

