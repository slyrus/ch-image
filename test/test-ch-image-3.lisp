
(in-package :ch-image-test)


#+ch-image-has-freetype-ffi
(defun test-argb-image-text ()
  (destructuring-bind (height width) (list 300 600)
    (let ((img (make-instance 'argb-8888-image :width width :height height)))
      (let ((context (ch-image:make-text-context)))

        (ch-image:set-font context :times-new-roman 12)
        (ch-image:draw-string img context
                               "Times!"
                               24 24)
        
        (ch-image:set-font context :arial 36)
        (ch-image:draw-string img context
                               "Arial!"
                               64 24)
        
        (ch-image:set-font context :futura 72)
        (ch-image:draw-string img context
                               "Futura!"
                               140 24)
        
        (ch-image:set-font context :gill-sans 110)
        (ch-image:draw-string img context
                               "Gill Sans!"
                               270 24))
    
      img)))

#+ch-image-has-freetype-ffi
(defun test-gray-image-text ()
  (destructuring-bind (height width) (list 300 600)
    (let ((img (make-instance 'ub8-matrix-image :width width :height height)))
      (let ((context (ch-image:make-text-context)))

        (ch-image:set-font context :times-new-roman 12)
        (ch-image:draw-string img context
                               "Times!"
                               24 24)
        
        (ch-image:set-font context :arial 36)
        (ch-image:draw-string img context
                               "Arial!"
                               64 24)
        
        (ch-image:set-font context :futura 72)
        (ch-image:draw-string img context
                               "Futura!"
                               140 24)
        
        (ch-image:set-font context :gill-sans 110)
        (ch-image:draw-string img context
                               "Gill Sans!"
                               270 24))
    
      img)))
