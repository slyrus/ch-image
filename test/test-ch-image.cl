
(in-package :image-test)

(defun imgtest1 ()
  (let*
      ((width 600)
       (height 400)
       (img (make-instance 'argb-8888-image :width width :height height)))
    (dotimes (i height)
      (dotimes (j width)
	(set-argb-values img j i 255 127 63 63)
	)))
  t)

(defun imgtest2 ()
  (let*
      ((width 600)
       (height 400)
       (img (make-instance 'gray-image :width width :height height)))
    (describe img)
    (dotimes (i height)
      (dotimes (j width)
	(set-gray-value img j i 63)
	)))
  t)

(defun imgtest3 ()
  (let*
      ((width 600)
       (height 400)
       (img (make-instance 'matrix-gray-image :width width :height height)))
    (describe img)
    (dotimes (i height)
      (dotimes (j width)
	(set-gray-value img j i 63)
	)))
  t)

(defun run-tests ()
  (let ((run (util:make-test-run)))
    (time (util:run-test #'imgtest1 "imgtest1" run))
    (time (util:run-test #'imgtest2 "imgtest2" run))
    (time (util:run-test #'imgtest3 "imgtest3" run)))
  (print 'done))
