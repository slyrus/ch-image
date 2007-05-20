
(in-package :ch-image-test)

(defun image-test-1 ()
  (let* ((width 600)
         (height 400)
         (img (make-instance 'argb-8888-image :width width :height height)))
    (dotimes (i height)
      (dotimes (j width)
	(set-argb-values img i j 255 127 63 63)
	)))
  t)

(defun image-test-2 ()
  (let* ((width 600)
         (height 400)
         (img (make-instance 'gray-image :width width :height height)))
    (dotimes (i height)
      (dotimes (j width)
	(set-gray-value img i j 63)
	)))
  t)

(defun image-test-3 ()
  (let* ((width 600)
         (height 400)
         (img (make-instance 'ub8-matrix-image :width width :height height)))
    (dotimes (i height)
      (dotimes (j width)
	(set-gray-value img i j 63))))
  t)

(defun image-test-4 ()
  (let* ((width 600)
         (height 400)
         (img (make-instance 'argb-8888-image :width width :height height)))

    (ch-image::fill-rectangle img 0 0 399 599 (list 255 5 50 15))
    (ch-image::draw-circle img 200 200 100 (list 255 0 0 255))
    (ch-image::fill-circle img 300 300 75 (list 255 255 0 50))
    (ch-image::horiz-line img 75 100 300 (list 255 0 255 0))
    (ch-image::vert-line img 100 300 400 (list 255 255 255 255))
    (ch-image::vert-line img 100 300 370 (list 255 255 255 255))
    (ch-image::draw-rectangle img 120 140 220 180 (list 255 127 127 127))
    (ch-image::fill-rectangle img 320 70 340 130 (list 255 0 100 100))

    (ch-image::draw-line img 10 10 50 200 (list 255 128 128 255))
    (ch-image::draw-line img 50 10 10 200 (list 255 255 128 255))
    (ch-image::draw-line img 10 200 50 10 (list 255 128 128 255))

    (ch-image::draw-triangle img 10 300 50 280 60 330 (list 255 255 0 0))

    (ch-image::draw-polygon img '((200 . 100) (190 . 110) (200 . 120) (210 . 110) (200 . 100))
                            (list 255 128 255 128))

    (ch-image::fill-rectangle img 200 300 210 310 (list 255 255 255 25))
    (ch-image::fill-rectangle img 220 300 230 310 (list 255 255 255 25))
    (ch-image::fill-rectangle img 240 300 250 310 (list 255 255 255 25))

    (ch-image:write-jpeg-file "argb-image.jpeg" img)))

(defun image-test-5 ()
  (let* ((width 600)
         (height 500)
         (img (make-instance 'argb-8888-image :width width :height height)))

    (ch-image::draw-line img 10 10 50 200 (list 255 255 128 128))
    (ch-image::draw-line img 50 10 10 200 (list 255 128 255 128))
    (ch-image::draw-line img 20 200 60 10 (list 255 128 128 255))
    (ch-image::draw-line img 420 200 60 10 (list 255 128 255 255))
    (ch-image::draw-line img 60 20 420 210 (list 255 255 255 128))
    
    (ch-image:write-tiff-file "argb-image.tiff" img)))

(defun image-test-6 ()
  (let* ((width 600)
         (height 500)
         (img (make-instance 'argb-8888-image :width width :height height)))
    (dotimes (i 150)
      (let ((radius (random 50))
            (y (+ 50 (random 400)))
            (x (+ 50 (random 500)))
            (color (list 255 (random 255) (random 255) (random 255))))
        (if (> (random 2) 0)
            (ch-image::draw-circle img y x radius color)
            (ch-image::fill-circle img y x radius color))))
    (ch-image:write-jpeg-file "circles.jpg" img)
    (ch-image:write-png-file "circles.png" img)))

(defun run-tests ()
  (let ((run (ch-util:make-test-run)))
    (time (ch-util:run-test #'image-test-1 "image-test-1" run))
    (time (ch-util:run-test #'image-test-2 "image-test-2" run))
    (time (ch-util:run-test #'image-test-3 "image-test-3" run)))
  (print 'done))


(defun make-shapes-test-image ()
  (let ((path
         (merge-pathnames #p"shapes.png"
                          (ch-asdf::asdf-lookup-path "asdf:/ch-image-test/test/images"))))
    (let ((img (make-instance 'ch-image::bit-matrix-image :rows 128 :cols 128 :initial-element 0)))
      (ch-image::fill-rectangle img 4 4 10 10 1)
      (ch-image::fill-rectangle img 20 20 40 40 1)
      (ch-image::draw-circle img 75 20 10 1)
      (ch-image::write-image-file path (ch-image::make-norm-ub8-image img))
      (values path img))))

