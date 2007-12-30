
(in-package :ch-image-test)

(defun test/copy-pixels ()
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

    (let ((dest (make-instance 'argb-8888-image :width 800 :height 600)))
      (ch-image::copy-pixels img dest 0 399 0 599 100 499 100 699)
      (ch-image:write-png-file "copied-argb-image.png" dest))))
