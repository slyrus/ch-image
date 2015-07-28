
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
         (img (make-instance 'matrix-gray-image :width width :height height)))
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

(defun make-test-argb-8888-image ()
  (let* ((width 600)
         (height 400)
         (img (make-instance 'argb-8888-image :width width :height height)))
    (fill-rectangle img 0 0 399 599 (list 255 5 50 15))
    (draw-circle img 200 200 100 (list 255 0 0 255))
    (fill-circle img 300 300 75 (list 255 255 0 50))
    (horiz-line img 75 100 300 (list 255 0 255 0))
    (vert-line img 100 300 400 (list 255 255 255 255))
    (vert-line img 100 300 370 (list 255 255 255 255))
    (draw-rectangle img 120 140 220 180 (list 255 127 127 127))
    (fill-rectangle img 320 70 340 130 (list 255 0 100 100))
    (draw-line img 10 10 50 200 (list 255 128 128 255))
    (draw-line img 50 10 10 200 (list 255 255 128 255))
    (draw-line img 10 200 50 10 (list 255 128 128 255))
    (draw-triangle img 10 300 50 280 60 330 (list 255 255 0 0))
    (draw-polygon img '((200 . 100) (190 . 110) (200 . 120) (210 . 110) (200 . 100))
                            (list 255 128 255 128))
    (fill-rectangle img 200 300 210 310 (list 255 255 255 25))
    (fill-rectangle img 220 300 230 310 (list 255 255 255 25))
    (fill-rectangle img 240 300 250 310 (list 255 255 255 25))
    img))

(defun image-test-4 ()
  (let ((img (make-test-argb-8888-image)))
    (ch-image:write-jpeg-file "argb-image.jpeg" img)))

(defun image-test-4-stream ()
  (let ((img (make-test-argb-8888-image)))
    (with-open-file (stream "argb-image-from-stream.jpeg"
                            :direction :output :element-type 'unsigned-byte :if-exists :supersede)
      (ch-image:write-jpeg-stream stream  img))))

(defun image-test-5 ()
  (let* ((width 600)
         (height 500)
         (img (make-instance 'argb-8888-image :width width :height height)))

    (draw-line img 10 10 50 200 (list 255 255 128 128))
    (draw-line img 50 10 10 200 (list 255 128 255 128))
    (draw-line img 20 200 60 10 (list 255 128 128 255))
    (draw-line img 420 200 60 10 (list 255 128 255 255))
    (draw-line img 60 20 420 210 (list 255 255 255 128))
    
    #+ch-image-has-retrospectiff
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
            (draw-circle img y x radius color)
            (fill-circle img y x radius color))))
    (ch-image:write-jpeg-file "circles.jpg" img)
    (ch-image:write-png-file "circles.png" img)))

(defun image-test-7 ()
  (let* ((width 600)
         (height 500)
         (img (make-instance 'ub8-matrix-image :width width :height height)))
    (dotimes (i 150)
      (let ((radius (random 50))
            (y (+ 50 (random 400)))
            (x (+ 50 (random 500)))
            (color (random 255)))
        (if (> (random 2) 0)
            (draw-circle img y x radius color)
            (fill-circle img y x radius color))))
    (ch-image:write-jpeg-file "circles-gray.jpg" img)
    (ch-image:write-png-file "circles-gray.png" img)))

(defun run-tests ()
  (let ((run (make-test-run)))
    (time (run-test #'image-test-1 "image-test-1" run))
    (time (run-test #'image-test-2 "image-test-2" run))
    (time (run-test #'image-test-3 "image-test-3" run)))
  (print 'done))


(defun make-shapes-test-image ()
  (let ((path
         (asdf:component-pathname
          (reduce #'asdf:find-component
                  (list nil "ch-image-test" "test" "images" "shapes.png")))))
    (let ((img (make-instance 'bit-matrix-image :rows 128 :cols 128 :initial-element 0)))
      (fill-rectangle img 4 4 10 10 1)
      (fill-rectangle img 20 20 40 40 1)
      (draw-circle img 75 20 10 1)
      (write-image-file path (ch-image::make-norm-ub8-image img))
      (values path img))))


(defun make-argb-shapes-test-image ()
  (let ((path
         (asdf:component-pathname
          (reduce #'asdf:find-component
                  (list nil "ch-image-test" "test" "images" "argb-shapes.png")))))
    (let ((img (make-instance 'argb-8888-image :height 128 :width 128)))
      (fill-rectangle img 4 4 10 10 '(255 255 0 0))
      (fill-rectangle img 20 20 40 40 '(255 0 255 255))
      (draw-circle img 75 20 10 '(255 0 0 255))
      (write-image-file path img)
      (values path img))))

(defun make-rgb-shapes-test-image ()
  (let ((path
         (asdf:component-pathname
          (reduce #'asdf:find-component
                  (list nil "ch-image-test" "test" "images" "rgb-shapes.png")))))
    (let ((img (make-instance 'rgb-888-image :height 128 :width 128)))
      (fill-rectangle img 4 4 10 10 '(255 0 0))
      (fill-rectangle img 20 20 40 40 '(0 255 255))
      (draw-circle img 75 20 10 '(0 0 255))
      (write-image-file path img)
      (values path img))))


(defun test/read-jpeg-image-from-stream ()
  (let ((srcfile (test-img "euc-jpeg"))
        (destfile (merge-pathnames *output-image-path* "output-read-jpeg-image-from-stream.jpeg")))
    (with-open-file (stream srcfile :direction :input :element-type 'unsigned-byte)
      (let ((img (read-jpeg-stream stream)))
        (when img
          (write-jpeg-file destfile img))))))

(defun test/write-jpeg-image-to-stream ()
  (let ((srcfile (test-img "euc-jpeg"))
        (destfile (merge-pathnames *output-image-path* "output-write-jpeg-image-to-stream.jpeg")))
    (let ((img (read-jpeg-file srcfile)))
      (when img
        (with-open-file (stream destfile
                                :direction :output 
                                :element-type 'unsigned-byte
                                :if-exists :supersede)
          (write-jpeg-stream stream img))))))

(defun test/read-tiff-image-from-stream ()
  (let ((srcfile (test-img "euc-tiff"))
        (destfile (merge-pathnames *output-image-path* "output-read-tiff-image-from-stream.jpeg")))
    (with-open-file (stream srcfile :direction :input :element-type 'unsigned-byte)
      (let ((img (read-tiff-stream stream)))
        (when img
          (write-jpeg-file destfile img))))))

(defun test/write-tiff-image-to-stream ()
  (let ((srcfile (test-img "euc-jpeg"))
        (destfile (merge-pathnames *output-image-path* "output-write-tiff-image-to-stream.tiff")))
    (let ((img (read-jpeg-file srcfile)))
      (when img
        (with-open-file (stream destfile
                                :direction :output 
                                :element-type 'unsigned-byte
                                :if-exists :supersede)
          (write-tiff-stream stream img))))))

(defun test/read-png-image-from-stream ()
  (let ((srcfile (test-img "euc-png"))
        (destfile (merge-pathnames *output-image-path* "output-read-png-image-from-stream.jpeg")))
    (with-open-file (stream srcfile :direction :input :element-type 'unsigned-byte)
      (let ((img (read-png-stream stream)))
        (when img
          (write-jpeg-file destfile img))))))

(defun test/read-gray-png-image-from-stream ()
  (let ((srcfile "test/images/bluesky-gray.png")
        (destfile (merge-pathnames *output-image-path* "output-read-gray-png-image-from-stream.jpeg")))
    (with-open-file (stream srcfile :direction :input :element-type 'unsigned-byte)
      (let ((img (read-png-stream stream)))
        (when img
          (write-jpeg-file destfile img))))))

(defun test/read-bw-png-image-from-stream ()
  (let ((srcfile "test/images/bluesky-gray.png")
        (destfile (merge-pathnames *output-image-path* "output-read-bw-png-image-from-stream.jpeg")))
    (with-open-file (stream srcfile :direction :input :element-type 'unsigned-byte)
      (let ((img (read-png-stream stream)))
        (when img
          (write-jpeg-file destfile img))))))

(defun test/read-indexed-png-image-from-stream ()
  (let ((srcfile "test/images/bluesky-indexed.png")
        (destfile (merge-pathnames *output-image-path* "output-read-indexed-png-image-from-stream.jpeg")))
    (with-open-file (stream srcfile :direction :input :element-type 'unsigned-byte)
      (let ((img (read-png-stream stream)))
        (when img
          (write-jpeg-file destfile img))))))

(defun test/write-gray-tiff-image ()
  (let ((srcfile "test/images/eucgray.tiff")
        (destfile (merge-pathnames *output-image-path* "output-write-gray-tiff-image.tiff")))
    (with-open-file (stream srcfile :direction :input :element-type 'unsigned-byte)
      (let ((img (read-tiff-stream stream)))
        (when img
          (write-tiff-file destfile img))))))

