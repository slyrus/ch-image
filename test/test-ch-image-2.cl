
(in-package :ch-image-test)

(defun test-gray-image ()
  (destructuring-bind (height width) (list 400 300)
    (let ((img (make-instance 'gray-image :width width :height height)))
      (dotimes (h height)
	(dotimes (w width)
	  (set-gray-value img h w (mod (* w 3) 255))))
      img)))

(defun test-argb-image ()
  (destructuring-bind (height width) (list 400 300)
    (let ((img (make-instance 'argb-8888-image :width width :height height))
	  (radfudge (* 6 pi .5)))
      (declare (ignorable radfudge))
      (dotimes (h height)
	(dotimes (w width)
	  (set-argb-values img h w
			   (floor (* 255 (/ w width)))
			   25 150
;			   (abs (floor (* 255 (sin (* radfudge (/ h height))))))
;			   (abs (floor (* 200 (sin (* .2 radfudge (/ w width))))))
			   (mod (* w h) 255))))
      img)))

(defparameter *test-argb-image* (test-argb-image))

(defun test-gray-image-write-tiff (destfile)
  (write-tiff-file destfile (test-gray-image)))

(defun test-argb-image-write-tiff (destfile)
  (write-tiff-file destfile *test-argb-image*))

(defun test-gray-image-write-jpeg (destfile)
  (write-jpeg-file destfile (test-gray-image)))

(defun test-argb-image-write-jpeg (destfile)
  (write-jpeg-file destfile *test-argb-image*))

(defun test-img (file)
  (asdf:component-pathname
   (asdf:find-component
    (asdf:find-component
     (asdf:find-component
      (asdf:find-system "ch-image-test")
      "test")
     "images")
    file)))

(defparameter *output-image-path*
  (let ((eucpath (test-img "euc-tiff")))
    (merge-pathnames
     (make-pathname :directory (list :relative :up "output-images"))
     (make-pathname :directory (pathname-directory eucpath)))))

(ensure-directories-exist *output-image-path*)

(defun test-output-img (file) (merge-pathnames *output-image-path* file))

(defun imageio-test1 ()
  (test-gray-image-write-tiff (test-output-img "grayimgtest.tiff"))
  t)

(defun imageio-test2 ()
  (test-argb-image-write-tiff (test-output-img "argbimgtest.tiff"))
  t)

(defun imageio-test3 ()
  (test-gray-image-write-jpeg (test-output-img "grayimgtest.jpeg"))
  t)

(defun imageio-test4 ()
  (test-argb-image-write-jpeg (test-output-img "argbimgtest.jpeg"))
  t)

;;; RGB TIFF -> RGB TIFF
(defun imageio-test5 ()
  (let ((srcfile (test-img "euc-tiff"))
	(destfile (test-output-img "argbreadtest.tiff")))
    (let ((img (read-tiff-file srcfile)))
      (when img
	(write-tiff-file destfile img)
	t))))

;;; RGB TIFF -> GRAY TIFF
(defun imageio-test6 ()
  (let ((srcfile (test-img "eucgray-tiff"))
	(destfile (test-output-img "grayreadtest.tiff")))
    (let ((img (read-tiff-file srcfile)))
      (when img
	(write-tiff-file destfile img)
	t))))

;;; RGB JPEG -> RGB JPEG
(defun imageio-test7 ()
  (let ((srcfile (test-img "euc-jpeg"))
	(destfile (test-output-img "argbreadtest.jpeg")))
    (let ((img (read-jpeg-file srcfile)))
      (when img
	(write-jpeg-file destfile img)
	t))))

;;; GRAY JPEG -> GRAY JPEG
(defun imageio-test8 ()
  (let ((srcfile (test-img "eucgray-jpeg"))
	(destfile (test-output-img "grayreadtest.jpeg")))
    (let ((img (read-jpeg-file srcfile)))
      (when img
	(write-jpeg-file destfile img)
	t))))

;;; RGB TIFF -> RGB JPEG
(defun imageio-test9 ()
  (let ((srcfile (test-img "euc-tiff"))
	(destfile (test-output-img "tiffreadtest.jpeg")))
    (let ((img (read-tiff-file srcfile)))
      (when img
	(write-jpeg-file destfile img)
	t))))

;;; RGB JPEG -> RGB TIFF
(defun imageio-test10 ()
  (let ((srcfile (test-img "euc-jpeg"))
	(destfile (test-output-img "jpegreadtest.tiff")))
    (let ((img (read-jpeg-file srcfile)))
      (when img
	(write-tiff-file destfile img)
	t))))


(defun run-tests-2 ()
  (let ((run (ch-util:make-test-run)))

    (ch-util:run-test #'imageio-test1 "imageio-test1" run)
    (ch-util:run-test #'imageio-test2 "imageio-test2" run)
    (ch-util:run-test #'imageio-test3 "imageio-test3" run)
    (ch-util:run-test #'imageio-test4 "imageio-test4" run)
    (ch-util:run-test #'imageio-test5 "imageio-test5" run)
    (ch-util:run-test #'imageio-test6 "imageio-test6" run)
    (ch-util:run-test #'imageio-test7 "imageio-test7" run)
    (ch-util:run-test #'imageio-test8 "imageio-test8" run)
    (ch-util:run-test #'imageio-test9 "imageio-test9" run)
    (ch-util:run-test #'imageio-test10 "imageio-test10" run)
    ))

(defun test-gaussian-blur ()
  (let ((images-component
         (asdf:find-component
          (asdf:find-component
           (asdf:find-system "ch-image-test") "test")
          "images")))
    (let ((inputfile
           (asdf:component-pathname
            (asdf:find-component
             images-component "sunset-lzw")))
          (imagedir (asdf:component-pathname images-component)))
      (ensure-directories-exist imagedir)
      (let ((img (read-tiff-file inputfile)))
	(time
	 (progn
	   (setf (ch-image:image-a img)
		 (clem:gaussian-blur (ch-image:image-a img) :k 2))
           
	   (setf (ch-image:image-r img)
		 (clem:gaussian-blur (ch-image:image-r img) :k 2))
           
           (setf (ch-image:image-g img)
                 (clem:gaussian-blur (ch-image:image-g img) :k 2))
	   
	   (setf (ch-image:image-b img)
		 (clem:gaussian-blur (ch-image:image-b img) :k 2))))
        (setf (image-height img) (clem:rows (ch-image:image-r img)))
        (setf (image-width img) (clem:cols (ch-image:image-r img)))
	(write-tiff-file
         (test-output-img
          (merge-pathnames (make-pathname :name "blur-sunset" :type "tiff")
                           imagedir))
         img)
        img))))

(defun test-gaussian-blur-2 ()
  (let ((images-component
         (asdf:find-component
          (asdf:find-component
           (asdf:find-system "ch-image-test") "test")
          "images")))
    (let ((inputfile
           (asdf:component-pathname
            (asdf:find-component
             images-component "sunset-lzw")))
          (imagedir (asdf:component-pathname images-component)))
      (ensure-directories-exist imagedir)
      (let ((img (read-tiff-file inputfile)))
	(time
         (ch-image:gaussian-blur-image img :k 2))
        (write-tiff-file
         (test-output-img
          (merge-pathnames (make-pathname :name "blur-sunset" :type "tiff")
                           imagedir))
         img)
        img))))

(defun test-affine-transform ()
  (let ((images-component
         (asdf:find-component
          (asdf:find-component
           (asdf:find-system "ch-image-test") "test")
          "images")))
    (let ((inputfile
           (asdf:component-pathname
            (asdf:find-component
             images-component "euc-tiff")))
          (imagedir (asdf:component-pathname images-component)))
      (ensure-directories-exist imagedir)
      (let ((path (merge-pathnames (make-pathname :name "xfrm-euc" :type "tiff")
                                   imagedir)))
        (let ((img (ch-image:read-tiff-file inputfile)))
          (print img)
          (ch-image:affine-transform-image
           img (clem:make-affine-transformation :x-shift 0d0 :y-shift 0d0
                                                 :theta (* -.15 pi)
                                                 :x-shear 0.05d0
                                                 :y-shear 0d0
                                                 :x-scale (log 1.5)
                                                 :y-scale (log 1.5))
           :interpolation :bilinear
           :u '(-100 . 100)
           :v '(-100 . 100)
           :x '(-100 . 100)
           :y '(-100 . 100))
          (ch-image:write-tiff-file 
           (test-output-img path) img))))))

(defun test-affine-transform-2 ()
  (let ((images-component
         (asdf:find-component
          (asdf:find-component
           (asdf:find-system "ch-image-test") "test")
          "images")))
    (let ((inputfile
           (asdf:component-pathname
            (asdf:find-component
             images-component "euc-tiff")))
          (imagedir (asdf:component-pathname images-component)))
      (ensure-directories-exist imagedir)
      (let ((path (merge-pathnames (make-pathname :name "xfrm-euc-2" :type "tiff")
                                   imagedir)))
        (let ((img (ch-image:read-tiff-file inputfile))
              (xfrm (clem:make-affine-transformation :x-shift 0d0 :y-shift 0d0
                                                      :theta (* -.15 pi)
                                                      :x-shear 0.00d0
                                                      :y-shear 0d0
                                                      :x-scale (log 2)
                                                      :y-scale (log 2))))
          (let ((u1 (- (/ (image-width img) 2)))
                (u2 (/ (image-width img) 2))
                (v1 (- (/ (image-height img) 2)))
                (v2 (/ (image-height img) 2)))
            (multiple-value-bind (x1 y1 x2 y2)
                (clem:compute-bounds u1 v1 u2 v2 xfrm)
              (let ((ximg
                     (ch-image:affine-transform-image
                      img xfrm
                      :interpolation :bilinear
                      :u (cons u1 u2)
                      :v (cons v1 v2))))
                (ch-image:write-tiff-file 
                 (test-output-img path) ximg)))))))))

(defun load-euclid-image ()
  (let* ((images-component
         (asdf:find-component
          (asdf:find-component
           (asdf:find-system "ch-image-test") "test")
          "images"))
         (inputfile
          (asdf:component-pathname
           (asdf:find-component
            images-component "euc-tiff")))
         (img (ch-image:read-tiff-file inputfile)))
    img))

(defun test-affine-transform-3 (&optional (img (load-euclid-image)))
  (let ((xfrm (clem:make-affine-transformation :x-shift 0d0 :y-shift 0d0
                                                :theta (* (/ 14 180) pi)
                                                :x-shear 0.0d0
                                                :y-shear 0d0
                                                :x-scale (log 1.25)
                                                :y-scale (log 1.25))))
    (let ((ximg
           (ch-image:affine-transform-image
            img xfrm
            :interpolation :bilinear
            :u '(-200 . 200) :v '(-200 . 200) :x '(-200 . 200) :y '(-200 . 200))))
      (ch-image:write-tiff-file 
       (test-output-img (make-pathname :name "xfrm-euc-3" :type "tiff")) ximg))))

