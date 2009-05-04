
(in-package :ch-image-test)

(defun test-gray-image ()
  (destructuring-bind (height width) (list 400 300)
    (let ((img (make-instance 'matrix-gray-image :width width :height height)))
      (dotimes (h height)
        (dotimes (w width)
          (set-gray-value img h w (mod (* w 3) 255))))
      img)))

(defun test-rgb-image ()
  (destructuring-bind (height width) (list 400 300)
    (let ((img (make-instance 'rgb-888-image :width width :height height))
          (radfudge (* 6 pi .5)))
      (declare (ignorable radfudge))
      (dotimes (h height)
        (dotimes (w width)
          (set-rgb-values img h w
                          (abs (floor (* 255 (sin (* radfudge (/ h height))))))
                          (abs (floor (* 200 (cos (* .2 radfudge (/ w width))))))
                          (mod (* w h) 255)) ))
      img)))

(defun test-argb-image ()
  (destructuring-bind (height width) (list 400 300)
    (let ((img (make-instance 'argb-8888-image :width width :height height))
          (radfudge (* 6 pi .5)))
      (declare (ignorable radfudge))
      (dotimes (h height)
        (dotimes (w width)
          (set-argb-values img h w
                           (abs (floor (+ 191 (* 64 (cos (* 2 radfudge (/ w width)))))))
                           (abs (floor (* 255 (sin (* radfudge (/ h height))))))
                           (abs (floor (* 200 (cos (* .2 radfudge (/ w width))))))
                           (mod (* w h) 255)) ))
      img)))

(defparameter *test-gray-image* (test-gray-image))
(defparameter *test-rgb-image* (test-rgb-image))
(defparameter *test-argb-image* (test-argb-image))

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
  (truename
   (merge-pathnames
    (make-pathname :directory (list :relative :up "output-images"))
    (make-pathname :directory 
                   (pathname-directory
                    (asdf:component-pathname
                     (asdf:find-component
                      (asdf:find-component
                       (asdf:find-system "ch-image-test")
                       "test")
                      "images")))))))

(ensure-directories-exist *output-image-path*)

(defun test/write-grayscale-image-tiff-1 ()
  (write-tiff-file 
   (merge-pathnames *output-image-path* "grayimgtest.tiff")
   (test-gray-image)))

(defun test/write-rgb-image-tiff-1 ()
  (write-tiff-file 
   (merge-pathnames *output-image-path* "rgbimgtest.tiff")
   *test-rgb-image*))

(defun test/write-argb-image-tiff-1 ()
  (write-tiff-file
   (merge-pathnames *output-image-path* "argbimgtest.tiff")
   *test-argb-image*))

(defun test/write-grayscale-image-jpeg-1 ()
  (write-jpeg-file
   (merge-pathnames *output-image-path* "grayimgtest.jpeg")
   (test-gray-image)))

(defun test/write-rgb-image-jpeg-1 ()
  (write-jpeg-file
   (merge-pathnames *output-image-path* "rgbimgtest.jpeg")
   *test-rgb-image*))

(defun test/write-argb-image-jpeg-1 ()
  (write-jpeg-file 
   (merge-pathnames *output-image-path* "argbimgtest.jpeg")
   *test-argb-image*))

;;; RGB TIFF -> RGB TIFF
(defun test/read-rbg-tiff-write-rgb-tiff-1 ()
  (let ((srcfile (test-img "euc-tiff"))
        (destfile (merge-pathnames *output-image-path* "rgbreadtest.tiff")))
    (let ((img (read-tiff-file srcfile)))
      (when img
        (write-tiff-file destfile img)))))

;;; RGB TIFF -> GRAY TIFF
(defun test/read-rgb-tiff-write-grayscale-tiff-1 ()
  (let ((srcfile (test-img "euc-tiff"))
        (destfile (merge-pathnames *output-image-path* "grayreadtest.tiff")))
    (let ((img (cadr (get-channels (read-tiff-file srcfile)))))
      (when img
        (write-tiff-file destfile img)))))

;;; GRAY TIFF -> GRAY TIFF
(defun test/read-grayscale-tiff-write-grayscale-tiff-1 ()
  (let ((srcfile (test-img "eucgray-tiff"))
        (destfile (merge-pathnames *output-image-path* "grayreadtest3.tiff")))
    (let ((img (read-tiff-file srcfile)))
      (when img
        (write-tiff-file destfile img)))))

(defun test/read-rgb-jpeg-write-grayscale-tiff-1 ()
  (let ((srcfile (test-img "euc-jpeg"))
        (destfile (merge-pathnames *output-image-path* "grayreadtest2.tiff")))
    (let ((img (cadr (get-channels (read-jpeg-file srcfile)))))
      (when img
        (write-tiff-file destfile img)))))

;;; RGB JPEG -> RGB JPEG
(defun test/read-rgb-jpeg-write-rgb-jpeg-1 ()
  (let ((srcfile (test-img "euc-jpeg"))
        (destfile (merge-pathnames *output-image-path* "argbreadtest.jpeg")))
    (let ((img (read-jpeg-file srcfile)))
      (when img
        (write-jpeg-file destfile img)))))

;;; GRAY JPEG -> GRAY JPEG
(defun test/read-grayscale-jpeg-write-grayscale-jpeg-1 ()
  (let ((srcfile (test-img "eucgray-jpeg"))
        (destfile (merge-pathnames *output-image-path* "grayreadtest.jpeg")))
    (let ((img (read-jpeg-file srcfile)))
      (when img
        (write-jpeg-file destfile img)))))

;;; RGB TIFF -> RGB JPEG
(defun test/read-rgb-tiff-write-rgb-jpeg-1 ()
  (let ((srcfile (test-img "euc-tiff"))
        (destfile (merge-pathnames *output-image-path* "tiffreadtest.jpeg")))
    (let ((img (read-tiff-file srcfile)))
      (when img
        (write-jpeg-file destfile img)))))

;;; RGB JPEG -> RGB TIFF
(defun test/read-rgb-jpeg-write-rgb-tiff-1 ()
  (let ((srcfile (test-img "euc-jpeg"))
        (destfile (merge-pathnames *output-image-path* "jpegreadtest.tiff")))
    (let ((img (read-jpeg-file srcfile)))
      (when img
        (write-tiff-file destfile img)))))


(defun run-tests-2 ()
  (flet ((run-it (test-name)
           (funcall test-name)))
    (mapcar #'run-it
            '(test/write-grayscale-image-tiff-1
              test/write-rgb-image-tiff-1
              test/write-argb-image-tiff-1
                
              test/write-grayscale-image-jpeg-1
              test/write-rgb-image-jpeg-1
              test/write-argb-image-jpeg-1
                
              test/read-rbg-tiff-write-rgb-tiff-1
              test/read-rgb-tiff-write-grayscale-tiff-1
              test/read-grayscale-tiff-write-grayscale-tiff-1
                
              test/read-rgb-jpeg-write-grayscale-tiff-1
                
              test/read-rgb-jpeg-write-rgb-jpeg-1
              test/read-grayscale-jpeg-write-grayscale-jpeg-1
              test/read-rgb-tiff-write-rgb-jpeg-1
              test/read-rgb-jpeg-write-rgb-tiff-1

              ))))

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
         (merge-pathnames *output-image-path*
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
         (merge-pathnames *output-image-path*
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
          (let ((ximg
                 (ch-image:affine-transform-image
                  img (clem:make-affine-transformation :x-shift 0d0 :y-shift 0d0
                                                       :theta (* -.15 pi)
                                                       :x-shear 0.05d0
                                                       :y-shear 0d0
                                                       :x-scale 1.5
                                                       :y-scale 1.5)
                  :interpolation :bilinear
                  :u '(-100 . 100)
                  :v '(-100 . 100)
                  :x '(-100 . 100)
                  :y '(-100 . 100))))
            (ch-image:write-tiff-file 
             (merge-pathnames *output-image-path* path) ximg)))))))

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
                                                      :x-scale 2
                                                      :y-scale 2)))
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
                 (merge-pathnames *output-image-path* path) ximg)))))))))

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
                                                :x-scale 1.25
                                                :y-scale 1.25)))
    (let ((ximg
           (ch-image:affine-transform-image
            img xfrm
            :interpolation :bilinear
            :u '(-200 . 200) :v '(-200 . 200) :x '(-200 . 200) :y '(-200 . 200))))
      (ch-image:write-tiff-file 
       (merge-pathnames *output-image-path* (make-pathname :name "xfrm-euc-3" :type "tiff")) ximg))))


