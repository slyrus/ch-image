;;;
;;; file: tiffimage.lisp
;;; author: cyrus harmon
;;; 

;;; TODO:
;;;  1. Function to read an arbitrary tiff image and return the proper
;;;     kind of image (argb, gray, etc...)
;;;  2. Support for 565-rgb images
;;;  3. Support for 888-rgb images
;;;  4. Support for FFF-rgb and FFFF-argb images
;;;  5. Support for 48-rgb and 64-argb images
;;;  6. Better support for gray read operations
;;;  7. TIFF compression on write

(in-package :ch-image)

(defun read-tiff-file (pathname)
  "reads a TIFF file and returns either a 32-bit ARGB image or an 8-bit
grayscale image"
  (declare (optimize (debug 2)))
  (let ((tiff-image (tiff:read-tiff-file pathname))
        image)
    (with-accessors ((image-length tiff:tiff-image-length)
                     (image-width tiff:tiff-image-width)
                     (samples-per-pixel tiff:tiff-image-samples-per-pixel) 
                     (image-data tiff:tiff-image-data))
        tiff-image
      (cond ((= samples-per-pixel 1) ;; Grayscale
             (setf image (make-instance 'matrix-gray-image
                                        :width image-width
                                        :height image-length))
             (loop for i below image-length
                do 
                  (loop for j below image-width
                     do 
                       (let ((pixoff (+ (* i image-width) j)))
                         (set-gray-value image i j
                                         (aref image-data pixoff))))))
            ((= samples-per-pixel 3) ;; RGB
             (setf image (make-instance 'rgb-888-image
                                        :width image-width
                                        :height image-length))
             (loop for i below image-length
                do 
                  (loop for j below image-width
                     do 
                       (let ((pixoff (* 3 (+ (* i image-width) j))))
                         (set-rgb-values image i j
                                         (aref image-data pixoff)
                                         (aref image-data (incf pixoff))
                                         (aref image-data (incf pixoff)))))))
            ((= samples-per-pixel 4) ;; ARGB
             (setf image (make-instance 'argb-8888-image
                                        :width image-width
                                        :height image-length))
             (loop for i below image-length
                do 
                  (loop for j below image-width
                     do 
                       (let ((pixoff (* 4 (+ (* i image-width) j))))
                         (set-argb-values image i j
                                          (aref image-data (+ pixoff 3))
                                          (aref image-data pixoff)
                                          (aref image-data (incf pixoff))
                                          (aref image-data (incf pixoff)))))))))
    image))
  
(defgeneric make-tiff-image (image)
  (:documentation "Makes a tiff:tiff-image from a ch-image:image"))

(defmethod make-tiff-image ((image ub8-matrix))
  (let ((tiff-image (make-instance 'tiff:tiff-image
                                   :width (cols image)
                                   :length (rows image)
                                   :bits-per-sample 8
                                   :samples-per-pixel 1
                                   :data (make-array (* (cols image)
                                                        (rows image))))))
    (with-accessors ((image-length tiff:tiff-image-length)
                     (image-width tiff:tiff-image-width)
                     (samples-per-pixel tiff:tiff-image-samples-per-pixel) 
                     (image-data tiff:tiff-image-data))
        tiff-image
      (loop for i below image-length
         do 
           (loop for j below image-width
              do 
                (let ((pixoff (+ (* i image-width) j)))
                  (setf (aref image-data pixoff) (mref image i j))))))
    tiff-image))

(defmethod make-tiff-image ((image matrix-gray-image))
  (let ((tiff-image (make-instance 'tiff:tiff-image
                                   :width (image-width image)
                                   :length (image-height image)
                                   :bits-per-sample 8
                                   :samples-per-pixel 1
                                   :data (make-array (* (image-width image)
                                                        (image-height image))))))
    (with-accessors ((image-length tiff:tiff-image-length)
                     (image-width tiff:tiff-image-width)
                     (samples-per-pixel tiff:tiff-image-samples-per-pixel) 
                     (image-data tiff:tiff-image-data))
        tiff-image
      (loop for i below image-length
         do 
           (loop for j below image-width
              do 
                (let ((pixoff (+ (* i image-width) j)))
                  (setf (aref image-data pixoff) (get-gray-value image i j))))))
    tiff-image))

(defmethod make-tiff-image ((image argb-8888-image))
  (let ((tiff-image (make-instance 'tiff:tiff-image
                                   :width (image-width image)
                                   :length (image-height image)
                                   :bits-per-sample '(8 8 8 8)
                                   :samples-per-pixel 4
                                   :data (make-array (* (image-width image)
                                                        (image-height image)
                                                        4)))))
    (with-accessors ((image-length tiff:tiff-image-length)
                     (image-width tiff:tiff-image-width)
                     (samples-per-pixel tiff:tiff-image-samples-per-pixel) 
                     (image-data tiff:tiff-image-data))
        tiff-image
      (loop for i below image-length
         do 
           (loop for j below image-width
              do 
                (let ((pixoff (* 4 (+ (* i image-width) j))))
                  (multiple-value-bind
                        (a r g b)
                      (get-argb-values image i j)
                    (setf (aref image-data pixoff) r
                          (aref image-data (incf pixoff)) g
                          (aref image-data (incf pixoff)) b
                          (aref image-data (incf pixoff)) a))))))
    tiff-image))

(defmethod make-tiff-image ((image rgb-888-image))
  (let ((tiff-image (make-instance 'tiff:tiff-image
                                   :width (image-width image)
                                   :length (image-height image)
                                   :bits-per-sample '(8 8 8)
                                   :samples-per-pixel 3
                                   :data (make-array (* (image-width image)
                                                        (image-height image)
                                                        3)))))
    (with-accessors ((image-length tiff:tiff-image-length)
                     (image-width tiff:tiff-image-width)
                     (samples-per-pixel tiff:tiff-image-samples-per-pixel) 
                     (image-data tiff:tiff-image-data))
        tiff-image
      (loop for i below image-length
         do 
           (loop for j below image-width
              do 
                (let ((pixoff (* 3 (+ (* i image-width) j))))
                  (multiple-value-bind
                        (r g b)
                      (get-rgb-values image i j)
                    (setf (aref image-data pixoff) r
                          (aref image-data (incf pixoff)) g
                          (aref image-data (incf pixoff)) b))))))
    tiff-image))

(defun write-tiff-file (pathname image)
  (let ((tiff-image (make-tiff-image image)))
    (tiff::write-tiff-file pathname tiff-image)))
