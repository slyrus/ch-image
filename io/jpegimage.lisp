
(in-package :ch-image)

(defconstant +ncomp-gray+ 1)
(defconstant +ncomp-rgb+ 3)
(defconstant +ncomp-argb+ 4)

(defmacro postincf (x &optional (step 1))
  (let ((pre (gensym)))
    `(let ((,pre ,x))
       (incf ,x ,step)
       ,pre)))

(defun jpeg-gray-to-gray-image (buffer width height)
  (let ((offset 0) (img (make-instance 'ub8-matrix-image :width width :height height)))
    (map-pixels #'(lambda (img x y)
		    (set-pixel img x y (svref buffer (postincf offset))))
		img)))

(defun jpeg-rgb-to-gray-image (buffer width height)
  (let ((offset 0) (img (make-instance 'ub8-matrix-image :width width :height height)))
    (map-pixels #'(lambda (img x y)
		    (let ((v (+ (svref buffer (postincf offset))
				(svref buffer (postincf offset))
				(svref buffer (postincf offset)))))
		      (set-pixel img x y (floor (/ v 3)))))
		img)))

(defun jpeg-rgb-to-argb-image (buffer width height)
  (let ((offset 0) (img (make-instance 'argb-8888-image :width width :height height)))
    (map-pixels #'(lambda (img x y)
		    (let ((b (svref buffer (postincf offset)))
			  (g (svref buffer (postincf offset)))
			  (r (svref buffer (postincf offset))))
		      (set-pixel img x y (list 255 r g b))))
		img)))

(defun read-jpeg-file (srcfile)
  (multiple-value-bind (buffer height width ncomp)
      (jpeg:decode-image srcfile)
    (cond
      ((= ncomp 3)
       (jpeg-rgb-to-argb-image buffer width height))
      ((= ncomp 1)
       (jpeg-gray-to-gray-image buffer width height)))))

(defparameter *gray-q-tabs* (vector jpeg::*q-luminance*))

(defgeneric write-jpeg-file (filename img))

(defmethod write-jpeg-file (filename (img gray-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img))))
	(offset 0))
    (map-pixels #'(lambda (img x y)
		    (setf (svref jpegimg (postincf offset)) (get-pixel img x y)))
		img)
    (jpeg:encode-image filename jpegimg +ncomp-gray+
                       (image-height img) (image-width img) :q-tabs *gray-q-tabs*)
    (truename filename)))

(defmethod write-jpeg-stream (stream (img gray-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img))))
	(offset 0))
    (map-pixels #'(lambda (img x y)
		    (setf (svref jpegimg (postincf offset)) (get-pixel img x y)))
		img)
    (jpeg::encode-image-stream stream jpegimg +ncomp-gray+
		  (image-height img) (image-width img) :q-tabs *gray-q-tabs*)))

(defparameter *rgb-sampling* '((1 1)(1 1)(1 1)))

(defmethod write-jpeg-file (filename (img argb-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img) +ncomp-rgb+)))
        (offset 0))
    (map-pixels #'(lambda (img x y)
                    (multiple-value-bind (a r g b) (get-argb-values img x y)
                      (declare (ignore a))
                      (setf (svref jpegimg (postincf offset)) b)
                      (setf (svref jpegimg (postincf offset)) g)
                      (setf (svref jpegimg (postincf offset)) r)))
                img)
    (jpeg:encode-image filename jpegimg +ncomp-rgb+
                       (image-height img) (image-width img)
                       :sampling *rgb-sampling*))
  (truename filename))

(defmethod write-jpeg-file (filename (img rgb-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img) +ncomp-rgb+)))
	(offset 0))
    (map-pixels #'(lambda (img x y)
		    (multiple-value-bind (r g b) (get-rgb-values img x y)
		      (setf (svref jpegimg (postincf offset)) b)
		      (setf (svref jpegimg (postincf offset)) g)
		      (setf (svref jpegimg (postincf offset)) r)))
		img)
    (jpeg:encode-image filename jpegimg +ncomp-rgb+
                       (image-height img) (image-width img)
                       :sampling *rgb-sampling*)
    (truename filename)))

(defmethod write-jpeg-stream (stream (img argb-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img) +ncomp-rgb+)))
	(offset 0))
    (map-pixels #'(lambda (img x y)
		    (multiple-value-bind (a r g b) (get-argb-values img x y)
		      (declare (ignore a))
		      (setf (svref jpegimg (postincf offset)) b)
		      (setf (svref jpegimg (postincf offset)) g)
		      (setf (svref jpegimg (postincf offset)) r)))
		img)
    (jpeg::encode-image-stream stream jpegimg +ncomp-rgb+
                         (image-height img) (image-width img)
                         :sampling *rgb-sampling*)))

(defmethod write-jpeg-stream (stream (img rgb-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img) +ncomp-rgb+)))
	(offset 0))
    (map-pixels #'(lambda (img x y)
		    (multiple-value-bind (r g b) (get-rgb-values img x y)
		      (setf (svref jpegimg (postincf offset)) b)
		      (setf (svref jpegimg (postincf offset)) g)
		      (setf (svref jpegimg (postincf offset)) r)))
		img)
    (jpeg::encode-image-stream stream jpegimg +ncomp-rgb+
                               (image-height img) (image-width img)
                               :sampling *rgb-sampling*)))

(defun gray-image-matrix-to-jpeg-array (m width height)
  (let ((b 0)
	(jpegimg (make-array (* width height))))
    (dotimes (h height)
      (dotimes (w width)
	(setf (svref jpegimg b) (clem::val m h w))
	(incf b)))
    jpegimg))
  
(defmethod gray-matrix-write-jpeg (filename (m clem:matrix) width height)
  (jpeg:encode-image filename
		      (gray-image-matrix-to-jpeg-array m width height)
		      1 height width :q-tabs (vector jpeg::*q-luminance*)))

(defmethod gray-matrix-write-jpeg-stream (img-stream (m clem:matrix) width height)
  (jpeg::encode-image-stream img-stream
		      (gray-image-matrix-to-jpeg-array m width height)
		      1 height width :q-tabs (vector jpeg::*q-luminance*)))

