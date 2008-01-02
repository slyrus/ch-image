
(in-package :ch-image)

(defconstant +ncomp-gray+ 1)
(defconstant +ncomp-rgb+ 3)
(defconstant +ncomp-argb+ 4)

(defun jpeg-gray-to-gray-image (buffer width height)
  (let ((offset 0) (img (make-instance 'ub8-matrix-image :width width :height height)))
    (map-pixels #'(lambda (img x y)
		    (set-pixel img x y (svref buffer (ch-util:postincf offset))))
		img)))

(defun jpeg-rgb-to-gray-image (buffer width height)
  (let ((offset 0) (img (make-instance 'ub8-matrix-image :width width :height height)))
    (map-pixels #'(lambda (img x y)
		    (let ((v (+ (svref buffer (ch-util:postincf offset))
				(svref buffer (ch-util:postincf offset))
				(svref buffer (ch-util:postincf offset)))))
		      (set-pixel img x y (floor (/ v 3)))))
		img)))

(defun jpeg-rgb-to-argb-image (buffer width height)
  (let ((offset 0) (img (make-instance 'argb-8888-image :width width :height height)))
    (map-pixels #'(lambda (img x y)
		    (let ((b (svref buffer (ch-util:postincf offset)))
			  (g (svref buffer (ch-util:postincf offset)))
			  (r (svref buffer (ch-util:postincf offset))))
		      (set-pixel img x y (list 255 r g b))))
		img)))

(defun read-jpeg-file (srcfile)
  (multiple-value-bind (buffer height width jpegimage)
      (jpeg:decode-image srcfile)
    (cond
      ((= (jpeg::descriptor-ncomp jpegimage) 3)
       (jpeg-rgb-to-argb-image buffer width height))
      ((= (jpeg::descriptor-ncomp jpegimage) 1)
       (jpeg-gray-to-gray-image buffer width height)))))

(defparameter *gray-q-tabs* (vector jpeg::*q-luminance*))

(defmethod write-jpeg-file (filename (img gray-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img))))
	(offset 0))
    (map-pixels #'(lambda (img x y)
		    (setf (svref jpegimg (ch-util:postincf offset)) (get-pixel img x y)))
		img)
    (jpeg:encode-image filename jpegimg +ncomp-gray+
		  (image-height img) (image-width img) :q-tabs *gray-q-tabs*)))

(defmethod write-jpeg-stream (stream (img gray-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img))))
	(offset 0))
    (map-pixels #'(lambda (img x y)
		    (setf (svref jpegimg (ch-util:postincf offset)) (get-pixel img x y)))
		img)
    (jpeg::encode-image-stream stream jpegimg +ncomp-gray+
		  (image-height img) (image-width img) :q-tabs *gray-q-tabs*)))

(defparameter *argb-q-tabs*
  (vector jpeg::*q-luminance-hi* jpeg::*q-chrominance-hi*
	  jpeg::*q-luminance-hi* jpeg::*q-chrominance-hi*))

(defparameter *argb-sampling* '((1 1)(1 1)(1 1)(1 1)))

(defmethod write-jpeg-file-with-alpha (filename (img argb-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img) +ncomp-argb+)))
	(offset 0))
    (map-pixels #'(lambda (img x y)
		    (multiple-value-bind (a r g b) (get-argb-values img x y)
		      (setf (svref jpegimg (ch-util:postincf offset)) a)
		      (setf (svref jpegimg (ch-util:postincf offset)) b)
		      (setf (svref jpegimg (ch-util:postincf offset)) g)
		      (setf (svref jpegimg (ch-util:postincf offset)) r)))
		img)
    (jpeg:encode-image filename jpegimg +ncomp-argb+ 
		  (image-height img) (image-width img)
		  :q-tabs *argb-q-tabs*
		  :sampling *argb-sampling*)))

(defparameter *rgb-sampling* '((1 1)(1 1)(1 1)))

(defmethod write-jpeg-file (filename (img argb-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img) +ncomp-rgb+)))
	(offset 0))
    (map-pixels #'(lambda (img x y)
		    (multiple-value-bind (a r g b) (get-argb-values img x y)
		      (declare (ignore a))
		      (setf (svref jpegimg (ch-util:postincf offset)) b)
		      (setf (svref jpegimg (ch-util:postincf offset)) g)
		      (setf (svref jpegimg (ch-util:postincf offset)) r)))
		img)
    (jpeg:encode-image filename jpegimg +ncomp-rgb+
		  (image-height img) (image-width img)
		  :sampling *rgb-sampling*
		  )
    (truename filename)))

(defmethod write-jpeg-stream (stream (img argb-image))
  (let ((jpegimg (make-array (* (image-width img) (image-height img) +ncomp-rgb+)))
	(offset 0))
    (map-pixels #'(lambda (img x y)
		    (multiple-value-bind (a r g b) (get-argb-values img x y)
		      (declare (ignore a))
		      (setf (svref jpegimg (ch-util:postincf offset)) b)
		      (setf (svref jpegimg (ch-util:postincf offset)) g)
		      (setf (svref jpegimg (ch-util:postincf offset)) r)))
		img)
    (jpeg::encode-image-stream stream jpegimg +ncomp-rgb+
                         (image-height img) (image-width img)
                         :sampling *rgb-sampling*
                         )))

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
