;;
;; file: ch-image.cl
;; author: cyrus harmon
;; time-stamp: Fri Apr 23 12:19:55 EDT 2004
;;

;;;
;;; This file contains the core of the image common-lisp class.
;;; This class represents images and provides utility functions for
;;; accessing images and image content.
;;;
;;; The main open question is how do we store multichannel (e.g. RGB)
;;; images? Do we store the images as planes? As interleaved planes?
;;; As an array of RGB values? I'm going to try the separate plane
;;; approach and see where that gets us.
;;;

;;; should the image accessor functions be row, col or x,y???

;;; TODO:
;;;  1. Support for 565-rgb images
;;;  2. Support for 888-rgb images
;;;  3. Support for FFF-rgb and FFFF-argb images
;;;  4. Support for 48-rgb and 64-argb images
;;;  5. Support for chunky images 

;;; sub-pixel element sizes:
;;; 5 - 5 bits
;;; 6 - 6 bits
;;; 8 - 8 bits
;;; W - 16 bits (half-word)
;;; L - 32 bits (word)
;;; F - single float
;;; D - double float

(in-package :ch-image)

(defclass image ()
  ((data :accessor image-data)
   (width :accessor image-width :initarg :width)
   (height :accessor image-height :initarg :height)
   (channels :accessor image-channels :initform 1)
   )
  (:documentation "abstract image class"))

(defmethod image ((width fixnum) (height fixnum))
  (let ((img (make-instance 'image)))
    (setf (image-width img) width)
    (setf (image-height img) height)
    img))

(defclass rgb-image (image)
  ((r :accessor image-r)
   (g :accessor image-g)
   (b :accessor image-b))
  (:documentation "RGB (Red/Green/Blue) image class"))

(defclass rgb-888-image (rgb-image) ())

(defmethod shared-initialize :after
    ((img rgb-888-image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (and (slot-boundp img 'width)
	   (slot-boundp img 'height))
      (with-slots (width height) img
	(setf (image-r img) (make-instance 'ub8-matrix :rows height :cols width))
	(setf (image-g img) (make-instance 'ub8-matrix :rows height :cols width))
	(setf (image-b img) (make-instance 'ub8-matrix :rows height :cols width))
	(setf (image-data img) (list (image-r img) (image-g img) (image-b img))))))

(defclass rgb-www-image (rgb-image) ())

(defmethod shared-initialize :after
    ((img rgb-www-image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (and (slot-boundp img 'width)
	   (slot-boundp img 'height))
      (with-slots (width height) img
	(setf (image-r img) (make-instance 'ub16-matrix :rows height :cols width))
	(setf (image-g img) (make-instance 'ub16-matrix :rows height :cols width))
	(setf (image-b img) (make-instance 'ub16-matrix :rows height :cols width))
	(setf (image-data img) (list (image-r img) (image-g img) (image-b img))))))

(defgeneric copy-image (img))
(defmethod copy-image ((img1 rgb-www-image))
  (let ((img2 (make-instance 'rgb-www-image
			     :height (image-height img1)
			     :width (image-width img1))))
    (let ((r1 (clem::matrix-vals (image-r img1)))
	  (g1 (clem::matrix-vals (image-g img1)))
	  (b1 (clem::matrix-vals (image-b img1)))
	  (r2 (clem::matrix-vals (image-r img2)))
	  (g2 (clem::matrix-vals (image-g img2)))
	  (b2 (clem::matrix-vals (image-b img2))))
      (declare (type (simple-array (unsigned-byte 16) (* *)) r1)
	       (type (simple-array (unsigned-byte 16) (* *)) g1)
	       (type (simple-array (unsigned-byte 16) (* *)) b1)
	       (type (simple-array (unsigned-byte 16) (* *)) r2)
	       (type (simple-array (unsigned-byte 16) (* *)) g2)
	       (type (simple-array (unsigned-byte 16) (* *)) b2))
      (loop for i from 0 below (image-height img1)
	 do
	   (loop for j from 0 below (image-width img1)
	      do 
		(setf (aref r2 i j) (aref r1 i j)
		      (aref g2 i j) (aref g1 i j)
		      (aref b2 i j) (aref b1 i j)))))
    img2))

(defclass rgb-fff-image (rgb-image) ())

(defmethod shared-initialize :after
    ((img rgb-fff-image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (and (slot-boundp img 'width)
	   (slot-boundp img 'height))
      (with-slots (width height) img
	(setf (image-r img) (make-instance 'double-float-matrix :rows height :cols width))
	(setf (image-g img) (make-instance 'double-float-matrix :rows height :cols width))
	(setf (image-b img) (make-instance 'double-float-matrix :rows height :cols width))
	(setf (image-data img) (list (image-r img) (image-g img) (image-b img))))))

(defclass argb-image (image)
  ((a :accessor image-a)
   (r :accessor image-r)
   (g :accessor image-g)
   (b :accessor image-b))
  (:documentation "ARGB (Alpha/Red/Green/Blue) image class"))

(defclass argb-8888-image (argb-image) ())

(defmethod shared-initialize :after
    ((img argb-8888-image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (and (slot-boundp img 'width)
	   (slot-boundp img 'height))
      (let ((width (slot-value img 'width))
	    (height (slot-value img 'height)))
	(setf (image-a img) (make-instance 'ub8-matrix :rows height :cols width))
	(setf (image-r img) (make-instance 'ub8-matrix :rows height :cols width))
	(setf (image-g img) (make-instance 'ub8-matrix :rows height :cols width))
	(setf (image-b img) (make-instance 'ub8-matrix :rows height :cols width))
	(setf (image-data img) (list (image-a img) (image-r img) (image-g img) (image-b img))))))

(defmethod pad-image ((img argb-8888-image))
  (setf (image-a img) (pad-matrix (image-a img)))
  (setf (image-r img) (pad-matrix (image-r img)))
  (setf (image-g img) (pad-matrix (image-g img)))
  (setf (image-b img) (pad-matrix (image-b img)))
  (setf (image-data img) (list (image-a img) (image-r img) (image-g img) (image-b img)))
  (setf (image-width img) (rows (image-r img)))
  (setf (image-height img) (cols (image-r img)))
  img)
  
(defmethod set-argb-values ((img argb-image)
			    (row fixnum)
			    (col fixnum)
			    (a fixnum)
			    (r fixnum)
			    (g fixnum)
			    (b fixnum))
  "Sets the alpha, red, green and blue values at x, y"
  (setf (clem::mref (image-a img) row col) a)
  (setf (clem::mref (image-r img) row col) r)
  (setf (clem::mref (image-g img) row col) g)
  (setf (clem::mref (image-b img) row col) b)
  )

(defmethod get-argb-values ((img argb-image) (row fixnum) (col fixnum))
  "Gets the alpha, red, green and blue values at x, y"
  (values (val (image-a img) row col)
	  (val (image-r img) row col)
	  (val (image-g img) row col)
	  (val (image-b img) row col)))

(defclass gray-image (image) ()
  (:documentation "Grayscale 8-bit image class"))

(defmethod shared-initialize :after
    ((img gray-image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (and (slot-boundp img 'width)
	   (slot-boundp img 'height))
      (let ((width (slot-value img 'width))
	    (height (slot-value img 'height)))
	(setf (image-data img)
	      (make-instance 'ub8-matrix :rows height :cols width)))))

(defmethod pad-image ((img gray-image))
  (set-image-data img (pad-matrix (image-data img))))

(defmethod get-gray-value ((img gray-image) (row fixnum) (col fixnum))
  "Gets the grayscale value at row, col"
  (val (image-data img) row col))

(defmethod set-gray-value ((img gray-image) (row fixnum) (col fixnum) (v fixnum))
  "Sets the grayscale value at row, col to v"
  (let ((m (image-data img)))
    (setf (clem::mref m row col) (clem::fit m v))))

(defmethod set-image-data ((img gray-image) (m matrix))
  "Sets the gray-image data to the matrix m and updates image-width and image-height"
  (destructuring-bind (h w) (dim m)
    (setf (image-height img) h)
    (setf (image-width img) w)
    (setf (image-data img) m)))

(defun map-pixels (f img)
  (dotimes (row (image-height img))
    (declare (dynamic-extent row) (fixnum row))
    (dotimes (col (image-width img))
      (declare (dynamic-extent col) (fixnum col))
      (funcall f img row col)))
  img)

(defmethod set-pixel ((img image) row col val)
  (declare (ignore row col val))
  (print "set-pixel not implemented for generic image class"))

(defmethod get-pixel ((img image) row col)
  (declare (ignore row col))
  (print "set-pixel not implemented for generic image class"))

(defmethod set-pixel ((img gray-image) row col val)
  (set-gray-value img row col val))

(defmethod get-pixel ((img gray-image) row col)
  (get-gray-value img row col))

(defmethod set-pixel ((img argb-image) row col val)
  (set-argb-values img row col
		   (car val)
		   (cadr val)
		   (caddr val)
		   (cadddr val)))

(defmethod get-pixel ((img argb-image) row col)
  (multiple-value-bind (a r g b) (get-argb-values img row col)
    (list a r g b)))

(defun rgb-to-gray-pixel (r g b)
  (declare (dynamic-extent r g b) (fixnum r g b))
  (floor (/ (+ r g b) 3)))

(defmethod argb-image-to-gray-image ((src argb-image))
  (let ((dest (make-instance 'gray-image :width (image-width src) :height (image-height src))))
    (map-pixels #'(lambda (img row col)
		    (declare (dynamic-extent row col) (fixnum row col))
		    (destructuring-bind (a r g b) (get-pixel img row col)
		      (declare (dynamic-extent a r g b) (fixnum a r g b))
		      (declare (ignore a))
		      (set-pixel dest row col (rgb-to-gray-pixel r g b))))
		src)
    dest))

(defclass matrix-gray-image (gray-image ub8-matrix)
  ((clem:rows :initarg :height :accessor image-height)
   (clem:cols :initarg :width :accessor image-width)
   (clem::initial-element :accessor initial-element
			    :initarg :initial-element
			    :initform (coerce 0 'unsigned-byte)))
  (:metaclass clem::standard-matrix-class)
  (:element-type (unsigned-byte 8))
  (:documentation "Grayscale 8-bit image class that is also a matrix"))

;(defmethod shared-initialize :around
;    ((img matrix-gray-image) slot-names &rest initargs &key &allow-other-keys)
;  (let ((rows) (cols) (width) (height))
;    (labels ((parse-init-args (alist)
;	       (let ((arg (pop alist))
;		     (val (pop alist)))
;		 (cond
;		   ((equal arg :width) (setf rows val))
;		   ((equal arg :height) (setf cols val))
;		   )
;		 (when alist (parse-init-args alist)))))
;      (parse-init-args initargs))
;    (if (not (getf initargs :width))
;	(setf width (getf initargs :rows)))
;    (if (not (getf initargs :height))
;	(setf height (getf initargs :cols)))
;    
;    (apply #'call-next-method img slot-names (append initargs
;						     (if rows (list :rows rows))
;						     (if cols (list :cols cols))
;						     (if width (list :width width))
;						     (if height (list :height height))
;						     ))))

(defmethod shared-initialize :after
    ((img matrix-gray-image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (image-data img) img))

