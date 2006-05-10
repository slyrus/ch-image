;;
;; file: ch-image.cl
;; author: cyrus harmon
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
;;; H - 16 bits (half-word)
;;; W - 32 bits (word)
;;; F - single float
;;; D - double float

(in-package :ch-image)

(defclass clip-region () ())

(defclass clip-rect (clip-region)
  ((y1 :accessor y1 :initarg :y1)
   (x1 :accessor x1 :initarg :x1)
   (y2 :accessor y2 :initarg :y2)
   (x2 :accessor x2 :initarg :x2)))

(defclass image ()
  ((data :accessor image-data)
   (height :accessor image-height :initarg :height)
   (width :accessor image-width :initarg :width)
   (channels :accessor image-channels :initform 1)
   (clip-region :accessor clip-region :initarg :clip-region))
  (:documentation "abstract image class"))

(defun image-dim (img)
  (values (image-height img) (image-width img)))

(defmethod shared-initialize :after
    ((img image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (when (and (slot-boundp img 'width)
             (slot-boundp img 'height))
    (setf (clip-region img) (make-instance 'clip-rect
                                           :y1 0 :x1 0 :y2 (image-height img) :x2 (image-width img)))))

(defmethod image ((width fixnum) (height fixnum))
  (let ((img (make-instance 'image)))
    (setf (image-height img) height)
    (setf (image-width img) width)
    img))

(defclass multichannel-image (image) ())

(defclass rgb-image (multichannel-image)
  ((r :accessor image-r)
   (g :accessor image-g)
   (b :accessor image-b))
  (:documentation "RGB (Red/Green/Blue) image class"))

(defmethod get-channels ((img rgb-image))
  (list (image-r img) (image-g img) (image-b img)))

(defmethod set-channels ((img rgb-image) channels)
  (setf (image-r img) (second channels))
  (setf (image-g img) (third channels))
  (setf (image-b img) (fourth channels)))


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

(defclass rgb-hhh-image (rgb-image) ())

(defmethod shared-initialize :after
    ((img rgb-hhh-image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (and (slot-boundp img 'width)
	   (slot-boundp img 'height))
      (with-slots (width height) img
	(setf (image-r img) (make-instance 'ub16-matrix :rows height :cols width))
	(setf (image-g img) (make-instance 'ub16-matrix :rows height :cols width))
	(setf (image-b img) (make-instance 'ub16-matrix :rows height :cols width))
	(setf (image-data img) (list (image-r img) (image-g img) (image-b img))))))

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

(defclass argb-image (multichannel-image)
  ((a :accessor image-a)
   (r :accessor image-r)
   (g :accessor image-g)
   (b :accessor image-b))
  (:documentation "ARGB (Alpha/Red/Green/Blue) image class"))

(defmethod get-channels ((img argb-image))
  (list (image-a img) (image-r img) (image-g img) (image-b img)))

(defmethod set-channels ((img argb-image) channels)
  (setf (image-a img) (first channels))
  (setf (image-r img) (second channels))
  (setf (image-g img) (third channels))
  (setf (image-b img) (fourth channels)))

(defclass argb-8888-image (argb-image) ())

(defmethod shared-initialize :after
    ((img argb-8888-image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (and (slot-boundp img 'width)
	   (slot-boundp img 'height))
      (let ((width (slot-value img 'width))
	    (height (slot-value img 'height)))
	(setf (image-a img) (make-instance 'ub8-matrix :rows height :cols width :initial-element 255))
	(setf (image-r img) (make-instance 'ub8-matrix :rows height :cols width))
	(setf (image-g img) (make-instance 'ub8-matrix :rows height :cols width))
	(setf (image-b img) (make-instance 'ub8-matrix :rows height :cols width))
	(setf (image-data img) (list (image-a img) (image-r img) (image-g img) (image-b img))))))

(defclass argb-hhhh-image (argb-image) ())

(defmethod shared-initialize :after
    ((img argb-hhhh-image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (and (slot-boundp img 'width)
	   (slot-boundp img 'height))
      (let ((width (slot-value img 'width))
	    (height (slot-value img 'height)))
	(setf (image-a img) (make-instance 'ub16-matrix :rows height :cols width))
	(setf (image-r img) (make-instance 'ub16-matrix :rows height :cols width))
	(setf (image-g img) (make-instance 'ub16-matrix :rows height :cols width))
	(setf (image-b img) (make-instance 'ub16-matrix :rows height :cols width))
	(setf (image-data img) (list (image-a img) (image-r img) (image-g img) (image-b img))))))

(defclass argb-ffff-image (argb-image) ())

(defmethod shared-initialize :after
    ((img argb-ffff-image) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (and (slot-boundp img 'width)
	   (slot-boundp img 'height))
      (with-slots (width height) img
	(setf (image-a img) (make-instance 'double-float-matrix :rows height :cols width))
	(setf (image-r img) (make-instance 'double-float-matrix :rows height :cols width))
	(setf (image-g img) (make-instance 'double-float-matrix :rows height :cols width))
	(setf (image-b img) (make-instance 'double-float-matrix :rows height :cols width))
	(setf (image-data img) (list (image-a img)
                                     (image-r img)
                                     (image-g img)
                                     (image-b img))))))

(defmethod pad-image ((img argb-image))
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
  (setf (clem::mref (image-b img) row col) b))

(defmethod set-argb-values ((img argb-8888-image)
			    (row fixnum)
			    (col fixnum)
			    (a fixnum)
			    (r fixnum)
			    (g fixnum)
			    (b fixnum))
  "Sets the alpha, red, green and blue values at x, y"
  (setf (clem::ub8-matrix-mref (the clem:ub8-matrix (image-a img)) row col) a)
  (setf (clem::ub8-matrix-mref (the clem:ub8-matrix (image-r img)) row col) r)
  (setf (clem::ub8-matrix-mref (the clem:ub8-matrix (image-g img)) row col) g)
  (setf (clem::ub8-matrix-mref (the clem:ub8-matrix (image-b img)) row col) b))

(defmethod get-argb-values ((img argb-image) (row fixnum) (col fixnum))
  "Gets the alpha, red, green and blue values at x, y"
  (values (val (image-a img) row col)
	  (val (image-r img) row col)
	  (val (image-g img) row col)
	  (val (image-b img) row col)))

(defclass image-channel (image) ()
  (:documentation "base class for a single channel image"))

(defmethod get-channels ((img image-channel))
  (list (image-data img)))

(defmethod set-channels ((img image-channel) channels)
  (setf (image-data img) (first channels)))

(defmethod shared-initialize :after
    ((img image-channel) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (if (and (slot-boundp img 'width)
	   (slot-boundp img 'height))
      (let ((width (slot-value img 'width))
	    (height (slot-value img 'height)))
	(setf (image-data img)
	      (make-instance 'ub8-matrix :rows height :cols width)))))

(defmethod pad-image ((img image-channel))
  (set-image-data img (pad-matrix (image-data img))))

(defgeneric get-channel-value (img row col))
(defmethod get-channel-value ((img image-channel) (row fixnum) (col fixnum))
  "Gets the value at row, col"
  (val (image-data img) row col))

(defgeneric set-channel-value (img row col v))
(defmethod set-channel-value ((img image-channel) (row fixnum) (col fixnum) v)
  "Sets the value at row, col to v"
  (let ((m (image-data img)))
    (setf (clem::mref m row col) v)))

(defmethod set-image-data ((img image-channel) (m matrix))
  "Sets the image-channel data to the matrix m and updates image-width and image-height"
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
  (error "set-pixel not implemented for generic image class"))

(defmethod or-pixel ((img image) row col val)
  (declare (ignore row col val))
  (error "or-pixel not implemented for generic image class"))

(defmethod xor-pixel ((img image) row col val)
  (declare (ignore row col val))
  (error "xor-pixel not implemented for generic image class"))

(defmethod and-pixel ((img image) row col val)
  (declare (ignore row col val))
  (error "and-pixel not implemented for generic image class"))

(defmethod get-pixel ((img image) row col)
  (declare (ignore row col))
  (error "get-pixel not implemented for generic image class"))

(defmethod set-pixel ((img image-channel) row col val)
  (set-channel-value img row col val))

(defmethod get-pixel ((img image-channel) row col)
  (get-channel-value img row col))

(defmethod or-pixel ((img image-channel) row col val)
  (set-channel-value img row col (logior (get-channel-value img row col) val)))

(defmethod xor-pixel ((img image-channel) row col val)
  (set-channel-value img row col (logxor (get-channel-value img row col) val)))

(defmethod and-pixel ((img image-channel) row col val)
  (set-channel-value img row col (logand (get-channel-value img row col) val)))

(defmethod set-pixel ((img argb-image) row col val)
  (set-argb-values img row col
		   (car val)
		   (cadr val)
		   (caddr val)
		   (cadddr val)))

(defmethod or-pixel ((img argb-image) row col val)
  (multiple-value-bind (a r g b) (get-argb-values img row col)
    (set-argb-values img row col
                     (logior a (car val))
                     (logior r (cadr val))
                     (logior g (caddr val))
                     (logior b (cadddr val)))))

(defmethod xor-pixel ((img argb-image) row col val)
  (multiple-value-bind (a r g b) (get-argb-values img row col)
    (set-argb-values img row col
                     (logxor a (car val))
                     (logxor r (cadr val))
                     (logxor g (caddr val))
                     (logxor b (cadddr val)))))

(defmethod and-pixel ((img argb-image) row col val)
  (multiple-value-bind (a r g b) (get-argb-values img row col)
    (set-argb-values img row col
                     (logand a (car val))
                     (logand r (cadr val))
                     (logand g (caddr val))
                     (logand b (cadddr val)))))

(defmethod get-pixel ((img argb-image) row col)
  (multiple-value-bind (a r g b) (get-argb-values img row col)
    (list a r g b)))

(defclass gray-image (image-channel) ()
  (:documentation "Grayscale 8-bit image class"))

(defmethod set-gray-value ((img gray-image) (row fixnum) (col fixnum) value)
  (set-channel-value img row col value))

(defmethod get-gray-value ((img gray-image) (row fixnum) (col fixnum))
  (get-channel-value img row col))

(defmethod get-pixel ((img gray-image) row col)
  (get-gray-value img row col))

(defclass matrix-image-channel (image-channel matrix)
  ((clem:rows :initarg :height :accessor image-height)
   (clem:cols :initarg :width :accessor image-width)
   (clem::initial-element :accessor initial-element
			    :initarg :initial-element
			    :initform (coerce 0 'unsigned-byte)))
  (:metaclass clem::standard-matrix-class)
  (:documentation "image channel class that is also a matrix"))

(defmethod shared-initialize :after
    ((img matrix-image-channel) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (when (and (slot-boundp img 'clem:rows)
             (slot-boundp img 'clem:cols))
    (setf (clip-region img) (make-instance 'clip-rect
                                           :y1 0 :x1 0 :y2 (clem:rows img) :x2 (clem:cols img)))))

(defclass ub8-matrix-image-channel (ub8-matrix matrix-image-channel) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "8-bit image channel class that is also a matrix"))

(defclass complex-matrix-image-channel (complex-matrix matrix-image-channel) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "complex image channel class that is also a matrix"))

(defmethod shared-initialize :after
    ((img complex-matrix-image-channel) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (image-data img) img))


(defclass complex-matrix-image (complex-matrix-image-channel gray-image) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "complex image channel class that is also a matrix"))

(defclass matrix-gray-image (matrix-image-channel gray-image) ()
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

(defclass ub8-matrix-image (ub8-matrix matrix-gray-image) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "Grayscale 8-bit image class that is also a matrix"))

(defmethod set-channels ((img ub8-matrix-image) channels)
  (setf (clem::matrix-vals img) (clem::matrix-vals (car channels))))

(defmethod get-channels ((img ub8-matrix-image))
  (list img))

(defmethod set-channel-value ((img ub8-matrix-image) (row fixnum) (col fixnum) (v fixnum))
  "Sets the grayscale value at row, col to v"
  (declare (type fixnum row col v))
  (let ((m (image-data img)))
    (let ((a (clem::matrix-vals m)))
      (declare (type (simple-array (unsigned-byte 8) (* *)) a))
      (setf (aref a row col) (clem::fit m v)))))

(defclass ub16-matrix-image-channel (ub16-matrix matrix-image-channel) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "16-unsigned-bit image channel class that is also a matrix"))

(defclass ub16-matrix-image (ub16-matrix matrix-gray-image) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "16-unsigned-bit image class that is also a matrix"))

(defclass ub32-matrix-image-channel (ub32-matrix matrix-image-channel) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "32-unsigned-bit image channel class that is also a matrix"))

(defclass ub32-matrix-image (ub32-matrix matrix-gray-image) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "32-unsigned-bit image class that is also a matrix"))

(defclass bit-matrix-image-channel (bit-matrix matrix-image-channel) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "1-bit image channel class that is also a matrix"))

(defclass bit-matrix-image (bit-matrix-image-channel matrix-gray-image) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "1-bit image class that is also a matrix"))

(defclass sb8-matrix-image-channel (sb8-matrix matrix-image-channel) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "8-signed-bit image channel class that is also a matrix"))

(defclass sb8-matrix-image (sb8-matrix matrix-gray-image) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "8-signed-bit image class that is also a matrix"))

(defclass sb16-matrix-image-channel (sb16-matrix matrix-image-channel) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "16-signed-bit image channel class that is also a matrix"))

(defclass sb16-matrix-image (sb16-matrix matrix-gray-image) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "16-signed-bit image class that is also a matrix"))

(defclass sb32-matrix-image-channel (sb32-matrix matrix-image-channel) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "32-signed-bit image channel class that is also a matrix"))

(defclass sb32-matrix-image (sb32-matrix matrix-gray-image) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "32-signed-bit image class that is also a matrix"))

(defclass double-float-matrix-image-channel (double-float-matrix matrix-image-channel) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "double-float image channel class that is also a matrix"))

(defclass double-float-matrix-image (double-float-matrix matrix-gray-image) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "double-float image class that is also a matrix"))

(defclass single-float-matrix-image-channel (single-float-matrix matrix-image-channel) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "single-float image channel class that is also a matrix"))

(defclass single-float-matrix-image (single-float-matrix matrix-gray-image) ()
  (:metaclass clem::standard-matrix-class)
  (:documentation "single-float image class that is also a matrix"))

