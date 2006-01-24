;;;
;;; file: tiffimage.cl
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

;;; NOTE!!! Need to do error checking on all of the TIFFOpen calls!!!

(in-package :ch-image)

(defun read-tiff-file (srcfile)
  "reads a TIFF file and returns either a 32-bit ARGB image or an 8-bit
grayscale image"
  (let ((truename (namestring (truename srcfile))))
    (let ((tif (tiff-ffi:tiff-open truename "r")))
      (when tif
	(prog1
	    (let ((w (tiff-ffi:tiff-get-field-image-width tif))
		  (h (tiff-ffi:tiff-get-field-image-length tif))
		  (samples (tiff-ffi::tiff-get-field-samples-per-pixel tif)))
	      (cond
		((not (/= samples 3 4))
		 (let ((ptr (sb-alien:make-alien sb-alien:unsigned-int (* w h))))
		   (tiff-ffi:tiff-read-rgba-image-oriented
                    tif w h ptr :orientation tiff-ffi::+ORIENTATION-TOPLEFT+)
		   (prog1
		       (tiff-rgba-to-argb-image ptr w h)
		     (tiff-ffi:tiff-free ptr))))
		((= samples 1)
                 ;; This is a hack - I use TIFFReadRGBAImageOriented
                 ;; and then convert to grayscale. Should go straight
                 ;; to grayscale, but that's more work...
		 (let ((ptr (sb-alien:make-alien sb-alien:unsigned-int (* w h))))
		   (tiff-ffi:tiff-read-rgba-image-oriented
                    tif w h ptr :orientation tiff-ffi::+ORIENTATION-TOPLEFT+)
		   (prog1
		       (tiff-rgba-to-gray-image ptr w h)
		     (tiff-ffi:tiff-free ptr))))))
	  (tiff-ffi:tiff-close tif))))))
  
(deftype unsigned-byte-ptr () '(sb-alien:alien (* sb-alien:unsigned-char)))
(deftype unsigned-byte-ptr-ptr () '(sb-alien:alien (* (* sb-alien:unsigned-char))))
(deftype unsigned-int-ptr () '(sb-alien:alien (* sb-alien:unsigned-int)))

(defun unix-name (filename)
  (let ((name (pathname-name filename))
	(type (pathname-type filename))
	(directory (pathname-directory filename)))
    (apply
     #'concatenate 'string
     (namestring (truename (make-pathname :directory directory)))
     "/" name
     (when type (list "." type)))))

(defmethod write-tiff-file (filename (img gray-image))
  (declare (optimize (debug 3)))
  (let ((truename (ch-util:unix-name filename)))
    (let ((f (tiff-ffi:tiff-open truename "w"))
	  (width (image-width img))
	  (height (image-height img)))
      (declare (type fixnum width height))
      (tiff-ffi:tiff-set-field-image-width f width)
      (tiff-ffi:tiff-set-field-image-length f height)
      (tiff-ffi:tiff-set-field-bits-per-sample f 8)
      (tiff-ffi:tiff-set-field-photometric f tiff-ffi::+PHOTOMETRIC-MINISBLACK+)
      (tiff-ffi:tiff-set-field-samples-per-pixel f 1)
      (tiff-ffi:tiff-set-field-planar-config f tiff-ffi::+PLANARCONFIG-CONTIG+)
      (tiff-ffi:tiff-set-field-orientation f tiff-ffi::+ORIENTATION-TOPLEFT+)
      (let ((scanline (sb-alien:make-alien sb-alien:unsigned-char (* width 1))))
	(declare (optimize (safety 0) (speed 3)))
	(locally (declare (type unsigned-byte-ptr scanline))
	  (dotimes (h height)
	    (declare (type fixnum h))
	    (dotimes (w width)
	      (declare (type fixnum w))
	      (let ((g (get-pixel img h w)))
		(setf (sb-alien:deref scanline w) g)))
	    (tiff-ffi:tiff-write-scanline f scanline h)))
	(sb-alien:free-alien scanline))
      (tiff-ffi:tiff-close f))
    truename))

(defun tiff-rgba-to-gray-image (rgba width height)
  (declare (optimize (safety 0) (speed 3)))
  (declare (type fixnum width height))
  (locally (declare (type unsigned-int-ptr rgba))
    (let ((offset 0)
	  (img (make-instance 'ub8-matrix-image :width width :height height)))
      (declare (type fixnum offset))
      (dotimes (h height)
	(declare (dynamic-extent h) (type fixnum h))
	(dotimes (w width)
	  (declare (dynamic-extent w) (type fixnum w))
	  (let ((val (sb-alien:deref rgba (ch-util:postincf offset))))
	    (declare (type (unsigned-byte 32) val))
	    (let ((v (+ (the (unsigned-byte 8) (ldb (byte 8 0) val)) ;; r
			(the (unsigned-byte 8) (ldb (byte 8 8) val))  ;; g
			(the (unsigned-byte 8) (ldb (byte 8 16) val))))) ;; b
	      (declare (type (unsigned-byte 32) v))
	      (set-pixel img h w (/ (- v (mod v 3)) 3))))))
      img)))

(defun tiff-rgba-to-argb-image (rgba width height)
  (declare (optimize (safety 0) (speed 3)))
  (declare (type fixnum width height))
  (locally (declare (type unsigned-int-ptr rgba))
    (let ((offset 0)
	  (img (make-instance 'argb-8888-image :width width :height height)))
      (declare (type fixnum offset))
      (dotimes (h height)
	(declare (dynamic-extent h) (type fixnum h))
	(dotimes (w width)
	  (declare (dynamic-extent w) (type fixnum w))
	  (let ((val (sb-alien:deref rgba (ch-util:postincf offset))))
	    (declare (type (unsigned-byte 32) val))
	    (set-argb-values img h w 
			     (the (unsigned-byte 8) (ldb (byte 8 24) val)) ;; a
			     (the (unsigned-byte 8) (ldb (byte 8 0) val)) ;; r
			     (the (unsigned-byte 8) (ldb (byte 8 8) val))  ;; g
			     (the (unsigned-byte 8) (ldb (byte 8 16) val)))))) ;; b
      img)))

(defmethod write-tiff-file (filename (img argb-image))
  (let ((truename (ch-util:unix-name filename)))
    (let ((f (tiff-ffi:tiff-open truename "w"))
	  (width (image-width img))
	  (height (image-height img)))
      (declare (type fixnum width height))

      #+(or sparc sparc-v9 powerpc ppc big-endian)
      (tiff-ffi:tiff-set-field-fill-order f tiff-ffi::+FILLORDER_MSB2LSB+)
      #-(or sparc sparc-v9 powerpc ppc big-endian)
      (tiff-ffi:tiff-set-field-fill-order f tiff-ffi::+FILLORDER_LSB2MSB+)
      
      (tiff-ffi:tiff-set-field-image-width f width)
      (tiff-ffi:tiff-set-field-image-length f height)
      (tiff-ffi:tiff-set-field-bits-per-sample f 8)
      (tiff-ffi:tiff-set-field-photometric f tiff-ffi::+PHOTOMETRIC-RGB+)
      (tiff-ffi:tiff-set-field-samples-per-pixel f 4)
      (tiff-ffi:tiff-set-field-planar-config f tiff-ffi::+PLANARCONFIG-CONTIG+)
      (tiff-ffi:tiff-set-field-orientation f tiff-ffi::+ORIENTATION-TOPLEFT+)
      (let ((scanline (sb-alien:make-alien sb-alien:unsigned-int (* width 4))))
	(declare (optimize (safety 0) (speed 3)))
	(locally (declare (type unsigned-int-ptr scanline))
	  (dotimes (h height)
	    (declare (type fixnum h))
	    (let ((destoff 0))
	      (declare (type fixnum destoff))
	      (dotimes (w width)
		(declare (type fixnum w))
		(destructuring-bind (a r g b) (get-pixel img h w)
	      ;;; TIFF scanlines are stored in rgba order
		  (let ((val 0))
		    (declare (type (unsigned-byte 32) val))
		    ;; I wish I didn't have to do this endianness hack here, but I
		    ;; can't seem to keep liftiff happy otherwise.
		    #+(or sparc sparc-v9 powerpc ppc big-endian)
		    (progn
		      (setf (the (unsigned-byte 8) (ldb (byte 8 24) val)) r)
		      (setf (the (unsigned-byte 8) (ldb (byte 8 16) val)) g)
		      (setf (the (unsigned-byte 8) (ldb (byte 8 8) val)) b)
		      (setf (the (unsigned-byte 8) (ldb (byte 8 0) val)) a))
		    #-(or sparc sparc-v9 powerpc ppc big-endian)
		    (progn
		      (setf (the (unsigned-byte 8) (ldb (byte 8 24) val)) a)
		      (setf (the (unsigned-byte 8) (ldb (byte 8 16) val)) b)
		      (setf (the (unsigned-byte 8) (ldb (byte 8 8) val)) g)
		      (setf (the (unsigned-byte 8) (ldb (byte 8 0) val)) r))
		    (setf (sb-alien:deref scanline (ch-util:postincf destoff)) val)))))
	    (tiff-ffi:tiff-write-scanline f scanline h))
	  (sb-alien:free-alien scanline)))
      (tiff-ffi:tiff-close f))
    truename))