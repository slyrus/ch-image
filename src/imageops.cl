;;
;; file: imageops.cl
;; author: cyrus harmon
;; time-stamp: Tue Jul  6 17:03:39 PDT 2004
;;

(in-package :image)

(defparameter *masked-pixel* '(255 0 0 0))

(defun mask-image (img seg &key (mask-val 0) (masked-pixel *masked-pixel*))
  (let ((mask (make-instance 'argb-8888-image :width (image-width img) :height (image-height img))))
    (map-pixels #'(lambda (img x y)
		    (if (/= (get-pixel seg x y) mask-val)
			(set-pixel mask x y (get-pixel img x y))	
			(set-pixel mask x y masked-pixel)	
			))
		img)
    mask))

(defun flip-image (img)
  (let ((h (image-height img))
	(w (image-width img)))
    (declare (dynamic-extent h w) (fixnum h w))
    (let ((fimg (make-instance (class-of img) :width w :height h)))
      (dotimes (i (image-height img))
	(declare (dynamic-extent i) (fixnum i))
	(dotimes (j (image-width img))
	  (declare (dynamic-extent j) (fixnum j))
	  (set-pixel fimg (- w j 1) i
		     (get-pixel img j i))))
      fimg)))

(defmethod get-gray-image-levels ((img gray-image))
  (let ((l (make-array 256)))
    (dotimes (i (image-height img))
      (declare (dynamic-extent i) (fixnum i))
      (dotimes (j (image-width img))
	(declare (dynamic-extent j) (fixnum j))
	(setf (aref l (get-pixel img j i)) 1)))
    l))

(defmethod get-level-ordering ((l vector))
  (let ((o (make-array 256 :initial-element 0))
	(v 0)
	k)
    (dotimes (i 256)
      (declare (dynamic-extent i) (fixnum i))
      (when (> (aref l i) 0)
	(setf (aref o i) (postincf v))
	(push i k)))
    (values o (reverse k))))

(defmethod argb-image-to-blue-image ((src argb-image))
  (let ((dest (make-instance 'gray-image :width (image-width src) :height (image-height src))))
    (map-pixels #'(lambda (img x y)
		    (destructuring-bind (a r g b) (get-pixel img x y)
		      (declare (ignore a r g))
		      (set-pixel dest x y b)))
		src)
    dest))

