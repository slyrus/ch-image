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
    (let ((fimg (make-instance (class-of img) :width w :height h)))
      (dotimes (i (image-height img))
	(dotimes (j (image-width img))
	  (set-pixel fimg (- w j 1) i
		     (get-pixel img j i))))
      fimg)))
