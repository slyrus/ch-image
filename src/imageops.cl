;;
;; file: imageops.cl
;; author: cyrus harmon
;;

(in-package :ch-image)

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
	  (set-pixel fimg i (- w j 1)
		     (get-pixel img i j))))
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

(defgeneric affine-transform-image (img xfrm &key u v x y interpolation background))
(defmethod affine-transform-image ((img image)
                                   (xfrm clem:affine-transformation)
                                   &key
                                   u v x y
                                   (interpolation nil interpolation-supplied-p)
                                   (background nil background-supplied-p))
  (let ((ximg (make-instance (class-of img)
                             :height 0
                             :width 0)))
    (set-channels
     ximg
     (mapcar #'(lambda (channel)
                 (apply #'clem::affine-transform channel xfrm
                        (append
                         (when u (list :u u))
                         (when v (list :v v))
                         (when x (list :x x))
                         (when y (list :y y))
                         (when background-supplied-p
                           (list :background background))
                         (when interpolation-supplied-p
                           (list :interpolation interpolation)))))
             (get-channels img)))
    (setf (image-height ximg) (rows (car (get-channels ximg))))
    (setf (image-width ximg) (cols (car (get-channels ximg))))
    ximg))

(defun split-around-zero (k &key integer)
  (let ((khalf (/ k 2.0d0)))
    (if integer
        (print (cons (1+ (floor (- khalf))) (floor khalf)))
        (print (cons (1+ (- khalf)) khalf)))))
        

;;; FIXME: Check to see what happens when constrain-proportions is t
;;; and no padding is needed!
(defun resize-image (img y x &key
                     (interpolation :bilinear)
                     (constrain-proportions nil))
  "Resize an image to new size y rows by x columns. If constrain
   proportions is t, then the xscale and yscale will be set to
   whichever scale has the largest absolute value and the image
   will be padded as needed."
  (let ((oldy (image-height img))
        (oldx (image-width img)))
    (let ((xscale (log (/ x oldx)))
          (yscale (log (/ y oldy))))
      (when constrain-proportions
        (setf xscale (* (signum xscale) (max (abs xscale) (abs yscale))))
        (setf yscale (* (signum yscale) (max (abs xscale) (abs yscale)))))
      (let ((xfrm (make-affine-transformation :x-scale xscale
                                              :y-scale yscale)))
        (let ((n (affine-transform-image
                  img xfrm
                  :interpolation interpolation
                  :u (split-around-zero oldx)
                  :v (split-around-zero oldy)
                  :x (split-around-zero x :integer t)
                  :y (split-around-zero y :integer t))))
          n)))))

(defun gaussian-blur-image (img &key (k 2) (sigma 1) (truncate nil))
  (declare (ignore truncate))
  (let* ((hr (clem::gaussian-kernel-1d k sigma))
         (hc (transpose hr)))
    (let ((mr (image-height img)) (mc (image-width img)) (hrows (rows hr)) (hcols (cols hc)))
      (let ((z1r mr) (z1c (+ mc hcols -1))
            (z2r (+ mr hrows -1)) (z2c (+ mc hcols -1))
            (matrix-class (class-of (car (get-channels img)))))
        (let ((z1 (make-instance matrix-class :rows z1r :cols z1c)))
          (set-channels
           img
           (mapcar #'(lambda (channel)
                       (let ((z2 (make-instance matrix-class :rows z2r :cols z2c)))
                         (clem::%separable-discrete-convolve channel hr hc z1 z2 :norm-v nil)))
                   (get-channels img)))
          (setf (image-height img) (clem:rows (ch-image::image-r img)))
          (setf (image-width img) (clem:cols (ch-image::image-r img)))))))
  img)

