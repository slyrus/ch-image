;;
;; file: imageops.cl
;; author: cyrus harmon
;;

(in-package :ch-image)

(defparameter *masked-argb-pixel* '(255 0 0 0))
(defparameter *masked-rgb-pixel* '(0 0 0))
(defparameter *masked-gray-pixel* 0)

(defun get-default-masked-pixel (img)
  (typecase img
    (argb-8888-image *masked-argb-pixel*)
    (rgb-888-image *masked-rgb-pixel*)
    (gray-image *masked-gray-pixel*)))

(defgeneric mask-image (img seg &key mask-val masked-pixel))

(defmethod mask-image ((img image) (seg image) &key (mask-val 0) (masked-pixel (get-default-masked-pixel img)))
  (let ((mask (make-instance (class-of img)
                             :width (image-width img) :height (image-height img))))
    (map-pixels #'(lambda (img y x)
		    (if (/= (get-pixel seg y x) mask-val)
			(set-pixel mask y x (get-pixel img y x))	
			(set-pixel mask y x masked-pixel)))
		img)
    mask))

(defmethod mask-image ((img image) (seg bit-matrix) &key (mask-val 0) (masked-pixel (get-default-masked-pixel img)))
  (let ((mask (make-instance (class-of img)
                             :width (image-width img) :height (image-height img))))
    (map-pixels #'(lambda (img y x)
		    (if (/= (clem::mref seg y x) mask-val)
			(set-pixel mask y x (get-pixel img y x))	
			(set-pixel mask y x masked-pixel)))
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
	(setf (aref o i) v)
        (incf v)
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

(defmethod gray-image-to-argb-8888-image ((src gray-image) &key (alpha 255))
  (let ((dest (make-instance 'argb-8888-image :width (image-width src) :height (image-height src))))
    (map-pixels #'(lambda (img x y)
		    (let ((v (get-pixel img x y)))
		      (set-pixel dest x y (list alpha v v v))))
		src)
    dest))

(defgeneric crop-image (img &key y1 x1 y2 x2))
(defmethod crop-image ((img image) &key y1 x1 y2 x2)
  (let ((ximg (make-instance (class-of img)
                             :height 0 :width 0))
        (new-rows (1+ (- y2 y1)))
        (new-cols (1+ (- x2 x1))))
    (set-channels
     ximg
     (mapcar #'(lambda (channel)
                 (let ((new-channel (make-instance (class-of channel)
                                                   :rows new-rows :cols new-cols)))
                   (clem:matrix-move-range-2d channel new-channel
                                              y1 y2 x1 x2
                                              0 (1- new-rows) 0 (1- new-cols))))
                   (get-channels img)))
    (setf (image-height ximg) (rows (car (get-channels ximg))))
    (setf (image-width ximg) (cols (car (get-channels ximg))))
    ximg))

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
        (cons (floor (- khalf)) (ceiling khalf))
        (cons (+ (- khalf) 0.5d0) (+ khalf 0.5d0)))))
        

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
    (let ((yscale (/ y oldy))
          (xscale (/ x oldx)))
      (let ((max-or-min
             (if (or (< yscale 1)
                     (< xscale 1))
                 #'min
                 #'max)))
        (when constrain-proportions
          (setf yscale (* (signum yscale)
                          (funcall max-or-min (abs xscale) (abs yscale))))
          (setf xscale (* (signum xscale)
                          (funcall max-or-min (abs xscale) (abs yscale))))))
      (let ((xfrm (make-affine-transformation :x-scale xscale
                                              :y-scale yscale)))
        (let ((n (affine-transform-image
                  img xfrm
                  :interpolation interpolation
                  :u (split-around-zero oldx :integer nil)
                  :v (split-around-zero oldy :integer nil)
                  :x (split-around-zero x :integer nil)
                  :y (split-around-zero y :integer nil))))
          n)))))

(defun gaussian-blur-image (img &key (k 2) (sigma 1) (trim k))
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
                         (clem::mat-trim (clem::%separable-discrete-convolve channel hr hc z1 z2 :norm-v nil)
                                         trim)))
                   (get-channels img)))
          (setf (image-height img) (clem:rows (ch-image::image-r img)))
          (setf (image-width img) (clem:cols (ch-image::image-r img)))))))
  img)

(defparameter *sharpen-kernel-1d*
  (clem:array->double-float-matrix #2A((-.125d0) (1.25d0) (-.125d0))))

(defun sharpen-image-separable (img &key (trim 2))
  (let* ((hr *sharpen-kernel-1d*)
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
                         (clem::mat-trim (clem::%separable-discrete-convolve channel hr hc z1 z2 :norm-v nil)
                                         trim)))
                   (get-channels img)))
          (setf (image-height img) (clem:rows (ch-image::image-r img)))
          (setf (image-width img) (clem:cols (ch-image::image-r img)))))))
  img)

(defun discrete-convolve-image (img kernel)
  (let ((copy (copy-image img)))
    (set-channels
     copy
     (map-channels
      #'(lambda (channel)
          (clem:copy-to-ub8-matrix
           (clem:discrete-convolve
            (clem:copy-to-double-float-matrix channel) kernel)
           :constrain t))
      img))
    copy))

(defparameter *sharpen-kernel*
  (clem:array->double-float-matrix #2A((-.125d0 -.125d0 -.125d0) (-.125d0 2d0 -.125d0) (-.125d0 -.125d0 -.125d0))))

(defun sharpen-image (img)
  (discrete-convolve-image img *sharpen-kernel*))

(defun map-channels (function image)
  (mapcar #'(lambda (channel)
              (apply function (list channel)))
          (get-channels image)))

(defgeneric posterize (image levels))

(defmethod posterize ((img clem:ub8-matrix) levels)
  (let* ((max (clem::maxval (class-of img)))
         (level-inv (truncate (/ max levels)))
         (p (clem::mat-copy img)))
    (clem:mloop (((p (unsigned-byte 8) a)) m n i j)
      (setf (aref a i j)
            (* level-inv (truncate (/ (aref a i j) level-inv)))))
    p))

(defgeneric foreground-pixel-list (img &key background-pixel))

(defmethod foreground-pixel-list ((img image) &key (background-pixel 0))
  (loop for i below (image-height img)
     append
       (loop for j below (image-width img)
          append (unless (equal (get-pixel img i j)
                                background-pixel)
                   (list (cons i j))))))

(defmethod foreground-pixel-list ((img matrix) &key (background-pixel 0))
  (loop for i below (clem:rows img)
     append
       (loop for j below (clem:cols img)
          append (unless (equal (clem:mref img i j)
                                background-pixel)
                   (list (cons i j))))))

(defparameter *preset-default-color-list*
    '((255 0 0 0)
      (255 255 0 128)
      (255 0 128 0)
      (255 192 192 64)
      (255 255 0 0)
      (255 0 128 255)
      (255 255 255 0)
      (255 0 255 0)
      (255 0 0 255)
      (255 128 128 0)
      (255 255 0 255)
      (255 0 128 128)
      (255 128 0 128)
      (255 255 255 255)
      (255 128 0 0)
      (255 0 0 128)
      (255 255 128 0)
      (255 0 255 128)
      (255 128 0 255)
      (255 128 255 0)
      (255 0 255 255)
      (255 64 255 255)
      (255 255 64 255)
      (255 255 255 64)
      (255 64 64 255)
      (255 255 64 64)
      (255 64 255 64)
      (255 64 64 128)
      (255 128 64 64)
      (255 64 128 64)
      (255 64 128 128)
      (255 128 64 128)
      (255 192 64 192)
      (255 64 192 192)))

(defparameter *default-color-map*
  (make-array 256))

(loop for i from 0 for c in *preset-default-color-list*
   do (setf (elt *default-color-map* i) c))

(loop for i from (length *preset-default-color-list*) below 256
   do (setf (elt *default-color-map* i)
            (list 255
                  (random 256)
                  (random 256)
                  (random 256))))

(defgeneric color-map-image (img &key colors class))

(defmethod color-map-image ((img matrix)
                            &key
                            (colors *default-color-map*)
                            (class 'argb-8888-image))
  (let ((cimg (make-instance class
                             :height (rows img)
                             :width (cols img))))
    (dotimes (i (image-height cimg))
      (dotimes (j (image-width cimg))
        (set-pixel cimg i j (elt colors (clem:mref img i j)))))
    cimg))


