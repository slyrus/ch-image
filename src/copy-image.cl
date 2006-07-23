;;
;; file: copy-image.cl
;; package: ch-image
;; author: cyrus harmon
;;

(in-package :ch-image)

(defgeneric copy-image (img))

(defmethod copy-image ((img1 image-channel))
  (let ((img2 (make-instance (class-of img1)
			     :height (image-height img1)
			     :width (image-width img1))))
    (let ((src (clem::matrix-vals img1))
          (dest (clem::matrix-vals img2)))
      (loop for i from 0 below (image-height img1)
	 do
         (loop for j from 0 below (image-width img1)
            do 
            (setf (aref dest i j) (aref src i j)))))
    img2))

(defmethod copy-image ((img1 rgb-image))
  (let ((img2 (make-instance (class-of img1)
			     :height (image-height img1)
			     :width (image-width img1))))
    (let ((r1 (clem::matrix-vals (image-r img1)))
	  (g1 (clem::matrix-vals (image-g img1)))
	  (b1 (clem::matrix-vals (image-b img1)))
	  (r2 (clem::matrix-vals (image-r img2)))
	  (g2 (clem::matrix-vals (image-g img2)))
	  (b2 (clem::matrix-vals (image-b img2))))
      (loop for i from 0 below (image-height img1)
	 do
         (loop for j from 0 below (image-width img1)
            do 
            (setf (aref r2 i j) (aref r1 i j)
                  (aref g2 i j) (aref g1 i j)
                  (aref b2 i j) (aref b1 i j)))))
    img2))

(defmethod copy-image ((img1 rgb-888-image))
  (let ((img2 (make-instance 'rgb-888-image
			     :height (image-height img1)
			     :width (image-width img1))))
    (let ((r1 (clem::matrix-vals (image-r img1)))
	  (g1 (clem::matrix-vals (image-g img1)))
	  (b1 (clem::matrix-vals (image-b img1)))
	  (r2 (clem::matrix-vals (image-r img2)))
	  (g2 (clem::matrix-vals (image-g img2)))
	  (b2 (clem::matrix-vals (image-b img2))))
      (declare (type (simple-array (unsigned-byte 8) (* *)) r1)
	       (type (simple-array (unsigned-byte 8) (* *)) g1)
	       (type (simple-array (unsigned-byte 8) (* *)) b1)
	       (type (simple-array (unsigned-byte 8) (* *)) r2)
	       (type (simple-array (unsigned-byte 8) (* *)) g2)
	       (type (simple-array (unsigned-byte 8) (* *)) b2))
      (loop for i from 0 below (image-height img1)
	 do
	   (loop for j from 0 below (image-width img1)
	      do 
		(setf (aref r2 i j) (aref r1 i j)
		      (aref g2 i j) (aref g1 i j)
		      (aref b2 i j) (aref b1 i j)))))
    img2))

(defmethod copy-image ((img1 rgb-hhh-image))
  (let ((img2 (make-instance 'rgb-hhh-image
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

(defmethod copy-image ((img1 argb-image))
  (let ((img2 (make-instance (class-of img1)
			     :height (image-height img1)
			     :width (image-width img1))))
    (let ((a1 (clem::matrix-vals (image-a img1)))
	  (r1 (clem::matrix-vals (image-r img1)))
	  (g1 (clem::matrix-vals (image-g img1)))
	  (b1 (clem::matrix-vals (image-b img1)))
	  (a2 (clem::matrix-vals (image-a img2)))
	  (r2 (clem::matrix-vals (image-r img2)))
	  (g2 (clem::matrix-vals (image-g img2)))
	  (b2 (clem::matrix-vals (image-b img2))))
      (loop for i from 0 below (image-height img1)
	 do
	   (loop for j from 0 below (image-width img1)
	      do 
		(setf (aref a2 i j) (aref a1 i j)
		      (aref r2 i j) (aref r1 i j)
		      (aref g2 i j) (aref g1 i j)
		      (aref b2 i j) (aref b1 i j)))))
    img2))

(defmethod copy-image ((img1 argb-8888-image))
  (let ((img2 (make-instance (class-of img1)
			     :height (image-height img1)
			     :width (image-width img1))))
    (let ((a1 (clem::matrix-vals (image-a img1)))
	  (r1 (clem::matrix-vals (image-r img1)))
	  (g1 (clem::matrix-vals (image-g img1)))
	  (b1 (clem::matrix-vals (image-b img1)))
	  (a2 (clem::matrix-vals (image-a img2)))
	  (r2 (clem::matrix-vals (image-r img2)))
	  (g2 (clem::matrix-vals (image-g img2)))
	  (b2 (clem::matrix-vals (image-b img2))))
      (declare (type (simple-array (unsigned-byte 8) (* *)) a1)
	       (type (simple-array (unsigned-byte 8) (* *)) r1)
	       (type (simple-array (unsigned-byte 8) (* *)) g1)
	       (type (simple-array (unsigned-byte 8) (* *)) b1)
	       (type (simple-array (unsigned-byte 8) (* *)) a2)
	       (type (simple-array (unsigned-byte 8) (* *)) r2)
	       (type (simple-array (unsigned-byte 8) (* *)) g2)
	       (type (simple-array (unsigned-byte 8) (* *)) b2))
      (loop for i from 0 below (image-height img1)
	 do
	   (loop for j from 0 below (image-width img1)
	      do 
		(setf (aref a2 i j) (aref a1 i j)
		      (aref r2 i j) (aref r1 i j)
		      (aref g2 i j) (aref g1 i j)
		      (aref b2 i j) (aref b1 i j)))))
    img2))

(defmethod copy-image ((img1 argb-hhhh-image))
  (let ((img2 (make-instance (class-of img1)
			     :height (image-height img1)
			     :width (image-width img1))))
    (let ((a1 (clem::matrix-vals (image-a img1)))
	  (r1 (clem::matrix-vals (image-r img1)))
	  (g1 (clem::matrix-vals (image-g img1)))
	  (b1 (clem::matrix-vals (image-b img1)))
	  (a2 (clem::matrix-vals (image-a img2)))
	  (r2 (clem::matrix-vals (image-r img2)))
	  (g2 (clem::matrix-vals (image-g img2)))
	  (b2 (clem::matrix-vals (image-b img2))))
      (declare (type (simple-array (unsigned-byte 16) (* *)) a1)
	       (type (simple-array (unsigned-byte 16) (* *)) r1)
	       (type (simple-array (unsigned-byte 16) (* *)) g1)
	       (type (simple-array (unsigned-byte 16) (* *)) b1)
	       (type (simple-array (unsigned-byte 16) (* *)) a2)
	       (type (simple-array (unsigned-byte 16) (* *)) r2)
	       (type (simple-array (unsigned-byte 16) (* *)) g2)
	       (type (simple-array (unsigned-byte 16) (* *)) b2))
      (loop for i from 0 below (image-height img1)
	 do
	   (loop for j from 0 below (image-width img1)
	      do 
		(setf (aref a2 i j) (aref a1 i j)
		      (aref r2 i j) (aref r1 i j)
		      (aref g2 i j) (aref g1 i j)
		      (aref b2 i j) (aref b1 i j)))))
    img2))

(defgeneric copy-pixels (src
                         dest
                         src-ystart src-yend src-xstart src-xend
                         dest-ystart dest-yend dest-xstart dest-xend))

;; FIXME! Specialized versions of this will go much faster!
(defmethod copy-pixels ((src image)
                        (dest image)
                        src-ystart src-yend src-xstart src-xend
                        dest-ystart dest-yend dest-xstart dest-xend)
  ;; FIXME add bounds checking here!
  (loop for srcy from src-ystart to src-yend
     for desty from dest-ystart to dest-yend
     do (loop for srcx from src-xstart to src-xend
           for destx from dest-xstart to dest-xend
           do (set-pixel dest desty destx
                         (get-pixel src srcy srcx)))))

