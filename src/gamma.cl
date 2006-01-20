;;
;; file: gamma.cl
;; package: ch-image
;; author: cyrus harmon
;;

(in-package :ch-image)

(defun make-integer-gamma-curve (gamma &key (bits 16))
  (let ((curve (make-array `(,(ash 1 bits)) :element-type `(unsigned-byte ,bits)))
        (maxval (1- (ash 1 bits))))
    (let ((maxdouble (float maxval 0d0)))
      (loop for i from 0 to maxval
         do 
           (let ((val (truncate (* (expt (/ i maxdouble) gamma) maxdouble))))
             (setf (aref curve i)
                   (cond ((> 0 val) 0)
                         ((> val maxval) maxval)
                         (t val))))))
    curve))

(defparameter *gamma-045* (make-integer-gamma-curve 0.45d0))

(defgeneric apply-gamma-curve (img gamma-curve))

(defmethod apply-gamma-curve ((img rgb-hhh-image) gamma-curve)
  (let ((img2 (copy-image img)))
    (let ((r (ch-image::image-r img2))
          (g (ch-image::image-g img2))
          (b (ch-image::image-b img2)))
      (let ((ra (clem::matrix-vals r))
            (ga (clem::matrix-vals g))
            (ba (clem::matrix-vals b)))
        (declare (type (simple-array (unsigned-byte 16) (* *)) ra ga ba)
                 (type (simple-array (unsigned-byte 16) (*)) gamma-curve))
        (loop for i from 0 below (ch-image::image-height img2)
           do	   
             (loop for j from 0 below (ch-image::image-width img2)
                do
                  (setf (aref ra i j) (aref gamma-curve (aref ra i j))
                        (aref ga i j) (aref gamma-curve (aref ga i j))
                        (aref ba i j) (aref gamma-curve (aref ba i j)))))))
    img2))

(defmethod apply-gamma-curve ((img argb-hhhh-image) gamma-curve)
  (let ((img2 (copy-image img)))
    (let ((a (ch-image::image-a img2))
          (r (ch-image::image-r img2))
          (g (ch-image::image-g img2))
          (b (ch-image::image-b img2)))
      (let ((aa (clem::matrix-vals a))
            (ra (clem::matrix-vals r))
            (ga (clem::matrix-vals g))
            (ba (clem::matrix-vals b)))
        (declare (type (simple-array (unsigned-byte 16) (* *)) aa ra ga ba))
        (loop for i from 0 below (ch-image::image-height img2)
           do	   
             (loop for j from 0 below (ch-image::image-width img2)
                do
                  (setf (aref aa i j) (aref gamma-curve (aref aa i j))
                        (aref ra i j) (aref gamma-curve (aref ra i j))
                        (aref ga i j) (aref gamma-curve (aref ga i j))
                        (aref ba i j) (aref gamma-curve (aref ba i j)))))))
    img2))

(defmethod apply-gamma-curve ((img rgb-888-image) gamma-curve)
  (let ((img2 (copy-image img)))
    (let ((r (ch-image::image-r img2))
          (g (ch-image::image-g img2))
          (b (ch-image::image-b img2)))
      (let ((ra (clem::matrix-vals r))
            (ga (clem::matrix-vals g))
            (ba (clem::matrix-vals b)))
        (declare (type (simple-array (unsigned-byte 8) (* *)) ra ga ba))
        (loop for i from 0 below (ch-image::image-height img2)
           do	   
             (loop for j from 0 below (ch-image::image-width img2)
                do
                  (setf (aref ra i j) (aref gamma-curve (aref ra i j))
                        (aref ga i j) (aref gamma-curve (aref ga i j))
                        (aref ba i j) (aref gamma-curve (aref ba i j)))))))
    img2))

(defmethod apply-gamma-curve ((img argb-8888-image) gamma-curve)
  (let ((img2 (copy-image img)))
    (let ((a (ch-image::image-a img2))
          (r (ch-image::image-r img2))
          (g (ch-image::image-g img2))
          (b (ch-image::image-b img2)))
      (let ((aa (clem::matrix-vals a))
            (ra (clem::matrix-vals r))
            (ga (clem::matrix-vals g))
            (ba (clem::matrix-vals b)))
        (declare (type (simple-array (unsigned-byte 8) (* *)) aa ra ga ba))
        (loop for i from 0 below (ch-image::image-height img2)
           do	   
             (loop for j from 0 below (ch-image::image-width img2)
                do
                  (setf (aref aa i j) (aref gamma-curve (aref aa i j))
                        (aref ra i j) (aref gamma-curve (aref ra i j))
                        (aref ga i j) (aref gamma-curve (aref ga i j))
                        (aref ba i j) (aref gamma-curve (aref ba i j)))))))
    img2))

(defmethod apply-gamma-curve ((img ub8-matrix-image) gamma-curve)
  (let ((img2 (copy-image img)))
    (let ((v (clem::matrix-vals img2)))
      (declare (type (simple-array (unsigned-byte 8) (* *)) v))
      (loop for i from 0 below (ch-image::image-height img2)
         do	   
           (loop for j from 0 below (ch-image::image-width img2)
              do
                (setf (aref v i j) (aref gamma-curve (aref v i j))))))
    img2))

(defgeneric apply-gamma (img gamma))

(defmethod apply-gamma ((img argb-8888-image) gamma)
  (let ((gamma-curve (make-gamma-curve gamma :bits 8)))
    (apply-gamma-curve img gamma-curve)))

