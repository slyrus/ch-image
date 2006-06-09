;;
;; file: gamma.cl
;; package: ch-image
;; author: cyrus harmon
;;

(in-package :ch-image)

(defun make-gamma-curve-lookup-table (gamma &key (bits 16))
  "Returns an array of length 2^bits of type unsigned-byte of
length bits that contains where the kth element contains the
value (k/2^bits-1)^gamma * 2^bits-1. The resulting curve can be
used by the apply-gamma-curve to apply a gamma curve to an image
using a lookup table of gamma values, rather than computing the
appropriate value for each pixel."
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

(defparameter *gamma-045* (make-gamma-curve-lookup-table 0.45d0))

(defgeneric apply-gamma-curve-lookup-table (img gamma-curve)
  (:documentation "applys a gamma curve (usually created with
  make-gamma-curve-lookup-table to perform a gamma curve
  operation on an image by looking up the values in a lookup
  table, rather than computing them for eacho pixel"))

(defmethod apply-gamma-curve-lookup-table ((img rgb-hhh-image) gamma-curve)
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

(defmethod apply-gamma-curve-lookup-table ((img argb-hhhh-image) gamma-curve)
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

(defmethod apply-gamma-curve-lookup-table ((img rgb-888-image) gamma-curve)
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

(defmethod apply-gamma-curve-lookup-table ((img argb-8888-image) gamma-curve)
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

(defmethod apply-gamma-curve-lookup-table ((img ub8-matrix-image) gamma-curve)
  (let ((img2 (copy-image img)))
    (let ((v (clem::matrix-vals img2)))
      (declare (type (simple-array (unsigned-byte 8) (* *)) v))
      (loop for i from 0 below (ch-image::image-height img2)
         do	   
           (loop for j from 0 below (ch-image::image-width img2)
              do
                (setf (aref v i j) (aref gamma-curve (aref v i j))))))
    img2))

(defgeneric apply-gamma (img gamma)
  (:documentation "Returns a copy of img to which a gamma curve
  of exponent gamma has been applied."))

(defmethod apply-gamma ((img argb-8888-image) gamma)
  (let ((gamma-curve (make-gamma-curve-lookup-table gamma :bits 8)))
    (apply-gamma-curve-lookup-table img gamma-curve)))

(defmethod apply-gamma ((img rgb-888-image) gamma)
  (let ((gamma-curve (make-gamma-curve-lookup-table gamma :bits 8)))
    (apply-gamma-curve-lookup-table img gamma-curve)))

(defmethod apply-gamma ((img argb-hhhh-image) gamma)
  (let ((gamma-curve (make-gamma-curve-lookup-table gamma :bits 16)))
    (apply-gamma-curve-lookup-table img gamma-curve)))

(defmethod apply-gamma ((img rgb-hhh-image) gamma)
  (let ((gamma-curve (make-gamma-curve-lookup-table gamma :bits 16)))
    (apply-gamma-curve-lookup-table img gamma-curve)))

