;;
;; file: conversion.cl
;; package: ch-image
;; author: cyrus harmon
;;

(in-package :ch-image)

(declaim (ftype (function (fixnum fixnum fixnum) fixnum)
                rgb-to-gray-pixel)
         (inline rgb-to-gray-pixel))

(defun rgb-to-gray-pixel (r g b)
  (declare (dynamic-extent r g b) (fixnum r g b))
  (floor (/ (+ r g b) 3.0d0)))

(defmethod argb-image-to-gray-image ((src argb-image))
  (declare (optimize (speed 3)))
  (let ((dest (make-instance 'ub8-matrix-image :width (image-width src) :height (image-height src))))
    (map-pixels #'(lambda (img row col)
		    (declare (dynamic-extent row col) (fixnum row col))
		    (multiple-value-bind (a r g b) (get-argb-values img row col)
		      (declare (dynamic-extent a r g b) (fixnum a r g b))
		      (declare (ignore a))
		      (set-pixel dest row col (rgb-to-gray-pixel r g b))))
		src)
    dest))

(defmethod argb-8888-image-to-argb-ffff-image ((src argb-8888-image))
  (declare (optimize (speed 3)))
  (let ((dest (make-instance 'argb-ffff-image
                             :height (image-height src)
                             :width (image-width src))))
    (map-pixels #'(lambda (img row col)
		    (declare (dynamic-extent row col) (fixnum row col))
		    (multiple-value-bind (a r g b) (get-argb-values img row col)
		      (declare (dynamic-extent a r g b)
                               (type (unsigned-byte 8) a r g b))
		      (set-pixel dest row col a r g b)))
		src)
    dest))

(defmethod argb-8888-image-to-argb-hhhh-image ((src argb-8888-image))
  (declare (optimize (speed 3)))
  (let ((dest (make-instance 'argb-hhhh-image
                             :height (image-height src)
                             :width (image-width src))))
    (map-pixels #'(lambda (img row col)
		    (declare (dynamic-extent row col) (fixnum row col))
		    (multiple-value-bind (a r g b) (get-argb-values img row col)
		      (declare (dynamic-extent a r g b)
                               (type (unsigned-byte 8) a r g b))
		      (set-pixel dest row col
                                 (* 256 a)
                                 (* 256 r)
                                 (* 256 g)
                                 (* 256 b))))
                src)
    dest))

(defmethod argb-hhhh-image-to-argb-8888-image ((src argb-hhhh-image))
  (declare (optimize (speed 3)))
  (let ((dest (make-instance 'argb-8888-image
                             :height (image-height src)
                             :width (image-width src))))
    (map-pixels #'(lambda (img row col)
		    (declare (dynamic-extent row col) (fixnum row col))
		    (multiple-value-bind (a r g b) (get-argb-values img row col)
		      (declare (dynamic-extent a r g b)
                               (type (unsigned-byte 16) a r g b))
		      (set-pixel dest row col
                                 (truncate a 256)
                                 (truncate r 256)
                                 (truncate g 256)
                                 (truncate b 256))))
                src)
    dest))

(defun make-matrix-image (m &key (matrix-type 'ch-image::ub8-matrix-image))
  (let ((img (make-instance matrix-type
                            :rows (clem:rows m)
                            :cols (clem:cols m))))
      (clem::matrix-move m img)
      img))

(defun make-norm-ub8-image (matrix)
  (make-matrix-image
   (clem:norm-0-255 (clem:copy-to-double-float-matrix matrix))))

(defun bit-matrix->ub8-image (m)
  (make-matrix-image
   (clem:norm-0-255
    (clem:copy-to-ub8-matrix m))))
  
