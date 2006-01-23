
(in-package #:cl-user)

(defpackage #:ch-image (:use #:cl #:asdf #:ch-util #:clem)
  (:export #:image
	   #:image-data
	   #:image-width
	   #:image-height

           #:get-channels

	   #:map-pixels
	   #:set-pixel
	   #:get-pixel

	   #:rgb-image
	   #:rgb-888-image
	   #:rgb-hhh-image

	   #:argb-image
	   #:argb-8888-image
	   #:get-argb-values
	   #:set-argb-values
	   #:gray-image
	   #:matrix-gray-image
	   #:get-gray-value
	   #:set-gray-value
	   #:set-image-data


	   #:bit-matrix-image

	   #:ub8-matrix-image
	   #:ub16-matrix-image
	   #:ub32-matrix-image

	   #:sb8-matrix-image
	   #:sb16-matrix-image
	   #:sb32-matrix-image

	   #:single-float-matrix-image
	   #:double-float-matrix-image

	   #:complex-matrix-image

	   #:argb-image-to-gray-image
	   
	   ;;; imageops.cl
	   #:*masked-pixel*
	   #:mask-image
	   #:flip-image

	   #:argb-image-to-blue-image

           #:affine-transform-image
           #:resize-image
           #:crop-image

           ;;; shapes
           #:horiz-line
           #:vert-line
           #:draw-line

           #:draw-circle
           #:fill-circle

           #:draw-rectangle
           #:fill-rectangle
           
           #:draw-triangle
           #:draw-polygon
	   ))

