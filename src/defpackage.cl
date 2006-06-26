
(in-package #:cl-user)

(defpackage #:ch-image
  (:use #:cl #:ch-util #:clem)
  (:export #:image
	   #:image-data
	   #:image-width
	   #:image-height

           #:copy-image
           
	   #:map-pixels
	   #:set-pixel
	   #:get-pixel

           #:or-pixel
           #:xor-pixel
           #:and-pixel
           
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

           #:get-channels
           #:set-channels
           #:map-channels

           #:image-r
           #:image-g
           #:image-b
           #:image-a
                                            
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

           ;; clipping
           #:clip-region
           #:clip-rect
           #:y1
           #:x1
           #:y2
           #:x2
           
	   ;; imageops.cl
	   #:*masked-pixel*
	   #:mask-image
	   #:flip-image

	   #:argb-image-to-blue-image

           #:affine-transform-image
           #:resize-image
           #:crop-image

           ;; shapes
           #:horiz-line
           #:vert-line
           #:draw-line

           #:draw-circle
           #:fill-circle

           #:draw-rectangle
           #:fill-rectangle
           
           #:draw-triangle
           #:draw-polygon
           
           ;; I/O
           #:read-image-file
           #:write-image-file

           #:resize-images-in-directory

           #:read-tiff-file
	   #:write-tiff-file
	   #:write-argb-image-tiff-file
	   #:tiff-rgba-to-gray-image
	   
	   #:read-jpeg-file
	   #:write-jpeg-file
	   #:jpeg-rgb-to-gray-image
	   #:jpeg-gray-to-gray-image
	   #:jpeg-rgb-to-argb-image

           #:write-png-file

           ;; text rendering stuff
           #:text-context
           #:context-face

           #:glyph

           #:font

           #:make-text-context
           #:set-font
           #:get-glyph
           #:draw-char
           #:draw-string

           ;; morphology stuff
           #:4-neighbors
           #:8-neighbors

           #:label-components
           #:component-internal-boundary
           #:component-external-boundary

           #:component-boundary

           #:distance-transform
           
           ;; conversion
           #:make-matrix-image
           #:bit-matrix->ub8-image

           ;; gamma curves
           #:apply-gamma

           ;;
           #:gaussian-blur-image
           ))

(defpackage #:ch-image-drawing
  (:use #:cl #:ch-image #:ch-util #:clem))

