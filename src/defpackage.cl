
(in-package #:cl-user)

(defpackage #:image (:use #:cl #:asdf #:util #:matrix)
  (:export #:image
	   #:image-data
	   #:image-width
	   #:image-height

	   #:map-pixels
	   #:set-pixel
	   #:get-pixel

	   #:argb-image
	   #:argb-8888-image
	   #:get-argb-values
	   #:set-argb-values
	   #:gray-image
	   #:matrix-gray-image
	   #:get-gray-value
	   #:set-gray-value
	   #:set-image-data
	   
	   #:argb-image-to-gray-image
	   
	   ;;; imageops.cl
	   #:*masked-pixel*
	   #:mask-image
	   #:flip-image

	   #:argb-image-to-blue-image

	   ))

