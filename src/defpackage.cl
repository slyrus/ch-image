
(in-package #:cl-user)

(defpackage #:ch-image (:use #:cl #:asdf #:ch-util #:clem)
  (:export #:image
	   #:image-data
	   #:image-width
	   #:image-height

	   #:map-pixels
	   #:set-pixel
	   #:get-pixel

	   #:rgb-image
	   #:rgb-888-image
	   #:rgb-www-image

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

