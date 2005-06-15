
(defpackage #:image-test-system (:use #:asdf #:cl))
(in-package #:image-test-system)

;;;;
;;;; The following section customizes asdf to work with filenames
;;;; with a .cl extension and to put fasl files in a separate
;;;; directory.
;;;;
;;;; To enable this behvior, use asdf component type
;;;;  :image-test-cl-source-file
;;;;
(defclass image-test-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c image-test-cl-source-file) (s module)) "cl")

(defmethod asdf::output-files :around ((operation compile-op) (c image-test-cl-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))


(defsystem :image-test
  :version "20040618.1"
  :depends-on (util image)
  :components
  ((:module :test
	    :components
	    ((:file "defpackage")
	     (:file "test-image"))
	    :serial t)))
