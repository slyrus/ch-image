
(defpackage #:image-test-system (:use #:asdf #:cl))
(in-package #:image-test-system)

(defsystem :image-test
  :version "20040618.1"
  :depends-on (util image)
  :components
  ((:module :test
	    :components
	    ((:file "defpackage")
	     (:file "test-image")
	     )
	    :serial t
	    )))

(defparameter *this-system* :image-test)

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative
			      #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c cl-source-file) (s (eql (find-system *this-system*))))
  "cl")

(defmethod asdf::output-fasl-files ((operation compile-op) (c cl-source-file)
				    (s (eql (find-system *this-system*))))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))
