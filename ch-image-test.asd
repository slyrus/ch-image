
(defpackage #:image-test-system (:use #:asdf #:cl))
(in-package #:image-test-system)

;;; begin asdf fasl and filename hackery
(defclass image-test-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative
			      #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c image-test-cl-source-file) (s module)) "cl")

(defmethod asdf::output-fasl-files ((operation compile-op) (c image-test-cl-source-file)
				    (s module))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))

(defmethod asdf::output-files :around ((operation compile-op) (c image-test-cl-source-file))
  (let ((m (compute-applicable-methods #'asdf::output-fasl-files (list operation c (component-system c)))))
    (if m
	(asdf::output-fasl-files operation c (component-system c))
	(call-next-method operation c))))
;;; end asdf hackery

(defsystem :image-test
  :version "20040618.1"
  :depends-on (util image)
  :components
  ((:module :test
	    :components
	    ((:file "defpackage")
	     (:file "test-image"))
	    :serial t)))
