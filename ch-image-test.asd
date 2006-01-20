
(defpackage #:ch-image-test-system (:use #:asdf #:cl))
(in-package #:ch-image-test-system)

;;;;
;;;; The following section customizes asdf to work with filenames
;;;; with a .cl extension and to put fasl files in a separate
;;;; directory.
;;;;
;;;; To enable this behvior, use asdf component type
;;;;  :ch-image-test-cl-source-file
;;;;
(defclass ch-image-test-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c ch-image-test-cl-source-file) (s module)) "cl")

(defmethod asdf::output-files :around ((operation compile-op) (c ch-image-test-cl-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))

(defsystem :ch-image-test
  :version "0.1.2+-20060119"
  :depends-on (ch-util ch-image ch-imageio)
  :components
  ((:module
    :test
    :components
    ((:ch-image-test-cl-source-file "defpackage")
     (:ch-image-test-cl-source-file "test-ch-image" :depends-on ("defpackage"))
     (:ch-image-test-cl-source-file "examples" :depends-on ("defpackage"))))))

