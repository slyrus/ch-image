
(defpackage #:image-system (:use #:asdf #:cl))
(in-package #:image-system)

;;; begin asdf fasl and filename hackery
(defclass image-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative
			      #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c image-cl-source-file) (s module)) "cl")

(defmethod asdf::output-fasl-files ((operation compile-op) (c image-cl-source-file)
				    (s module))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))

(defmethod asdf::output-files :around ((operation compile-op) (c image-cl-source-file))
  (let ((m (compute-applicable-methods #'asdf::output-fasl-files (list operation c (component-system c)))))
    (if m
	(asdf::output-fasl-files operation c (component-system c))
	(call-next-method operation c))))
;;; end asdf hackery

(let* ((parentdir
	(truename (make-pathname :directory
				 (append (pathname-directory *load-truename*)
					 (list :up))))))
  (defun add-registry-paths (paths)
    (and paths (pushnew (merge-pathnames (make-pathname :directory (list :relative (car paths)))
					 parentdir)
			asdf:*central-registry* :test 'equal)
	 (add-registry-paths (cdr paths)))))

(add-registry-paths '("util" "clem"))

(defsystem :image
  :version "20040704.1"
  :depends-on (util clem)
  :components
  ((:module :src
	    :components
	    ((:image-cl-source-file "defpackage")
	     (:image-cl-source-file "image")
	     (:image-cl-source-file "imageops"))
	    :serial t)))
