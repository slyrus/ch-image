
(defpackage #:image-system (:use #:asdf #:cl))
(in-package #:image-system)

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
	    ((:file "defpackage")
	     (:file "image")
	     (:file "imageops")
	     )
	    :serial t
	    ))
  )

(defparameter *this-system* :image)

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

