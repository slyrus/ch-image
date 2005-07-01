
(defpackage #:ch-image-system (:use #:asdf #:cl))
(in-package #:ch-image-system)

;;;;
;;;; The following section customizes asdf to work with filenames
;;;; with a .cl extension and to put fasl files in a separate
;;;; directory.
;;;;
;;;; To enable this behvior, use asdf component type
;;;;  :ch-image-cl-source-file
;;;;
(defclass ch-image-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c ch-image-cl-source-file) (s module)) "cl")

(defmethod asdf::output-files :around ((operation compile-op) (c ch-image-cl-source-file))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))


(defvar *registry-directories*
  (list (make-pathname :directory "/usr/local/share/lisp")
        (make-pathname :directory "/bobo/share/lisp")
	(merge-pathnames
	 (make-pathname :directory (list :relative :up))
	 (make-pathname :directory (pathname-directory *load-truename*)))))

(defun add-registry-path (path)
  (dolist (dir *registry-directories*)
    (let ((p (merge-pathnames
              (make-pathname :directory (cons :relative (if (not (listp path)) (list path) path)))
              dir)))
      (when (probe-file p)
        (pushnew p asdf:*central-registry* :test 'equal)
        (return-from add-registry-path p)))))

(mapcar #'(lambda (x) (add-registry-path x))
	'("ch-util" "clem"))

(defsystem :ch-image
  :name "ch-image"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :version "0.1.0-20050701"
  :description "image representation and processing"
  :depends-on (ch-util clem)
  :components
  ((:module
    :src
    :components
    ((:ch-image-cl-source-file "defpackage")
     (:ch-image-cl-source-file "ch-image" :depends-on ("defpackage"))
     (:ch-image-cl-source-file "imageops"  :depends-on ("defpackage" "ch-image"))))
   (:static-file "Makefile")
   (:static-file "README")
   (:static-file "LICENSE")
   (:static-file "bootstrap" :pathname #p"bootstrap.cl")))

