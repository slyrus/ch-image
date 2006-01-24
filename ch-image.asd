
(defpackage #:ch-image-system (:use #:asdf #:cl))
(in-package #:ch-image-system)

;;;; we need to test for the presence of various packages and only
;;;; load IO routines when the appropriate packages are present

(defmacro test-io-library (lib)
  `(handler-case
       (progn
         (print (cons 'loading ,lib))
         (asdf:operate 'asdf:load-op ,lib)
         (pushnew (intern 
                   (concatenate 'string
                                (string-upcase "ch-image-has-")
                                (symbol-name ,lib))
                   :keyword)
                  *features*))
     (missing-component (x)
       (declare (ignore x))
       (format *error-output* "~%ch-image: ~A disabled~&" ,lib))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (test-io-library :jpeg)
  (test-io-library :tiff-ffi)
  (test-io-library :salza)
  (test-io-library :salza-png))

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


(defsystem :ch-image
  :name "ch-image"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :version "0.1.2-20050724"
  :description "image representation and processing"
  :depends-on (ch-util clem)
  :components
  ((:module
    :src
    :components
    ((:ch-image-cl-source-file "defpackage")
     (:ch-image-cl-source-file "ch-image" :depends-on ("defpackage"))
     (:ch-image-cl-source-file "copy-image"  :depends-on ("defpackage" "ch-image"))
     (:ch-image-cl-source-file "conversion"  :depends-on ("defpackage" "ch-image"))
     (:ch-image-cl-source-file "imageops"  :depends-on ("defpackage" "ch-image"))
     (:ch-image-cl-source-file "shapes"  :depends-on ("defpackage" "ch-image"))
     (:ch-image-cl-source-file "gamma"  :depends-on ("defpackage" "ch-image"))))
   (:module
    :io
    :components
    (#+ch-image-has-tiff-ffi
     (:ch-image-cl-source-file "tiffimage")
     #+ch-image-has-jpeg
     (:ch-image-cl-source-file "jpegimage")
     #+(and ch-image-has-salza ch-image-has-salza-png)
     (:ch-image-cl-source-file "pngimage")
     (:ch-image-cl-source-file "imageio"
                               :depends-on (#+ch-image-has-tiff-ffi
                                            "tiffimage"
                                            #+ch-image-has-jpeg
                                            "jpegimage"))))
   (:static-file "README")
   (:static-file "LICENSE")
   (:static-file "bootstrap" :pathname #p"bootstrap.cl")))

