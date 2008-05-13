
(defpackage #:ch-image-system (:use #:asdf #:cl))
(in-package #:ch-image-system)

;;;; we need to test for the presence of various packages and only
;;;; load IO routines when the appropriate packages are present

(defmacro safe-load-io-library (lib)
  `(handler-case
       (progn
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
  (safe-load-io-library :cl-jpeg)
  (safe-load-io-library :tiff-ffi)
  (safe-load-io-library :zpng)
  (safe-load-io-library :freetype-ffi))

(defsystem :ch-image
  :name "ch-image"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :description "image representation and processing"
  :depends-on (ch-util clem zpng)
  :components
  ((:static-file "version" :pathname #p"version.lisp-expr")
   (:module
    :src
    :components
    ((:cl-source-file "defpackage")
     (:cl-source-file "ch-image" :depends-on ("defpackage"))
     (:cl-source-file "copy-image"  :depends-on ("defpackage" "ch-image"))
     (:cl-source-file "conversion"  :depends-on ("defpackage" "ch-image"))
     (:cl-source-file "imageops"  :depends-on ("defpackage" "ch-image"))
     (:cl-source-file "morphology"  :depends-on ("defpackage" "ch-image"))
     (:cl-source-file "distance"  :depends-on ("defpackage" "ch-image"))
     (:cl-source-file "shapes"  :depends-on ("defpackage" "ch-image"))
     (:cl-source-file "text"  :depends-on ("defpackage" "ch-image"))
     #+ch-image-has-freetype-ffi
     (:cl-source-file "freetype-text"  :depends-on ("text"))
     (:cl-source-file "gamma"  :depends-on ("defpackage" "ch-image"))))
   (:module
    :io
    :components
    (#+ch-image-has-tiff-ffi
     (:cl-source-file "tiffimage")
     #+ch-image-has-cl-jpeg
     (:cl-source-file "jpegimage")
     #+(and ch-image-has-zpng)
     (:cl-source-file "pngimage")
     (:cl-source-file "imageio"
                      :depends-on (#+ch-image-has-tiff-ffi
                                   "tiffimage"
                                   #+ch-image-has-cl-jpeg
                                   "jpegimage")))
    :depends-on (:src))
   (:static-file "README")
   (:static-file "LICENSE")
   (:static-file "NEWS")
   (:static-file "TODO")
   (:static-file "ChangeLog")))


