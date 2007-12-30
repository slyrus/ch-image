
(asdf:operate 'asdf:load-op :ch-asdf)

(defpackage #:ch-image-test-system (:use #:asdf #:ch-asdf #:cl))
(in-package #:ch-image-test-system)

(asdf:defsystem :ch-image-test
  :name "ch-image-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :depends-on (ch-util ch-image ch-image)
  :components
  ((:module
    :test
    :components
    ((:cl-source-file "defpackage")
     (:cl-source-file "test-ch-image" :depends-on ("defpackage"))
     (:cl-source-file "test-ch-image-2" :depends-on ("defpackage"))
     (:cl-source-file "test-ch-image-3" :depends-on ("defpackage"))
     (:cl-source-file "examples" :depends-on ("defpackage"))
     (:module
      :images
      :components
      ((:tiff-file "euc-tiff" :pathname #p"euc.tiff")
       (:tiff-file "eucgray-tiff" :pathname #p"eucgray.tiff")
       (:jpeg-file "euc-jpeg" :pathname #p"euc.jpeg")
       (:jpeg-file "eucgray-jpeg" :pathname #p"eucgray.jpeg")
       (:tiff-file "sunset-lzw" :pathname #p"sunset-lzw.tiff")
       (:jpeg-file "sanfran" :pathname #p"sanfran.jpg")))))))

