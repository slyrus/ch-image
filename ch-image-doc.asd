
(asdf:operate 'asdf:load-op :ch-asdf)

(defpackage #:ch-image-doc-system (:use #:asdf #:ch-asdf #:cl))
(in-package #:ch-image-doc-system)

(defsystem :ch-image-doc
  :name "ch-image-doc"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :depends-on (com.gigamonkeys.markup ch-asdf-markup ch-bib ch-util ch-image)
  :components
  ((:static-file "make-tinaa-docs" :pathname #p"make-tinaa-docs.lisp")
   (:module
    :doc
    :components
    ((:module :gmarkup :pathname #P""
              :components ((:markup-file "ch-image")))
     (:module :latex :pathname #P""
              :depends-on (gmarkup)
              :components ((:markup-latex-file "ch-image")))
     (:module :pdf :pathname #P""
              :depends-on (gmarkup latex)
              :components ((:markup-pdf-file "ch-image")))
     (:module :xhtml :pathname #P""
              :depends-on (gmarkup)
              :components ((:markup-xhtml-file "ch-image")))
     (:module :images
              :components ((:jpeg-file "sanfran")
                           (:jpeg-file "salad")))
     (:module :output-images
              :components ((:png-file "circles")
                           (:png-file "example1")
                           (:jpeg-file "salad-big")
                           (:jpeg-file "salad-cropped")
                           (:jpeg-file "salad-trans")
                           (:jpeg-file "salad-trans2")
                           (:jpeg-file "salad-trans3")
                           (:jpeg-file "sanfran-lighter")
                           (:png-file "sanfran")
                           ))
     (:static-file "simple" :pathname #p"simple.css")
     (:tinaa-directory :tinaa)))))

