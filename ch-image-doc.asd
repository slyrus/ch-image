
(asdf:operate 'asdf:load-op :ch-asdf)

(defpackage #:ch-image-doc-system (:use #:asdf #:ch-asdf #:cl))
(in-package #:ch-image-doc-system)

(defsystem :ch-image-doc
  :name "ch-image-doc"
  :author "Cyrus Harmon" 
  :version "0.1.2+-20060119"
  :licence "BSD"
  :depends-on (com.gigamonkeys.markup ch-asdf-markup ch-bib ch-util ch-image)
  :components
  ((:module
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
     (:static-file "simple" :pathname #p"simple.css")))))

