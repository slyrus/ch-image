
(asdf:defsystem :ch-image
  :name "ch-image"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :version "0.4.10"
  :description "image representation and processing"
  :depends-on (clem retrospectiff zpng png-read cl-jpeg)
  :components
  ((:module
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
     (:cl-source-file "gamma"  :depends-on ("defpackage" "ch-image"))))
   (:module
    :io
    :components
    ((:cl-source-file "tiffimage")
     (:cl-source-file "jpegimage")
     (:cl-source-file "pngimage")
     (:cl-source-file "imageio"
                      :depends-on ("tiffimage"
                                   "jpegimage"
                                   "pngimage")))
    :depends-on (:src))
   (:static-file "README")
   (:static-file "LICENSE")
   (:static-file "NEWS")
   (:static-file "TODO")
   (:static-file "ChangeLog")))

