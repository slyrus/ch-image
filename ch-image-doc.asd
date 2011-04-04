
(asdf:operate 'asdf:load-op :asdf-objects)
(asdf:operate 'asdf:load-op :smarkup)

(defpackage #:ch-image-doc-system (:use #:cl #:asdf #:asdf-objects #:smarkup))
(in-package #:ch-image-doc-system)

(defclass ch-image-mixin () ())

(defmethod perform ((op compile-op) (c ch-image-mixin))
  (let ((*default-pathname-defaults*
         (component-pathname (component-parent c))))
    (call-next-method)))

(defmethod perform ((op load-op) (c ch-image-mixin))
  (let ((*default-pathname-defaults*
         (component-pathname (component-parent c))))
    (call-next-method)))

(defmethod perform ((op asdf-objects:generate-op) (c ch-image-mixin))
  (let ((*default-pathname-defaults*
         (component-pathname (component-parent c))))
    (call-next-method)))

(defclass my-filtered-object (ch-image-mixin smarkup:filtered-object) ())

(defclass my-object-latex-file (ch-image-mixin smarkup:object-latex-file) ())

(defclass my-object-cl-pdf-file (ch-image-mixin smarkup:object-cl-pdf-file) ())

(defsystem :ch-image-doc
  :name "ch-image-doc"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.4.3"
  :licence "BSD"
  :depends-on (asdf-objects ch-image smarkup)
  :components
  ((:static-file "make-tinaa-docs" :pathname #p"make-tinaa-docs.lisp")
   (:module
    :doc
    :components
    ((:smarkup-object-from-file :ch-image-sexp :pathname #p"ch-image.sexp")
     (:my-filtered-object :ch-image-filtered-sexp
                          :filters (:lisp :smarkup-metadata :ref)
                          :depends-on (:ch-image-sexp)
                          :input-object :ch-image-sexp)
     
     (:filtered-object :ch-image-pdf-filtered-sexp
                       :filters (:html-metadata)
                       :depends-on (:ch-image-filtered-sexp)
                       :input-object :ch-image-filtered-sexp)
     (:my-object-cl-pdf-file :ch-image-pdf
                             :pathname #p"ch-image.pdf"
                             :depends-on (:ch-image-pdf-filtered-sexp)
                             :input-object :ch-image-pdf-filtered-sexp)

     (:my-filtered-object :ch-image-html-filtered-sexp
                          :filters (:html-metadata)
                          :depends-on (:ch-image-filtered-sexp)
                          :input-object :ch-image-filtered-sexp)
     (:object-xhtml-file :ch-image-xhtml
                         :pathname #p"ch-image.xhtml"
                         :depends-on (:ch-image-html-filtered-sexp)
                         :input-object :ch-image-html-filtered-sexp)
     
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
     (:static-file "ch-image-bib" :pathname #p"ch-image.bib")
     (:module :tinaa)))))

