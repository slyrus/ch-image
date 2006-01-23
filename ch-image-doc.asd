
(asdf:operate 'asdf:load-op :ch-asdf)
(asdf:operate 'asdf:load-op :ch-asdf-markup)

(defpackage #:ch-image-doc-system (:use #:asdf #:ch-asdf #:ch-asdf-markup #:cl))
(in-package #:ch-image-doc-system)

;; we need functions in gigamonkey-markup and ch-util to build this
;; system, so we have to explicitly load them here. The depends-on
;; stuff implies that the code contained in the asdf system depends on
;; those other systems, not that the system has to be loaded before
;; the build operations can be compiled.
;;
;; So load them here:
(asdf:operate 'asdf:load-op 'com.gigamonkeys.markup)
(asdf:operate 'asdf:load-op 'ch-util)

(defsystem :ch-image-doc
  :name "ch-image-doc"
  :author "Cyrus Harmon" 
  :version "0.1.2+-20060119"
  :licence "BSD"
  :depends-on (ch-asdf ch-bib ch-util ch-image ch-imageio puri
               com.gigamonkeys.markup)
  :components
  ((:module
    :doc
    :components
    ((:module
      :gmarkup
      :pathname #P""
      :components
      ((:markup-file "ch-image")))
     (:module
      :latex
      :pathname #P""
      :depends-on (gmarkup)
      :components
      ((:markup-latex-file "ch-image")))
     (:module
      :pdf
      :pathname #P""
      :depends-on (gmarkup latex)
      :components
      ((:markup-pdf-file "ch-image")))
     (:module
      :xhtml
      :pathname #P""
      :depends-on (gmarkup)
      :components
      ((:markup-xhtml-file "ch-image")))
     ))))


;;; NOTES!!!  Ok, we're getting somewhere here. Now we should add pdf
;;; and xhtml files to the top-level component. We can move the
;;; app-open stuff down into the load methods for these and we should
;;; be in good shape...

