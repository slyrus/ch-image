
(defpackage #:ch-image-doc-system (:use #:asdf #:cl))
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

;;; global stuff that should probably be moved elsewhere
(defclass generated-source-file (source-file) ())
(defmethod operation-done-p ((o operation) (c generated-source-file))
  (let ((in-files (input-files o c)))
    (if in-files
        (and (every #'probe-file in-files)
             (call-next-method))
        (call-next-method))))

(defclass pdf-file (source-file) ())
(defmethod source-file-type ((c pdf-file) (s module)) "pdf")

(defclass xhtml-file (html-file) ())
(defmethod source-file-type ((c xhtml-file) (s module)) "xhtml")

(defclass bibtex-style-file (source-file) ())
(defmethod source-file-type ((c bibtex-style-file) (s module))
  "bst")

(defclass cl-bibtex-style-file (cl-source-file) ())
(defmethod source-file-type ((c cl-bibtex-style-file) (s module))
  "lbst")

(defmacro with-component-directory ((component) &body body)
  `(ch-util::with-current-directory
       (make-pathname
        :directory (pathname-directory
                    (component-pathname ,component)))
     ,@body))

;;;;
;;;; The following section customizes asdf to work with filenames
;;;; with a .cl extension and to put fasl files in a separate
;;;; directory.
;;;;
;;;; To enable this behvior, use asdf component type
;;;;  :ch-image-doc-cl-source-file
;;;;
(defclass ch-image-doc-cl-source-file (cl-source-file) ())

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))
(defmethod source-file-type ((c ch-image-doc-cl-source-file) (s module)) "cl")

(defmethod asdf::output-files :around ((operation compile-op)
                                       (c ch-image-doc-cl-source-file))
  (list (merge-pathnames *fasl-directory*
                         (compile-file-pathname (component-pathname c)))))

(defclass markup-file (source-file) ())
(defmethod source-file-type ((c markup-file) (s module)) "gmarkup")
(defmethod perform ((operation compile-op) (c markup-file))
  (with-component-directory (c)
    (markup::render :latex (component-pathname c)
                    :filters '(:lisp :markup-metadata :ref)
                    :output-file-type "tex")
    (markup::render :xhtml (component-pathname c)
                    :filters
                    '(:lisp :markup-metadata :html-metadata :ref))))
(defmethod perform ((operation load-op) (c markup-file)))
  
(defparameter *pdflatex-program* "pdflatex")
(defparameter *pdflatex-program-path*
  (let ((found (sb-ext:find-executable-in-search-path
                *pdflatex-program*)))
    (unless found
      (setf found 
            #+darwin "/sw/bin/pdflatex"
            #-darwin "/usr/local/bin/pdflatex"))
    found))

(defclass markup-latex-file (generated-source-file) ())
(defmethod source-file-type ((c markup-latex-file) (s module)) "tex")

(defmethod perform ((operation compile-op) (c markup-latex-file))
  (with-component-directory (c)
    (let ((unix-path (ch-util::unix-name (component-pathname c))))
      (ch-util::run-program *pdflatex-program-path*
                            (list unix-path))
      ;; we have to do this twice to get the references right!
      ;; maybe 3x?
      (ch-util::run-program *pdflatex-program-path*
                            (list unix-path)))))

(defmethod perform ((operation load-op) (c markup-latex-file)))

(defclass markup-pdf-file (pdf-file generated-source-file) ())
(defmethod perform ((operation compile-op) (c markup-pdf-file)))
(defmethod perform ((operation load-op) (c markup-pdf-file))
  (ch-util::app-open (ch-util::unix-name (component-pathname c))))

(defclass markup-xhtml-file (xhtml-file) ())
(defmethod perform ((operation compile-op) (c markup-xhtml-file)))
(defmethod perform ((operation load-op) (c markup-xhtml-file))
  (ch-util::firefox-open (ch-util::unix-name (component-pathname c))))

(defclass markup-bibtex-style-file (bibtex-style-file) ())
(defmethod perform ((operation compile-op) (c markup-bibtex-style-file))
  (let ((unix-path (ch-util::unix-name (component-pathname c))))
    (let ((lbst-file (merge-pathnames (make-pathname :type "lbst")
                                      (component-pathname c))))
      (when (probe-file lbst-file)
        (delete-file lbst-file))
      (bibtex-compiler:compile-bst-file unix-path lbst-file))))
(defmethod perform ((operation load-op) (c markup-bibtex-style-file)))

(defclass markup-cl-bibtex-style-file (cl-bibtex-style-file) ())

(defsystem :ch-image-doc
  :name "ch-image-doc"
  :author "Cyrus Harmon" 
  :version "0.1.2+-20060119"
  :licence "BSD"
  :depends-on (ch-util ch-image ch-imageio com.gigamonkeys.markup)
  :components
  ((:module
    :doc
    :components
    ((:module
      :gmarkup-bibtex-style
      :pathname #P""
      :components
      ((:markup-bibtex-style-file "Science")))
     (:module
      :gmarkup
      :pathname #P""
      :depends-on (:gmarkup-bibtex-style)
      :components
      ((:markup-cl-bibtex-style-file "Science")
       (:markup-file "ch-image")))
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

