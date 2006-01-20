

(defun make-latex ()
  (markup::render :latex "test.foo"
                  :filters '(:test :lisp :markup-metadata :ref)
                  :output-file-type "tex"))

(defun make-html ()
  (markup::render :xhtml "test.foo"
                  :filters '(:test :lisp :markup-metadata :html-metadata :ref)))

(defun make-and-show-html ()
  (let ((html-file (make-html)))
    (ch-util::firefox-open (ch-util::unix-name html-file))))

(defparameter *pdflatex-program*
  #+darwin "/sw/bin/pdflatex"
  #-darwin "/usr/local/bin/pdflatex")

(defun make-latex-pdf ()
  (let ((latex-file (make-latex)))
    (ch-util::run-program *pdflatex-program* (list (ch-util::unix-name latex-file)))
    ;;; we have to do this twice to get the references right! - maybe 3x?
    (ch-util::run-program *pdflatex-program* (list (ch-util::unix-name latex-file)))
    (let ((pdf-file (merge-pathnames (make-pathname :type "pdf") latex-file)))
      (ch-util::app-open (ch-util::unix-name pdf-file)))))


;;; bibtex stuff

(defparameter *science-lbst-file* "science.lbst"))

(defun compile-bibtex-file ()
  (when (probe-file *science-lbst-file*)
    (delete-file *science-lbst-file*))
  (bibtex-compiler:compile-bst-file "Science.bst" *science-lbst-file*))

(defun load-bibtex-style ()
  (load *science-lbst-file*))

(defun test-bbl-output ()
  (let ((style-function (bibtex-compiler:find-bibtex-style "science"))
        (bibtex-runtime:*cite-keys* markup::*cite-keys*)
        (bibtex-runtime:*bib-macros* (make-hash-table :test #'equalp))
        (bibtex-runtime:*bib-database* (make-hash-table :test #'equalp))
        (bibtex-runtime:*bib-files* '("fruitfly")))
    (funcall style-function)))

(defparameter *bibtex-style* "science")

(defun get-fdefinition (name package)
  (fdefinition (intern name package)))

(defun no-op ()
  )

(defun delimiter ()

(defun test-bbl-output-2 ()
  (let ((style-function (bibtex-compiler:find-bibtex-style *bibtex-style*))
        (bibtex-runtime:*cite-keys* markup::*cite-keys*)
        (bibtex-runtime:*bib-macros* (make-hash-table :test #'equalp))
        (bibtex-runtime:*bib-database* (make-hash-table :test #'equalp))
        (bibtex-runtime:*bib-files* '("fruitfly")))
    (let ((package
           (find-package
            (string-upcase
             (concatenate 'string "bibtex-style-" *bibtex-style*)))))
      (let ((old-begin-bib (get-fdefinition "BEGIN-BIB" package))
            (old-end-bib (get-fdefinition "END-BIB" package))
            (old-output-bibitem (get-fdefinition "OUTPUT-BIBITEM" package)))
        (setf (fdefinition (intern "BEGIN-BIB" package)) #'no-op)
        (setf (fdefinition (intern "END-BIB" package)) #'no-op)
        (setf (fdefinition (intern "OUTPUT-BIBITEM" package)) #'no-op)
        (funcall style-function)
        (setf (fdefinition (intern "BEGIN-BIB" package)) old-begin-bib)
        (setf (fdefinition (intern "END-BIB" package)) old-end-bib)
        (setf (fdefinition (intern "OUTPUT-BIBITEM" package)) old-output-bibitem)))))

