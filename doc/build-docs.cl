
(require 'asdf)

(asdf:operate 'asdf:load-op 'com.gigamonkeys.markup)
(asdf:operate 'asdf:load-op 'ch-image)
(asdf:operate 'asdf:load-op 'ch-imageio)

(setf markup::*baseline-skip* "12pt")
(setf markup::*par-skip* "0pt")

(defparameter *document* "ch-image.gmarkup")

(defun make-latex ()
  (markup::render :latex *document*
                  :filters '(:lisp :markup-metadata :ref)
                  :output-file-type "tex"))

(defun make-html ()
  (markup::render :xhtml "ch-image.gmarkup"
                  :filters '(:lisp :markup-metadata :html-metadata :ref)))

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
      (print pdf-file)
      (ch-util::app-open (ch-util::unix-name pdf-file)))))


(defparameter *bibtex-style* "science")
(defparameter *science-lbst-file* "science.lbst")

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
        (bibtex-runtime:*bib-files* '("lisp")))
    (funcall style-function)))
