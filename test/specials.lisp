
(in-package :ch-image-test)

(defparameter *output-image-path*
  (merge-pathnames
   (make-pathname :directory (list :relative :up "output-images"))
   (asdf:component-pathname
    (reduce #'asdf:find-component
            (list nil "ch-image-test" "test" "images")))))

