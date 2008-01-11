
(require 'asdf)

(asdf:operate 'asdf:load-op 'ch-image)
(asdf:operate 'asdf:load-op 'ch-image-doc)
(asdf:operate 'asdf:load-op 'tinaa)

(defun ch-image-doc-system::make-tinaa-docs ()
  (asdf:operate 'asdf:load-op 'tinaa)
  (let ((tinaa::*short-documentation-length* 512))
    (tinaa:document-system
     'package 'ch-image (asdf:component-pathname
                         (asdf:find-component
                          (asdf:find-component
                           (asdf:find-system 'ch-image-doc)
                           "doc")
                          "tinaa")))))

(ch-image-doc-system::make-tinaa-docs)
