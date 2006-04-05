
(require 'asdf)

(asdf:operate 'asdf:load-op 'ch-image)
(asdf:operate 'asdf:load-op 'tinaa)

(defun ch-image-system::make-tinaa-docs ()
  (asdf:operate 'asdf:load-op 'tinaa)
  (tinaa:document-system
   'package 'ch-image (asdf:component-pathname
                   (asdf:find-component
                    (asdf:find-system 'ch-image)
                    "tinaadoc"))))
  
(ch-image-system::make-tinaa-docs)
