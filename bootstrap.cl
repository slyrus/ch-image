
(require 'asdf)

(let ((curdir (directory-namestring *load-truename*)))
  (pushnew (make-pathname :directory curdir) asdf:*central-registry*)
  (asdf:operate 'asdf:load-op 'ch-image)
  (asdf:operate 'asdf:load-op 'ch-image-test))

(ch-image-test:run-tests)
