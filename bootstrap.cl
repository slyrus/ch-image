
(require 'asdf)

(let ((curdir (directory-namestring *load-truename*)))
  (pushnew (make-pathname :directory curdir) asdf:*central-registry*)
  (asdf:operate 'asdf:load-op 'image)
  (asdf:operate 'asdf:load-op 'image-test))

(image-test:run-tests)
