
(require 'asdf)

(asdf:operate 'asdf:load-op 'ch-image)
(asdf:operate 'asdf:load-op 'ch-image-test)

(ch-image-test:run-tests)

