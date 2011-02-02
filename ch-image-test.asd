
(asdf:defsystem :ch-image-test
  :name "ch-image-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.4.3"
  :depends-on (ch-image)
  :components
  ((:module :test
            :serial t
            :components
            ((:cl-source-file "defpackage")
             (:cl-source-file "defpackage")
             (:cl-source-file "testharness")
             (:cl-source-file "test-ch-image")
             (:cl-source-file "test-ch-image-2")
             (:cl-source-file "test-ch-image-3")
             (:cl-source-file "examples")
             (:module
              :images
              :components
              ((:static-file "euc-tiff" :pathname #p"euc.tiff")
               (:static-file "eucgray-tiff" :pathname #p"eucgray.tiff")
               (:static-file "euc-jpeg" :pathname #p"euc.jpeg")
               (:static-file "eucgray-jpeg" :pathname #p"eucgray.jpeg")
               (:static-file "sunset-lzw" :pathname #p"sunset-lzw.tiff")
               (:static-file "sanfran" :pathname #p"sanfran.jpg")))))))

