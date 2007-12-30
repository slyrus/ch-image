
(in-package :ch-image)

(defgeneric write-png-file (file img))

(defmethod write-png-file (file (img argb-image))
  (ch-png::write-2d-planar-png file
                               (ch-image::image-height img)
                               (ch-image::image-width img)
                               8
                               (list
                                (clem::matrix-vals (ch-image::image-r img))
                                (clem::matrix-vals (ch-image::image-g img))
                                (clem::matrix-vals (ch-image::image-b img))
                                (clem::matrix-vals (ch-image::image-a img)))
                               :if-exists :supersede))

(defmethod write-png-stream (stream (img argb-image))
  (ch-png::write-2d-planar-png-stream stream
                                      (ch-image::image-height img)
                                      (ch-image::image-width img)
                                      8
                                      (list
                                       (clem::matrix-vals (ch-image::image-r img))
                                       (clem::matrix-vals (ch-image::image-g img))
                                       (clem::matrix-vals (ch-image::image-b img))
                                       (clem::matrix-vals (ch-image::image-a img)))))

(defmethod write-png-file (file (img bit-matrix-image))
  (ch-png::write-2d-planar-png file
                               (ch-image::image-height img)
                               (ch-image::image-width img)
                               8
                               (list (clem::matrix-vals (make-norm-ub8-image img)))
                               :if-exists :supersede))

(defmethod write-png-file (file (img ub8-matrix-image))
  (ch-png::write-2d-planar-png file
                               (ch-image::image-height img)
                               (ch-image::image-width img)
                               8
                               (list (clem::matrix-vals img))
                               :if-exists :supersede))

(defmethod write-png-stream (stream (img ub8-matrix-image))
  (ch-png::write-2d-planar-png-stream stream
                                      (ch-image::image-height img)
                                      (ch-image::image-width img)
                                      8
                                      (list (clem::matrix-vals img))))


