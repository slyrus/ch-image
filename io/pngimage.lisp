
(in-package :ch-image)

(defclass planar-png (zpng:png)
  ((channels :initarg :channels :accessor channels)))

(defmethod shared-initialize :after ((png planar-png) slot-names
                                     &rest initargs &key image-data)
  (declare (ignore slot-names initargs))
  (unless image-data
    (setf (zpng::%image-data png)
          (let ((num-channels (zpng:samples-per-pixel png))
                (rowstride (zpng::rowstride png)))
            (with-accessors ((height zpng:height)
                             (width zpng:width)
                             (bpp zpng::bpp)
                             (channels channels))
                png
              (let ((data (make-array (* height width num-channels (ash bpp -3))
                                      :element-type '(unsigned-byte 8))))
                (let ((roffset 0))
                  (dotimes (i height)
                    (let ((coffset roffset))
                      (ecase bpp
                        (8 (dotimes (j width)
                             (dolist (channel-data channels)
                               (setf (aref data coffset)
                                     (aref channel-data i j))
                               (incf coffset))))
                        (16 (dotimes (j width)
                              (dolist (channel-data channels)
                                (loop for k from 0 to 1
                                   do 
                                   (setf (aref data coffset)
                                         (logand #xFF (ash (aref channel-data i j) (+ -8 (* k 8)))))
                                   (incf coffset)))))))
                    (incf roffset rowstride)))
                data))))))

(defgeneric make-image-png (image))
(defgeneric write-png-file (file img))
(defgeneric write-png-stream (file img))

(defmethod make-image-png ((img argb-image))
  (make-instance 'planar-png
                 :color-type :truecolor-alpha
                 :height (ch-image:image-height img)
                 :width (ch-image:image-width img)
                 :bpp 8
                 :channels
                 (list
                  (clem::matrix-vals (ch-image:image-r img))
                  (clem::matrix-vals (ch-image:image-g img))
                  (clem::matrix-vals (ch-image:image-b img))
                  (clem::matrix-vals (ch-image:image-a img)))))

(defmethod make-image-png ((img rgb-image))
  (make-instance 'planar-png
                 :color-type :truecolor
                 :height (ch-image:image-height img)
                 :width (ch-image:image-width img)
                 :bpp 8
                 :channels
                 (list
                  (clem::matrix-vals (ch-image:image-r img))
                  (clem::matrix-vals (ch-image:image-g img))
                  (clem::matrix-vals (ch-image:image-b img)))))

(defmethod make-image-png ((img ub8-matrix-image))
  (make-instance 'planar-png
                 :color-type :grayscale
                 :height (ch-image:image-height img)
                 :width (ch-image:image-width img)
                 :bpp 8
                 :channels
                 (list (clem::matrix-vals img))))

(defmethod make-image-png ((img bit-matrix-image))
  (make-instance 'planar-png
                 :color-type :grayscale
                 :height (ch-image:image-height img)
                 :width (ch-image:image-width img)
                 :bpp 8
                 :channels
                 (list (clem::matrix-vals (make-norm-ub8-image img)))))

(defmethod write-png-file (file (img image))
  (zpng:write-png (make-image-png img) file :if-exists :supersede))

(defmethod write-png-stream (stream (img image))
  (zpng:write-png-stream (make-image-png img) stream))

