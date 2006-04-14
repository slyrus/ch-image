
(in-package :ch-image)

(defmacro multiple-value-list-remove-nulls (values)
  `(remove-if #'null (multiple-value-list ,values)))

(defun 4-neighbors (matrix i j)
  "Returns four values, each value is either a list containing
the coordinates of a 4-neighbor of (i,j) in matrix or nil if the
neighbor would be outside of the matrix. The order of the values
is top, left, bottom, right."
  (declare (type fixnum i j))
  (destructuring-bind (rows cols)
      (clem:dim matrix)
    (declare (type fixnum rows cols))
    (values (when (> i 0) (list (1- i) j)) ; top
            (when (> j 0) (list i (1- j))) ; left 
            (when (< i (1- rows)) (list (1+ i) j)) ; bottom
            (when (< j (1- cols)) (list i (1+ j))) ; right
            )))

(defun 8-neighbors (matrix i j)
  "Returns eight values, each value is either a list containing
the coordinates of an 8-neighbor of (i,j) in matrix or nil if the
neighbor would be outside of the matrix. The order of the values
is top, left, bottom, right."
  (declare (type fixnum i j))
  (destructuring-bind (rows cols)
      (clem:dim matrix)
    (declare (type fixnum rows cols))
    (values (when (and (> i 0) (> j 0)) (list (1- i) (1- j))) ; top-left
            (when (> j 0) (list i (1- j))) ; left
            (when (and (< i (1- rows)) (> j 0)) (list (1+ i) (1- j))) ; bottom-left
            (when (< i (1- rows)) (list (1+ i) j)) ; bottom
            (when (and (< i (1- rows)) (< j (1- cols))) (list (1+ i) (1+ j))) ; bottom-right
            (when (< j (1- cols)) (list i (1+ j))) ; right
            (when (and (> i 0) (< j (1- cols))) (list (1- i) (1+ j))) ; top-right
            (when (> i 0) (list (1- i) j)) ; top
            )))

(defun label-components (matrix &key (neighbor-function #'4-neighbors))
  "Returns a new matrix containing labels of the connected
components of matrix. The default neighbor-fucntion is
4-neighbors."
  (destructuring-bind (rows cols)
      (clem:dim matrix)
    (let ((label-matrix (make-instance 'clem::ub8-matrix
                              :rows rows :cols cols
                              :initial-element 0))
          (stack)
          (label-value 0))
      (dotimes (i rows)
        (dotimes (j cols)
          (when (= 0 (clem::mref label-matrix i j))
            (let ((current-label-value (clem::mref matrix i j)))
              (incf label-value)
              (setf (clem::mref label-matrix i j) label-value)
              (mapcar (lambda (p)
                        (destructuring-bind (ni nj) p
                          (when (= current-label-value (clem::mref matrix ni nj))
                            (push p stack)
                            (setf (clem::mref label-matrix ni nj) label-value))))
                      (multiple-value-list-remove-nulls
                       (funcall neighbor-function matrix i j)))
              ;; now we walk through the list....
              (do ((k (pop stack) (pop stack)))
                  ((null k))
                (mapcar (lambda (p)
                          (destructuring-bind (ni nj) p
                            (when (and (= current-label-value (clem::mref matrix ni nj))
                                       (= 0 (clem::mref label-matrix ni nj)))
                              (push p stack)
                              (setf (clem::mref label-matrix ni nj) label-value))))
                        (multiple-value-list-remove-nulls
                         (funcall neighbor-function matrix (car k) (cadr k)))))))))
      ;; this is an ugly hack to deal with the fact that our matrix
      ;; addition API still needs work!
      (clem::mat-add label-matrix (make-instance 'clem::sb8-scalar
                                                 :initial-element -1)))))


(defun component-internal-boundary (matrix label &key
                                   (neighbor-function #'4-neighbors) (value 1))
  "Returns the internal boundary of component label in the
labelled connected components matrix. neighbor-function is
4-neighbors by default, using 8-neighbors will use 8 neighbors
instead of 4 for computing the boundary."
  (destructuring-bind (rows cols)
      (clem:dim matrix)
    (flet ((neighbor-labels (i j)
             (mapcar (lambda (p)
                       (destructuring-bind (ni nj) p
                         (clem::mref matrix ni nj)))
                     (multiple-value-list-remove-nulls
                      (funcall neighbor-function matrix i j)))))
      (let ((label-matrix (make-instance 'clem::ub8-matrix
                                         :rows rows :cols cols
                                         :initial-element 0)))
        (dotimes (i rows)
          (dotimes (j cols)
            (when (= label (clem::mref matrix i j))
              (unless (apply #'= label (neighbor-labels i j))
                (setf (clem::mref label-matrix i j) value)))))
        label-matrix))))


(defun component-external-boundary (matrix label &key
                                   (neighbor-function #'4-neighbors) (value 1))
  "Returns the external boundary of component label in the
labelled connected components matrix. neighbor-function is
4-neighbors by default, using 8-neighbors will use 8 neighbors
instead of 4 for computing the boundary."
  (destructuring-bind (rows cols)
      (clem:dim matrix)
    (flet ((neighbor-labels (i j)
             (mapcar (lambda (p)
                       (destructuring-bind (ni nj) p
                         (clem::mref matrix ni nj)))
                     (multiple-value-list-remove-nulls
                      (funcall neighbor-function matrix i j)))))
      (let ((label-matrix (make-instance 'clem::ub8-matrix
                                         :rows rows :cols cols
                                         :initial-element 0)))
        (dotimes (i rows)
          (dotimes (j cols)
            (unless (= label (clem::mref matrix i j))
              (when (member label (neighbor-labels i j) :test #'=)
                (setf (clem::mref label-matrix i j) value)))))
        label-matrix))))

(defun component-boundary (matrix label &key
                          (neighbor-function #'4-neighbors) (value 1))
  "Returns the union of the interal and external boundaries of
the component whose value is label. neighbor-function is
4-neighbors by default, using 8-neighbors will use 8 neighbors
instead of 4 for computing the boundary."
  (clem::mlogior (apply #'component-internal-boundary matrix label
                        (append
                         (when neighbor-function (list :neighbor-function neighbor-function))
                         (when value (list :value value))))
                 (apply #'component-external-boundary matrix label
                        (append
                         (when neighbor-function (list :neighbor-function neighbor-function))
                         (when value (list :value value))))))
