
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
    (let ((label-matrix (make-instance 'clem:ub8-matrix
                              :rows rows :cols cols
                              :initial-element 0))
          (stack)
          (label-value 0))
      (dotimes (i rows)
        (dotimes (j cols)
          (when (= 0 (clem:mref label-matrix i j))
            (let ((current-label-value (clem:mref matrix i j)))
              (incf label-value)
              (setf (clem:mref label-matrix i j) label-value)
              (mapcar (lambda (p)
                        (destructuring-bind (ni nj) p
                          (when (= current-label-value (clem:mref matrix ni nj))
                            (push p stack)
                            (setf (clem:mref label-matrix ni nj) label-value))))
                      (multiple-value-list-remove-nulls
                       (funcall neighbor-function matrix i j)))
              ;; now we walk through the list....
              (do ((k (pop stack) (pop stack)))
                  ((null k))
                (mapcar (lambda (p)
                          (destructuring-bind (ni nj) p
                            (when (and (= current-label-value (clem:mref matrix ni nj))
                                       (= 0 (clem:mref label-matrix ni nj)))
                              (push p stack)
                              (setf (clem:mref label-matrix ni nj) label-value))))
                        (multiple-value-list-remove-nulls
                         (funcall neighbor-function matrix (car k) (cadr k)))))))))
      ;; this is an ugly hack to deal with the fact that our matrix
      ;; addition API still needs work!
      (clem:mat-add label-matrix -1))))


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
                         (clem:mref matrix ni nj)))
                     (multiple-value-list-remove-nulls
                      (funcall neighbor-function matrix i j)))))
      (let ((label-matrix (make-instance 'clem:ub8-matrix
                                         :rows rows :cols cols
                                         :initial-element 0)))
        (dotimes (i rows)
          (dotimes (j cols)
            (when (= label (clem:mref matrix i j))
              (unless (apply #'= label (neighbor-labels i j))
                (setf (clem:mref label-matrix i j) value)))))
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
                         (clem:mref matrix ni nj)))
                     (multiple-value-list-remove-nulls
                      (funcall neighbor-function matrix i j)))))
      (let ((label-matrix (make-instance 'clem:ub8-matrix
                                         :rows rows :cols cols
                                         :initial-element 0)))
        (dotimes (i rows)
          (dotimes (j cols)
            (unless (= label (clem:mref matrix i j))
              (when (member label (neighbor-labels i j) :test #'=)
                (setf (clem:mref label-matrix i j) value)))))
        label-matrix))))

(defun component-boundary (matrix label &key
                          (neighbor-function #'4-neighbors) (value 1))
  "Returns the union of the interal and external boundaries of
the component whose value is label. neighbor-function is
4-neighbors by default, using 8-neighbors will use 8 neighbors
instead of 4 for computing the boundary."
  (clem:mlogior (apply #'component-internal-boundary matrix label
                        (append
                         (when neighbor-function (list :neighbor-function neighbor-function))
                         (when value (list :value value))))
                 (apply #'component-external-boundary matrix label
                        (append
                         (when neighbor-function (list :neighbor-function neighbor-function))
                         (when value (list :value value))))))

(defun backward-4-neighbors (matrix i j)
  "Returns two values, each value is either a list containing the
coordinates of a backward 4-neighbor of (i,j), that is the left
or top neighbor, in matrix or nil if the neighbor would be
outside of the matrix. The order of the values is top, left."
  (declare (type fixnum i j))
  (destructuring-bind (rows cols)
      (clem:dim matrix)
    (declare (type fixnum rows cols)
             (ignore rows cols))
    (values (when (> i 0) (list (1- i) j)) ; top
            (when (> j 0) (list i (1- j))) ; left
            )))

(defun forward-4-neighbors (matrix i j)
  "Returns two values, each value is either a list containing the
coordinates of a forward 4-neighbor of (i,j), that is the bottom
or right neighbor, in matrix or nil if the neighbor would be
outside of the matrix. The order of the values is bottom, right."
  (declare (type fixnum i j))
  (destructuring-bind (rows cols)
      (clem:dim matrix)
    (declare (type fixnum rows cols))
    (values (when (< i (1- rows)) (list (1+ i) j)) ; bottom
            (when (< j (1- cols)) (list i (1+ j))) ; right
            )))

(defun backward-8-neighbors (matrix i j)
  "Returns four values, each value is either a list containing
the coordinates of a backward 8-neighbor of (i,j), that is the
top-left, top, top-right, or left neighbor, in matrix or nil if
the neighbor would be outside of the matrix. The order of the
values is top-left, top, top-right, left."
  (declare (type fixnum i j))
  (destructuring-bind (rows cols)
      (clem:dim matrix)
    (declare (type fixnum rows cols)
             (ignore rows))
    (values (when (and (> i 0) (> j 0)) (list (1- i) (1- j))) ; top-left
            (when (> i 0) (list (1- i) j)) ; top
            (when (and  (> i 0) (< j (1- cols))) (list (1- i) (1+ j))) ; top-right
            (when (> j 0) (list i (1- j))) ; left
            )))

(defun forward-8-neighbors (matrix i j)
  "Returns two values, each value is either a list containing the
coordinates of a forward 4-neighbor of (i,j), that is the
bottom-left, bottom, bottom-right, or right neighbor, in matrix
or nil if the neighbor would be outside of the matrix. The order
of the values is bottom-left, bottom, bottom-right, right."
  (declare (type fixnum i j))
  (destructuring-bind (rows cols)
      (clem:dim matrix)
    (declare (type fixnum rows cols))
    (values (when (and (< i (1- rows)) (> j 0)) (list (1+ i) (1- j))) ; bottom-left
            (when (< i (1- rows)) (list (1+ i) j)) ; bottom
            (when (and (< i (1- rows)) (< j (1- cols))) (list (1+ i) (1+ j))) ; bottom-right
            (when (< j (1- cols)) (list i (1+ j))) ; right
            )))

(defun distance-transform (in &key (border 0))
  "Computes the distance transform of an image. The distance
transform is a matrix where the value of each pixel is the
distance between that pixel and the closest zero-valued
pixel. This function uses the algorithm described in Soille,
2003, p. 48."
  (destructuring-bind (rows cols)
      (clem:dim in)
    (let ((dist (clem:copy-to-ub32-matrix in)))
      (dotimes (i rows)
        (dotimes (j cols)
          (when (> (clem:mref dist i j) 0)
            (setf (clem:mref dist i j)
                  (1+ (apply
                       #'min
                       (mapcar
                        (lambda (p) (if p (clem:mref dist (car p) (cadr p)) border))
                        (multiple-value-list
                         (backward-4-neighbors dist i j)))))))))
      (loop for i from (1- rows) downto 0
         do 
         (loop for j from (1- cols) downto 0
            do
            (unless (equal (clem:mref dist i j) 0)
              (setf (clem:mref dist i j)
                    (min (clem:mref dist i j)
                         (1+ (apply
                              #'min
                              (mapcar
                               (lambda (p) (if p (clem:mref dist (car p) (cadr p)) border))
                               (multiple-value-list
                                (forward-4-neighbors dist i j))))))))))
      dist)))


(defun distance-transform-2 (in &key (border 0))
  "Computes the distance transform of an image. The distance
transform is a matrix where the value of each pixel is the
distance between that pixel and the closest zero-valued
pixel. This function uses the algorithm described in Soille,
2003, p. 48."
  (destructuring-bind (rows cols)
      (clem:dim in)
    (let ((dist (clem:copy-to-ub32-matrix in)))
      (dotimes (i rows)
        (dotimes (j cols)
          (when (= (clem:mref dist i j) 0)
            (setf (clem:mref dist i j)
                  (1+ (apply
                       #'min
                       (mapcar
                        (lambda (p) (if p (clem:mref dist (car p) (cadr p)) border))
                        (multiple-value-list
                         (backward-4-neighbors dist i j)))))))))
      (loop for i from (1- rows) downto 0
         do 
           (loop for j from (1- cols) downto 0
              do
                (when (equal (clem:mref dist i j) 0)
                  (setf (clem:mref dist i j)
                        (min (clem:mref dist i j)
                             (1+ (apply
                                  #'min
                                  (mapcar
                                   (lambda (p) (if p (clem:mref dist (car p) (cadr p)) border))
                                   (multiple-value-list
                                    (forward-4-neighbors dist i j))))))))))
      dist)))


(defun distance-transform-3 (in &key (border 0) (maxval 255))
  "Computes the distance transform of an image. The distance
transform is a matrix where the value of each pixel is the
distance between that pixel and the closest zero-valued
pixel. This function uses the algorithm described in Soille,
2003, p. 48."
  (destructuring-bind (rows cols)
      (clem:dim in)
    (let ((dist (clem:copy-to-ub32-matrix in)))
      (dotimes (i rows)
        (dotimes (j cols)
          (if (> (clem:mref dist i j) 0)
              (setf (clem:mref dist i j) maxval)
              (setf (clem:mref dist i j)
                    (apply
                     #'max
                     0
                     (mapcar
                      (lambda (p) (if p (1- (clem:mref dist (car p) (cadr p))) border))
                      (multiple-value-list
                       (backward-4-neighbors dist i j))))))))
      (loop for i from (1- rows) downto 0
         do 
           (loop for j from (1- cols) downto 0
              do
                (unless (equal (clem:mref dist i j) maxval)
                  (setf (clem:mref dist i j)
                        (max (clem:mref dist i j)
                             (apply
                              #'max
                              0
                              (mapcar
                               (lambda (p) (if p (1- (clem:mref dist (car p) (cadr p))) border))
                               (multiple-value-list
                                (forward-4-neighbors dist i j)))))))))
      dist)))


(defun distance-transform-4 (in &key (slope 1) (maxval 255))
  "Computes the distance transform of an image. The distance
transform is a matrix where the value of each pixel is the
distance between that pixel and the closest zero-valued
pixel. This function uses the algorithm described in Soille,
2003, p. 48."
  (destructuring-bind (rows cols)
      (clem:dim in)
    (let ((dist (clem:copy-to-ub32-matrix in)))
      (dotimes (i rows)
        (dotimes (j cols)
          (if (> (clem:mref dist i j) 0)
              (setf (clem:mref dist i j) 0)
              (setf (clem:mref dist i j)
                    (apply
                     #'min
                     maxval
                     (mapcan
                      (lambda (p) (when p (list (+ slope (clem:mref dist (car p) (cadr p))))))
                      (multiple-value-list
                       (backward-4-neighbors dist i j))))))))
      (loop for i from (1- rows) downto 0
         do 
           (loop for j from (1- cols) downto 0
              do
                (unless (equal (clem:mref dist i j) 0)
                  (setf (clem:mref dist i j)
                        (min (clem:mref dist i j)
                             (apply
                              #'min
                              maxval
                              (mapcan
                               (lambda (p) (when p (list (+ slope (clem:mref dist (car p) (cadr p))))))
                               (multiple-value-list
                                (forward-4-neighbors dist i j)))))))))
      dist)))

(defun distance-transform-3-4 (in)
  "Computes the distance transform of an image. The distance
transform is a matrix where the value of each pixel is the
distance between that pixel and the closest zero-valued
pixel. This function uses the algorithm described in Soille,
2003, p. 48."
  (destructuring-bind (rows cols)
      (clem:dim in)
    (let ((dist (clem:copy-to-ub32-matrix in)))
      (dotimes (i rows)
        (dotimes (j cols)
          (if (> (clem:mref dist i j) 0)
              (setf (clem:mref dist i j) 0)
              (setf (clem:mref dist i j) #xffff))))
      (loop for i from 1 below rows
         do
           (loop for j from 1 below cols
              do
                (setf (clem:mref dist i j)
                      (apply
                       #'min
                       (clem:mref dist i j)
                       (multiple-value-bind (top-left top top-right left)
                           (backward-8-neighbors dist i j)
                         (append
                          (when top-left (list (+ 4 (clem:mref dist (car top-left) (cadr top-left)))))
                          (when top (list (+ 3 (clem:mref dist (car top) (cadr top)))))
                          (when top-right (list (+ 4 (clem:mref dist (car top-right) (cadr top-right)))))
                          (when left (list (+ 3 (clem:mref dist (car left) (cadr left)))))))))))
      (loop for i from (- rows 1) downto 0
         do 
           (loop for j from (- cols 1) downto 0
              do
                (setf (clem:mref dist i j)
                      (apply
                       #'min
                       (clem:mref dist i j)
                       (multiple-value-bind (bottom-left bottom bottom-right right)
                           (forward-8-neighbors dist i j)
                         (append
                          (when bottom-left (list (+ 4 (clem:mref dist (car bottom-left) (cadr bottom-left)))))
                          (when bottom (list (+ 3 (clem:mref dist (car bottom) (cadr bottom)))))
                          (when bottom-right (list (+ 4 (clem:mref dist (car bottom-right) (cadr bottom-right)))))
                          (when right (list (+ 3 (clem:mref dist (car right) (cadr right)))))))))))
      dist)))
