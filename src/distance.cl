
(in-package :ch-image)

(defun image-l1-distance (b0 b1)
  (let ((pixels (* (clem:rows b0) (clem:cols b0))))
    (coerce (/ (clem:sum
                (clem:mat-abs (clem:m- b0 b1)))
               pixels)
            'double-float)))

(defun image-l2-distance (b0 b1)
  (let ((pixels (* (clem:rows b0) (clem:cols b0))))
    (coerce (/ (clem:sum
                (clem:mat-square
                 (clem::copy-to-double-float-matrix (clem:m- b0 b1))))
               pixels)
            'double-float)))

(defun square (x)
  (* x x))

;;;
;;; normalized cross correlation, as described in Lewis, 1995.

(defun normalized-cross-correlation (image feature &key (u 0) (v 0))
  (flet ((square (a) (* a a)))
    (let ((fsum 0)
          (tsum 0)
          (pixels (* (clem:rows feature) (clem:cols feature))))
      (loop for j from v below (+ v (clem:rows feature))
         for y from 0
         do
         (loop for i from u below (+ u (clem::cols feature))
            for x from 0
            do
            (incf fsum (clem::mref image j i))
            (incf tsum (clem::mref feature y x))))
      (let ((favg (coerce (/ fsum pixels) 'double-float))
            (tavg (coerce (/ tsum pixels) 'double-float))
            (csum 0)
            (dfsum 0)
            (dtsum 0))
        (loop for j from v below (+ v (clem:rows feature))
           for y from 0
           do
           (loop for i from u below (+ u (clem::cols feature))
              for x from 0
              do
              (incf csum (* (- (clem::mref image j i) favg)
                            (- (clem::mref feature y x) tavg)))
              (incf dfsum (square (- (clem::mref image j i) favg)))
              (incf dtsum (square (- (clem::mref feature y x) tavg)))))
	(let ((denom (sqrt (* dfsum dtsum))))
	  (if (not (zerop denom))
	      (coerce (/ csum denom) 'double-float)
	      0d0))))))

;;;
;;; an (incomplete) implementation of an algorithm to compute the
;;; hausdorff distance between two rasterized images, using the
;;; approach of Huttenlocher, Klanderman and Rucklidge, 1991,
;;; pp. 19-20.

#+(or)
(defun hausdorff-distance (A B tau f1 f2)
  (declare (ignore f1 f2))
  ;; step 1, compute the bounds of A and B.
  (let ((ma (image-height A))
        (na (image-width A))
        (mb (image-height B))
        (nb (image-width B)))
    ;; step 2, compute D, the distance transform of B. In this
    ;; implementation, we use the approximate distance transform,
    ;; given in Bourgefors, 1988.
    (let ((d (distance-transform-3-4 b)))
      (declare (ignore d))
    
      ;; step 3, compute D', the distance transform of a padded
      ;; version of A.
      (let ((aprime (make-instance (class-of a)
                                   :height (+ ma (* 2 mb))
                                   :width (+ na (* 2 nb)))))
        (copy-pixels a
                     aprime
                     0 (1- ma)
                     0 (1- na)
                     mb (1- (+ ma mb))
                     nb (1- (+ na nb)))
        (let ((dprime (distance-transform-3-4 aprime)))

          ;; step 4, compute D'+x, the distance to the closest pixel
          ;; (in the increasing x direction) of D' which is less than
          ;; or equal to tau.
          (let ((dprimeplus (clem::mat-copy-proto dprime)))
            (loop for i from 0 below (clem:rows dprime)
               do (loop for j from 0 below (clem:cols dprime)
                     do
                     (loop for k from j below (clem:cols dprime)
                        for q from 0
                        until (< (clem:mref dprime i k) tau)
                        finally (setf (clem:mref dprimeplus i j)
                                      (if (= k (clem:cols dprime))
                                          k q)))))
            ;; step 5, what is (floor f1q) ??
            (write-image-file "dprimeplus.png"
                              (make-norm-ub8-image dprimeplus))
            (let ((m (mat-copy-proto dprime))
                  (tl)
                  (tlprime))
              (declare (ignore m tl tlprime)))))))))


