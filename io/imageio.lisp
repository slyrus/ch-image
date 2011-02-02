;;;
;;; file: imageio.lisp
;;; author: cyrus harmon
;;; creation date: Mon Dec  5 12:22:59 2005
;;; 

(in-package :ch-image)

(defun make-hash-table-from-alist (alist &key (test #'eql))
  (let ((h (make-hash-table :test test)))
    (loop for (x . y) in alist
       do (setf (gethash x h) y))
    h))

(defparameter *image-read-functions*
  (make-hash-table-from-alist
   (append '(("tiff" . read-tiff-file)
             ("tif" . read-tiff-file))
           '(("jpeg" . read-jpeg-file)
             ("jpg" . read-jpeg-file)
             ("jpe" . read-jpeg-file))
           '(("png" . read-png-file)))
   :test #'equalp))

(defparameter *image-write-functions*
  (make-hash-table-from-alist
   (append '(("tiff" . write-tiff-file)
             ("tif" . write-tiff-file))
           '(("jpeg" . write-jpeg-file)
             ("jpg" . write-jpeg-file)
             ("jpe" . write-jpeg-file))
           '(("png" . write-png-file)))
   :test #'equalp))

(defun image-file-read-function (path)
  (gethash (pathname-type path) *image-read-functions*))

(defun image-file-write-function (path)
  (gethash (pathname-type path) *image-write-functions*))

(defun read-image-file (path)
  (let ((f (image-file-read-function path)))
    (funcall f path)))

(defun write-image-file (path image)
  (let ((f (image-file-write-function path)))
    (funcall f path image)))

(defun write-matrix-image-file (path matrix)
  (let ((f (image-file-write-function path)))
    (funcall f path (make-matrix-image matrix))))

(defun get-image-files-in-directory (directory type)
  (cond ((null type) nil)
        ((listp type)
         (append (get-image-files-in-directory directory (car type))
                 (get-image-files-in-directory directory (cdr type))))
        (t (directory
            (merge-pathnames
             (make-pathname :name :wild :type type)
             directory)))))
        
(defun resize-images-in-directory (src-dir dest-dir
                                   &key
                                   height width
                                   (source-types '("tiff"))
                                   (destination-type
                                    (if (listp source-types)
                                        (car source-types)
                                        source-types))
                                   (interpolation :bilinear)
                                   (constrain-proportions nil))
  (ensure-directories-exist dest-dir)
  (mapcar #'(lambda (file)
              (let ((src-img (read-image-file file)))
                (let ((dst-img (if (and height width)
                                   (ch-image::resize-image
                                    src-img height width
                                    :interpolation interpolation
                                    :constrain-proportions constrain-proportions)
                                   src-img)))
                  (let ((dest-path (merge-pathnames
                                    dest-dir
                                    (merge-pathnames
                                     (make-pathname :type destination-type)
                                     file))))
                    (write-image-file dest-path dst-img)
                    dest-path))))
          (get-image-files-in-directory src-dir source-types)))


