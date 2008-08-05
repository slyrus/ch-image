;;
;; file: freetype-text.cl
;; author: cyrus harmon
;;

(in-package :ch-image)

(defclass freetype-text-context (text-context)
  ((library :accessor context-library :initarg :library)
   (font :accessor context-font :initarg :font)))

(defmethod context-face ((context freetype-text-context))
  (face (context-font context)))

(defclass freetype-glyph (glyph)
  ((ft-glyph :accessor ft-glyph :initarg :ft-glyph)
   (char-index :accessor char-index :initarg :char-index)
   (rows :accessor rows :initarg :rows)
   (cols :accessor cols :initarg :cols)
   (matrix :accessor matrix :initarg :matrix)
   (bearing-y :accessor bearing-y :initarg :bearing-y)
   (bearing-x :accessor bearing-x :initarg :bearing-x)
   (bitmap-left :accessor bitmap-left :initarg :bitmap-left)
   (bitmap-top :accessor bitmap-top :initarg :bitmap-top)
   (vert-advance :accessor vert-advance :initarg :vert-advance)
   (hori-advance :accessor hori-advance :initarg :hori-advance)
   (advance :accessor advance :initarg :advance)))
  
(defclass freetype-font (font)
  ((name :accessor name :initarg name)
   (library :accessor library :initarg library)
   (face :accessor face)
   (glyph-cache :accessor glyph-cache :initform (make-hash-table))))

(defmethod shared-initialize :after
    ((font freetype-font) slot-names &rest initargs &key name library &allow-other-keys)
  (declare (ignore initargs)
           (type (sb-alien:alien (* freetype::|FT_Library|)) library))
  (setf (face font)
        (let ((font-path (freetype-ffi::get-font-path name)))
          (cond ((atom font-path)
                 (freetype-ffi::load-face library font-path))
                (t (case (car font-path)
                     (:postscript-type-1
                      (let ((face (freetype-ffi::load-face library (cadr font-path))))
                        (when (caddr font-path)
                          (freetype-ffi::attach-file face (caddr font-path)))
                        face))))))))

(defun set-font-metrics (font height &key (width 0)
                         (y-dpi 300)
                         (x-dpi 300))
  (apply #'freetype-ffi::set-char-size (face font) height
         (append (when width `(:width ,width))
                 (when y-dpi `(:y-dpi ,y-dpi))
                 (when x-dpi `(:x-dpi ,x-dpi))))
  (clrhash (glyph-cache font)))
  
(defmethod set-font ((context freetype-text-context)
                     font-name size
                     &key (x-dpi 72) (y-dpi 72))
  (let ((font (make-instance 'freetype-font
                             :name font-name
                             :library (context-library context))))
    (setf (context-font context) font)
    (set-font-metrics font size :x-dpi x-dpi :y-dpi y-dpi)))

(defmethod get-glyph ((font freetype-font) char)
  (let ((glyph (gethash char (glyph-cache font))))
    (if glyph
        glyph
        (let ((face (face font)))
          (declare (type (sb-alien:alien (* freetype::|FT_Face|)) face))
          (let ((char-index (freetype-ffi::get-char-index face char)))
            
            (freetype-ffi::load-glyph face char-index)
            (freetype-ffi::render-glyph face)
            (let ((glyph (sb-alien:slot face 'freetype::|glyph|)))
              (declare (type (sb-alien:alien freetype::|FT_GlyphSlot|) glyph))
              (let ((bitmap (sb-alien:slot glyph 'freetype::|bitmap|))
                    (metrics (sb-alien:slot glyph 'freetype::|metrics|)))
                (declare (type (sb-alien:alien freetype::|FT_Bitmap|) bitmap)
                         (type (sb-alien:alien freetype::|FT_Glyph_Metrics|) metrics))
                (let ((rows (sb-alien:slot bitmap 'freetype::|rows|))
                      (cols (sb-alien:slot bitmap 'freetype::|width|))
                      (buffer (sb-alien:slot bitmap 'freetype::|buffer|)))
                  (let ((bitmap-matrix (make-instance 'clem:ub8-matrix :rows rows :cols cols)))
                    (dotimes (i rows)
                      (dotimes (j cols)
                        (setf (clem::mref bitmap-matrix i j)
                              (sb-alien:deref buffer (+ (* i cols) j)))))
                    (let ((glyph-obj
                           (make-instance 'freetype-glyph
                                          :ft-glyph glyph
                                          :char-index char-index
                                          :rows rows
                                          :cols (sb-alien:slot bitmap 'freetype::|width|)
                                          :matrix bitmap-matrix
                                          :bearing-y
                                          (ash (sb-alien:slot metrics 'freetype::|horiBearingY|) -6)
                                          :bearing-x
                                          (ash (sb-alien:slot metrics 'freetype::|horiBearingX|) -6)
                                          :vert-advance (sb-alien:slot metrics 'freetype::|vertAdvance|)
                                          :hori-advance (sb-alien:slot metrics 'freetype::|horiAdvance|)
                                          :advance (sb-alien:slot glyph 'freetype::|advance|)
                                          :bitmap-left (sb-alien:slot glyph 'freetype::|bitmap_left|)
                                          :bitmap-top (sb-alien:slot glyph 'freetype::|bitmap_top|))))
                      (setf (gethash char (glyph-cache font)) glyph-obj)
                      glyph-obj))))))))))

(defgeneric draw-glyph-matrix (src dest yoff xoff &key &allow-other-keys))

(defmethod draw-glyph-matrix (src (dest image-channel) yoff xoff &key)
  (let ((rows (rows src))
	(cols (cols src)))
    (dotimes (i rows)
      (declare (type fixnum i))
      (dotimes (j cols)
	(declare (type fixnum j))
	(let ((val (clem::mref src i j)))
	  (ch-image::xor-pixel dest
			       (+ yoff i)
			       (+ xoff j)
                               val))))))

(defun blend (backdrop-color source-color)
  (declare (ignore backdrop-color))
  source-color)

(defun compost (backdrop-alpha
                source-alpha
                result-alpha
                backdrop-color
                source-color)
  (if (plusp result-alpha)
      (+ (* (- 1 (/ source-alpha result-alpha)) backdrop-color)
         (* (/ source-alpha result-alpha)
            (+ (* (- 1 backdrop-alpha) source-color)
               (* backdrop-alpha (blend backdrop-color source-color)))))
      backdrop-color))

(defmethod draw-glyph-matrix (src (dest argb-image) yoff xoff
                              &key
                              (color (list 255 255 255 255)))
  (declare (optimize (debug 3)))
  (labels ((pix-union (backdrop source)
             (+ backdrop source (- (* backdrop source))))
           
           
           (scale-up (v) (truncate (* 255 v))))
    (let ((rows (rows src))
          (cols (cols src))
          (aval (first color))
          (rval (second color))
          (gval (third color))
          (bval (fourth color)))
      (let ((avalf (/ aval 255.0))
            (rvalf (/ rval 255.0))
            (gvalf (/ gval 255.0))
            (bvalf (/ bval 255.0)))
        (dotimes (i rows)
          (declare (type fixnum i))
          (dotimes (j cols)
            (declare (type fixnum j))
            (let* ((val (clem::mref src i j))
                   (valf (/ val 255.0)))
              (handler-case
                  (progn
                    (let* ((background-val (ch-image:get-pixel
                                            dest
                                            (+ yoff i)
                                            (+ xoff j)))
                           (background-alpha (/ (first background-val) 255.0))
                           (background-red (/ (second background-val) 255.0))
                           (background-green (/ (third background-val) 255.0))
                           (background-blue (/ (fourth background-val) 255.0))
                           (result-alpha (pix-union background-alpha valf)))
                      (ch-image::set-pixel dest
                                           (+ yoff i)
                                           (+ xoff j)
                                           (let ((a (scale-up result-alpha))
                                                 (r (scale-up
                                                     (compost background-alpha
                                                              valf
                                                              result-alpha
                                                              background-red
                                                              rvalf)))
                                                 (g (scale-up
                                                     (compost background-alpha
                                                              valf
                                                              result-alpha
                                                              background-green
                                                              gvalf)))
                                                 (b (scale-up
                                                     (compost background-alpha
                                                              valf
                                                              result-alpha
                                                              background-blue
                                                              bvalf))))
                                             (list a r g b)))))
                (simple-type-error ())))))))))
#+nil
(defmethod draw-glyph-matrix (src (dest argb-image) yoff xoff
                              &key
                              (color (list 255 255 255 255)))
  (let ((rows (rows src))
        (cols (cols src))
        (aval (/ (first color) 255))
        (rval (second color))
        (gval (third color))
        (bval (fourth color)))
    (dotimes (i rows)
      (declare (type fixnum i))
      (dotimes (j cols)
        (declare (type fixnum j))
        (let* ((val (clem::mref src i j)))
          (handler-case
              (progn
                (ch-image::set-pixel dest
                                     (+ yoff i)
                                     (+ xoff j)
                                     (list (truncate (* val aval)) rval gval bval)))
            (simple-type-error ())))))))

(defmethod draw-char (img context char y x
                      &key
                      previous-char
                      (color (list 255 0 0 0))
                      (track 1.0d0))
  (declare (optimize (debug 2)))
  (let ((glyph (get-glyph (context-font context) char))
        (face (context-face context)))
    (let ((matrix (matrix glyph)))
      (let* ((kern (if previous-char
                       (let ((prev-glyph (get-glyph (context-font context) previous-char)))
                         (freetype-ffi::kern-pair face
                                                  (char-index prev-glyph)
                                                  (char-index glyph)))
                       0)))
	(draw-glyph-matrix matrix img
                           (- y (bitmap-top glyph))
                           (+ (ash kern -6) (bitmap-left glyph) x)
                           :color color)
        (values (round (* track (- (sb-alien:slot (advance glyph) 'freetype::|y|))))
                (round (* track (+ kern (sb-alien:slot (advance glyph) 'freetype::|x|)))))))))

(defmethod draw-string (img (context freetype-text-context) str y x
                        &key color transform track)
  (let ((font (context-font context)))
    (clrhash (glyph-cache font)))
  (let ((face (context-face context)))
    (when transform
      (sb-alien:with-alien ((matrix freetype::|FT_Matrix|)
                            (delta freetype::|FT_Vector|))
        (setf (sb-alien:slot matrix 'freetype::|xx|) (truncate (* #x10000 (mref transform 1 1)))
              (sb-alien:slot matrix 'freetype::|xy|) (truncate (* #x10000 (- (mref transform 0 1))))
              (sb-alien:slot matrix 'freetype::|yy|) (truncate (* #x10000 (mref transform 0 0)))
              (sb-alien:slot matrix 'freetype::|yx|) (truncate (* #x10000 (- (mref transform 1 0))))
              (sb-alien:slot delta 'freetype::|x|) (truncate (* #x10000 (mref transform 1 2)))
              (sb-alien:slot delta 'freetype::|y|) (truncate (* #x10000 (mref transform 0 2))))
        (freetype::|FT_Set_Transform| (sb-alien:deref face) (sb-alien:addr matrix) (sb-alien:addr delta)))))
  (loop for c across str with prev
     do (multiple-value-bind (yadv xadv)
            (apply #'draw-char img context c y x :previous-char prev
                   (append (when color `(:color ,color))
                           (when track `(:track ,track))))
          (setf prev c)
          (incf x (ash xadv -6))
          (incf y (ash yadv -6))))
  (list y x))

(defun make-text-context ()
  (make-instance 'freetype-text-context
                 :library (freetype-ffi::freetype-init-library)))

