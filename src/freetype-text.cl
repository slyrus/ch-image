;;
;; file: freetype-text.cl
;; author: cyrus harmon
;;

(in-package :ch-image)

;;; Note that we can't just iterate over the font directories (with
;;; some list of suggested nicknames perhaps), as some of the font
;;; names contains Unicode characters and this currently breaks SBCL.

(defparameter *platform-fonts*
    #+darwin '((:arial "/Library/Fonts/Arial")
               (:futura "/Library/Fonts/Futura.dfont")
               (:gill-sans "/Library/Fonts/GillSans.dfont")
               (:helvetica "/System/Library/Fonts/Helvetica.dfont")
               (:times-new-roman "/Library/Fonts/Times New Roman")
               (:monaco "/System/Library/Fonts/Monaco.dfont")
               (:times "/System/Library/Fonts/Times.dfont")))
;;; need font paths for linux!



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
   (vert-advance :accessor vert-advance :initarg :vert-advance)
   (hori-advance :accessor hori-advance :initarg :hori-advance)))
  
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
        (let ((font-path (cadr (assoc name *platform-fonts*))))
          (freetype-ffi::load-face library font-path))))

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
                             :library (context-library context) )))
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
                                          :hori-advance (sb-alien:slot metrics 'freetype::|horiAdvance|))))
                      (setf (gethash char (glyph-cache font)) glyph-obj)
                      glyph-obj))))))))))

(defun xor-blit-matrix (src dest yoff xoff &key (alpha 255))
  (let ((nalpha (logxor 255 alpha))
	(rows (rows src))
	(cols (cols src)))
    (dotimes (i rows)
      (declare (type fixnum i))
      (dotimes (j cols)
	(declare (type fixnum j))
	(let ((val (clem::mref src i j)))
	  (ch-image::xor-pixel dest
			       (+ yoff i)
			       (+ xoff j)
			       (list nalpha val val val)))))))

(defmethod draw-char (img context char y x &key previous-char (alpha 255))
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
	(xor-blit-matrix matrix img
			 (- y (bearing-y glyph))
			 (+ (ash kern -6) (bearing-x glyph) x)
			 :alpha alpha)
        (values (vert-advance glyph)
                (+ kern (hori-advance glyph)))))))

(defmethod draw-string (img (context freetype-text-context) str y x)
  (loop for c across str with prev
     do (multiple-value-bind (yadv xadv)
            (draw-char img context c y x :previous-char prev)
          (declare (ignore yadv))
          (setf prev c)
          (incf x (ash xadv -6)))))

(defun make-text-context ()
  (make-instance 'freetype-text-context
                 :library (freetype-ffi::freetype-init-library)))

