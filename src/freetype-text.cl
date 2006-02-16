;;
;; file: freetype-text.cl
;; author: cyrus harmon
;;

(in-package :ch-image)

(defparameter *platform-fonts*
    #+darwin '((:arial "/Library/Fonts/Arial")
               (:futura "/Library/Fonts/Futura.dfont")
               (:gill-sans "/Library/Fonts/GillSans.dfont")
               (:helvetica "/System/Library/Fonts/Helvetica.dfont")
               (:times-new-roman "/Library/Fonts/Times new Roman")
               (:monaco "/System/Library/Fonts/Monaco.dfont")
               (:times "/System/Library/Fonts/Times.dfont")))

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

(defmethod draw-char (img context char y x &key previous-char)
  (declare (optimize (debug 2)))
  (let ((glyph (get-glyph (context-font context) char))
        (face (context-face context)))
    (declare (type (sb-alien:alien (* freetype::|FT_Face|)) face))
    (let ((rows (rows glyph))
          (cols (cols glyph))
          (matrix (matrix glyph)))
      (let* ((kern (if previous-char
                       (let ((prev-glyph (get-glyph (context-font context) previous-char)))
                         (sb-alien:with-alien ((kerning freetype::|FT_Vector|))
                           (freetype::|FT_Get_Kerning|
                                      (sb-alien:deref face)
                                      (char-index prev-glyph)
                                      (char-index glyph)
                                     freetype::|FT_KERNING_DEFAULT|
                                     (sb-alien:addr kerning))
                           (sb-alien:slot kerning 'freetype::|x|)))
                       0))
             (pixel-kern (ash kern -6)))
        (loop for i fixnum from 0 below rows
           do 
           (dotimes (j cols)
             (declare (type fixnum j))
             (let ((val (clem::mref matrix i j)))
               (ch-image::or-pixel img
                                  (+ (- (bearing-y glyph)) y i)
                                  (+ pixel-kern (bearing-x glyph) x j)
                                  (list 0 val val val)))))
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

