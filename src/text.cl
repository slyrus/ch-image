;;
;; file: text.cl
;; author: cyrus harmon
;;

(in-package :ch-image)

(defclass text-context () ())

(defgeneric context-face (text-context))

(defclass glyph () ())

(defclass font () ())

(defgeneric set-font (context font size &key))

(defgeneric get-glyph (font char))

(defgeneric draw-char (img context char y x))

(defgeneric draw-string (img context str y x))

