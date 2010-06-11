;;;
;;; testharness.lisp
;;;
;;; Author: Cyrus Harmon <ch-lisp@bobobeach.com>
;;; Time-stamp: <2010-06-11 14:14:48 sly>
;;;

(in-package :ch-image-test)

(defparameter *verbose-test-results* nil)

(defstruct test-run
  (tests 0)
  (passed 0))

(defun run-test (f test-name run)
  (if (funcall f)
      (progn
	(when *verbose-test-results*
	  (format t "~&Test ~A Succeeded" test-name))
	(incf (test-run-passed run))
	(incf (test-run-tests run)))
      (progn
	(format t "~&Test ~A Failed!" test-name)
	(incf (test-run-tests run)))))

