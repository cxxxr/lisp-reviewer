(defpackage :lisp-reviewer/test/utilities
  (:use :cl)
  (:export :sample-file))
(in-package :lisp-reviewer/test/utilities)

(defun sample-file (filename)
  (let ((pathname (asdf:system-relative-pathname :lisp-reviewer (merge-pathnames filename "test/sample/"))))
    (assert (uiop:file-exists-p pathname))
    (probe-file pathname)))
