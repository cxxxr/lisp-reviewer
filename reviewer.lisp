(defpackage :lisp-reviewer/reviewer
  (:use :cl)
  (:import-from :lem-base)
  (:import-from :lem-lisp-syntax)
  (:import-from :sblint)
  (:export :review
           :reviewer
           :review-file
           :review-system))
(in-package :lisp-reviewer/reviewer)

(defgeneric review (reviewer point)
  (:method-combination progn))

(defclass reviewer ()
  ())

(defun review-file (reviewer pathname)
  (let* ((buffer (lem-base:find-file-buffer pathname
                                            :temporary t
                                            :enable-undo-p nil
                                            :syntax-table lem-lisp-syntax:*syntax-table*))
         (point (lem-base:buffer-point buffer)))
    (review reviewer point)
    (when (lem-base:buffer-modified-p buffer)
      (lem-base:write-to-file buffer (lem-base:buffer-filename buffer)))
    buffer))

(defun review-system (reviewer system-name)
  (dolist (pathname (sblint/utilities/asdf:all-project-pathnames-for-package-inferred-system system-name))
    (review-file reviewer pathname)))
