(defpackage :reviewer/reviewer
  (:use :cl)
  (:import-from :reviewer/reporter
                :reporter)
  (:export :review
           :reviewer
           :reviewer-reporter))
(in-package :reviewer/reviewer)

(defgeneric review (reviewer point))

(defclass reviewer ()
  ((reporter
    :initarg :report
    :initform (make-instance 'reporter)
    :reader reviewer-reporter)))
