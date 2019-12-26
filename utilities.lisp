(defpackage :reviewer/utilities
  (:use :cl)
  (:import-from :lem-base)
  (:export :read-form
           :backward-delete-form))
(in-package :reviewer/utilities)

(defun read-form (point)
  (lem-base:skip-space-and-comment-forward point)
  (lem-base:with-point ((start point))
    (lem-base:form-offset point 1)
    (values (read-from-string (lem-base:points-to-string start point))
            start)))

(defun backward-delete-form (point)
  (lem-base:with-point ((end point :left-inserting))
    (lem-base:form-offset point -1)
    (lem-base:delete-between-points point end)))
