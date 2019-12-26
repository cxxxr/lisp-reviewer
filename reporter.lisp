(defpackage :reviewer/reporter
  (:use :cl)
  (:import-from :reviewer/comment
                :comment-to-string)
  (:export :report
           :reporter))
(in-package :reviewer/reporter)

(defgeneric report (reporter comment))

(defclass reporter () ())

(defmethod report ((reporter reporter) comment)
  (format t "~A~%" (comment-to-string comment)))
