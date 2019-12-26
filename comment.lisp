(defpackage :reviewer/comment
  (:use :cl)
  (:export :comment
           :comment-to-string))
(in-package :reviewer/comment)

(defgeneric comment-to-string (object))

(defclass comment ()
  ((point :initarg :point
          :reader comment-point)))

(defmethod comment-to-string ((object comment))
  (let ((point (comment-point object)))
    (format t "~A:~D:~D:~A~%"
            (lem-base:buffer-filename (lem-base:point-buffer point))
            (lem-base:line-number-at-point point)
            (lem-base:point-column point)
            (type-of object))))
