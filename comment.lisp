(defpackage :reviewer/comment
  (:use :cl)
  (:import-from :lem-base)
  (:export :comment))
(in-package :reviewer/comment)

(defgeneric comment-to-string (object))

(define-condition comment (simple-condition)
  ((point :initarg :point
          :reader comment-point))
  (:report (lambda (condition stream)
             (let ((point (comment-point condition)))
               (format stream "~A:~D:~D:~A~%"
                       (lem-base:buffer-filename (lem-base:point-buffer point))
                       (lem-base:line-number-at-point point)
                       (lem-base:point-column point)
                       (type-of condition))))))
