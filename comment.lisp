(defpackage :reviewer/comment
  (:use :cl)
  (:export :comment))
(in-package :reviewer/comment)

(defgeneric comment-to-string (object))

(define-condition comment (simple-condition)
  ((line-number :initarg :line-number
                :reader comment-line-number)
   (column :initarg :column
           :reader comment-column)
   (file :initarg :file
         :reader comment-file)
   (description :initarg :description
                :initform nil
                :reader comment-description))
  (:report (lambda (condition stream)
             (format stream "~A:~D:~D:~A~%"
                     (comment-file condition)
                     (comment-line-number condition)
                     (comment-column condition)
                     (or (comment-description condition)
                         (type-of condition))))))
