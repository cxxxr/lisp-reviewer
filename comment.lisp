(defpackage :reviewer/comment
  (:use :cl)
  (:export :comment
           :write-comment-location
           :write-comment-message))
(in-package :reviewer/comment)

(defgeneric write-comment-message (comment stream))

(define-condition comment (simple-condition)
  ((line-number :initarg :line-number
                :reader comment-line-number)
   (column :initarg :column
           :reader comment-column)
   (file :initarg :file
         :reader comment-file))
  (:report (lambda (condition stream)
             (write-comment-message condition stream)
             (terpri stream))))

(defun write-comment-location (comment stream)
  (format stream "~A:~D:~D:"
          (comment-file comment)
          (comment-line-number comment)
          (comment-column comment)))

(defmethod write-comment-message :before ((comment comment) stream)
  (write-comment-location comment stream))

(defmethod write-comment-message ((comment comment) stream)
  (princ (or (comment-description comment)
             (type-of comment))
         stream))
