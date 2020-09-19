(defpackage :lisp-reviewer/comment
  (:use :cl)
  (:export :comment
           :comment-line-number
           :comment-column
           :comment-file
           :comment-equal
           :write-comment-location
           :write-comment-message))
(in-package :lisp-reviewer/comment)

(defgeneric write-comment-message (comment stream))

(define-condition comment (simple-condition)
  ((line-number :initarg :line-number
                :initform nil
                :reader comment-line-number)
   (column :initarg :column
           :initform nil
           :reader comment-column)
   (file :initarg :file
         :reader comment-file))
  (:report (lambda (condition stream)
             (write-comment-message condition stream)
             (terpri stream))))

(defun comment-equal (comment1 comment2)
  (and (eq (type-of comment1) (type-of comment2))
       (eql (comment-line-number comment1) (comment-line-number comment2))
       (eql (comment-column comment1) (comment-column comment2))
       (uiop:pathname-equal (comment-file comment1) (comment-file comment2))))

(defun write-comment-location (comment stream)
  (format stream "~A:~D:~D:"
          (comment-file comment)
          (comment-line-number comment)
          (comment-column comment)))

(defmethod write-comment-message :before ((comment comment) stream)
  (write-comment-location comment stream))

(defmethod write-comment-message ((comment comment) stream)
  (princ (type-of comment)
         stream))
