(defpackage :lisp-reviewer/reviewer/sblint
  (:use :cl)
  (:import-from :lisp-reviewer/comment
                :comment)
  (:import-from :lisp-reviewer/reviewer
                :reviewer)
  (:import-from :lisp-reviewer/restart
                :with-ignorable-restart-case)
  (:export :sblint-reviewer))
(in-package :lisp-reviewer/reviewer/sblint)

(define-condition sblint-comment (comment)
  ((description
    :initarg :description
    :reader sblint-comment-description)))

(defmethod write-comment-message ((comment sblint-comment) stream)
  (write-string (sblint-comment-description comment) stream))

(defclass sblint-reviewer (reviewer)
  ())

(defmethod review progn ((reviewer sblint-reviewer) point)
  (let* ((file (lem-base:buffer-filename (lem-base:point-buffer point)))
         (text
           (with-output-to-string (stream)
             (sblint:run-lint-file file stream))))
    (ppcre:do-register-groups (line-number column description)
        ("[^:]*:(\\d*):(\\d*):([^\\n]*)" text)
      (with-ignorable-restart-case
          (error (make-condition 'sblint-comment
                                 :file file
                                 :line-number line-number
                                 :column column
                                 :description description))))))
