(defpackage :lisp-reviewer/reviewer/header-in-package
  (:use :cl
        :lisp-reviewer/comment)
  (:import-from :lisp-reviewer/reviewer
                :reviewer)
  (:import-from :lisp-reviewer/restart
                :with-ignorable-restart-case)
  (:import-from :lisp-reviewer/utilities
                :read-form
                :backward-delete-form
                :make-comment-using-point)
  (:export :header-in-package-reviewer))
(in-package :lisp-reviewer/reviewer/header-in-package)

(define-condition stupid-cl-user-package-initialization (comment) ())

(defmethod write-comment-message ((comment stupid-cl-user-package-initialization) stream)
  (write-string "先頭に(in-package :cl-user)が存在する" stream))

(defclass header-in-package-reviewer (reviewer) ())

(defmethod review progn ((reviewer header-in-package-reviewer) point)
  (lem-base:buffer-start point)
  (multiple-value-bind (first-form form-point)
      (read-form point)
    (trivia:match first-form
      ((list 'in-package package-name)
       (alexandria:when-let (package (find-package package-name))
         (when (string= (package-name package) "COMMON-LISP-USER")
           (with-ignorable-restart-case
               (error (make-comment-using-point
                       'stupid-cl-user-package-initialization
                       form-point))
             (edit ()
                   :report "このフォームを削除する"
                   (backward-delete-form point)
                   (lem-base:delete-character point 1)))))))))
