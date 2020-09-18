(defpackage :lisp-reviewer/main
  (:nicknames :lisp-reviewer)
  (:import-from :lem)
  (:import-from :sblint)
  (:import-from :cl-ppcre)
  (:import-from :trivia)
  (:use :cl
        :lisp-reviewer/utilities
        :lisp-reviewer/comment
        :lisp-reviewer/restart
        :lisp-reviewer/reviewer
        :lisp-reviewer/reviewer/header-in-package
        :lisp-reviewer/reviewer/defpackage
        :lisp-reviewer/reviewer/sblint))
(in-package :lisp-reviewer/main)

;;;
(defclass main-reviewer (sblint-reviewer
                         header-in-package-reviewer
                         defpackage-reviewer)
  ())

(defun remove-unused-import-symbols (filename &key (ask t))
  (handler-bind ((comment (lambda (c)
                            (declare (ignore c))
                            (unless ask
                              (let ((restart (find-restart 'edit)))
                                (when restart
                                  (invoke-restart restart)))))))
    (review-file (make-instance 'defpackage-reviewer) filename)))
