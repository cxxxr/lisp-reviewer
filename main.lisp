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

(defun review-file (reviewer pathname)
  (let* ((buffer (lem-base:find-file-buffer pathname
                                            :temporary t
                                            :enable-undo-p nil
                                            :syntax-table lem-lisp-syntax:*syntax-table*))
         (point (lem-base:buffer-point buffer)))
    (review reviewer point)
    (when (lem-base:buffer-modified-p buffer)
      (lem-base:write-to-file buffer (lem-base:buffer-filename buffer)))
    buffer))

(defun review-system (reviewer system-name)
  (dolist (pathname (sblint/utilities/asdf:all-project-pathnames-for-package-inferred-system system-name))
    (review-file reviewer pathname)))

(defun test ()
  (handler-bind ((comment (lambda (c)
                            (format t "~&~A~&" c)
                            (invoke-restart (find-restart 'ignore)))))
    (review-file (make-instance 'defpackage-reviewer)
                 (asdf:system-relative-pathname :lisp-reviewer "sample/sample-1.lisp"))))

(defun remove-unused-import-symbols (filename &key (ask t))
  (handler-bind ((comment (lambda (c)
                            (declare (ignore c))
                            (unless ask
                              (let ((restart (find-restart 'edit)))
                                (when restart
                                  (invoke-restart restart)))))))
    (review-file (make-instance 'defpackage-reviewer) filename)))
