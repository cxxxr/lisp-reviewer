(defpackage :reviewer/main
  (:nicknames :reviewer)
  (:import-from :lem)
  (:use :cl
        :reviewer/utilities
        :reviewer/comment
        :reviewer/reporter
        :reviewer/reviewer))
(in-package :reviewer/main)

(defclass |先頭に(in-package :cl-user)が存在する| (comment) ())

(defclass package-reviewer (reviewer) ())

(defmethod review ((reviewer package-reviewer) pathname)
  (let* ((buffer (lem-base:find-file-buffer pathname :temporary t :enable-undo-p nil))
         (point (lem-base:buffer-point buffer)))
    (lem-base:buffer-start point)
    (multiple-value-bind (first-form form-point)
        (read-form point)
      (optima:match first-form
        ((list 'in-package package-name)
         (alexandria:when-let (package (find-package package-name))
           (when (string= (package-name package) "COMMON-LISP-USER")
             (report (reviewer-reporter reviewer)
                     (make-instance '|先頭に(in-package :cl-user)が存在する|
                                    :point form-point)))))))))

(defun review-file (pathname)
  (review (make-instance 'package-reviewer) pathname)
  (values))
