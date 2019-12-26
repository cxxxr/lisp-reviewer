(defpackage :reviewer/main
  (:nicknames :reviewer)
  (:import-from :lem)
  (:use :cl))
(in-package :reviewer/main)

(defclass comment () ())

(defclass |先頭に(in-package :cl-user)が存在する| (comment) ())

(defgeneric review (reviewer pathname))
(defgeneric comment (reporter comment point))

(defclass reporter () ())

(defmethod comment ((reporter reporter) comment point)
  (format t "~D:~D:~A" (lem-base:line-number-at-point point) (lem-base:point-column point) (type-of comment)))

(defclass reviewer ()
  ((reporter
    :initarg :report
    :initform (make-instance 'reporter)
    :reader reviewer-reporter)))

(defclass package-reviewer (reviewer) ())

(defun read-form (point)
  (lem-base:skip-space-and-comment-forward point)
  (lem-base:with-point ((start point))
    (lem-base:form-offset point 1)
    (values (read-from-string (lem-base:points-to-string start point))
            start)))

(defmethod review ((reviewer package-reviewer) pathname)
  (let* ((buffer (lem-base:find-file-buffer pathname :temporary t :enable-undo-p nil))
         (point (lem-base:buffer-point buffer)))
    (multiple-value-bind (first-form form-point)
        (read-form point)
      (optima:match first-form
        ((list 'in-package package-name)
         (alexandria:when-let (package (find-package package-name))
           (when (string= (package-name package) "COMMON-LISP-USER")
             (comment (reviewer-reporter reviewer)
                      (make-instance '|先頭に(in-package :cl-user)が存在する|)
                      form-point))))))))
