(defpackage :lisp-reviewer/utilities
  (:use :cl)
  (:import-from :lem-base)
  (:export :read-form
           :backward-delete-form
           :forward-delete-form
           :delete-forward-spaces
           :get-file-from-point
           :xrefs
           :make-comment-using-point))
(in-package :lisp-reviewer/utilities)

(defun read-form (point)
  (lem-base:skip-space-and-comment-forward point)
  (lem-base:with-point ((start point))
    (if (lem-base:form-offset point 1)
        (let* ((eof '#:eof)
               (form (read-from-string (lem-base:points-to-string start point) nil eof)))
          (if (eq form eof)
              (values nil nil)
              (values form start)))
        (values nil nil))))

(defun backward-delete-form (point)
  (lem-base:with-point ((end point :left-inserting))
    (lem-base:form-offset point -1)
    (lem-base:delete-between-points point end)))

(defun forward-delete-form (point)
  (lem-base:with-point ((end point :left-inserting))
    (lem-base:form-offset end 1)
    (lem-base:delete-between-points point end)))

(defun delete-forward-spaces (point)
  (loop :while (lem-base:syntax-space-char-p (lem-base:character-at point))
        :do (lem-base:delete-character point)))

(defun get-file-from-point (point)
  (lem-base:buffer-filename (lem-base:point-buffer point)))

(defun xrefs (name)
  (loop :with files := '()
        :for (type . definitions)
        :in (swank:xrefs '(:calls :macroexpands :binds
                           :references :sets :specializes)
                         name)
        :do (loop :for definition :In definitions
                  :do (destructuring-bind (ref-name location) definition
                        (declare (ignore ref-name))
                        (alexandria:destructuring-ecase location
                          ((:error _message)
                           (declare (ignore _message)))
                          ((:location location _position _hints)
                           (declare (ignore _position _hints))
                           (alexandria:destructuring-case location
                             ((:file file)
                              (push file files))
                             ((:buffer-and-file _buffer file)
                              (declare (ignore _buffer))
                              (push file files)))))))
        :finally (return files)))

(defun make-comment-using-point (type point &rest args)
  (apply #'make-condition type
         :line-number (lem-base:line-number-at-point point)
         :column (lem-base:point-column point)
         :file (get-file-from-point point)
         args))
