(defpackage :reviewer/utilities
  (:use :cl)
  (:import-from :lem-base)
  (:export :read-form
           :backward-delete-form
           :get-file-from-point
           :xrefs))
(in-package :reviewer/utilities)

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
                          ((:location location _position _hints)
                           (declare (ignore _position _hints))
                           (alexandria:destructuring-case location
                             ((:file file)
                              (push file files))
                             ((:buffer-and-file _buffer file)
                              (declare (ignore _buffer))
                              (push file files)))))))
        :finally (return files)))
