(defpackage :reviewer/main
  (:nicknames :reviewer)
  (:import-from :lem)
  (:import-from :sblint)
  (:import-from :cl-ppcre)
  (:use :cl
        :reviewer/utilities
        :reviewer/comment
        :reviewer/reviewer))
(in-package :reviewer/main)

(define-condition |先頭に(in-package :cl-user)が存在する| (comment) ())

(defclass header-in-package-reviewer (reviewer) ())

(defmethod review progn ((reviewer header-in-package-reviewer) point)
  (lem-base:buffer-start point)
  (multiple-value-bind (first-form form-point)
      (read-form point)
    (optima:match first-form
      ((list 'in-package package-name)
       (alexandria:when-let (package (find-package package-name))
         (when (string= (package-name package) "COMMON-LISP-USER")
           (restart-case
               (error (make-condition '|先頭に(in-package :cl-user)が存在する|
                                      :line-number (lem-base:line-number-at-point form-point)
                                      :column (lem-base:point-column form-point)
                                      :pathname (lem-base:buffer-filename
                                                 (lem-base:point-buffer point))))
             (ignore ()
               :report "Ignore report")
             (edit ()
               :report "このフォームを削除する"
               (backward-delete-form point)
               (lem-base:delete-character point 1)))))))))

(defclass defpackage-reviewer (reviewer) ())

(defstruct package-info
  name
  import-from-list)

(defun normalize-import-from (options)
  (let ((import-from-list '()))
    (dolist (option options)
      (alexandria:destructuring-case option
        ((:import-from package-name &rest import-names)
         (let ((package-name (string package-name)))
           (setf import-names (mapcar #'string import-names))
           (let ((import-from (find package-name import-from-list :test #'string= :key #'first)))
             (if import-from
                 (setf (cdr import-from) (append import-names (cdr import-from)))
                 (push (cons package-name import-names)
                       import-from-list)))))))
    (dolist (import-from import-from-list)
      (setf (cdr import-from)
            (delete-duplicates (cdr import-from)
                               :test #'string=)))
    import-from-list))

(defmethod review progn ((reviewer defpackage-reviewer) point)
  (let ((package-info-list '()))
    (lem-base:buffer-start point)
    (loop
      (multiple-value-bind (form form-point)
          (read-form point)
        (unless form-point (return))
        (optima:match form
          ((list* 'defpackage package-name options)
           (let ((import-from-list (normalize-import-from options)))
             (push (make-package-info
                    :name package-name
                    :import-from-list (dolist (elt import-from-list import-from-list)
                                        (setf (cdr elt)
                                              (delete-duplicates (cdr elt)
                                                                 :test #'string=))))
                   package-info-list))
           (return)))))
    package-info-list))

(define-condition sblint-comment (comment)
  ())

(defclass sblint-reviewer (reviewer)
  ())

(defmethod review progn ((reviewer sblint-reviewer) point)
  (let* ((file (lem-base:buffer-filename (lem-base:point-buffer point)))
         (text
           (with-output-to-string (stream)
             (sblint:run-lint-file file stream))))
    (ppcre:do-register-groups (line-number column description) ("[^:]*:(\\d*):(\\d*):([^\\n]*)" text)
      (with-simple-restart (ignore "Ignore report")
        (error (make-condition 'sblint-comment
                               :file file
                               :line-number line-number
                               :column column
                               :description description))))))

(defclass main-reviewer (sblint-reviewer
                         header-in-package-reviewer
                         defpackage-reviewer)
  ())

(defun review-file (reviewer pathname)
  (let* ((buffer (lem-base:find-file-buffer pathname :temporary t :enable-undo-p nil))
         (point (lem-base:buffer-point buffer)))
    (review reviewer point)
    ;; (lem-base:write-to-file buffer (lem-base:buffer-filename buffer))
    ))
