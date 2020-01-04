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

(defun get-file-from-point (point)
  (lem-base:buffer-filename (lem-base:point-buffer point)))

(defun make-condition-using-point (type point)
  (make-condition type
                  :line-number (lem-base:line-number-at-point point)
                  :column (lem-base:point-column point)
                  :file (get-file-from-point point)))

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
               (error (make-condition-using-point
                       '|先頭に(in-package :cl-user)が存在する|
                       form-point))
             (ignore ()
               :report "Ignore report")
             (edit ()
               :report "このフォームを削除する"
               (backward-delete-form point)
               (lem-base:delete-character point 1)))))))))

(define-condition |一つのファイルにdefpackageが二つ存在する| (comment) ())
(define-condition |defpackageが存在しない| (comment) ())
(define-condition |importしたシンボルは使われていない| (comment) ())

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
  (let ((package-info nil)
        (file (get-file-from-point point)))
    (lem-base:buffer-start point)
    (loop
      (multiple-value-bind (form form-point)
          (read-form point)
        (unless form-point (return))
        (optima:match form
          ((list* 'defpackage package-name options)
           (when package-info
             (error (make-condition-using-point '|一つのファイルにdefpackageが二つ存在する| form-point)))
           (let ((import-from-list (normalize-import-from options)))
             (setq package-info
                   (make-package-info
                    :name package-name
                    :import-from-list (dolist (elt import-from-list import-from-list)
                                        (setf (cdr elt)
                                              (delete-duplicates (cdr elt)
                                                                 :test #'string=)))))
             (loop :for (package-name . import-names) :in (package-info-import-from-list package-info)
                   :do (dolist (import-name import-names)
                         (unless (member file
                                         (reviewer/utilities::xrefs
                                          (swank:xrefs '(:calls :macroexpands :binds
                                                         :references :sets :specializes)
                                                       import-name))
                                         :test #'uiop:pathname-equal)
                           (error (make-condition-using-point '|importしたシンボルは使われていない|
                                                              form-point))))))))))
    (unless package-info
      (error (make-condition '|defpackageが存在しない|)))))

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
