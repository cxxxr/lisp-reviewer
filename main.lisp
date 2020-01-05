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

(defmacro reporter-restart-case (expression &body clauses)
  `(restart-case ,expression
     (ignore ()
       :report "このレポートを無視する")
     ,@clauses))

(defun make-condition-using-point (type point &rest args)
  (apply #'make-condition type
         :line-number (lem-base:line-number-at-point point)
         :column (lem-base:point-column point)
         :file (get-file-from-point point)
         args))

;;;
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
           (reporter-restart-case
               (error (make-condition-using-point
                       '|先頭に(in-package :cl-user)が存在する|
                       form-point))
             (edit ()
                   :report "このフォームを削除する"
                   (backward-delete-form point)
                   (lem-base:delete-character point 1)))))))))

;;;
(define-condition |一つのファイルにdefpackageが複数存在する| (comment) ())
(define-condition |defpackageが存在しない| (comment) ())
(define-condition |importしたシンボルは使われていない| (comment)
  ((import-name
    :initarg :import-name
    :reader comment-import-name)))

(defmethod write-comment-message ((comment |importしたシンボルは使われていない|) stream)
  (format stream "~Aは使われていません" (comment-import-name comment)))

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

(defun find-import-from-package (point package-name)
  (lem-base:with-point ((end point))
    (lem-base:form-offset end 1)
    (lem-base:scan-lists point 1 -1)
    (handler-case
        (loop
          (lem-base:scan-lists point 1 -1)
          (lem-base:skip-whitespace-forward point)
          (when (string-equal "import-from"
                              (read-from-string (lem-base:symbol-string-at-point point)))
            (lem-base:form-offset point 1)
            (lem-base:skip-whitespace-forward point)
            (when (string-equal (read-from-string (lem-base:symbol-string-at-point point))
                                package-name)
              (lem-base:form-offset point 1)
              (return point)))
          (lem-base:scan-lists point 1 1))
      (lem:editor-error ()
        nil))))

(defun find-import-name (point package-name import-name)
  (when (find-import-from-package point package-name)
    (loop
      (unless (lem-base:form-offset point 1)
        (return))
      (when (string-equal (read-from-string (lem-base:symbol-string-at-point point))
                          import-name)
        (lem-base:form-offset point -1)
        (return point)))))

(defmethod review progn ((reviewer defpackage-reviewer) point)
  ;; - defpackageが無い、または複数ある
  ;; import-fromのレビュー
  ;; - 存在しないパッケージ、シンボルがある
  ;; - 重複するシンボルがある、パッケージ指定が重複している場合は一つにまとめる
  ;; - 使っていないシンボルをimportしている
  (let ((package-info nil)
        (file (get-file-from-point point))
        (delete-import-names '()))
    (lem-base:buffer-start point)
    (loop
      (multiple-value-bind (form form-point)
          (read-form point)
        (unless form-point (return))
        (optima:match form
          ((list* 'defpackage package-name options)
           (when package-info
             ;; TODO: 片方を消すなどの変更案をリスタートにする
             (reporter-restart-case
                 (error (make-condition-using-point '|一つのファイルにdefpackageが複数存在する|
                                                    form-point))))
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
                         (unless (member file (xrefs import-name) :test #'uiop:pathname-equal)
                           (find-import-name form-point package-name import-name)
                           (reporter-restart-case
                               (error (make-condition-using-point
                                       '|importしたシンボルは使われていない|
                                       form-point
                                       :import-name import-name))
                             (edit ()
                                   :report (lambda (stream)
                                             (format stream "import-fromから~Aを削除する" import-name))
                                   (push (list package-name import-name) delete-import-names)))))))))))
    (unless package-info
      (error (make-condition '|defpackageが存在しない|)))
    (defparameter $ delete-import-names)))

;;;
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
      (reporter-restart-case
          (error (make-condition 'sblint-comment
                                 :file file
                                 :line-number line-number
                                 :column column
                                 :description description))))))

;;;
(defclass main-reviewer (sblint-reviewer
                         header-in-package-reviewer
                         defpackage-reviewer)
  ())

(defun review-file (reviewer pathname)
  (let* ((buffer (lem-base:find-file-buffer pathname :temporary t :enable-undo-p nil))
         (point (lem-base:buffer-point buffer)))
    (review reviewer point)
    (lem-base:write-to-file buffer (lem-base:buffer-filename buffer))
    ))
