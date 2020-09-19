(defpackage :lisp-reviewer/reviewer/defpackage
  (:use :cl
        :lisp-reviewer/comment
        :lisp-reviewer/utilities
        :lisp-reviewer/reviewer)
  (:import-from :lisp-reviewer/restart
                :with-ignorable-restart-case)
  (:export :unused-imported-symbol
           :defpackage-reviewer))
(in-package :lisp-reviewer/reviewer/defpackage)

(define-condition multiple-defpackages-in-one-file (comment) ())

(defmethod write-comment-message ((comment multiple-defpackages-in-one-file) stream)
  (write-string "一つのファイルにdefpackageが複数存在する" stream))

(define-condition defpackage-does-not-exist (comment) ())

(defmethod write-comment-message ((comment defpackage-does-not-exist) stream)
  (write-string "defpackageが存在しない" stream))

(define-condition unused-imported-symbol (comment)
  ((import-name
    :initarg :import-name
    :reader comment-import-name)))

(defmethod write-comment-message ((comment unused-imported-symbol) stream)
  (format stream "importした~Aは使われていません" (comment-import-name comment)))

(defclass defpackage-reviewer (reviewer) ())

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
      (lem-base:editor-error ()
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

(defun search-symbol-name-in-package (symbol-name point)
  (lem-base:with-point ((p point))
    (lem-base:buffer-start p)
    (loop
      (multiple-value-bind (form form-point)
          (read-form p)
        (unless (eq 'defpackage (first form))
          (return))
        (unless form-point
          (return-from search-symbol-name-in-package nil))))
    (lem-base:search-forward-symbol p symbol-name)))

(defun use-symbol-in-file-p (package-name import-name point)
  (or (member (get-file-from-point point)
              (xrefs (format nil "~A::~A" package-name import-name)) :test #'uiop:pathname-equal)
      (search-symbol-name-in-package import-name point)))

(defmethod review progn ((reviewer defpackage-reviewer) point)
  ;; - [X] defpackageが無い、または複数ある
  ;; - [ ] import-fromに重複するシンボルがある、パッケージ指定が重複している場合は一つにまとめる
  ;; - [ ] import-fromに存在しないパッケージ、シンボルがある
  ;; - [X] import-fromで使っていないシンボルをimportしている
  (let ((seen-defpackage nil))
    (lem-base:buffer-start point)
    (loop
      (multiple-value-bind (form form-point)
          (read-form point)
        (unless form-point (return))
        (trivia:match form
          ((list* 'defpackage _ options)
           (if seen-defpackage
               ;; TODO: 片方を消すなどの変更案をリスタートにする
               (with-ignorable-restart-case
                   (error (make-comment-using-point
                           'multiple-defpackages-in-one-file
                           form-point)))
               (setf seen-defpackage t))
           (let ((import-from-list (normalize-import-from options))
                 (deleting-import-names '()))
             (loop :for (package-name . import-names) :in import-from-list
                   :do (dolist (import-name import-names)
                         (unless (use-symbol-in-file-p package-name import-name point)
                           (lem-base:with-point ((p form-point))
                             (find-import-name p package-name import-name)
                             (with-ignorable-restart-case
                                 (error (make-comment-using-point
                                         'unused-imported-symbol
                                         p
                                         :import-name import-name))
                               (edit ()
                                     :report (lambda (stream)
                                               (format stream
                                                       "import-fromから~Aを削除する"
                                                       import-name))
                                     (push (list package-name import-name)
                                           deleting-import-names)))))))
             (loop :for (package-name import-name) :in deleting-import-names
                   :do (lem-base:with-point ((p form-point))
                         (when (find-import-name p package-name import-name)
                           (forward-delete-form p)
                           (delete-forward-spaces p)
                           (when (eql (lem-base:character-at p) #\))
                             (lem-base:skip-whitespace-backward p)
                             (delete-forward-spaces p))))))))))
    (unless seen-defpackage
      (error (make-condition 'defpackage-does-not-exist)))))
