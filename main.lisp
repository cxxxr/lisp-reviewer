(defpackage :lisp-reviewer/main
  (:nicknames :reviewer)
  (:import-from :lem)
  (:import-from :sblint)
  (:import-from :cl-ppcre)
  (:import-from :trivia)
  (:use :cl
        :lisp-reviewer/utilities
        :lisp-reviewer/comment
        :lisp-reviewer/restart
        :lisp-reviewer/reviewer))
(in-package :lisp-reviewer/main)

(defun make-comment-using-point (type point &rest args)
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
    (trivia:match first-form
      ((list 'in-package package-name)
       (alexandria:when-let (package (find-package package-name))
         (when (string= (package-name package) "COMMON-LISP-USER")
           (with-ignorable-restart-case
               (error (make-comment-using-point
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
                           '|一つのファイルにdefpackageが複数存在する|
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
                                         '|importしたシンボルは使われていない|
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
      (error (make-condition '|defpackageが存在しない|)))))

;;;
(define-condition sblint-comment (comment)
  ((description
    :initarg :description
    :reader sblint-comment-description)))

(defmethod write-comment-message ((comment sblint-comment) stream)
  (write-string (sblint-comment-description comment) stream))

(defclass sblint-reviewer (reviewer)
  ())

(defmethod review progn ((reviewer sblint-reviewer) point)
  (let* ((file (lem-base:buffer-filename (lem-base:point-buffer point)))
         (text
           (with-output-to-string (stream)
             (sblint:run-lint-file file stream))))
    (ppcre:do-register-groups (line-number column description)
        ("[^:]*:(\\d*):(\\d*):([^\\n]*)" text)
      (with-ignorable-restart-case
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
