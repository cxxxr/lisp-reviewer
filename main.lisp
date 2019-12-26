(defpackage :reviewer/main
  (:nicknames :reviewer)
  (:import-from :lem)
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
           (restart-case (error (make-condition '|先頭に(in-package :cl-user)が存在する|
                                                :point form-point))
             (skip ())
             (edit ()
               :report "このフォームを削除する"
               (backward-delete-form point)
               (lem-base:delete-character point 1)))))))))

(defclass defpackage-reviewer (reviewer) ())

(defvar *package-info-list* '())

(defstruct package-info
  name
  import-from-list)

(defmethod review progn ((reviewer defpackage-reviewer) point)
  (let ((*package-info-list* '()))
    (lem-base:buffer-start point)
    (loop
      (multiple-value-bind (form form-point)
          (read-form point)
        (unless form-point (return))
        (optima:match form
          ((list* 'defpackage package-name options)
           (let ((import-from-list '()))
             (dolist (option options)
               (alexandria:destructuring-case option
                 ((:import-from package-name &rest import-names)
                  (push (cons (string package-name)
                              (mapcar #'string import-names))
                        import-from-list))))
             (push (make-package-info :name package-name
                                      :import-from-list import-from-list)
                   *package-info-list*))
           (return)))))
    (pprint *package-info-list*)))

(defclass main-reviewer (header-in-package-reviewer
                         defpackage-reviewer)
  ())

(defun review-file (reviewer pathname)
  (let* ((buffer (lem-base:find-file-buffer pathname :temporary t :enable-undo-p nil))
         (point (lem-base:buffer-point buffer)))
    (review reviewer point)
    ;; (lem-base:write-to-file buffer (lem-base:buffer-filename buffer))
    ))
