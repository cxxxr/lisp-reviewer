(defpackage :lisp-reviewer/test/reviewer/defpackage
  (:use :cl
        :rove
        :lisp-reviewer/comment
        :lisp-reviewer/reviewer
        :lisp-reviewer/reviewer/defpackage)
  (:import-from :lisp-reviewer/test/utilities
                :sample-file))
(in-package :lisp-reviewer/test/reviewer/defpackage)

(defun test (reviewer file)
  (let ((conditions '()))
    (handler-bind ((comment
                     (lambda (c)
                       (push c conditions)
                       (invoke-restart (find-restart 'ignore)))))
      (review-file reviewer file))
    conditions))

(defun equals-comments (expected-comments actual-comments)
  (ok (alexandria:set-equal actual-comments
                            expected-comments
                            :test #'comment-equal))
  (ok (alexandria:set-equal (mapcar #'princ-to-string actual-comments)
                            (mapcar #'princ-to-string expected-comments)
                            :test #'string=)))

(deftest unused-imported-symbol
  (let* ((file (sample-file "defpackage-reviewer-sample-1.lisp"))
         (conditions (test (make-instance 'defpackage-reviewer) file))
         (expected-conditions
           (list (make-condition 'unused-imported-symbol
                                 :line-number 4
                                 :column 16
                                 :file file
                                 :import-name 'scan)
                 (make-condition 'unused-imported-symbol
                                 :line-number 6
                                 :column 16
                                 :file file
                                 :import-name 'when-let))))
    (ok (equals-comments conditions expected-conditions))))

(deftest multiple-defpackages-in-one-file
  (let* ((file (sample-file "defpackage-reviewer-sample-2.lisp"))
         (conditions (test (make-instance 'defpackage-reviewer) file))
         (expected-conditions
           (list (make-instance 'multiple-defpackages-in-one-file
                                :line-number 2
                                :column 0
                                :file file))))
    (ok (equals-comments expected-conditions
                         conditions))))
