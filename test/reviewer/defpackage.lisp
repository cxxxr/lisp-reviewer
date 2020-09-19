(defpackage :lisp-reviewer/test/reviewer/defpackage
  (:use :cl
        :rove
        :lisp-reviewer/comment
        :lisp-reviewer/reviewer
        :lisp-reviewer/reviewer/defpackage)
  (:import-from :lisp-reviewer/test/utilities
                :sample-file))
(in-package :lisp-reviewer/test/reviewer/defpackage)

(deftest defpackage-reviewer
  (let ((file (sample-file "defpackage-reviewer-sample-1.lisp"))
        (conditions '()))
    (handler-bind ((comment
                     (lambda (c)
                       (push c conditions)
                       (invoke-restart (find-restart 'ignore)))))
      (review-file (make-instance 'defpackage-reviewer) file))
    (let ((expected-conditions
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
      (ok (alexandria:set-equal conditions
                                expected-conditions
                                :test #'comment-equal))
      (ok (alexandria:set-equal (mapcar #'princ-to-string conditions)
                                (mapcar #'princ-to-string expected-conditions)
                                :test #'string=)))))
