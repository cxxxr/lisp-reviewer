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
  (let ((file (sample-file "sample-1.lisp"))
        (conditions '()))
    (handler-bind ((comment
                     (lambda (c)
                       (push c conditions)
                       (invoke-restart (find-restart 'lisp-reviewer/restart:ignore)))))
      (review-file (make-instance 'defpackage-reviewer) file))
    (let ((test-functions
            (list (lambda (c)
                    (ok (typep c 'LISP-REVIEWER/REVIEWER/DEFPACKAGE::|importしたシンボルは使われていない|))
                    (ok (= 5 (comment-line-number c)))
                    (ok (= 16 (comment-column c)))
                    (ok (uiop:pathname-equal file (comment-file c)))))))
      (ok (= (length conditions) (length test-functions)))
      (loop :for c :in (nreverse conditions)
            :for test-function :in test-functions
            :do (funcall test-function c)))))
