(defpackage :lisp-reviewer/test/reviewer/defpackage
  (:use :cl
        :rove
        :lisp-reviewer/reviewer
        :lisp-reviewer/reviewer/defpackage)
  (:import-from :lisp-reviewer/test/utilities
                :sample-file))
(in-package :lisp-reviewer/test/reviewer/defpackage)

(deftest defpackage-reviewer
  (let ((file (sample-file "sample-1.lisp"))
        (conditions '()))
    (handler-bind ((LISP-REVIEWER/REVIEWER/DEFPACKAGE::|importしたシンボルは使われていない|
                     (lambda (c)
                       (push c conditions)
                       (invoke-restart (find-restart 'lisp-reviewer/restart:ignore)))))
      (review-file (make-instance 'defpackage-reviewer) file))
    (loop :for c :in (nreverse conditions)
          :for test
          :in (list (lambda (c)
                      (ok (typep c 'LISP-REVIEWER/REVIEWER/DEFPACKAGE::|importしたシンボルは使われていない|))
                      (ok (= 5 (lisp-reviewer/comment::comment-line-number c)))
                      (ok (= 16 (lisp-reviewer/comment::comment-column c)))
                      (ok (uiop:pathname-equal file (lisp-reviewer/comment::comment-file c)))))
          :do (funcall test c))))
