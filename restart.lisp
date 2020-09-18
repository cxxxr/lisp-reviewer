(defpackage :lisp-reviewer/restart
  (:use :cl)
  (:export :with-ignorable-restart-case))
(in-package :lisp-reviewer/restart)

(defmacro with-ignorable-restart-case (expression &body clauses)
  `(restart-case ,expression
     (ignore ()
       :report "このレポートを無視する")
     ,@clauses))
