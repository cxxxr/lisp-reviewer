(defpackage :lisp-reviewer/restart
  (:use :cl)
  (:export :ignore
           :edit
           :with-ignorable-restart-case))
(in-package :lisp-reviewer/restart)

(defmacro with-ignorable-restart-case (expression &body clauses)
  `(restart-case ,expression
     (ignore ()
       :report "このレポートを無視する")
     ,@clauses))
