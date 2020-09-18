(defpackage :lisp-reviewer/reviewer
  (:use :cl)
  (:export :review
           :reviewer))
(in-package :lisp-reviewer/reviewer)

(defgeneric review (reviewer point)
  (:method-combination progn))

(defclass reviewer ()
  ())
