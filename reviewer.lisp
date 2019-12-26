(defpackage :reviewer/reviewer
  (:use :cl)
  (:export :review
           :reviewer))
(in-package :reviewer/reviewer)

(defgeneric review (reviewer point))

(defclass reviewer ()
  ())
