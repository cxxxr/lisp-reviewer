(in-package :cl-user)
(defpackage :foo
  (:use :cl)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :cl-ppcre
                :scan))
(in-package :foo)

(defun hoge (x)
  x)
