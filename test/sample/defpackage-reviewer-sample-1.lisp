(defpackage :foo
  (:use :cl)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :alexandria
                :when-let))
(in-package :foo)
