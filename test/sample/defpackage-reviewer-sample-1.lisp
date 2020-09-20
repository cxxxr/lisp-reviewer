(in-package :cl-user)
(defpackage :foo
  (:use :cl)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :alexandria
                :when-let)
  (:export SCAN))
(in-package :foo)
