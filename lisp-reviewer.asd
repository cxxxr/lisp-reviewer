(defsystem "lisp-reviewer"
  :class :package-inferred-system
  :depends-on ("lisp-reviewer/main")
  :in-order-to ((test-op (test-op "lisp-reviewer/test"))))

(defsystem "lisp-reviewer/test"
  :class :package-inferred-system
  :depends-on ("lisp-reviewer/test/reviewer/defpackage"
               "rove")
  :pathname "test"
  :perform (test-op (o c)
                    (symbol-call :rove :run c)))
