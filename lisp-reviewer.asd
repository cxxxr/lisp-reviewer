(defsystem "lisp-reviewer"
  :class :package-inferred-system
  :depends-on ("lisp-reviewer/main"))

(defsystem "lisp-reviewer/test"
  :class :package-inferred-system
  :depends-on ("lisp-reviewer/test/reviewer/defpackage")
  :pathname "test"
  :perform (test-op (o c)
                    (symbol-call :rove :run c)))