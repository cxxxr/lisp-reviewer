(defsystem "lisp-reviewer"
  :class :package-inferred-system
  :depends-on ("lisp-reviewer/main"))

(defsystem "lisp-reviewer/test"
  :perform (test-op (o c) (symbol-call :lisp-reviewer/test/all :run)))
