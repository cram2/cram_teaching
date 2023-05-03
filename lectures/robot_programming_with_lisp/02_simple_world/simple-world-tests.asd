(defsystem simple-world-tests
  :author "Stefan Eirich"
  :depends-on (lisp-unit)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "tests" :depends-on ("package")))))
  :perform (test-op (operation component)
                    (symbol-call :lisp-unit '#:run-tests :all :simple-world-tests)))