(defsystem oop-world-tests
  :author "Stefan Eirich"
  :depends-on (bullet-wrapper lisp-unit)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "tests" :depends-on ("package"))))
               (:module "src"
                :components
                ((:file "package")
                 (:file "treasure-hunt" :depends-on ("package")))))
  :perform (test-op (operation component)
                    (symbol-call :lisp-unit '#:run-tests :all :oop-world)))