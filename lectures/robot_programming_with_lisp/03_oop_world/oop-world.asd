(defsystem oop-world
  :author "artnie"
  :depends-on (bullet-wrapper)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "oop-world" :depends-on ("package"))
     (:file "treasure-hunt" :depends-on ("package"
                                         "oop-world"))))))
