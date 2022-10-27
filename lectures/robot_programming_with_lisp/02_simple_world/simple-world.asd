(defsystem simple-world
  :author "aniedz"
  :depends-on (bullet-wrapper)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "treasure-hunt" :depends-on ("package"))))))
