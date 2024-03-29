
(asdf:defsystem turtle-goal-referee
  :depends-on (roslisp turtle_actionlib-msg
                       geometry_msgs-msg
                       std_msgs-msg
                       visualization_msgs-msg
                       tf2_msgs-msg
                       turtlesim-srv
                       cl-tf
                       roslisp-utilities
                       alexandria
                       cram-language)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "rooms" :depends-on ("package"))
             (:file "goal-publisher" :depends-on ("package" "rooms"))))))
