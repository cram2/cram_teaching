(in-package :cl-user)
(defpackage oop-world
  (:use #:common-lisp)
  (:export
   :+directions+
   :*headless*
   treasure-world
   entity
   wall
   treasure
   robot
   #:make-coordinate
   #:coordinate-x
   #:coordinate-y
   #:robot
   #:walls
   #:treasures
   #:coord
   #:world
   #:name
   #:color
   #:orientation
   #:add-to-world
   #:initialize-world
   #:move
   #:get-access-pose
   #:collect-treasure
   #:discover-world))
))