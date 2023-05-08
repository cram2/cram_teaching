(in-package :cl-user)

(defpackage functional-hunt
  (:use #:common-lisp)
  (:export
   :*headless*
   ENTITY
   ROBOT
   TREASURES
   #:MAKE-COORDINATE
   #:COORDINATE-X
   #:COORDINATE-Y
   #:COORD
   #:ORIENTATION
   #:INITIALIZE-WORLD
   #:MOVE
   #:COLLECT-TREASURE
   #:DISTANCE
   #:CLOSER
   #:SORT-TREASURES-BY-DISTANCE
   #:GET-ACCESS-POSE
   #:CLOSEST-ACCESSIBLE-TREASURE
   #:DISCOVER-WORLD))
