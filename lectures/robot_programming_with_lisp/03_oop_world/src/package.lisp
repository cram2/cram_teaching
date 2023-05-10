(in-package :cl-user)
(defpackage oop-world
  (:use #:common-lisp)
  (:export
   :+DIRECTIONS+
   :*HEADLESS*
   :TREASURE-WORLD
   :ENTITY
   :WALL
   :TREASURE
   :ROBOT
   #:MAKE-COORDINATE
   #:COORDINATE-X
   #:COORDINATE-Y
   #:ROBOT
   #:WALLS
   #:TREASURES
   #:COORD
   #:WORLD
   #:NAME
   #:COLOR
   #:ORIENTATION
   #:ADD-TO-WORLD
   #:INITIALIZE-WORLD
   #:MOVE
   #:GET-ACCESS-POSE
   #:COLLECT-TREASURE
   #:DISCOVER-WORLD))