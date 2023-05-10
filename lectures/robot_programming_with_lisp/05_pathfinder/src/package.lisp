(in-package :cl-user)

(defpackage pathfinder
  (:use #:common-lisp)
  (:export
   :*HEADLESS*
   :ENTITY
   :DEPOT
   :WALL
   :TREASURE
   #:ROBOT
   #:COORD
   #:ORIENTATION
   #:DEPOTS
   #:TRUNK
   #:MAKE-COORDINATE
   #:COORDINATE-X
   #:COORDINATE-Y
   #:TREASURES
   #:WORLD
   #:WALLS
   #:INITIALIZE-WORLD
   #:VALID-MOVE
   #:VALID-COORD
   #:COLLECT-TREASURE
   #:DEPOSIT-TREASURE
   #:MOVE
   #:GOAL-REACHED
   #:TURN
   #:FORWARD
   #:IN-PATH
   #:FIND-PATH
   #:STEPS
   #:DEAD-ENDS
   #:STEPS))
