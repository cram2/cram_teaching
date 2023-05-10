(in-package :cl-user)

(defpackage optimal-path
  (:use #:common-lisp)
  (:export
   :*HEADLESS*
   #:DISCOVER-WORLD
   #:EXPAND
   #:FIND-PATH
   #:GOAL-REACHED
   #:INITIALIZE-WORLD
   #:MAKE-COORDINATE
   #:REMOVE-UNREACHABLE-TREASURES
   #:ROBOT
   #:TREASURES
   #:VALID-MOVE))