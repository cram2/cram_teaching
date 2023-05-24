(in-package :cl-user)
(defpackage simple-world
  (:use #:common-lisp)
  (:export
   :*headless*
   :*treasures*
   :*treasures-found*
   :*world-map*
   #:discover-world
   #:find-object-coordinates
   #:initialize-walls
   #:initialize-world
   #:move-robot
   #:place-object
   #:place-robot
   #:symbol-at-pos))