(in-package :turtle-chase)

;;; This file will contain the 'follower' behaviour.

;; TODO implement

(defparameter *transform-listener* nil)

(defun init-listener ()
  "Initializes the *transform-listener*"
  )

(defun set-turtle-velocity (name &key (lin 0) (ang 0))
  "Publishes a velocity command once."
  )

(defun follow-turtle-in-a-loop (&optional (follower "turtle2") (followee "turtle1"))
  "Calculates the velocities for the follower. Sets the turtle velocity in a loop."
   )

