(in-package oop-world)

;;; The treasure-hunt contains code that will move the turtle and collect treasures.
;;;
;;; Please implement the 'oop-world' first, as you can't move anyhing that you can't see.
;;; As soon as the 'visualize-simulation' function makes your world instance appear in the
;;; simulation, you can start moving the robot around.

;; Describes the offset of each direction. Use the orientation as a key to get
;; the corresponding offset like this:
;; (alexandria:assoc-value +directions+ :NORTH)
;; or like this
;; (cdr (assoc :NORTH +directions+))
(alexandria:define-constant +directions+
    '((:NORTH 1 0)
      (:EAST 0 -1)
      (:SOUTH -1 0)
      (:WEST 0 1)) :test 'equal)

;;;;;;;;;;;;;;;;
;; BEGIN move ;;
;; 2P
;;
;; Implement the custom error ROBOT-COLLISION that provides an error message.
;; For its usage, see the MOVE method below.
;;
;; TODO: robot-collision error


(defgeneric move (robot x y orientation)
  (:documentation "Moves the robot to the given x and y coordinates if possible.
The robot can only stand on `free-space', not even under treasures.
Throws a ROBOT-COLLISION error if the target would cause a collision."))

(defmethod move ((robot robot) x y orientation)
  ;; TODO: implement
  )

;; After each call of the MOVE function we want to visualize the world in the simulation again.
;; Overload the method MOVE so the VISUALIZE-SIMULATION method will be called :AFTER each call
;; of the MOVE method.

;; TODO: Overload the MOVE method to VIZUALIZE-SIMULATION :after each call.

;; END move ;;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;; BEGIN collect ;;
;; 3P

(defgeneric get-access-pose (treasure)
  (:documentation "Returns x, y and orientation as a list, from where the treasure is reachable.
If no position can be found, return NIL.")
  (:method ((treasure entity))
    ;; TODO implement
    ))

(defgeneric collect-treasure (robot)
  (:documentation "Collects the treasure laying in front of the robot, if there is any.
Collectiong the treasure means removing it from the world's treasure slot."))

(defmethod collect-treasure ((robot robot))
    ;; TODO implement
    )

;; Like in the MOVE method, overload the COLLECT-TREASURE method, so the VISUALIZE-SIMULATION method
;; will be called :AFTER each call of the COLLECT-TREASURE method.

;; TODO: implement

(defgeneric discover-world (world)
  (:documentation "Moves the robot to each treasure and collects them.
The robot can `get-access-pose' to know, where to land to collect a treasure.")
  (:method ((world treasure-world))
    ;; TODO implement
    ))

