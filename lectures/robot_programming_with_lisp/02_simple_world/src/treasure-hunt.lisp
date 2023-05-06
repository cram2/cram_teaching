(in-package simple-world)

;;; Welcome to Assignment 2 of the "Robot Programming with Lisp" course.
;;; (7 Points)
;;;
;;;
;;; ## About the world
;;;
;;; In this assignment you have a robot living in a flat 2-dimensional world.
;;; This world only consist of symbols:
;;; robot (R), ground (G), walls (W) and treasures (A, B, C, D, E, F).
;;; Initially, the world only has ground and walls.
;;; It's state is kept in the variable *world-map*
;;; That *world-map* is a 2D array.
;;; The simulation reads and visualizes the array's content.
;;;
;;;
;;; ## Your task
;;;
;;; Your assignment is split in two mayor tasks:
;;; 1. Initializing the world by randomly placing treasures and the robot
;;;    on free ground-tiles in *world-map*
;;; 2. Moving the robot around to collect all treasures.
;;;
;;; Implement all the functions containing a TODO.
;;; To reset the world call (initialize-world)
;;; You main-function for this program is (discover-world),
;;;   which you should implement last.

(defvar *world-map*
  #2A((w w w w w w w w w w w w w w w w)
      (w w g g g g g w g g g w g g g w)
      (w w g g g g g w g g g w g g g w)
      (w w w w g w w w w g w w w w g w)
      (w g g g g g g g g g g g g g g w)
      (w w w w g w w w w w w g w w w w)
      (w g g w g w g g g w g g g w w w)
      (w g g w g w g g g w g g g w w w)
      (w g g g g g g g g w g g g w w w)
      (w g g w g w g g g w w w w w w w)
      (w w w w g w w w w w w w w w w w)
      (w g g w g w g g g w w w w w w w)
      (w g g g g w g g g w w w w w w w)
      (w g g w g g g g g w w w w w w w)
      (w w w w w w w w w w w w w w w w))
  "2D grid world.
The following symbols are used to represent things in the world:
- W for wall cell
- G for ground
- R for robot
- A, B, C, D, E, F for treasures.")

(defconstant +treasure-num+ 4
  "The maximum number of treasures that exist in the world.")

(defparameter *treasures* '()
  "The list of treasures in the world.")

(defun reset-treasures-list ()
  "Randomly draws 4 treasures and puts them into `*treasures*'."
  (setf *treasures*
        (subseq (alexandria:shuffle '(a b c d e f)) 0 +treasure-num+)))

(defvar *treasures-found* 0
  "The current amount of treasures found by the robot.")

(defvar *robot-coords* '(nil nil)
  "List of length 2, containing the x and y coordinate of the robot.")

;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN assignment ;;
(defun valid-coords (x y)
  "Checks if x and y are within bounds of `*world-map*'."
  ;; TODO implement 
    )

(defun symbol-at-pos (x y symbol)
  "When x y are valid, checks if the symbol at x y equals the given `symbol'."
  ;; TODO implement 
    )

(defun place-object (x y symbol)
  "When the given coordinates x y contain ground, replace it with the `symbol'."
  ;; TODO implement 
    )

(defun place-robot (x y)
  "Uses `place-object' to set the robot symbol 'r in the `*world-map*'.
When successful, updates the robot's coordinates in `*robot-coords*'."
  ;; TODO Implement 
    )

(defun initialize-world ()
  "This function initializes the 2D array map.
0. Resets walls, the simulation and the treasures list. (already implemented)
1. Places all symbols in `*treasures*' at random positions on the `*world-map*'.
2. Places the robot on a random place in the `*world-map*'.
3. Resets the counter `*treasures-found*'."
  (initialize-walls)
  (btr-wrapper:init-world))

(defun find-object-coordinates (symbol)
  "Searches `*world-map*' for the given `symbol' and returns it's coordinates."
  ;; TODO Implement 
    )

(defun move-robot (x y)
  "Moves the robot to x y in the world and updates `*robot-coords*'.
If the desired position is occupied by a treasure: clear the space,
increase `*treasures-found*', then move the robot there."
  ;; TODO implement 
    )

(defun discover-world ()
  "Moves the robot in the world until all the treasure is collected.
The robot has access to the world state `*world-map*' and therefore the
coordinates of treasures. Use `move-robot' to travel to each treasure, which
collects the treasure above the robot."
  ;; TODO implement 
    )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visualize the world ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-walls ()
  "Cleans the *world-map* array and initializes the walls and the ground."
  (setf *world-map* (make-array '(15 16) :initial-element 'g))
  (loop for i from 0 to 14
        do (setf (aref *world-map* i 0) 'w)
           (setf (aref *world-map* i 15) 'w)
           (setf (aref *world-map* 0 i) 'w)
           (setf (aref *world-map* 14 i) 'w))
  (mapcar (lambda (x-y)
            (setf (aref *world-map* (first x-y) (second x-y)) 'w))
          '((0 15)
            (1 1) (1 7) (1 11)
            (2 1) (2 7) (2 11)
            (3 1) (3 2) (3 3) (3 5) (3 6) (3 7) (3 8) (3 10) (3 11) (3 12)
            (3 13)
            (5 1) (5 2) (5 3) (5 5) (5 6) (5 7) (5 8) (5 9) (5 10) (5 12) (5 13)
            (5 14)
            (6 3) (6 5) (6 9) (6 13) (6 14)
            (7 3) (7 5) (7 9) (7 13) (7 14)
            (8 9) (8 13) (8 14)
            (9 3) (9 5) (9 9) (9 10) (9 11) (9 12) (9 13) (9 14)
            (10 1) (10 2) (10 3) (10 5) (10 6) (10 7) (10 8) (10 9) (10 10)
            (10 11) (10 12) (10 13) (10 14)
            (11 3) (11 5) (11 9) (11 10) (11 11) (11 12) (11 13) (11 14)
            (12 5) (12 9) (12 10) (12 11) (12 12) (12 13) (12 14)
            (13 3) (13 9) (13 10) (13 11) (13 12) (13 13) (13 14)
            (14 15))))

(defun visualize-simulation ()
  "Visualizes the current 2D-Array world in the simulation.
Call this function, every time the simulation needs to be updated."
  (let ((x-max (1- (first (array-dimensions *world-map*))))
        (y-max (1- (second (array-dimensions *world-map*)))))
    (if (not btr-wrapper:*world-initialized*)
        ;; initialize
        (progn
          (btr-wrapper:init-world)
          (loop for x to x-max do
            (loop for y to y-max
                  unless (or (eq (aref *world-map* x y) 'g)
                             (eq (aref *world-map* x y) 'w)) do
                    (btr-wrapper:spawn x y (aref *world-map* x y))))
          (setf btr-wrapper:*world-initialized* t))
        ;; update
        (progn
          (if (first *robot-coords*)
              ;; Teleport the robot
              (btr-wrapper:teleport-turtle (first *robot-coords*)
                                           (second *robot-coords*))
              (warn "Parameter *robot-coords* needs robot's x y coordinates."))
          (when (<= -2 (- +treasure-num+ *treasures-found*) +treasure-num+)
            ;; Remove treasures if necessary
            (loop for x to x-max do
              (loop for y to y-max
                    when (and (member (aref *world-map* x y) '(r g w))
                              (btr:object btr:*current-bullet-world*
                                          (intern (format nil "TREASURE~a-~a" x y))))
                      do (btr-utils:kill-object (intern (format nil "TREASURE~a-~a" x y))))))))))
