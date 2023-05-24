(in-package pathfinder-tests)

(setf *print-failures* t
      *print-errors* t
      *headless* t)

(define-test valid-move
    (let ((robot (robot (initialize-world))))
         (setf (coord robot) (make-coordinate :x 0 :y 0)
               (orientation robot) :NORTH)
         
         (assert-true (valid-move robot 0 0 :NORTH)) "Nothing changes"
         (assert-true (valid-move robot 1 0 :NORTH)) "Step forward"
         (assert-true (valid-move robot 0 0 :EAST)) "Turn right"
         (assert-false (valid-move robot 2 0 :NORTH)) "Invalid step"
         (assert-false (valid-move robot 1 0 :EAST)) "Changing both" ))

(define-test valid-coord
    (let ((world (initialize-world)))
         (setf (coord (robot world)) (make-coordinate :x 1 :y 1)
               (depots world) (make-hash-table)
               (gethash :RED (depots world)) (make-instance 'depot :color :RED :coordinate (make-coordinate :x 1 :y 2))
               (walls world) (list (make-instance 'wall :coordinate (make-coordinate :x 1 :y 3)))
               (treasures world) (list (make-instance 'treasure :coordinate (make-coordinate :x 1 :y 4))))
         
         (assert-true (valid-coord 1 1 world))
         (assert-true (valid-coord 2 2 world))
         (assert-false (valid-coord 1 2 world))
         (assert-false (valid-coord 1 3 world))
         (assert-false (valid-coord 1 4 world))
         (assert-false (valid-coord -1 -1 world))))

(define-test collect-treasure
    (let ((world (initialize-world))
          (treasure (make-instance 'entity :coordinate (make-coordinate :x 1 :y 0))))
         (setf (treasures world) (list treasure)
               (coord (robot world)) (make-coordinate :x 0 :y 0)
               (orientation (robot world)) :NORTH
               (trunk (robot world)) (make-array 2 :initial-contents '(nil nil)))
         
         "Tests if first treasure can be collectd."
         (collect-treasure (robot world))
         (assert-false (treasures world))
         (assert-true (aref (trunk (robot world)) 0))

         "Tests if second treasure can be collected."
         (setf (treasures world) (list treasure))
         (collect-treasure (robot world))
         (assert-false (treasures world))
         (assert-true (aref (trunk (robot world)) 1))
        
         "Tests if a third treasure won't be collected."
         (setf (treasures world) (list treasure))
         (collect-treasure (robot world))
         (assert-true (treasures world))))

(define-test deposit-treasure
    (let ((robot (robot (initialize-world))))
         
         "Deposit :RED treasure"
         (setf (coord robot) (make-coordinate :x 0 :y 0)
               (orientation robot) :NORTH
               (coord (gethash :RED (depots (world robot)))) (make-coordinate :x 1 :y 0)
               (aref (trunk robot) 0) (make-instance 'treasure :color :RED)
               (aref (trunk robot) 1) (make-instance 'treasure :color :BLUE))
         
         (deposit-treasure robot)
         (assert-false (aref (trunk robot) 0))
         (assert-true (aref (trunk robot) 1))
         
         "Deposit :BLUE treasure"
         (setf (coord (gethash :BLUE (depots (world robot)))) (make-coordinate :x 1 :y 0)
               (aref (trunk robot) 0) (make-instance 'treasure :color :RED)
               (aref (trunk robot) 1) (make-instance 'treasure :color :BLUE))
         
         (deposit-treasure robot)
         (assert-true (aref (trunk robot) 0))
         (assert-false (aref (trunk robot) 1))))

(define-test goal-reached
    (let ((treasure (make-instance 'entity :coordinate (make-coordinate :x 1 :y 2))))
         (assert-true (goal-reached 1 1 :WEST treasure))
         (assert-false (goal-reached 1 1 :EAST treasure))))

(define-test turn
    (assert-eq :EAST (turn :NORTH :RIGHT))
    (assert-eq :SOUTH (turn (turn :NORTH :RIGHT) :RIGHT))
    (assert-eq :WEST (turn :NORTH :LEFT))
    (assert-eq :SOUTH (turn (turn :NORTH :LEFT) :LEFT)))

(define-test forward
    (assert-equal '(2 3 :NORTH) (multiple-value-list (forward 1 3 :NORTH))))
    
(define-test in-path
    (assert-true (in-path 1 3 :NORTH '((2 3 :NORTH) (1 3 :NORTH))))
    (assert-false (in-path 1 3 :WEST '((2 3 :NORTH) (1 3 :NORTH)))))

(define-test move
    (let ((world (initialize-world))
          (steps (steps)))
         (setf (depots world) (make-hash-table)
               (gethash :RED (depots world)) (make-instance 'depot :color :RED :coordinate (make-coordinate :x 2 :y 2))
               (walls world) (list (make-instance 'wall :coordinate (make-coordinate :x 1 :y 3)))
               (treasures world) (list (make-instance 'treasure :coordinate (make-coordinate :x 1 :y 4)))
               (coord (robot world)) (make-coordinate :x 1 :y 1)
               (orientation (robot world)) :NORTH)
         
         (move (robot world) 1 1 :NORTH)
         (assert-eq (incf steps) (steps))
         
         (move (robot world) 2 1 :NORTH)
         (assert-eq (incf steps) (steps))
         
         (move (robot world) 2 1 :WEST) 
         (assert-eq (incf steps) (steps))
         
         (assert-error 'error (move (robot world) 2 2 :WEST))
         (assert-eq steps (steps))))

(define-test find-path
    (let* ((world (initialize-world))
           (coords (coord (robot world)))
           (x (coordinate-x coords))
           (y (coordinate-y coords))
           (orientation (orientation (robot world))))
         (dolist (treasure (treasures world))
             (let* ((path (find-path x y orientation treasure))
                    (access-pose (car (last path))))
                   (when access-pose
                       "Checks if first element in path is reachable from the robots position."
                       (assert-true (valid-move (robot world) (first (car path)) (second (car path)) (third (car path))))
                       "Checks if last element in path is valid access pose."
                       (assert-true (goal-reached (first access-pose) (second access-pose) (third access-pose) treasure))
                       "Checks if every step is valid."
                       (assert-true (every #'(lambda (x y) (valid-move (make-instance 'robot
                                                                                      :coordinate (make-coordinate :x (first x) :y (second x))
                                                                                      :orientation (third x))
                                                                       (first y) (second y) (third y))) path (rest path))))))))

(define-test dead-ends
    (assert-true (dead-ends)))

(define-test steps
    (assert-true (steps)))