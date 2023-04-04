(in-package :oop-world)

(define-test make-coordinate
    (let ((coord (make-coordinate)))
         (assert-eq 0 (coordinate-x coord))
         (assert-eq 0 (coordinate-y coord))))

(define-test make-instance-treasure-world
    (let ((world (make-instance 'treasure-world)))
         (assert-false (robot world))
         (assert-false (walls world))
         (assert-false (treasures world))))

(define-test make-instance-entity
    (let ((entity (make-instance 'entity)))
         (assert-true (coord entity))
         (assert-false (world entity))
         (assert-false (name entity))))
    
(define-test make-instance-treasure
    (let ((treasure (make-instance 'treasure)))
         (assert-eq :RED (color treasure))))

(define-test make-instance-robot
    (let ((robot (make-instance 'robot)))
         (assert-eq :NORTH (orientation robot))))

(define-test add-to-world-treasure
    (let ((world (make-instance 'treasure-world))
          (treasue (make-instance 'treasure)))
         (add-to-world treasure world)
         (assert-true (member treasure (treasures world)))))
         
(define-test add-to-world-robot
    (let ((world (make-instance 'treasure-world))
          (robot (make-instance 'robot)))
         (add-to-world robot world)
         (assert-equal robot (robot world))))

(define-test initialize-treasure
    (assert-error 'error (make-instance 'treasure :color :GREEN))
    (assert-true (make-instance 'treasure :color :BLUE))
    (assert-true (make-instance 'treasure :color :RED)))

(define-test initialize-robot
    (assert-error 'error (make-instance 'robot :orientation :BELOW))
    (assert-true (make-instance 'robot :orientation :NORTH))
    (assert-true (make-instance 'robot :orientation :EAST))
    (assert-true (make-instance 'robot :orientation :SOUTH))
    (assert-true (make-instance 'robot :orientation :WEST)))

(define-test move
    "Tests if the robot moves to a valid pose."
     (let ((coord-test (make-coordinate :x 1 :y 2))    
           (orientation-test :WEST)
           (robot (robot (initialize-world))))
          (setf (treasures (world robot)) '())
          (move robot (coordinate-x coord-test) (coordinate-y coord-test) orientation-test)
          (assert-true (and (equalp (coord robot) coord-test)
                            (eq (orientation robot) orientation-test))))
    
    "Tests if the robot won't change its pose if the orientation is wrong."
    (let* ((robot (robot (initialize-world)))
           (coord-robot (coord robot))
           (orientation-robot (orientation robot)))
          (move robot (coordinate-x coord-robot) (coordinate-y coord-robot) :MOSS)
          (assert-true (and (equalp (coord robot) coord-robot)
                            (eq (orientation robot) orientation-robot))))
    
    "Tests if the robot won't change its pose if the coordinate is wrong."
    (let* ((robot (robot (initialize-world)))
           (coord-robot (coord robot))
           (orientation-robot (orientation robot)))
          (move robot -1 -2 :WEST)
          (assert-true (and (equalp (coord robot) coord-robot)
                            (eq (orientation robot) orientation-robot))))
    
    "Tests if the robot won't change its pose if the coordinate contains an obstacle."
    (let* ((robot (robot (initialize-world)))
           (coord-robot (coord robot))
           (orientation-robot (orientation robot)))
          (move robot 0 0 :WEST)
          (assert-true (and (equalp (coord robot) coord-robot)
                            (eq (orientation robot) orientation-robot)))))
    
(define-test get-access-pose
    (let* ((treasure (car (treasures (initialize-world))))
           (access-pose (get-access-pose treasure))
           (coord-infront (make-coordinate :x (+ (first access-pose) (second (assoc (third access-pose) +directions+)))
                                           :y (+ (second access-pose) (third (assoc (third access-pose) +directions+))))))
          (assert-equalp coord-infront (coord treasure))))
    
(define-test collect-treasure 
    (let* ((world (initialize-world))
           (treasures (treasures world))
           (pose (get-access-pose (car treasures))))
          (move (robot world) (first pose) (second pose) (third pose))
          (collect-treasure (robot world))
          (assert-eq (- (length treasures) 1) (length (treasures world)))))

(define-test discover-world 
    (let ((world (initialize-world)))
         (discover-world world)
         (assert-false (treasures world))))