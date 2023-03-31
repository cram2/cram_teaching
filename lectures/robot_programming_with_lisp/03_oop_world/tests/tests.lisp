(in-package :oop-world)

(define-test initialize-treasure
    "Only :RED and :BLUE treasures are valid"
    (when (and (assert-error 'error (make-instance 'treasure :color :GREEN))
               (assert-true (make-instance 'treasure :color :BLUE))
               (assert-true (make-instance 'treasure :color :RED)))
        (format T "Bonus. Only :RED and :BLUE treasures are possible.~%")))

(define-test initialize-robot
    "Only :NORTH, :EAST, :SOUTH and :WEST are valid orientations"
    (when (and (assert-error 'error (make-instance 'robot :orientation :BELOW))
               (assert-true (make-instance 'robot :orientation :NORTH))
               (assert-true (make-instance 'robot :orientation :EAST))
               (assert-true (make-instance 'robot :orientation :SOUTH))
               (assert-true (make-instance 'robot :orientation :WEST)))
        (format T "Bonus. Only :NORTH, :EAST, :SOUTH and :WEST are possible orientations.")))

(define-test move
    "Tests if the robot moves to a valid pose."
     (let ((coord-test (make-coordinate :x 1 :y 2))    
           (orientation-test :WEST)
           (robot (robot (initialize-world))))
          (setf (treasures (world robot)) '())
          (move robot (coordinate-x coord-test) (coordinate-y coord-test) orientation-test)
           (unless (assert-true (and (equalp (coord robot) coord-test)
                     (eq (orientation robot) orientation-test)))
               (format T "test-move-valid: Expected pose (~a ~a) but got (~a ~a)~%" coord-test orientation-test (coord robot) (orientation robot))))

    "Tests if the robot won't change its pose if the orientation is wrong."
    (let* ((robot (robot (initialize-world)))
           (coord-robot (coord robot))
           (orientation-robot (orientation robot)))
          (move robot (coordinate-x coord-robot) (coordinate-y coord-robot) :MOSS)
          (unless (assert-true (and (equalp (coord robot) coord-robot)
                       (eq (orientation robot) orientation-robot)))
              (format T "test-move-orientation: Expected pose (~a ~a) but got (~a ~a)~%" coord-robot orientation-robot (coord robot) (orientation robot))))

    "Tests if the robot won't change its pose if the coordinate is wrong."
    (let* ((robot (robot (initialize-world)))
           (coord-robot (coord robot))
           (orientation-robot (orientation robot)))
          (move robot -1 -2 :WEST)
          (unless (assert-true (and (equalp (coord robot) coord-robot)
                       (eq (orientation robot) orientation-robot)))
              (format T "test-move-coordinate: Expected pose (~a ~a) but got (~a ~a)~%" coord-robot orientation-robot (coord robot) (orientation robot))))
    
    "Tests if the robot won't change its pose if the coordinate contains an obstacle."
    (let* ((robot (robot (initialize-world)))
           (coord-robot (coord robot))
           (orientation-robot (orientation robot)))
          (move robot 0 0 :WEST)
          (unless (assert-true (and (equalp (coord robot) coord-robot)
                       (eq (orientation robot) orientation-robot)))
              (format T "test-move-obstacle: Expected pose (~a ~a) but got (~a ~a)~%" coord-robot orientation-robot (coord robot) (orientation robot)))))

(define-test get-access-pose
    "Tests if the access pose of a treasure is calculated correctly."
    (let* ((treasure (car (treasures (initialize-world))))
           (access-pose (get-access-pose treasure))
           (coord-infront (make-coordinate :x (+ (first access-pose) (second (assoc (third access-pose) +directions+)))
                                           :y (+ (second access-pose) (third (assoc (third access-pose) +directions+))))))
          (unless (assert-equalp coord-infront (coord treasure))
              (format T "test-get-access-pose: Expected coordinate infront of robot ~a but got ~a~%" (coord treasure) coord-infront))))

(define-test collect-treasure 
    "Tests if the robot collects a treasure infront of it. Depends on 'get-access-pose' and 'move'."
    (let* ((world (initialize-world))
           (treasures (treasures world))
           (pose (get-access-pose (car treasures))))
          (move (robot world) (first pose) (second pose) (third pose))
          (collect-treasure (robot world))
         (unless (assert-eq (- (length treasures) 1) (length (treasures world)))
             (format T "test-collect-treasure: Expected ~a treasures remaining but got ~a treasures remaining.~%" (- (length treasures) 1) (length (treasures world))))))

(define-test discover-world 
    "Tests if the robot collects all treasures"
    (let ((world (initialize-world)))
         (discover-world world)
         (unless (assert-false (treasures world))
             (format T "test-discover-world: Expected 0 treasures remaining but got ~a treasures remaining.~%" (length (treasures world))))))