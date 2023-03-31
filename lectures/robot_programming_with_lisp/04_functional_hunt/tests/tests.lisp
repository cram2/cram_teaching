(in-package :functional-hunt)

(define-test move
    "Tests if the robot moves to a valid pose."
   (let ((coord-test (make-coordinate :x 1 :y 3))    
           (orientation-test :SOUTH)
           (robot (robot (initialize-world))))
          (move robot (coordinate-x coord-test) (coordinate-y coord-test) orientation-test)
           (unless (and (assert-equalp (coord robot) coord-test)
                        (assert-eq (orientation robot) orientation-test))
               (format T "test-move-valid: Expected pose (~a ~a) but got (~a ~a)~%" coord-test orientation-test (coord robot) (orientation robot))))

    "Tests if the robot only updates its position if the orientation is wrong."
    (let* ((coord-test (make-coordinate :x 2 :y 3))
           (robot (robot (initialize-world)))
           (orientation-robot (orientation robot)))
          (move robot (coordinate-x coord-test) (coordinate-y coord-test) :MOSS)
         (unless (and (assert-equalp (coord robot) coord-test)
                      (assert-eq (orientation robot) orientation-robot))
              (format T "test-move-orientation: Expected pose (~a ~a) but got (~a ~a)~%" coord-test orientation-robot (coord robot) (orientation robot))))

    "Tests if the robot only change its orientation if the coordinate is wrong."
    (let* ((robot (robot (initialize-world)))
           (coord-robot (coord robot))
           (orientation-test :NORTH))
          (move robot -1 -2 orientation-test)
           (unless (and (assert-equalp (coord robot) coord-robot)
                        (assert-eq (orientation robot) orientation-test))
              (format T "test-move-coordinate: Expected pose (~a ~a) but got (~a ~a)~%" coord-robot orientation-test (coord robot) (orientation robot)))))
    

(define-test collect-treasure 
    "Tests if the robot collects a treasure infront of it. Depends on 'get-access-pose' and 'move'."
    (let* ((world (initialize-world))
           (treasures (treasures world)))
          (collect-treasure (robot world))
         (unless (assert-eq (- (length treasures) 1) (length (treasures world)))
             (format T "test-collect-treasure: Expected ~a treasures remaining but got ~a treasures remaining.~%" (- (length treasures) 1) (length (treasures world))))))

(define-test distance
    "Tests geometric distance between two coordinates. Tolerance of 0.1 ."
    (let ((x (make-instance 'entity :coordinate (make-coordinate :x 1 :y 1)))
          (y (make-instance 'entity :coordinate (make-coordinate :x 3 :y 3))))
     (unless (assert-true (< (abs (- (distance x y) 2.828427)) 0.1))
         (format T "test-distance: Expected ~a but got ~a.~%" 2.828427 (distance x y)))))

(define-test closer
    "Tests closer function by comparing treasure three and four."
    (let ((world (initialize-world)))
         (unless (assert-equalp (third (treasures world))
                     (closer (robot world) (third (treasures world)) (fourth (treasures world))))
             (format T "test-closer: Expected third treasure but got fourth treasure.~%"))))

(define-test sort-treasures-by-distance
    "Tests if all treasures are sorted by distance. Depended on 'distance'"
    (let ((world (initialize-world)))
         (unless (assert-true (apply  #'<= (mapcar (alexandria:curry #'distance (robot world)) (sort-treasures-by-distance world))))
             (format T "test-sort-treasures-by-distance: Expected treasures to be sorted but got treasures sorted like this: ~a.~%" (mapcar (alexandria:curry #'distance (robot world)) (sort-treasures-by-distance world))))))

(define-test get-access-pose
    "Tests access pose of first treasure"
    (let ((access-pose (multiple-value-list (get-access-pose (first (treasures (initialize-world)))))))
         (unless (assert-true (member access-pose '((1 3 :EAST) (2 2 :SOUTH)) :test #'equal))
             (format T "test-get-access-pose: Expected access-pose to be member of '((1 3 :EAST) (2 2 :SOUTH)) but got ~a.~%" access-pose))))

(define-test closest-accessible-treasure
    "Tests closest accessible treasure."
    (let* ((world (initialize-world))
           (treasure-distance (distance (robot world) (closest-accessible-treasure world))))
          (unless (assert-eq 1.0 treasure-distance)
              (format T "test-closest-accessible-treasure: Expected treasure with distance 1.0 but got ~a.~%" treasure-distance))))

(define-test test-discover-world
    "Tests if the robot collects all treasures"
    (let ((world (initialize-world)))
         (discover-world world)
         (unless (assert-false (treasures world))
             (format T "test-discover-world: Expected 0 treasures remaining but got ~a treasures remaining.~%" (length (treasures world))))))
