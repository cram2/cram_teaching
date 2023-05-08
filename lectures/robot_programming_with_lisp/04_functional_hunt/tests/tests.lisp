(in-package :functional-hunt-tests)

(setf *print-failures* t
      *print-errors* t
      *headless* t)

(define-test move
    "Tests if the robot moves to a valid pose."
    (let ((coord-test (make-coordinate :x 1 :y 3))    
          (orientation-test :SOUTH)
          (robot (robot (initialize-world))))
         (move robot (coordinate-x coord-test) (coordinate-y coord-test) orientation-test)
         (assert-equalp coord-test (coord robot))
         (assert-eq orientation-test (orientation robot)))

    "Tests if the robot only updates its position if the orientation is wrong."
    (let* ((coord-test (make-coordinate :x 2 :y 3))
           (robot (robot (initialize-world)))
           (orientation-robot (orientation robot)))
          (move robot (coordinate-x coord-test) (coordinate-y coord-test) :MOSS)
          (assert-equalp coord-test (coord robot))
          (assert-eq orientation-robot (orientation robot)))

    "Tests if the robot only change its orientation if the coordinate is wrong."
    (let* ((robot (robot (initialize-world)))
           (coord-robot (coord robot))
           (orientation-test :NORTH))
          (move robot -1 -2 orientation-test)
          (assert-equalp coord-robot (coord robot))
          (assert-eq orientation-test (orientation robot))))

(define-test collect-treasure
    (let* ((world (initialize-world))
           (treasures (treasures world)))
          (collect-treasure (robot world))
          (assert-eq (- (length treasures) 1) (length (treasures world)))))

(define-test distance
    (let ((x (make-instance 'entity :coordinate (make-coordinate :x 1 :y 1)))
          (y (make-instance 'entity :coordinate (make-coordinate :x 3 :y 3))))
         (assert-true (< (abs (- (distance x y) 2.828427)) 0.1))))

(define-test closer
    (let ((world (initialize-world)))
         (assert-equalp (third (treasures world)) (closer (robot world) (third (treasures world)) (fourth (treasures world))))))

(define-test sort-treasures-by-distance
    (let ((world (initialize-world)))
         (assert-true (apply  #'<= (mapcar (alexandria:curry #'distance (robot world)) (sort-treasures-by-distance world))))))

(define-test get-access-pose
    (let ((access-pose (multiple-value-list (get-access-pose (first (treasures (initialize-world)))))))
         (assert-true (member access-pose '((1 3 :EAST) (2 2 :SOUTH)) :test #'equal))))

(define-test closest-accessible-treasure
    (let* ((world (initialize-world))
           (treasure-distance (distance (robot world) (closest-accessible-treasure world))))
          (assert-eq 1.0 treasure-distance)))

(define-test discover-world
    (let ((world (initialize-world)))
         (discover-world world)
         (assert-false (treasures world))))
