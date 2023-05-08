(in-package :oop-world-tests)

(setf *print-failures* t
      *print-errors* t
      *headless* t)

(define-test make-coordinate
    (let ((coord (make-coordinate)))
         (assert-eq 0 (coordinate-x coord))
         (assert-eq 0 (coordinate-y coord))))

(define-test make-instance-treasure-world
    (let ((slots (closer-mop:class-direct-slots (find-class 'treasure-world))))
         ;; Check robot
         (assert-equal 'ROBOT (closer-mop:slot-definition-name (first slots)))
         (assert-equal '(ROBOT) (closer-mop:slot-definition-readers (first slots)))         
         (assert-equal 'ROBOT (closer-mop:slot-definition-type (first slots)))
         (assert-equal '(:ROBOT) (closer-mop:slot-definition-initargs (first slots)))
         (assert-equal nil (closer-mop:slot-definition-initform (first slots)))
                       
         ; Check walls
         (assert-equal 'WALLS (closer-mop:slot-definition-name (second slots)))
         (assert-equal '(WALLS) (closer-mop:slot-definition-readers (second slots)))         
         (assert-equal 'LIST (closer-mop:slot-definition-type (second slots)))
         (assert-equal '(:WALLS) (closer-mop:slot-definition-initargs (second slots)))
         (assert-equal '(LIST) (closer-mop:slot-definition-initform (second slots)))
         
         ; Check treasures
         (assert-equal 'TREASURES (closer-mop:slot-definition-name (third slots)))
         (assert-equal '(TREASURES) (closer-mop:slot-definition-readers (third slots)))         
         (assert-equal 'LIST (closer-mop:slot-definition-type (third slots)))
         (assert-equal '(:TREASURES) (closer-mop:slot-definition-initargs (third slots)))
         (assert-equal '(LIST) (closer-mop:slot-definition-initform (third slots)))))

(define-test make-instance-entity
    (let ((slots (closer-mop:class-direct-slots (find-class 'entity))))
         ;; Check coord
         (assert-equal 'COORDINATE (closer-mop:slot-definition-name (first slots)))
         (assert-equal '(COORD) (closer-mop:slot-definition-readers (first slots)))         
         (assert-equal 'COORDINATE (closer-mop:slot-definition-type (first slots)))
         (assert-equal '(:COORD) (closer-mop:slot-definition-initargs (first slots)))
         (assert-equal '(MAKE-COORDINATE) (closer-mop:slot-definition-initform (first slots)))
                       
         ; Check world
         (assert-equal 'WORLD (closer-mop:slot-definition-name (second slots)))
         (assert-equal '(WORLD) (closer-mop:slot-definition-readers (second slots)))         
         (assert-equal 'TREASURE-WORLD (closer-mop:slot-definition-type (second slots)))
         (assert-equal '(:WORLD) (closer-mop:slot-definition-initargs (second slots)))
         (assert-equal nil (closer-mop:slot-definition-initform (second slots)))
         
         ; Check name
         (assert-equal 'NAME (closer-mop:slot-definition-name (third slots)))
         (assert-equal '(NAME) (closer-mop:slot-definition-readers (third slots)))         
         (assert-equal 'SYMBOL (closer-mop:slot-definition-type (third slots)))
         (assert-equal '(:NAME) (closer-mop:slot-definition-initargs (third slots)))
         (assert-equal nil (closer-mop:slot-definition-initform (third slots)))))
    
(define-test make-instance-treasure
    (let ((slots (closer-mop:class-direct-slots (find-class 'treasure))))
         (assert-equal :RED (closer-mop:slot-definition-initform (first slots)))))

(define-test make-instance-robot
    (let ((slots (closer-mop:class-direct-slots (find-class 'robot))))
         (assert-equal :NORTH (closer-mop:slot-definition-initform (first slots)))))

(define-test add-to-world-treasure
    (let ((world (make-instance 'treasure-world))
          (treasure (make-instance 'treasure)))
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