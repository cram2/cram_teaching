(in-package :simple-world)

(define-test test-place-object ()
    "Tests if objects can only be put on ground."
    (initialize-walls)
    (dolist (symbol (concatenate 'list '(r w) *treasures*))
        (setf (aref *world-map* 0 0) symbol)
        (place-object 0 0 'test)
        (unless (assert-eq (aref *world-map* 0 0) symbol)
            (format T "test-place-object: Expected ~a on tile 0 0 but got ~a~%" symbol (aref *world-map* 0 0))))
    (setf (aref *world-map* 2 2) 'g)
    (place-object 2 2 'test)
    (unless (assert-eq (aref *world-map* 2 2) 'test)
        (format T "test-place-object: Expected 'test on tile 2 2 but got ~a~%" (aref *world-map* 2 2))))

(define-test test-place-robot ()
    "Tests if the robot can only be put on ground."
    (initialize-walls)
    (dolist (symbol (concatenate 'list '(r w) *treasures*))
        (setf (aref *world-map* 0 0) symbol)
        (place-robot 0 0)
        (unless (assert-eq (aref *world-map* 0 0) symbol)
            (format T "test-place-robot: Expected ~a on tile 0 0 but got ~a~%" symbol (aref *world-map* 0 0))))
    (setf (aref *world-map* 2 2) 'g)
    (place-robot 2 2)
    (unless (assert-eq (aref *world-map* 2 2) 'r)
        (format T "test-place-robot: Expected 'r on tile 2 2 but got ~a~%" (aref *world-map* 2 2))))

(define-test test-initialize-world ()
    "Tests if the robot and all treasures are placed in the world."
    (initialize-world)
    (let ((n (alexandria:iota (array-total-size *world-map*))))
         (dolist (treasure (cons 'r  *treasures*))
             (unless (assert-true (some #'(lambda (i) (eq treasure (row-major-aref *world-map* i))) n))
                 (format T "test-initialize-world: Object ~a not found in the world.~%" treasure)))))

(define-test test-symbol-at-pos ()
    "Tests if return value is correct."
    (initialize-walls)
    (setf (aref *world-map* 0 0) 'w)
    (unless (assert-true (symbol-at-pos 0 0 'w))
        (format T "test-symbol-at-pos: Expected return value with coordinate 0 0 was T but got nil.~%"))
    (unless (assert-false (symbol-at-pos 0 0 'fail))
        (format T "test-symbol-at-pos: Expected return value with coordinate 0 0 was nil but got T.~%")))

(define-test test-find-object-coordinates ()
    "Tests if coordinates are found correctly."
    (initialize-walls)
    (setf (aref *world-map* 2 2) 'test)
    (unless (assert-equal (find-object-coordinates 'test) '(2 2))
        (format T "test-find-object-coordinate: Expected (2 2) but got ~a.~%" (find-object-coordinates 'test))
        (setf (aref *world-map* 2 11) 'bonus)
        (unless (equal (find-object-coordinates 'bonus) nil)
            (format T "test-find-object-coordinate: Bonus. Wall tiles are not checked in search of the object.~%"))))

(define-test test-move ()
    "Tests if robot won't move on wall tile."
    (initialize-walls)
    (setf *treasures-found* 0
          *treasures* '('test))    
    (move-robot 0 0)
    (unless (assert-eq *treasures-found* 0)
        (format T "test-move-wall: Expected *treasures-found* to be 0 but got ~a.~%" *treasures-found*))
    (unless (assert-eq (aref *world-map* 0 0) 'w)
        (format T "test-move-wall: Expected tile at coordinate 0 0 to be 'w but was ~a.~%" (aref *world-map* 0 0)))
        
    "Tests if robot collects treasure"
    (initialize-walls)
    (setf *treasures-found* 0
          *treasures* '(test)
          (aref *world-map* 2 2) 'test)
    (move-robot 2 2)
    (unless (assert-eq *treasures-found* 1)
        (format T "test-move-treasure: Expected *treasures-found* to be 1 but got ~a.~%" *treasures-found*))
    (unless (assert-eq (aref *world-map* 2 2) 'r)
        (format T "test-move-treasure: Expected tile at coordinate 2 2 to be 'r but was ~a.~%" (aref *world-map* 2 2))))

(define-test test-discover-world ()
    "Tests if the robot collects all treasures"
    (initialize-walls)
    (setf *treasures-found* 0
          *treasures* '(test test2)
          (aref *world-map* 2 2) 'test
          (aref *world-map* 3 3) 'test2)
    (discover-world)
    (unless (assert-eq *treasures-found* 2)
        (format T "test-discover-world: Expected *treasures-found* to be 2 but got ~a.~%" *treasures-found*)))