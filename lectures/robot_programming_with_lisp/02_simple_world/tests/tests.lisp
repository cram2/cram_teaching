(in-package :simple-world)

(define-test place-object
    (initialize-walls)
    "Iterates over all objects and checks if PLACE-OBJECT doesn't overwrite them."
    (dolist (symbol (concatenate 'list '(r w) *treasures*))
        (setf (aref *world-map* 0 0) symbol)
        (place-object 0 0 'test)
        (assert-eq symbol (aref *world-map* 0 0)))
    
    "GROUND is cool though."
    (setf (aref *world-map* 0 0) 'g)
    (place-object 0 0 'test)
    (assert-eq 'test (aref *world-map* 0 0)))

(define-test place-robot
    "Iterates over all objects and checks if PLACE-ROBOT doesn't overwrite them."
    (initialize-walls)
    (dolist (symbol (concatenate 'list '(r w) *treasures*))
        (setf (aref *world-map* 0 0) symbol)
        (place-robot 0 0)
        (assert-eq symbol (aref *world-map* 0 0)))
    
    "GROUND is cool though."
    (setf (aref *world-map* 0 0) 'g)
    (place-robot 0 0)
    (assert-eq 'r (aref *world-map* 0 0)))

(define-test initialize-world
    (initialize-world)
    "Checks if all symbols are placed."
    (let ((n (alexandria:iota (array-total-size *world-map*))))
         (dolist (treasure (cons 'r  *treasures*))
             (assert-true (some #'(lambda (i) (eq treasure (row-major-aref *world-map* i))) n)))))

(define-test symbol-at-pos
    (initialize-walls)
    (setf (aref *world-map* 0 0) 'w)
    (assert-true (symbol-at-pos 0 0 'w))
    (assert-false (symbol-at-pos 0 0 'fail)))

(define-test find-object-coordinates
    (initialize-walls)
    (setf (aref *world-map* 0 0) 'test)
    (assert-equal '(0 0) (find-object-coordinates 'test)))

(define-test test-move
    "Tests if robot won't move on wall tile."
    (initialize-walls)
    (setf *treasures-found* 0
          *treasures* '('test)
          (aref *world-map* 0 0) 'w)    
    (move-robot 0 0)
    (assert-eq 0 *treasures-found*)
    (assert-eq 'w (aref *world-map* 0 0))
        
    "Tests if robot collects treasure"
    (initialize-walls)
    (setf *treasures-found* 0
          *treasures* '(test)
          (aref *world-map* 0 0) 'test)
    (move-robot 0 0)
    (assert-eq 1 *treasures-found*)
    (assert-eq 'r (aref *world-map* 0 0)))

(define-test discover-world
    (initialize-walls)
    (setf *treasures-found* 0
          *treasures* '(test test2)
          (aref *world-map* 2 2) 'test
          (aref *world-map* 3 3) 'test2)
    (discover-world)
    (assert-eq 2 *treasures-found*))