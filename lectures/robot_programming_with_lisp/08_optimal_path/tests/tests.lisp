(in-package optimal-path)

(define-test expand
    (assert-equal '((2 2 :NORTH) (1 2 :EAST) (1 2 :WEST)) (expand '(1 2 :NORTH))))

(define-test find-path
    (let ((world (initialize-world)))
         (dolist (treasure (treasures world))
             (let* ((path (find-path treasure))
                    (access-pose (car (last path))))
                   (when access-pose
                       "Checks if first element in path is reachable from the robots position."
                       (assert-true (valid-move (robot world) (first (car path)) (second (car path)) (third (car path))))
                       "Checks if last element in path is a valid access pose."
                       (assert-true (goal-reached (first access-pose) (second access-pose) (third access-pose) treasure))
                       "Checks if every step is valid."
                       (loop for x in path
                             for y in (rest path)
                             do (let ((robot (make-instance 'robot 
                                                            :coordinate (make-coordinate :x (first x) :y (second x))
                                                            :orientation (third x))))
                                     (assert-true (valid-move robot (first y) (second y) (third y))))))))
         
         "Checks if FIND-PATH finds path of optimal length."
         (loop for treasure in (treasures *world*)
               for path-length in '(1 3 18 16 20 21 21 25 19 0) 
               do (assert-eq path-length (length (find-path treasure))))))

(define-test remove-unreachable-treasures
    (let* ((world (initialize-world))
           (unreachable (nth 9 (treasures world))))
          (remove-unreachable-treasures world)
          (assert-eq 9 (length (treasures world)))
          (assert-false (member unreachable (treasures world)))))
         
(define-test discover-world
     (let ((world (initialize-world)))
         (discover-world world)
         (assert-false (treasures world))))