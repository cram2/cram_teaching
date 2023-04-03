(in-package optimal-path)

(define-test expand
    (assert-equal (expand '(1 2 :NORTH)) '((2 2 :NORTH) (1 2 :EAST) (1 2 :WEST))))

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
                       (assert-true (every #'(lambda (x y) (valid-move (make-instance 'robot
                                                                                      :coordinate (make-coordinate :x (first x) :y (second x))
                                                                                      :orientation (third x))
                                                                       (first y) (second y) (third y))) path (rest path))))))
         "Checks if FIND-PATH finds path of optimal length."
         (assert-true (every #'(lambda (x y) (eq (length (find-path x)) y)) (treasures *world*) '(1 3 18 16 20 21 21 25 19 0)))))

(define-test remove-unreachable-treasures
    (let* ((world (initialize-world))
           (unreachable (nth 9 (treasures world))))
          (remove-unreachable-treasures world)
          (assert-eq (length (treasures world)) 9)
          (assert-false (member unreachable (treasures world)))))
         
(define-test discover-world
     (let ((world (initialize-world)))
         (discover-world world)
         (assert-false (treasures world))))