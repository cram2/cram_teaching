;;;
;;; Copyright (c) 2022, Arthur Niedzwiecki <aniedz@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Institute for Artificial Intelligence/
;;;       Universitaet Bremen nor the names of its contributors may be used to
;;;       endorse or promote products derived from this software without
;;;       specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :btr-wrapper)

(defun teleport-turtle (x y &optional orientation treasure-name1 treasure-name2)
  ;; <<<<<<<<<< fix getting the treasure names and failure handling
  "Move the turtle to the given x and y coordinate on the map."
  (let ((turtle (btr:object btr:*current-bullet-world* :turtle1)))
    (if turtle
        (let ((world-copy (btr::copy-world btr:*current-bullet-world*))
              (orientation-array (if orientation
                                     (case orientation
                                       (:NORTH '(0 0 0 1))
                                       (:SOUTH '(0 0 1 0))
                                       (:EAST '(0 0 -0.707d0 0.707d0))
                                       (:WEST '(0 0 0.707d0 0.707d0)))
                                     (with-slots (cl-tf:w cl-tf:x cl-tf:y cl-tf:z)
                                         (cl-tf::orientation (btr:pose turtle))
                                       (list cl-tf:x cl-tf:y cl-tf:z cl-tf:w)))))
          (prolog:prolog
           `(and (assert (btr:object-pose ,world-copy :turtle1 ((,x ,y 0) (0 0 0 1))))))
          (if (not (is-turtle-in-collision :turtle1 world-copy))
              ;; Seperating possible movement of treasures from robot, to prevent warnings.
              (when (progn (when treasure-name1
                             (prolog:prolog
                              `(and (btr:bullet-world ?w)
                                    (assert (btr:object-pose ?w ,treasure-name1
                                                             ((,x ,(+ y 0.085) 0.37) (0 0 0 1)))))))
                           (when treasure-name2
                             (prolog:prolog
                              `(and (btr:bullet-world ?w)
                                    (assert (btr:object-pose ?w ,treasure-name2
                                                             ((,x ,(- y 0.085) 0.37) (0 0 0 1)))))))
                           (prolog:prolog
                            `(and (btr:bullet-world ?w)
                                  (assert (btr:object-pose ?w :turtle1
                                                           ((,x ,y 0) ,orientation-array))))))
                (format nil "Successfully moved the turtle to x:~a y:~a." x y))
              (warn "BANG! Collision!")))
        (warn "Robot with name :turtle1 not found in the bullet world."))))

(defun move-turtle (turtle-name &optional (forwards t))
  "Move the turtle of given `turtle-name' forwards. Can be moved backwards, by setting `forwards' to NIL."
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (let* ((pose-offset (if forwards
                                (get-turtle-direction turtle-name)
                                (map 'list (lambda (coord) (* -1 coord)) (get-turtle-direction turtle-name))))
               (new-pose (with-slots (cl-tf::x cl-tf::y cl-tf::z)
                             (cl-tf:origin (btr:object-pose turtle-name))
                           (list (+ cl-tf:x (first pose-offset)) (+ cl-tf:y (second pose-offset)) cl-tf:z)))
               (orientation (with-slots (cl-tf:w cl-tf:x cl-tf:y cl-tf:z)
                                (cl-tf::orientation (btr:pose turtle))
                              (list cl-tf:x cl-tf:y cl-tf:z cl-tf:w)))
               (world-copy (btr::copy-world btr:*current-bullet-world*)))
          (prolog:prolog
             `(and (assert (btr:object-pose ,world-copy ,turtle-name (,new-pose (0 0 0 1))))))
          (if (not (is-turtle-in-collision turtle-name world-copy))
              (when (prolog:prolog
                      `(and (btr:bullet-world ?w)
                            (assert (btr:object-pose ?w ,turtle-name (,new-pose ,orientation)))))
                     (format t "Successfully moved the turtle to x:~a y:~a." (first new-pose) (second new-pose)))
              (warn "BANG! Collision!")))
        (warn "Robot with name ~a not found n the bullet world." turtle-name))))

(defun get-turtle-direction (turtle-name)
  (let ((z-orientation (cl-tf::z (cl-tf::orientation (btr:pose (btr:object btr:*current-bullet-world* turtle-name))))))
    (alexandria:switch (z-orientation :test #'<)
      (-0.7d0 '(0 -1)) ;; East
      (0.1d0 '(1 0)) ;; North
      (0.71d0 '(0 1)) ;; West
      (otherwise '(-1 0))))) ;;South

(defun turn-turtle (turtle-name direction)
  "The direction is either :left or :right."
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (flet ((change-angle-upon-direction (angle repair-angle)
                 (when repair-angle
                   (setf angle (* -1 angle)))
                 (case direction
                    (:left (+ angle (* pi 0.5d0)))
                    (:right (- angle (* pi 0.5d0)))
                    (otherwise angle))))
          (let* ((turtle-pose
                   (with-slots (cl-tf::x cl-tf::y cl-tf::z)
                       (cl-tf:origin (btr:object-pose turtle-name))
                     (list cl-tf:x cl-tf:y cl-tf:z)))
                 (turtle-orientation
                   (cl-tf::orientation (btr:pose turtle)))
                 (new-orientation
                   (with-slots (cl-tf:w cl-tf:x cl-tf:y cl-tf:z)
                       (cl-tf:axis-angle->quaternion
                        #(0 0 1)
                        (change-angle-upon-direction
                         (second (multiple-value-list
                                  (cl-tf:quaternion->axis-angle turtle-orientation)))
                         (< (slot-value turtle-orientation 'cl-tf:z) 0)))
                     (list cl-tf:x cl-tf:y cl-tf:z cl-tf:w))))
            (prolog:prolog `(and (btr:bullet-world ?w)
                                 (assert (btr:object-pose ?w
                                                          ,turtle-name
                                                          (,turtle-pose ,new-orientation)))))))
        (warn "Robot with name ~a not found n the bullet world." turtle-name))))

(defun turn-turtle-into-direction (turtle-name direction)
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (let ((orientation (case direction
                             (:NORTH '(0 0 0 1))
                             (:SOUTH '(0 0 1 0))
                             (:EAST '(0 0 -0.707d0 0.707d0))
                             (:WEST '(0 0 0.707d0 0.707d0))))
              (turtle-pose
                   (with-slots (cl-tf::x cl-tf::y cl-tf::z)
                       (cl-tf:origin (btr:object-pose turtle-name))
                     (list cl-tf:x cl-tf:y cl-tf:z))))
          (prolog:prolog `(and (btr:bullet-world ?w)
                                 (assert (btr:object-pose ?w
                                                          ,turtle-name
                                                          (,turtle-pose ,orientation)))))))))

(defun is-turtle-in-collision (&optional
                                 (world-to-check btr:*current-bullet-world*)
                                 (object-type :wall))
  (let ((collision-object-types
          (mapcar (lambda (collision-name)
                    (btr::item-types (btr:object world-to-check collision-name)))
                  (get-collision-objects world-to-check))))
    (member object-type (alexandria:flatten collision-object-types))))


