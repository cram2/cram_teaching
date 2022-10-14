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

(defparameter *treasure-id* 0)
(defparameter *world-initialized* nil)

(defun init-world ()
  (setf *treasure-id* 0)
  (setf *world-initialized* nil)
  (setf btr:*current-bullet-world* (make-instance 'btr:bt-reasoning-world))
  (add-objects-to-mesh-list)
  (prolog:prolog '(and
                   (btr:bullet-world ?w)
                   (btr:debug-window ?w)
                   (assert (btr:object ?w
                            :static-plane floor ((0.5 0.5 -.02) (0 0 0 1))
                            :normal (0 0 1) :constant 0))
                   (assert (btr:object ?w
                            :mesh :walls ((0.0 0.0 0.0) (0 0 0 1))
                            :mass 0.2 :color (0.2 0.2 0.2) :mesh :walls)))))

(defun add-objects-to-mesh-list (&optional (resource-path "/home/lectures/robot_programming_with_lisp/bullet_wrapper"))
  (let ((mesh-keys (map 'list #'first btr::*mesh-files*)))
    (unless (and (member :turtle mesh-keys)
                 (member :walls mesh-keys)
                 (member :treasure mesh-keys))
      (mapcar (lambda (object-filename-and-object-extension)
                (declare (type list object-filename-and-object-extension))
                (destructuring-bind (object-filename object-extension)
                    object-filename-and-object-extension
                  (let ((lisp-name (roslisp-utilities:lispify-ros-underscore-name
                                    object-filename :keyword)))
                    (pushnew (list lisp-name
                                   (format nil
                                           "~a/resource/~a.~a"
                                           resource-path
                                           object-filename
                                           object-extension)
                                   nil)
                             btr::*mesh-files*
                             :key #'car)
                    lisp-name)))
              (mapcar (lambda (pathname)
                        (list (pathname-name pathname) (pathname-type pathname)))
                      (directory (physics-utils:parse-uri
                                  (format nil
                                          "~a/resource/*.*" 
                                          resource-path))))))))

