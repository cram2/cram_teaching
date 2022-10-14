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

(defun collect-treasure (turtle-name)
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (let* ((pose-offset (get-turtle-direction turtle-name))
              (treasure-pose (with-slots (cl-tf::x cl-tf::y cl-tf::z)
                                 (cl-tf:origin (btr:object-pose turtle-name))
                               (list (+ cl-tf:x (first pose-offset)) (+ cl-tf:y (second pose-offset)) cl-tf:z)))
              (world-copy (btr::copy-world btr:*current-bullet-world*)))
          (prolog:prolog
             `(and (assert (btr:object-pose ,world-copy ,turtle-name (,treasure-pose (0 0 0 1))))))
          (if (is-turtle-in-collision turtle-name world-copy :treasure)
              (mapcar #'delete-object (get-collision-objects turtle-name world-copy))
              (warn "No treasure to collect. Does the turtle look into the right direction?")))
        (warn "Robot with name ~a not found n the bullet world." turtle-name))))

(defun get-object-in-view (turtle-name)
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (let* ((pose-offset (get-turtle-direction turtle-name))
              (poi-pose (with-slots (cl-tf::x cl-tf::y cl-tf::z)
                                 (cl-tf:origin (btr:object-pose turtle-name))
                               (list (+ cl-tf:x (first pose-offset)) (+ cl-tf:y (second pose-offset)) cl-tf:z)))
              (world-copy (btr::copy-world btr:*current-bullet-world*)))
          (prolog:prolog
             `(and (assert (btr:object-pose ,world-copy ,turtle-name (,poi-pose (0 0 0 1))))))
          (get-collision-objects turtle-name world-copy))
        (warn "Robot with name ~a not found n the bullet world." turtle-name))))

(defun carry-treasure (turtle-name treasure-name)
  (let ((treasures-carrying (length (cut:force-ll
                                     (prolog:prolog `(and (btr:bullet-world ?w)
                                                          (btr:contact ?w ,turtle-name ?x)))))))
    (if (< treasures-carrying 2)
        (when (prolog:prolog `(and (btr:bullet-world ?w)
                                   (btr:object-pose ?w ,turtle-name ?x)
                                   (assert (btr:object-pose-on ?w ,treasure-name ?x))))
          (format t "Successfully collected treasure ~a!" treasure-name))
        (warn "Something went wrong collecting the treasure!~%
        The turtle already carries ~a objects." treasures-carrying))))
