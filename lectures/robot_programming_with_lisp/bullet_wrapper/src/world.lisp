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

(defun spawn (x y type &optional (color '(0.2 0.2 0.2)))
  (case (alexandria:make-keyword type)
    ((:robot :r) (spawn-turtle x y))
    ((:wall :w) (roslisp:ros-warn spawn "Don't spawn walls manually!"))
    ((:treasure :t :a :b :c :d :e :f)
     (spawn-treasure x y (intern (format nil "TREASURE~a-~a" x y)) color))
    (:depot (spawn-depot x y color))
    (otherwise (warn "~a is no known object-type." type))))


(defun spawn-turtle (x y)
  (prolog:prolog
   `(and
     (btr:bullet-world ?w)
     (assert (btr:object ?w :mesh :turtle1 ((,x ,y 0) (0 0 0 1))
                         :mass 0.2 :color (0.3 0.5 0.3) :mesh :turtle)))))

(defun spawn-treasure (x y name &optional color)
  (let ((color-values (case color
                        (:blue '(0 0 1))
                        (:red '(1 0 0))
                        (otherwise '(0 0 0))))
        (treasure-name (if name
                           name
                           (intern (concatenate 'string "TREASURE"
                                                (write-to-string (incf *treasure-id*)))))))
    (prolog:prolog `(and
                     (btr:bullet-world ?w)
                     (assert (btr:object ?w :mesh ,treasure-name ((,x ,y 1) (0 0 0 1))
                                         :mass 0.2 :color ,color-values :mesh :treasure))))))

(defun spawn-depot (x y color)
  (let ((color-values (case color
                        (:blue '(0 0 1))
                        (:red '(1 0 0))
                        (otherwise '(0 0 0))))
        (depot-name (intern (concatenate 'string (symbol-name color) "DEPOT"))))
    (prolog:prolog `(and
                     (btr:bullet-world ?w)
                     (assert (btr:object ?w :mesh ,depot-name ((,x ,y -0.8) (0 0 0 1))
                                         :mass 0.2 :color ,color-values :mesh :wall))))))


(defun get-object-at-position (x y)
  (let ((world (btr::copy-world btr:*current-bullet-world*)))
    (prolog:prolog `(and
                     (assert (btr:object ,world :mesh test-object ((,x ,y 0.0) (0 0 0 1))
                                         :mass 0.2 :color (0.3 0.5 0.3) :mesh :treasure))))
    (get-collision-objects 'test-object world)))

(defun get-collision-objects (&optional (world-to-check btr:*current-bullet-world*))
  (mapcar (lambda (collision)
            (alexandria:assoc-value collision 'bullet-wrapper::?x))
          (cut:force-ll
           (prolog:prolog `(and (btr:contact ,world-to-check :turtle1 ?x))))))

(defun delete-object (object-name)
  (btr-utils:kill-object object-name))
