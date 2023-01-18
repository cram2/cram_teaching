(in-package turtleref)

(defparameter *frame-id* "map")
(defparameter *depot-transform* (cl-tf:make-transform-stamped
                         *frame-id*
                         "goal_depot"
                         0.0
                         (make-3d-vector 2.3880176544189453 -3.4013960361480713 0)
                         (make-identity-rotation)))

;; To choose the depot position, use rviz to find suitable coordinates.
;; Open rviz
;; In another terminal: rostopic echo /clicked_point
;; In rviz: Click 'publish point', then click on a place in the map

(defparameter +rooms-list+
  `(
    (01 . ,(cl-tf:make-pose
            (cl-tf:make-3d-vector 9.434587478637695 2.1448171138763428 0.00247192382812)
            (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 3.14)))
    (02 . ,(cl-tf:make-pose 
            (cl-tf:make-3d-vector -9.894691467285156 -9.792372703552246 -0.00143432617188)
            (cl-tf:make-quaternion 0 0 1 1)))
    (03 . ,(cl-tf:make-pose 
            (cl-tf:make-3d-vector 4.039072036743164  5.301904678344727 -0.00143432617188)
            (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 4.71)))
    (04 . ,(cl-tf:make-pose 
            (cl-tf:make-3d-vector -8.822787284851074 -4.106620788574219 0)
            (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 3.14)))
    (05 . ,(cl-tf:make-pose 
            (cl-tf:make-3d-vector 2.936960220336914 -3.6706149578094482 0)
            (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 3.14)))
    (06 . ,(cl-tf:make-pose 
            (cl-tf:make-3d-vector 10.317888259887695 -9.25477790832519 0)
            (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 3.14)))
    (07 . ,(cl-tf:make-pose 
            (cl-tf:make-3d-vector -4.445878982543945 5.391195297241211  0)
            (cl-tf:make-quaternion 0 0 1 1)))
    (08 . ,(cl-tf:make-pose 
            (cl-tf:make-3d-vector 2.180967330932617 -8.642352104187012 0)
            (cl-tf:make-quaternion 0 0 1 1)))
    (09 . ,(cl-tf:make-pose 
            (cl-tf:make-3d-vector -0.7341127395629883 -10.236560821533203 0)
            (cl-tf:make-quaternion 0 0 1 1)))))
