For the challange we need a map. Follow the Setup guide for how to record a map.

## Get the Referee 
The Referee will set the challenge for te internship. It will run on your personal PC. Do the following to get the Referee: 
* `git pull` the repository
* start the headless docker container
* go to the `internship_ws` and build the workspace with `catkin_make`.  This will be the workspace for your code.
* `source devel/setup.bash`
* remeber to `source` this repository automatically when you open a new terminal by putting the above line into your `.bashrc` file

Before you start the Referee, bringt the tortugabot to life. We need the following launchfiles
```bash
# Tortugabot
roslaunch tortugabot_bringup base_and_joy_and_laser.launch
# in another terminal
roslaunch tortugabot_bringup amcl.launch
```
Launch your Docker Container. Then the Referee can be run.  It is a Lisp script, which will compile a lot on the first run. Execute this on your personal PC.
```bash
rosrun turtle_goal_referee referee
```
Check out the TF frames in Rviz.

## World-state and Referee behaviour 

The Referee behaves as follows:
* When the `/base_footprint` frame is close to a goal or trash, the name of the frame is published to the `/goal_picked_up` topic
* When it is close to the depot, it drops off all treasures and publishes 'unloading' to the  `/goal_picked_up` topic
* The turtugabot can only hold 2 treasures. When the robot gets near another treasure, the Referee will not register it.

To keep track how many and which goals have been picked, implement a subscriber to the `/goal_picked_up` topic.

Feel free to change the coordinates of the goals and depot in `turtle_goal_referee/src/rooms.lisp`


## Move Base
```lisp
(ros-load:load-system :actionlib)
(ros-load:load-system :move_base_msgs-msg)
(ros-load:load-system :cl-tf)

(defvar *move-base-client* nil)

(defun init-action-client ()
  (setf *move-base-client* (actionlib:make-action-client
                            "move_base"
                            "move_base_msgs/MoveBaseAction"))
  (roslisp:ros-info (navigate-map)
                    "Waiting for move_base action server...")
  ;; workaround for race condition in actionlib wait-for server
  (loop until (actionlib:wait-for-server *move-base-client*))
  (roslisp:ros-info (navigate-map) 
                    "move_base action client created."))

(defun get-action-client ()
  (when (null *move-base-client*)
    (init-action-client))
  *move-base-client*)

(defun make-move-base-goal (pose-stamped-goal)
  (actionlib:make-action-goal (get-action-client)
    target_pose pose-stamped-goal))

(defun call-move-base-action (frame-id translation rotation)
  (unless (eq roslisp::*node-status* :running)
    (roslisp:start-ros-node "move-base-lisp-client"))
  (multiple-value-bind (result status)
      (let ((actionlib:*action-server-timeout* 10.0)
            (the-goal (cl-tf:to-msg 
                       (cl-tf:make-pose-stamped
                        frame-id
                        (roslisp::ros-time)
                        translation rotation))))
        (actionlib:call-goal
         (get-action-client)
         (make-move-base-goal the-goal)))
    (roslisp:ros-info (navigate-map) "Move_base action finished.")
    (values result status)))
```

## CRAM Plan Language
Follow these tutorials to get into control structures for reactive plans.
http://cram-system.org/tutorials/beginner/controlling_turtlesim_2
http://cram-system.org/tutorials/beginner/simple_plans
http://cram-system.org/tutorials/beginner/failure_handling

For a condensed version, see `turtle_goal_referee/src/cheat-sheet.lisp`

## Where to start with your own code
Create your own package in the internship_ws/src directory.
```bash
catkin_create_pkg <YOUR-PACKAGE-NAME> actionlib_lisp cram_tf roslisp
```
Create an ASD file and depend on the packages roslisp, cram-language, actionlib, turtle_actionlib-msg, turtlesim-srv, turtlesim-msg, cl-tf, cl-tf2, cram-tf, move_base_msgs-msg

Create the `src` directory for the Lisp project. Then create `package.lisp` in the src directory and fill it. Now you can add your own files.