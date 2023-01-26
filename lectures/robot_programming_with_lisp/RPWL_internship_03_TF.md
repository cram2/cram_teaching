# Find Goals

Start the TF buffer client, then the turtle goal referee

```bash
# Docker
rosrun tf2_ros buffer_server
```

```bash
# Docker, different terminal
rosrun turtle_goal_referee referee
```

Check out RViz for the frames. Add TF to the Display for that. You can also use `rosrun tf tf_echo "map" "goal_1"` to directly monitor the transform of goal_1.

In Lisp, the following initializes the connection to the TF buffer.
```lisp
(defparameter *tf-listener* nil)

(defun init-tf-listener ()
  (setf *tf-listener* (make-instance 'cl-tf2:buffer-client)))
```
Start a ros node
```lisp
(roslisp:start-ros-node "tf-listener")
```
Change the frames in the turtle_goal_referee/src/room.lisp and /goal-publisher.lisp files to position the goals, trash and depots as you like.
This will initialize the buffer client. Initialize it every time you restart the buffer server.
```lisp
(init-tf-listener)
``` 

Now use the `*tf-listener*` to lookup transforms, e.g. for a goal:
```
(cl-tf2:lookup-transform *tf-listener* "map" "goal_1")
```


