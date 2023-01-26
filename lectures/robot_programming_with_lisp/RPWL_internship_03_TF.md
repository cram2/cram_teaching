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

Check out RViz for the frames. You can change the frames in the turtle_goal_referee/src/room.lisp and /goal-publisher.lisp files

In Lisp, the following initislizes the connection to the TF buffer.
```lisp
(defparameter *tf-listener* nil)

(defun init-tf-listener ()
  (unless (eq (roslisp:node-status) :RUNNING)
    (roslisp:start-ros-node "tf-listener"))
  (setf *tf-listener* (make-instance 'cl-tf2:buffer-client)))
```

This will initialize the buffer client. Initialize it every time you restart the buffer server.
```
(init-tf-listener)
``` 

Now use the `*tf-listener*` to lookup transforms, e.g. for a goal:
```
(cl-tf2:lookup-transform *tf-listener* "map" "goal_1")
```


