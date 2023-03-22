# Assignment demo script

Inside `cram_container`:

``` 
roscd lisp_turtles
roslaucnch launch/turtle-party.launch
```

Inside notebook:
- Open new launcher
- Start a new terminal session

```
# roslisp_repl
```

- Turtlesim window left and roslisp_repl window right. Both on `Always in front`.
- Copy code from `assignment.ipynb` to `roslisp_repl`

```
CL_USER> (let* ((lecture-path '(:absolute "home" "lectures" "robot_programming_with_lisp"))
       (assignment-path (append lecture-path '("06_turtle_party" "ros_ws" "src" "lisp_turtles"))))
      (pushnew (make-pathname :directory assignment-path) asdf:*central-registry*))

CL_USER> (asdf:load-system :lisp-turtles)

CL_USER> (in-package :lturtle)

LTURTLE> (start-node)

LTURTLE> (draw 1)

LTURTLE> (loop-at-most-every 1 (draw 1))

LTURTLE> (turtle-party 5)
```

