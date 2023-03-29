# Assignment 6 - Turtle Party

Welcome to Assignment 6 of the "Robot Programming with Lisp" course. (7 Points)

In this assignment you will create two ROS packages, one for learning
the ROS Lisp API through simple examples, and the other one
for playing around with the `turtlesim` simulator.
There is a roslisp tutorial that will guide you through the process.
However, in order to be able to follow the tutorial, you first should
go through a couple of other introductory tutorials for getting comfortable
with ROS. Those are the beginner tutorials number
1.1.2 - 1.1.8 and 1.1.10 (you can skip 1.1.9)
of the official ROS documentation page:
http://wiki.ros.org/ROS/Tutorials#Beginner_Level
and if the Website is down, use the archive:
https://web.archive.org/web/20181118205753/wiki.ros.org/ROS/Tutorials#Beginner_Level

Once you're done with these, you can get to the roslisp tutorial:
http://wiki.ros.org/roslisp/Tutorials/OverviewVersion

When executing the commands from the tutorial, please make sure that you
don't have any typos and that you execute the commands in the correct directory.
Some of the descriptions in the tutorial are very concise which means
that you need to understand what's going on in order to be able to follow it.
If you were attentive in the beginner tutorials that should not be a problem.
Instead of roslisp_repl you can use jupyter notebook to execute the tutorial code. 

Before you go into the assignment, build your workspace and set up your entrypoint:

* You can find your ROS workspace in `lectures/robot_programming_with_lisp/06_turtle_party/ros_ws`
  * Open a terminal into the docker container
  * navigate to it with `cd`, you should see the `src` folder when you execute `ls`.
  * compile it with `catkin_make`
  * update your sources with `source devel/setup.bash`
  * adjust your `lectures/init.sh` file, to have the workspace automatically sourced
    * open the file with `nano init.sh` or from your host machine
    * adjust the commented lines to source the workspace and run jupyter on start
    * `source /home/lectures/robot_programming_with_lisp/06_turtle_party/ros_ws/devel/setup.bash`

Here are a couple of hints for you, just in case:

* The `package.xml` and `CMakeLists.txt` files are always located in the
  root directory of your ROS package, e.g. if you have a package called
  `tutorial_ros_package`, the above-mentioned files will be
  `tutorial_ros_package/package.xml` and `tutorial_ros_package/CMakeLists.txt`.
  This holds for any ROS package.

- The `.lisp` files are usually located under the `src` directory of your package
   or in a subdirectory thereof, e.g. `tutorial_ros_package/src/some-file.lisp`
   or `lisp_turtles/src/turtles/some-other-file.lisp`.
- When you rebuild your workspace, **restart the JupterNotebook** to apply any new sources.
- If you get errors in your Lisp shell it might help to restart the Jupyter kernel completely.

Once you're done with the tutorial and your turtle is successfully creating
amazing pieces of turtle art, the things you need to add are these:
1. In the directory `lisp_turtles/src/turtles/` (from the actionlib tutorial) create a new file `turtle-party.lisp`.
    Make sure this file is loaded when the `lisp-turtles` system is loaded, i.e.,
    add the corresponding entry into the `lisp-turtles.asd` file.
2. In the file `lisp_turtles/src/turtles/turtle-party.lisp` define a function
    DRAW that accepts one argument - TURTLE-ID - such that when
    `REPL> (loop-at-most-every 1 (draw 1))` is executed the turtle with the name "turtle1" would start
    drawing random things, just as in the very end of the roslisp tutorial.
    When `REPL> (loop-at-most-every 1 (draw 2))` is executed the turtle named "turtle2" starts drawing.
3. In the same file `lisp_turtles/src/turtles/turtle-party.lisp`
    create a function TURTLE-PARTY that takes one argument - TURTLE-COUNT -
    and does the following:
   * starts a new ROS node
   * spawns (TURTLE-COUNT - 1) number of turtles
   * and calls DRAW on TURTLE-COUNT number of turtles.

That is, `(turtle-party 5)` spawns 4 new turtles and sends 5 turtles
that are by then living in the turtlesim drawing.

You have successfully completed the homework if:
- your ROS packages compile with `catkin_make`
- your ROS systems successfully loads in the Notebook
- executing `(turtle-party 5)` in the Notebook with freshly loaded
  `lisp_turtles` package sends 5 turtles in a running turtlesim drawing.

